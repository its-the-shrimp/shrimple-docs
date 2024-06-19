use {
    std::{
        collections::HashMap,
        convert::identity,
        ffi::OsStr,
        fs::File,
        io::{BufRead, BufReader, Write},
        path::{Path, PathBuf},
        process::Stdio,
        ptr::{addr_of, addr_of_mut},
        sync::Arc,
        time::Instant,
    },
    anyhow::{bail, Context},
    rustdoc_types::{Crate, ExternalCrate, Id, Item, ItemKind, ItemSummary, Type, FORMAT_VERSION},
    serde::Deserialize,
    tokio::{process::Command, task::JoinSet, try_join},
    crossterm::{cursor::MoveToPreviousLine, ExecutableCommand},
    crate::{
        item_visitor::VisitorMut,
        utils::{levenshtein, BoolExt, EmptyError, Result, BOLD, CLEARLINE, GREEN, NOSTYLE, OK},
    },
};

/// contained in [`rustdoc_types::FnDecl::inputs`]
pub type FnArg = (String, Type);

pub struct Infer;

/// Needed for the auto-generated impl in [`crate::item_visitor`]
impl<'res> From<Infer> for &'res Infer {
    fn from(value: Infer) -> Self {
        unsafe { &*addr_of!(value) }
    }
}

/// Needed for the auto-generated impl in [`crate::item_visitor`]
impl<'res> From<Infer> for &'res mut Infer {
    fn from(mut value: Infer) -> Self {
        unsafe { &mut *addr_of_mut!(value) }
    }
}

pub type Lifetime = String;

#[derive(Debug, Deserialize)]
pub struct Target<'src> {
    pub name: &'src str,
    pub kind: Vec<&'src str>,
}

#[derive(Debug, Deserialize)]
struct Package<'src> {
    /// Changed to the lib name, or name with hyphens replaced with underscores
    name: String,
    #[serde(borrow)]
    targets: Vec<Target<'src>>,
    manifest_path: String,
}

impl<'src> Package<'src> {
    /// Converting the lifetime to `'static` is safe since the `targets` vector is emptied.
    fn normalise_name(self) -> Package<'static> {
        Package {
            name: self.targets
                .iter()
                .find(|t| t.kind.contains(&"lib"))
                .map_or_else(|| self.name.replace('-', "_"), |t| t.name.into()),
            targets: vec![],
            manifest_path: self.manifest_path
        }
    }
}

#[derive(Deserialize)]
/// Output of `cargo metadata` with only the fields that matter to us
struct CargoMetadata<'src> {
    #[serde(borrow)]
    packages: Vec<Package<'src>>,
}

struct IdNormaliser<'docs> {
    paths: &'docs HashMap<Id, ItemSummary>,
    crate_name: &'docs str,
    crates: &'docs HashMap<u32, ExternalCrate>,
    temp: String,
}

impl VisitorMut for IdNormaliser<'_> {
    #[allow(clippy::cast_possible_wrap)]
    fn visit_id(&mut self, x: &mut Id) -> Result {
        if let Some(item) = self.paths.get(x) {
            x.0.clear();
            x.0.push_str(match item.kind {
                ItemKind::Module if item.path.len() == 1 => "crate ",
                ItemKind::Module => "mod ",
                ItemKind::ExternCrate => "crate ",
                ItemKind::Struct => "struct ",
                ItemKind::Union => "union",
                ItemKind::Enum => "enum ",
                ItemKind::Function => "fn ",
                ItemKind::TraitAlias | ItemKind::Trait => "trait ",
                ItemKind::Static => "static ",
                ItemKind::Macro => "macro ",
                ItemKind::ProcAttribute => "attr ",
                ItemKind::ProcDerive => "derive ",
                ItemKind::AssocConst | ItemKind::Constant => "const ",
                ItemKind::AssocType | ItemKind::TypeAlias => "type ",
                ItemKind::Keyword => "keyword ",
                _ => "$:",
            });
            let Some((first, rest)) = item.path.split_first() else {
                return OK;
            };
            x.0.push_str(first);
            x.0.extend(rest.iter().flat_map(|x| ["::", x]));
        } else {
            let off = matches!(x.0.as_bytes(), [b'a' | b'b', b':', ..]).pick(2, 0);
            if off == 0 {
                self.temp.insert_str(0, "$:");
            }
            for id in x.0[off..].split_inclusive('-') {
                let (crate_id_str, rest) = id.split_once(':').unwrap_or((id, ""));
                let crate_id = crate_id_str
                    .parse()
                    .with_context(|| format!("item ID {:?} is malformatted", x.0))?;
                let crate_name = match crate_id {
                    0 => self.crate_name,
                    _ => self.crates.get(&crate_id)
                        .map(|x| &x.name)
                        .with_context(|| {
                            format!("unknown crate ID: {crate_id_str:?}\n\tfull ID: {}", x.0)
                        })?
                };
                self.temp.push_str(crate_name);
                self.temp.push(':');
                self.temp.push_str(rest);
            }
            x.0.clone_from(&self.temp);
            self.temp.clear();
        }
        OK
    }
}

// TODO: check why can't this Vec be an opaque iterable
fn parse_json_docs(path: impl AsRef<Path>) -> Result<Vec<(Id, Item)>> {
    let path = path.as_ref();
    let docs: Crate = match serde_json::from_reader(BufReader::new(File::open(path)?)) {
        Ok(x) => x,
        Err(e) => {
            #[derive(Deserialize)]
            struct V {
                format_version: u32,
            }
            let V { format_version } = serde_json::from_reader(File::open(path)?)?;
            if format_version != FORMAT_VERSION {
                bail!("incompatible version of rustdoc's JSON output: expected {}, got {}",
                    FORMAT_VERSION, format_version);
            }
            bail!(e)
        }
    };

    if docs.format_version != FORMAT_VERSION {
        bail!("incompatible version of rustdoc's JSON output: expected {}, got {}",
            FORMAT_VERSION, docs.format_version);
    }

    let mut id_normaliser = IdNormaliser {
        paths: &docs.paths,
        crate_name: path.file_name().and_then(OsStr::to_str)
            .context("no crate name extracted from the path to the JSON file")?,
        crates: &docs.external_crates,
        temp: String::new(),
    };
    
    docs.index.into_iter()
        .filter(|(k, _)| k.0.starts_with("0:"))
        .map(|(mut k, mut item)| {
            id_normaliser.visit_id(&mut k)?;
            id_normaliser.visit_item(&mut item)?;
            Ok((k, item))
        })
        .collect()
}

async fn document_crate(
    name: impl AsRef<str> + Send,
    manifest_path: impl AsRef<str> + Send,
    toolchain: impl AsRef<str> + Send,
) -> Result<impl IntoIterator<Item = (Id, Item)>> {
    let manifest_path = manifest_path.as_ref();
    let name = name.as_ref();
    let docs_gen = Command::new("rustup")
        .args([
            "run",
            toolchain.as_ref(),
            "cargo",
            "rustdoc",
            //"--all-features",
            "--manifest-path", manifest_path,
            "--color", "always",
            "-Zunstable-options",
            "--output-format", "json",
        ])
        .stderr(Stdio::piped())
        .stdout(Stdio::null())
        .output().await?;
    if !docs_gen.status.success() {
        bail!("`cargo rustdoc` failed; output:\n{}", String::from_utf8_lossy(&docs_gen.stderr));
    }

    let mut docs_path = Path::new(manifest_path)
        .parent().context("invalid Cargo manifest path")?
        .join("target/doc");
    docs_path.push(name);
    docs_path.set_extension("json");

    parse_json_docs(&docs_path)
}

async fn get_packages(toolchain: &str) -> Result<Vec<Package<'static>>> {
    let cmd = Command::new("rustup")
        .args([
            "run",
            toolchain,
            "cargo",
            "metadata",
            "--format-version=1",
        ])
        .stderr(Stdio::inherit())
        .output().await?;
    if !cmd.status.success() {
        bail!("`cargo metadata` failed");
    }
    let CargoMetadata { packages } = serde_json::from_slice(&cmd.stdout)?;
    // TODO: sort & collect simultaneously?
    let mut packages: Vec<_> = packages.into_iter().map(Package::normalise_name).collect();
    packages.sort_unstable_by(|p1, p2| p1.name.cmp(&p2.name));
    Ok(packages)
}

async fn get_std_docs_dir(toolchain: &str) -> Result<PathBuf> {
    let mut cmd = Command::new("rustup")
        .args(["show", "home"])
        .stderr(Stdio::inherit())
        .output().await?;
    if !cmd.status.success() {
        bail!("`rustup show home` failed");
    }
    let nl_at = cmd.stdout.iter().position(|&b| b == b'\n').unwrap_or(cmd.stdout.len());
    cmd.stdout.truncate(nl_at);
    let mut res = PathBuf::from(String::from_utf8(cmd.stdout)?);
    res.push("toolchains");
    res.push(toolchain);
    res.push("share/doc/rust/json/");
    Ok(res)
}

/// Filled in by [`Docs::search`]
pub struct SearchResult<'docs> {
    /// [`Id`] usable to index the docs directly.
    pub id: &'docs Id,
    /// Levenshtein distance from the searched term.
    pub distance: usize,
}

/// Aggregator of all the docs
#[derive(Default)]
pub struct Docs {
    pub index: HashMap<Id, Item>,
}

impl Docs {
    pub async fn new(
        toolchain: &Arc<str>,
        r#in: &mut (impl BufRead + Send),
        out: &mut (impl Write + Send),
    ) -> Result<Self> {
        let mut res = Self::default();

        writeln!(out, "{GREEN}{BOLD}Extracting{NOSTYLE} crate & system metadata")?;
        let (packages, std_docs_dir) = try_join!(
            get_packages(toolchain),
            get_std_docs_dir(toolchain),
        )?;

        let include_std_docs = std_docs_dir.try_exists()? || {
            writeln!(out, "Do you wish to install the docs for the standard library?")?;
            write  !(out, "Answer (y/n, anything else will abort the program): ")?;
            out.flush()?;
            let mut resp = [0u8; 2];
            r#in.read_exact(&mut resp)?;
            match &resp {
                b"y\n" => true,
                b"n\n" => false,
                _ => bail!(EmptyError),
            };
            if !Command::new("rustup")
                .args(["run", toolchain, "rustup", "component", "add", "rust-docs-json"])
                .status().await?
                .success()
            {
                bail!("`rustup run {toolchain} rustup component add rust-docs-json` failed");
            }
            true
        };

        let std_libs: Vec<PathBuf> = if include_std_docs {
            std_docs_dir.read_dir()?.map(|x| x.map(|x| x.path())).collect()
        } else {
            Ok(vec![])
        }?;
        let n_libs = packages.len() + std_libs.len();
        let mut n_processed = 0usize;
        let start = Instant::now();
        writeln!(out, "{GREEN}{BOLD}Documenting{NOSTYLE} libraries ... 0 / {n_libs}")?;

        for file in std_libs {
            res.index.extend(parse_json_docs(file)?);
            n_processed += 1;
            out.execute(MoveToPreviousLine(1))?;
            writeln!(out, "{CLEARLINE}{GREEN}{BOLD}Documenting{NOSTYLE} libraries ... \
                         {n_processed} / {n_libs}")?;
        }

        let mut documentors: JoinSet<_> = packages
            .into_iter()
            .map(|p| document_crate(p.name, p.manifest_path, toolchain.clone()))
            .collect();

        while let Some(items) = documentors.join_next().await {
            res.index.extend(items??);
            n_processed += 1;
            out.execute(MoveToPreviousLine(1))?;
            writeln!(out, "{CLEARLINE}{GREEN}{BOLD}Documenting{NOSTYLE} libraries ... \
                         {n_processed} / {n_libs}")?;
        }

        let duration = start.elapsed();
        out.execute(MoveToPreviousLine(1))?;
        writeln!(out, "{CLEARLINE}{GREEN}{BOLD}Documenting{NOSTYLE} libraries ... \
                     finished in {:.2} seconds", duration.as_secs_f64())?;
        Ok(res)
    }

    pub fn search<'docs>(&'docs self, term: &str, dst: &mut Vec<SearchResult<'docs>>) {
        dst.clear();
        let (item_kind, term) = term.split_once(' ').unwrap_or(("", term));
        for id in self.index.keys() {
            if matches!(id.0.as_bytes(), [b'a' | b'b' | b'$', b':', ..])
                || !id.0.starts_with(item_kind)
            {
                continue;
            }
            let distance = levenshtein(term, id.0.rsplit_once(':').map_or(&id.0, |x| x.1));
            let place = dst.binary_search_by_key(&distance, |res| res.distance)
                .unwrap_or_else(identity);
            dst.insert(place, SearchResult { id, distance });
        }
    }
}
