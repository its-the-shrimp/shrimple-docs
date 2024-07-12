use {
    crate::{
        item_visitor::VisitorMut,
        utils::{
            BoolExt,
            EmptyError,
            Exit,
            IteratorExt,
            Result,
            BOLD,
            GREEN,
            NOSTYLE,
            OK,
        }
    },
    anyhow::{bail, ensure, Context},
    rustdoc_types::{Crate, ExternalCrate, Id, Item, ItemKind, ItemSummary, Type, FORMAT_VERSION},
    serde::Deserialize,
    serde_json::Value,
    std::{
        collections::HashMap,
        ffi::OsStr,
        fs::File,
        io::{BufRead, BufReader, Read, Write},
        mem::take,
        path::{Path, PathBuf},
        process::{Output, Stdio},
        ptr::{addr_of, addr_of_mut},
        sync::Arc,
        vec,
    },
    tokio::{process::Command, try_join},
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
struct Dep<'src> {
    #[serde(borrow)]
    name: &'src str,
    features: Vec<&'src str>,
}

#[derive(Debug, Deserialize)]
struct Target<'src> {
    name: &'src str,
    kind: Vec<&'src str>,
}

#[derive(Debug, Deserialize)]
struct Package<'src> {
    id: &'src str,
    name: String,
    #[serde(default)]
    metadata: Value,
    #[serde(borrow)]
    targets: Vec<Target<'src>>,
    dependencies: Vec<Dep<'src>>,
    manifest_path: String,
}

#[derive(Debug, Deserialize)]
struct Resolve<'src> {
    root: &'src str,
}

#[derive(Deserialize)]
/// Output of `cargo metadata` with only the fields that matter to us
struct CargoMetadata<'src> {
    #[serde(borrow)]
    packages: Vec<Package<'src>>,
    resolve: Resolve<'src>,
    target_directory: PathBuf,
}

/// Created by processing [`CargoMetadata`]
pub struct DocsGen {
    packages: vec::IntoIter<Documentable>,
    toolchain: Arc<str>,
    target_directory: PathBuf,
}

impl DocsGen {
    pub async fn document_next(&mut self, offline: bool)
        -> Option<Result<(Vec<(Arc<str>, Item)>, Arc<str>)>>
    {
        let next = self.packages.next()?;
        let name = next.name.clone();
        Some(next.document(offline, &self.toolchain, &self.target_directory).await.map(|x| (x, name)))
    }
}

struct IdNormaliser<'docs> {
    paths: &'docs HashMap<Id, ItemSummary>,
    crate_name: &'docs str,
    crates: &'docs HashMap<u32, ExternalCrate>,
    temp: String,
}

impl VisitorMut for IdNormaliser<'_> {
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
            self.temp.push_str("$:");
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

fn parse_json_docs(path: impl AsRef<Path>) -> Result<Vec<(Arc<str>, Item)>> {
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
        .map(|(mut k, mut item)| {
            id_normaliser.visit_id(&mut k)?;
            id_normaliser.visit_item(&mut item)?;
            Ok((k.0.into(), item))
        })
        .collect()
}

#[derive(Debug, Default, Deserialize)]
#[serde(default, rename_all = "kebab-case")]
/// Value of `package.metadata.docs.rs` in a crate
struct DocConfig {
    features: Vec<String>,
    rustdoc_args: Vec<String>,
    cargo_args: Vec<String>,
    rustc_args: Vec<String>,
    all_features: bool,
    no_default_features: bool,
}

/// A package & its flags as given by the package for docs.rs
struct Documentable {
    // TODO: allow for documenting multiple crates defined by 1 package
    name: Arc<str>,
    manifest_path: String,
    config: DocConfig,
}

impl TryFrom<Package<'_>> for Documentable {
    type Error = anyhow::Error;

    fn try_from(mut p: Package) -> Result<Self, Self::Error> {
        let raw_config = take(&mut p.metadata["docs"]["rs"]);
        let mut config = serde_json::from_value::<Option<DocConfig>>(raw_config)?
                .unwrap_or_default();
        config.rustdoc_args.retain(|a| !matches!(&**a, "--generate-link-to-definition"));
        Ok(Self {
            name: p.targets
                .iter()
                .find(|t| t.kind.contains(&"lib"))
                .map_or_else(|| p.name.replace('-', "_").into(), |t| t.name.into()),
            manifest_path: p.manifest_path,
            config,
        })
    }
}

impl Documentable {
    async fn document(
        self,
        offline: bool,
        toolchain: impl AsRef<str> + Send,
        target_directory: impl AsRef<Path> + Send,
    ) -> Result<Vec<(Arc<str>, Item)>> {
        let target_directory = target_directory.as_ref();
        let mut cmd = Command::new("rustup");
        cmd
            .args(["run", toolchain.as_ref(), "cargo"])
            .args(self.config.cargo_args)
            .args(["rustdoc", "--verbose", "--manifest-path", &self.manifest_path, "--target-dir"])
            .arg(target_directory)
            .args(offline.then_some("--offline"))
            .args(self.config.all_features.then_some("--all-features"))
            .args(self.config.no_default_features.then_some("--no-default-features"))
            .args(self.config.features.iter().flat_map(|f| ["-F", f]))
            .args(["-Zunstable-options", "--output-format=json", "--", "--cfg", "docsrs"])
            .args(self.config.rustdoc_args)
            .env("DOCSRS", "")
            .env("CARGO_ENCODED_RUSTFLAGS", self.config.rustc_args.join("\x1f"))
            .stdout(Stdio::null());
        let Output { status, stderr, .. } = cmd
            .output().await
            .context("failed to launch `rustup run cargo rustdoc`")?;
        if !status.success() {
            bail!("`cargo rustdoc` failed;\ncommand: {cmd:#?}\noutput:\n{}",
                String::from_utf8_lossy(&stderr));
        }

        let mut docs_path = target_directory.join("doc");
        docs_path.push(&*self.name);
        docs_path.set_extension("json");

        parse_json_docs(docs_path)
    }
}

async fn get_gen_ctx(toolchain: Arc<str>) -> Result<DocsGen> {
    let Output { status, stdout, .. } = Command::new("rustup")
        .args(["run", &toolchain, "cargo", "metadata", "--format-version=1"])
        .stderr(Stdio::inherit())
        .output().await?;
    ensure!(status.success(), "`cargo metadata` failed");

    let mut metadata: CargoMetadata = serde_json::from_slice(&stdout)?;
    let deps = metadata.packages.iter_mut()
        .find(|x| x.id == metadata.resolve.root)
        .map(|x| take(&mut x.dependencies))
        .context("couldn't find the root package")?;
    let mut packages: Vec<Documentable> = metadata.packages.into_iter()
        .map(Documentable::try_from)
        .try_collect()?;

    for Dep { name, features } in deps {
        if features.is_empty() { continue }
    
        let dep = packages.iter_mut().find(|x| &*x.name == name)
            .with_context(|| format!("failed to unify dependency features: package {name:?} \
                                      not found"))?;
        if !dep.config.all_features {
            dep.config.features.extend(features.into_iter().map(Into::into));
        }
    }

    Ok(DocsGen {
        packages: packages.into_iter(),
        toolchain,
        target_directory: metadata.target_directory,
    })
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
pub struct SearchResult {
    /// ID usable to index the docs directly.
    pub id: Arc<str>,
}

async fn get_nightly_toolchain(r#in: &mut (impl Read + Send), out: &mut (impl Write + Send))
    -> Result<Arc<str>>
{
    let toolchains = Command::new("rustup")
        .args(["toolchain", "list"])
        .stderr(Stdio::inherit())
        .stdout(Stdio::piped())
        .output().await?
        .stdout;
    let toolchains = String::from_utf8(toolchains).context("`rustup` gave non-UTF8 output")?;

    Ok(if let Some(x) = toolchains.lines().rfind(|x| x.starts_with("nightly")) {
        Arc::<str>::from(x.split_once(' ').map_or(x, |x| x.0))
    } else {
        writeln!(out, "It appears that you don't have a nightly Rust toolchain installed.")?;
        writeln!(out, "A nightly toolchain is essential for this tool to function.")?;
        writeln!(out, "Do you wish to install it?")?;
        write  !(out, "Answer (y/n, anything else will abort the program): ")?;
        out.flush()?;

        let mut resp = [0u8; 2];
        r#in.read_exact(&mut resp)?;
        match &resp {
            b"y\n" => {}
            b"n\n" => bail!(Exit),
            _ => bail!(EmptyError),
        };

        let status = Command::new("rustup")
            .args(["toolchain", "install", "nightly", "--component", "rust-docs-json"])
            .stderr(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status().await
            .context("failed to launch `rustup toolchain install nightly`")?;
        ensure!(status.success(), "`rustup` failed");

        Arc::from("nightly")
    })
}

/// Aggregator of all the docs
pub struct Docs {
    fzf_input_path: Box<Path>,
    index: HashMap<Arc<str>, Item>,
}

impl Docs {
    pub async fn new(
        r#in: &mut (impl Read + Send),
        out: &mut (impl Write + Send),
        offline: bool,
    ) -> Result<Self> {
        let mut index = HashMap::new();
        let toolchain = get_nightly_toolchain(r#in, out).await?;

        writeln!(out, "{GREEN}{BOLD}Extracting{NOSTYLE} crate and system metadata")?;
        let (mut ctx, std_docs_dir) = try_join! {
            get_gen_ctx(toolchain.clone()),
            get_std_docs_dir(&toolchain),
        }?;

        let include_std_docs = std_docs_dir.try_exists()? || 'install_docs: {
            writeln!(out, "Do you wish to install the docs for the standard library?")?;
            write  !(out, "Answer (y/n, anything else will abort the program): ")?;
            out.flush()?;
            let mut resp = [0u8; 2];
            r#in.read_exact(&mut resp)?;
            match &resp {
                b"y\n" => {}
                b"n\n" => break 'install_docs false,
                _ => bail!(EmptyError),
            };
            if !Command::new("rustup")
                .args(["run", &toolchain, "rustup", "component", "add", "rust-docs-json"])
                .status().await?
                .success()
            {
                bail!("`rustup component add rust-docs-json` failed");
            }
            true
        };

        let fzf_input_path = ctx.target_directory.join(".fzfinput").into_boxed_path();
        let mut fzf_input = File::create(&fzf_input_path)?;

        if include_std_docs {
            for entry in std_docs_dir.read_dir()? {
                let file = entry?.path();
                let name = Path::new(file.file_stem().context("no standard library name")?)
                    .display();
                writeln!(out, "{GREEN}{BOLD}Documenting{NOSTYLE} {name}")?;
                let docs = parse_json_docs(file)?;
                index.reserve(docs.len());
                for (id, item) in docs {
                    if id.bytes().nth(1) != Some(b':') {
                        writeln!(fzf_input, "{id}")?;
                    }
                    index.insert(id, item);
                }
            }
        }

        while let Some(docs) = ctx.document_next(offline).await {
            let (docs, name) = docs?;
            writeln!(out, "{GREEN}{BOLD}Documenting{NOSTYLE} {name}")?;
            index.reserve(docs.len());
            for (id, item) in docs {
                if id.bytes().nth(1) != Some(b':') {
                    writeln!(fzf_input, "{id}")?;
                }
                index.insert(id, item);
            }
        }

        Ok(Self { fzf_input_path, index })
    }

    pub const fn index(&self) -> &HashMap<Arc<str>, Item> {
        &self.index
    }

    /// `term` is guaranteed to be unchanged, this is just an optimisation
    pub fn search(&self, term: &mut String, dst: &mut Vec<SearchResult>) -> Result {
        dst.clear();

        let item_kind_constrained = if term.is_empty() {
            dst.extend(self.index.keys()
                .filter(|x| x.bytes().nth(1) != Some(b':'))
                .cloned()
                .map(|id| SearchResult { id }));
            return OK
        } else if term.contains(' ') {
            term.insert(0, '^');
            true
        } else {
            false
        };

        let fzf = std::process::Command::new("fzf")
            .args(["-f", term])
            .stdin(File::open(&self.fzf_input_path)?)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn();

        if item_kind_constrained {
            term.remove(0);
        }

        let mut fzf = fzf?;
        let mut stdout = fzf.stdout.take()
            .map(BufReader::new)
            .context("failed to get the stdout of `fzf`")?;
        let mut line = String::new();

        if !fzf.wait()?.success() {
            let mut err_msg = "`fzf` failed\nstderr:".to_owned();
            if let Some(stderr) = &mut fzf.stderr {
                err_msg.push('\n');
                stderr.read_to_string(&mut err_msg)?;
            } else {
                err_msg.push_str(" <failed to get stderr>");
            }
            bail!(err_msg)
        }

        while stdout.read_line(&mut line)? > 0 {
            let (id, _) = self.index
                .get_key_value(line.trim_end_matches('\n'))
                .with_context(|| format!("invalid otuput from `fzf`: item {line:?} not found"))?;
            dst.push(SearchResult { id: id.clone() });
            line.clear();
        }

        Ok(())
    }
}
