use std::{collections::HashMap, convert::identity, io::Write, ops::Range, path::Path, process::{Command, Stdio}, ptr::{addr_of, addr_of_mut}, sync::Arc};
use anyhow::{bail, Context};
use rustdoc_types::{Crate, ExternalCrate, Id, Item, ItemSummary, Type, FORMAT_VERSION};
use serde::Deserialize;
use tokio::{fs::read_to_string, task::JoinSet};
use crate::{
    item_visitor::VisitorMut,
    utils::{levenshtein, BoolExt, Result, BOLD, GREEN, NOSTYLE, OK, CLEARLINE},
};
use crossterm::{cursor::MoveToPreviousLine, ExecutableCommand};

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
    manifest_path: Option<String>,
}

impl<'src> Package<'src> {
    /// For crates such as `alloc`, `core`, etc.
    fn builtin(name: &'src str) -> Self {
        Self {
            name: name.into(),
            targets: vec![],
            manifest_path: None,
        }
    }

    fn normalise_name(&mut self) {
        self.name = self.targets
            .iter()
            .find(|t| t.kind.contains(&"lib"))
            .map_or_else(|| self.name.replace('-', "_"), |t| t.name.into());
        self.targets = vec![];
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
    ranges_temp: Vec<(Range<usize>, &'docs str)>,
}

impl VisitorMut for IdNormaliser<'_> {
    #[allow(clippy::cast_possible_wrap)]
    fn visit_id(&mut self, x: &mut Id) -> Result {
        if let Some(item) = self.paths.get(x) {
            *x = Id(item.path.join("::"));
        } else {
            let init_off = matches!(x.0.as_bytes(), [b'a' | b'b', b':', ..]).pick(2, 0);
            let mut off = init_off;
            for id in x.0[off..].split('-') {
                let crate_id_str = id.split_once(':').map_or(id, |x| x.0);
                let crate_id = crate_id_str
                    .parse()
                    .with_context(|| format!("item ID {:?} is malformatted", x.0))?;
                let new_crate_name = match crate_id {
                    0 => self.crate_name,
                    _ => self.crates.get(&crate_id)
                        .map(|x| &x.name)
                        .with_context(|| {
                            format!("unknown crate ID: {crate_id_str:?}\n\tfull ID: {}", x.0)
                        })?
                };
                self.ranges_temp.push((off .. off + crate_id_str.len(), new_crate_name));
                off += id.len() + 1;
            }

            let mut off = 0isize;
            for (mut range, new_crate_name) in self.ranges_temp.drain(..) {
                range.start = range.start.wrapping_add_signed(off);
                range.end = range.end.wrapping_add_signed(off);
                x.0.replace_range(range.clone(), new_crate_name);
                off += new_crate_name.len().wrapping_sub(range.end - range.start) as isize;
            }
            x.0.replace_range(..init_off, "$:");
        }
        OK
    }
}

/// `out` is only used for logging
async fn document_crate(
    name: impl AsRef<str> + Send,
    manifest_path: impl AsRef<str> + Send,
    toolchain: impl AsRef<str> + Send,
) -> Result<HashMap<Id, Item>> {
    let manifest_path = manifest_path.as_ref();
    let name = name.as_ref();
    let docs_gen = tokio::process::Command::new("rustup")
        .args([
            "run",
            toolchain.as_ref(),
            "cargo",
            "rustdoc",
            "--manifest-path",
            manifest_path,
            "--color=always",
            "-Zunstable-options",
            "--output-format=json",
        ])
        .stderr(Stdio::piped())
        .stdout(Stdio::null())
        .output().await?;
    if !docs_gen.status.success() {
        bail!("`cargo rustdoc` failed; output:\n{}", String::from_utf8_lossy(&docs_gen.stderr));
    }

    let mut docs_path = Path::new(manifest_path)
        .parent().context("invalid Cargo manifest path")?
        .to_owned();
    docs_path.push("target");
    docs_path.push("doc");
    docs_path.push(name);
    docs_path.set_extension("json");
    let json = read_to_string(docs_path).await?;

    let docs: Crate = match serde_json::from_str(&json) {
        Ok(x) => x,
        Err(e) => {
            #[derive(Deserialize)]
            struct V {
                format_version: u32,
            }
            let V { format_version } = serde_json::from_str(&json)?;
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
        crate_name: name,
        crates: &docs.external_crates,
        ranges_temp: Vec::with_capacity(2),
    };
    
    docs.index.into_iter()
        //.filter(|(k, _)| k.0.starts_with("0:"))
        .map(|(mut k, mut item)| {
            id_normaliser.visit_id(&mut k)?;
            id_normaliser.visit_item(&mut item)?;
            Ok((k, item))
        })
        .collect()
}

/// Filled in by [`Docs::search`]
pub struct SearchResult<'docs> {
    /// [`Id`] usable to index the docs directly.
    pub id: &'docs Id,
    /// Levenshtein distance from the searched term.
    pub distance: usize,
}

/// Aggregator of all the docs
pub struct Docs {
    pub index: HashMap<Id, Item>,
}

impl Docs {
    pub async fn new(toolchain: &Arc<str>, out: &mut (impl Write + Send)) -> Result<Self> {
        writeln!(out, "{GREEN}{BOLD}Extracting{NOSTYLE} cargo metadata")?;
        let cmd = Command::new("rustup")
            .args([
                "run",
                toolchain,
                "cargo",
                "metadata",
                "--format-version=1",
            ])
            .stderr(Stdio::inherit())
            .output()?;
        if !cmd.status.success() {
            bail!("`cargo metadata` failed");
        }
        let CargoMetadata { mut packages, .. } = serde_json::from_slice(&cmd.stdout)?;

        packages.iter_mut().for_each(Package::normalise_name);
        packages.push(Package::builtin("std"));
        packages.push(Package::builtin("alloc"));
        packages.push(Package::builtin("core"));
        packages.push(Package::builtin("proc_macro"));
        packages.sort_unstable_by(|p1, p2| p1.name.cmp(&p2.name));

        let n_packages = packages.len();
        let mut n_processed = 0usize;
        writeln!(out, "{GREEN}{BOLD}Documenting{NOSTYLE} libraries ... 0 / {n_packages}")?;
        let mut documentors: JoinSet<_> = packages
            .into_iter()
            .filter_map(|package| package.manifest_path.zip(Some(package.name)))
            .map(|(manifest_path, name)| document_crate(name, manifest_path, toolchain.clone()))
            .collect();

        let mut res = Self { index: HashMap::new() };
        while let Some(items) = documentors.join_next().await {
            res.index.extend(items??);
            n_processed += 1;
            out.execute(MoveToPreviousLine(1))?;
            writeln!(out, "{CLEARLINE}{GREEN}{BOLD}Documenting{NOSTYLE} libraries ... \
                         {n_processed} / {n_packages}")?;
        }
        Ok(res)
    }

    pub fn search<'docs>(&'docs self, term: &str, dst: &mut Vec<SearchResult<'docs>>) {
        dst.clear();
        for id in self.index.keys() {
            if matches!(id.0.as_bytes(), [b'a' | b'b' | b'$', b':', ..]) {
                continue;
            }
            let distance = levenshtein(term, id.0.rsplit_once(':').map_or(&id.0, |x| x.1));
            let place = dst.binary_search_by_key(&distance, |res| res.distance)
                .unwrap_or_else(identity);
            dst.insert(place, SearchResult { id, distance });
        }
    }
}
