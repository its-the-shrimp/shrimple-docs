use std::{borrow::Cow, collections::HashMap, fs::File, io::Write, iter::once, ops::Range, process::{Command, Stdio}, ptr::{addr_of, addr_of_mut}};
use anyhow::{bail, Context};
use rustdoc_types::{Crate, ExternalCrate, Id, Item, ItemSummary, Type, FORMAT_VERSION};
use serde::Deserialize;
use crate::{item_visitor::VisitorMut, stack_format, utils::{ReadExt, Result, StrExt, StringExt, BOLD, GREEN, NOSTYLE, OK}};

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
    name: Cow<'src, str>,
    id: &'src str,
    targets: Vec<Target<'src>>,
    manifest_path: Option<&'src str>,
}

impl<'src> Package<'src> {
    /// For crates such as `alloc`, `core`, etc.
    fn builtin(name: &'src str) -> Self {
        Self {
            name: name.into(),
            id: name,
            targets: vec![],
            manifest_path: None,
        }
    }

    fn normalise_name(&mut self) {
        self.name = self.targets
            .iter()
            .find(|t| t.kind.contains(&"lib"))
            .map_or_else(|| self.name.replace('-', "_").into(), |t| t.name.into());
        self.targets = vec![];
    }
}

#[derive(Deserialize)]
struct Root<'src> {
    #[serde(borrow)]
    root: Option<&'src str>,
}

#[derive(Deserialize)]
/// Output of `cargo metadata` with only the fields that matter to us
struct CargoMetadata<'src> {
    #[serde(borrow)]
    packages: Vec<Package<'src>>,
    resolve: Option<Root<'src>>,
}

/// Aggregator of all the docs
#[derive(Default)]
pub struct Docs {
    pub index: HashMap<Id, Item>,
    pub paths: HashMap<Id, ItemSummary>,
}

struct CrateIdMapper {
    map: HashMap<u32, u32>,
    ranges_temp: Vec<(Range<usize>, u32)>,
}

impl VisitorMut for CrateIdMapper {
    #[allow(clippy::cast_possible_wrap)]
    fn visit_id(&mut self, x: &mut Id) -> Result {
        if x.0.starts_with(['a', 'b']) {
            return OK;
        }

        let mut off = 0;
        for id in x.0.split('-') {
            let crate_id_str = id.split_once(':').map_or(id, |x| x.0);
            let crate_id = crate_id_str
                .parse()
                .with_context(|| format!("item ID {:?} is malformatted", x.0))?;
            let Some(&new_crate_id) = self.map.get(&crate_id) else {
                bail!("unknown crate ID: {crate_id_str:?}\n\tfull ID: {}", x.0);
            };
            self.ranges_temp.push((off .. off + crate_id_str.len(), new_crate_id));
            off += id.len() + 1;
        }
        let mut off = 0isize;
        for (mut range, new_crate_id) in self.ranges_temp.drain(..) {
            range.start = range.start.wrapping_add_signed(off);
            range.end = range.end.wrapping_add_signed(off);
            let new_crate_id_str = stack_format!(cap: 20, "{new_crate_id}")?;
            x.0.replace_range(range.clone(), &new_crate_id_str);
            off += new_crate_id_str.len().wrapping_sub(range.end - range.start) as isize;
        }
        OK
    }
}

impl CrateIdMapper {
    /// `dst` must be sorted by `Package::name`
    fn new(crate_dst_id: u32, src_index: HashMap<u32, ExternalCrate>, dst_index: &[Package])
        -> Self
    {
        let mut map: HashMap<u32, u32> = src_index.into_iter()
            .filter_map(|(id, x)| dst_index.binary_search_by_key(&&*x.name, |p| &p.name)
                //.inspect_err(|_| eprintln!("warning: could not find crate {:?}", x.name))
                .ok()
                .and_then(|new_id| new_id.try_into().ok())
                .map(|new_id| (id, new_id)))
            .collect();
        map.insert(0, crate_dst_id);
        Self { map, ranges_temp: vec![] }
    }
}

/// `out` is only used for logging
fn gen_rustdoc_crate(
    name: &str,
    manifest_path: Option<&str>,
    toolchain: &str,
    out: &mut impl Write,
) -> Result<Crate> {
    writeln!(out, "{GREEN}{BOLD}Documenting{NOSTYLE} {name}")?;

    let mut docs_gen = Command::new("rustup")
        .args([
            "run",
            toolchain,
            "cargo",
            "rustdoc",
            "--color=always",
            "-Zunstable-options",
            "--output-format=json",
        ])
        .args(manifest_path.into_iter().flat_map(|path| ["--manifest-path", path]))
        .stderr(Stdio::piped())
        .stdout(Stdio::null())
        .spawn()?;

    let docs_path = docs_gen
        .stderr
        .as_mut()
        .context("failed to get the stderr of `cargo rustdoc`")?
        // TODO: remove this allocation
        .buffered_lines()
        .take_while(Result::is_ok)
        .last()
        .context("`cargo doc`'s stderr empty")?
        .context("failed to process stderr of `cargo doc`")?
        // Safety: the second return value of `split_once` starts right after the delimiter
        .try_map(|last| unsafe {
            if !docs_gen.wait()?.success() {
                bail!("`cargo rustdoc` failed");
            }
            last.split_once('/')
                .context("`cargo rustdoc`'s stderr malformatted") 
                .map(|s| s.1.stretch_left(1))
        })?;

    let docs: Crate = match serde_json::from_reader(File::open(&docs_path)?) {
        Ok(x) => x,
        Err(e) => {
            #[derive(Deserialize)]
            struct V {
                format_version: u32,
            }
            let V { format_version } = serde_json::from_reader(File::open(&docs_path)?)?;
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
    Ok(docs)
}

impl Docs {
    pub fn new(toolchain: &str, out: &mut impl Write) -> Result<Self> {
        let mut res = Self::default();

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
        let CargoMetadata { mut packages, resolve } = serde_json::from_slice(&cmd.stdout)?;
        let Some(Root { root: Some(root_abs_id) }) = resolve else {
            bail!("`cargo metadata` didn't provide a dependency graph")
        };

        packages.iter_mut().for_each(Package::normalise_name);
        packages.push(Package::builtin("std"));
        packages.push(Package::builtin("alloc"));
        packages.push(Package::builtin("core"));
        packages.push(Package::builtin("proc_macro"));
        packages.sort_unstable_by(|p1, p2| p1.name.cmp(&p2.name));

        let (root_id, root) = packages
            .iter()
            .enumerate()
            .find(|x| x.1.id == root_abs_id)
            .context("root package not found")?;
        for (crate_id, Package { name, manifest_path, .. }) in once((root_id, root))
            .chain(packages.iter().enumerate().filter(|x| x.0 != root_id))
        {
            let Some(manifest_path) = manifest_path else {
                continue;
            };
            let crate_id = u32::try_from(crate_id).ok().context("too many crates")?;
            let docs = gen_rustdoc_crate(name, Some(manifest_path), toolchain, out)?;
        
            let mut crate_id_mapper = CrateIdMapper::new(crate_id, docs.external_crates, &packages);

            res.paths.extend(docs.paths.into_iter().filter_map(|(mut k, summary)| {
                if k.0.starts_with(['a', 'b']) || !k.0.starts_with("0:") {
                    return None;
                }

                crate_id_mapper.visit_id(&mut k).ok()?;
                Some((k, summary))
            }));

            let mut err = OK;
            res.index.extend(docs.index.into_iter().filter_map(|(mut k, mut item)| {
                if err.is_err() || k.0.starts_with(['a', 'b']) || !k.0.starts_with("0:") {
                    return None;
                }
                crate_id_mapper.visit_id(&mut k).map_err(|e| err = Err(e)).ok()?;
                crate_id_mapper.visit_item(&mut item).map_err(|e| err = Err(e)).ok()?;
                Some((k, item))
            }));
            err?;
        }

        Ok(res)
    }
}
