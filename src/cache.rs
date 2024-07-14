use {
    crate::utils::{OptionExt, Result, OK},
    anyhow::Context,
    dirs::cache_dir,
    rustdoc_types::{Item, FORMAT_VERSION},
    std::{
        fs::{create_dir_all, remove_file, File},
        io::{BufReader, BufWriter},
        path::PathBuf,
        sync::{Arc, OnceLock},
    },
};

// The cache directory structure is "/<registry>/<name>/<version>/<format_version>.json"
// TODO: replace with LazyLock in 1.80
static CACHE_ROOT: OnceLock<Option<PathBuf>> = OnceLock::new();

#[allow(clippy::assertions_on_constants)] // TODO: remove in 1.81
const CACHE_FILENAME: &str = {
    assert!(FORMAT_VERSION == 30);
    "30.json"
};

pub fn load(registry: &str, name: &str, version: &str) -> Result<Option<Vec<(Arc<str>, Item)>>> {
    let mut cache_path = CACHE_ROOT
        .get_or_init(|| cache_dir().inspect_mut(|x| x.push("shrimple-docs")))
        .clone()
        .context("failed to get the cache directory of the system")?;

    cache_path.extend([registry, name, version]);
    create_dir_all(&cache_path)
        .with_context(|| format!("failed to create directory {cache_path:?}"))?;

    cache_path.push(CACHE_FILENAME);
    if !cache_path.try_exists()? {
        return Ok(None);
    }
    Ok(serde_json::from_reader(BufReader::new(File::open(cache_path)?))?)
}

pub fn store(items: &[(Arc<str>, Item)], registry: &str, name: &str, version: &str) -> Result {
    let mut cache_path = CACHE_ROOT
        .get_or_init(|| cache_dir().inspect_mut(|x| x.push("shrimple-docs")))
        .clone()
        .context("failed to get the cache directory of the system")?;

    cache_path.extend([registry, name, version]);
    create_dir_all(&cache_path)
        .with_context(|| format!("failed to create directory {cache_path:?}"))?;

    for entry in cache_path.read_dir()? {
        let path = entry?.path();
        remove_file(&path)
            .with_context(|| format!("failed to delete cache file {path:?}"))?;
    }

    cache_path.push(CACHE_FILENAME);
    serde_json::to_writer(BufWriter::new(File::create(cache_path)?), items)?;
    OK
}
