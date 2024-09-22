use {
    crate::{errfmt, utils::{Result, OK}},
    anyhow::Context,
    dirs::cache_dir,
    rustdoc_types::{Item, FORMAT_VERSION},
    std::{
        fs::{create_dir_all, remove_file, File},
        io::{BufReader, BufWriter},
        path::PathBuf,
        sync::{Arc, LazyLock},
    },
};

// The cache directory structure is "/<registry>/<name>/<version>/<format_version>.json"
static CACHE_ROOT: LazyLock<Option<PathBuf>> = LazyLock::new(|| {
    let mut res = cache_dir()?;
    res.push("shrimple-docs");
    Some(res)
});

const CACHE_FILENAME: &str = {
    assert!(FORMAT_VERSION == 33);
    "33.bincode"
};

pub fn load(registry: &str, name: &str, version: &str) -> Result<Option<Vec<(Arc<str>, Item)>>> {
    let mut cache_path = CACHE_ROOT
        .clone()
        .context("failed to get the cache directory of the system")?;

    cache_path.extend([registry, name, version]);
    create_dir_all(&cache_path)
        .with_context(errfmt!("create directory {:?}", cache_path))?;

    cache_path.push(CACHE_FILENAME);
    if !cache_path.try_exists()? {
        return Ok(None);
    }

    bincode::deserialize_from(BufReader::new(File::open(&cache_path)?)) 
        .map(Some)
        .with_context(errfmt!("read cache from {:?}", cache_path))
}

pub fn store(items: &[(Arc<str>, Item)], registry: &str, name: &str, version: &str) -> Result {
    let mut cache_path = CACHE_ROOT
        .clone()
        .context("failed to get the cache directory of the system")?;

    cache_path.extend([registry, name, version]);
    create_dir_all(&cache_path).with_context(errfmt!("create directory {:?}", cache_path))?;

    for entry in cache_path.read_dir()? {
        let path = entry?.path();
        remove_file(&path).with_context(errfmt!("delete cache file {:?}", path))?;
    }

    cache_path.push(CACHE_FILENAME);
    bincode::serialize_into(BufWriter::new(File::create(&cache_path)?), items)
        .with_context(errfmt!("write cache to {:?}", cache_path))?;
    OK
}
