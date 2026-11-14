//! Gemeinsame Test-Fixtures (nur `#[cfg(test)]`).
//!
//! Genau eine Zustaendigkeit: synthetische `PkgVersion`- und `RepoIndex`-
//! Bausteine fuer alle Unit-Tests in `src/` (ersetzt die duplizierten
//! `pkg(...)`-Helfer, Task 5.2).

use std::collections::HashMap;

use crate::mod_03_manifest::parse_manifest;
use crate::mod_05_repo_index::{PkgVersion, RepoIndex};

/// Alle Felder eines synthetischen `PkgVersion` (Builder mit Defaults).
#[derive(Debug, Clone)]
pub struct PkgFixture {
    cpn: String,
    version: String,
    keywords: String,
    iuse: String,
    rdepend: String,
    bdepend: String,
    src_uri: String,
    pdepend: String,
    required_use: String,
    slot: String,
    manifest: String,
    binpkg: Option<u64>,
}

impl PkgFixture {
    /// Neues Fixture (`cat/pkg`, Version `1.0`, `KEYWORDS=amd64`).
    pub fn new(cpn: &str) -> Self {
        Self {
            cpn: cpn.to_string(),
            version: "1.0".to_string(),
            keywords: "amd64".to_string(),
            iuse: String::new(),
            rdepend: String::new(),
            bdepend: String::new(),
            src_uri: "http://x/a.tar.gz".to_string(),
            pdepend: String::new(),
            required_use: String::new(),
            slot: "0".to_string(),
            manifest: "DIST a.tar.gz 100\n".to_string(),
            binpkg: Some(10),
        }
    }

    /// Version setzen (`version()` impliziert den CPV).
    pub fn version(mut self, version: &str) -> Self {
        self.version = version.to_string();
        self
    }

    /// `KEYWORDS`-Zeile setzen.
    pub fn keywords(mut self, keywords: &str) -> Self {
        self.keywords = keywords.to_string();
        self
    }

    /// `IUSE`-Zeile setzen.
    pub fn iuse(mut self, iuse: &str) -> Self {
        self.iuse = iuse.to_string();
        self
    }

    /// `RDEPEND`-Zeile setzen.
    pub fn rdepend(mut self, rdepend: &str) -> Self {
        self.rdepend = rdepend.to_string();
        self
    }

    /// `BDEPEND`-Zeile setzen.
    pub fn bdepend(mut self, bdepend: &str) -> Self {
        self.bdepend = bdepend.to_string();
        self
    }

    /// `SRC_URI`-Zeile setzen.
    pub fn src_uri(mut self, src_uri: &str) -> Self {
        self.src_uri = src_uri.to_string();
        self
    }

    /// `PDEPEND`-Zeile setzen.
    pub fn pdepend(mut self, pdepend: &str) -> Self {
        self.pdepend = pdepend.to_string();
        self
    }

    /// `REQUIRED_USE`-Zeile setzen.
    pub fn required_use(mut self, required_use: &str) -> Self {
        self.required_use = required_use.to_string();
        self
    }

    /// Manifest-`DIST`-Zeilen setzen.
    pub fn manifest(mut self, manifest: &str) -> Self {
        self.manifest = manifest.to_string();
        self
    }

    /// Binhost-Groesse setzen (`None` = kein Binpkg).
    pub fn binpkg(mut self, binpkg: Option<u64>) -> Self {
        self.binpkg = binpkg;
        self
    }

    /// Bauen (parst den Cache-Eintrag, wie echte `md5-cache`-Dateien).
    pub fn build(self) -> PkgVersion {
        use portage_metadata::CacheEntry;
        let text = format!(
            "EAPI=8\nDESCRIPTION=t\nSLOT={slot}\nKEYWORDS={keywords}\nIUSE={iuse}\n\
             RDEPEND={rdepend}\nBDEPEND={bdepend}\nPDEPEND={pdepend}\n\
             REQUIRED_USE={required_use}\nSRC_URI={src_uri}\n",
            slot = self.slot,
            keywords = self.keywords,
            iuse = self.iuse,
            rdepend = self.rdepend,
            bdepend = self.bdepend,
            pdepend = self.pdepend,
            required_use = self.required_use,
            src_uri = self.src_uri,
        );
        let entry = CacheEntry::parse(&text).unwrap();
        let mut iuse_defaults = HashMap::new();
        for flag in &entry.metadata.iuse {
            iuse_defaults.insert(flag.name().to_string(), flag.is_enabled_default());
        }
        PkgVersion {
            cpn: self.cpn.clone(),
            cpv: format!("{}-{}", self.cpn, self.version),
            version_str: self.version.clone(),
            version: portage_atom::Version::parse(&self.version).unwrap(),
            slot: self.slot.clone(),
            iuse_defaults,
            entry,
            manifest: parse_manifest(&self.manifest),
            binhost_size: self.binpkg,
            base_use: std::sync::OnceLock::new(),
        }
    }
}

/// `RepoIndex` aus Paketliste bauen (Versionslisten absteigend sortiert).
pub fn repo_index(pkgs: Vec<PkgVersion>) -> RepoIndex {
    let mut versions: HashMap<String, Vec<PkgVersion>> = HashMap::new();
    for p in pkgs {
        versions.entry(p.cpn.clone()).or_default().push(p);
    }
    let mut cache_files = 0;
    for list in versions.values_mut() {
        cache_files += list.len();
        list.sort_by(|a, b| b.version.cmp(&a.version));
    }
    RepoIndex {
        versions,
        cache_files,
        load_ms: 0,
    }
}
