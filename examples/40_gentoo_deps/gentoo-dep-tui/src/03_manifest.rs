//! 03_manifest — Parser fuer Gentoo-`Manifest`-Dateien.
//!
//! Genau eine Zustaendigkeit: `DIST <datei> <bytes> ...` → Dateigroessen.
//! Die Fein-Metrik fuer Flag-Deltas (wenn kein neues Paket, aber neue
//! Distfiles durch `SRC_URI`-USE-Bedingungen entstehen).

use std::collections::HashMap;
use std::path::Path;

use crate::mod_01_types::{EngineError, EngineResult};

/// Geparste `Manifest`-Datei: Dateiname → Groesse in Bytes.
#[derive(Debug, Clone, Default)]
pub struct Manifest {
    /// Alle `DIST`-Eintraege.
    pub dist_sizes: HashMap<String, u64>,
}

impl Manifest {
    /// Leeres Manifest.
    pub fn empty() -> Self {
        Self::default()
    }

    /// Groesse einer Distdatei (`None` = unbekannt).
    pub fn size_of(&self, filename: &str) -> Option<u64> {
        self.dist_sizes.get(filename).copied()
    }

    /// Summiert bekannte Dateien; unbekannte werden ignoriert und gezaehlt.
    pub fn sum_files(&self, filenames: &[String]) -> (u64, usize) {
        let mut total = 0u64;
        let mut unknown = 0usize;
        for name in filenames {
            match self.dist_sizes.get(name) {
                Some(size) => total = total.saturating_add(*size),
                None => unknown += 1,
            }
        }
        (total, unknown)
    }
}

/// Parst `Manifest`-Text (nur `DIST`-Zeilen sind relevant).
pub fn parse_manifest(text: &str) -> Manifest {
    let mut dist_sizes = HashMap::new();
    for line in text.lines() {
        let mut parts = line.split_whitespace();
        if let (Some("DIST"), Some(name), Some(size)) = (parts.next(), parts.next(), parts.next())
            && let Ok(bytes) = size.parse::<u64>()
        {
            dist_sizes.insert(name.to_string(), bytes);
        }
    }
    Manifest { dist_sizes }
}

/// Laedt `<paketdir>/Manifest` (fehlt sie, gilt leeres Manifest).
pub fn load_manifest_file(package_dir: &Path) -> EngineResult<Manifest> {
    let path = package_dir.join("Manifest");
    match std::fs::read_to_string(&path) {
        Ok(text) => Ok(parse_manifest(&text)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(Manifest::empty()),
        Err(e) => Err(EngineError::Io {
            path: path.display().to_string(),
            source: e,
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = "\
DIST linux-6.17.tar.xz 145234112 BLAKE2B abc SHA512 def\n\
DIST genpatches-6.17-2.base.tar.xz 512000 BLAKE2B 00 SHA512 11\n\
EBUILD foo-1.0.ebuild 1234 BLAKE2B aa SHA512 bb\n\
MISC files/foo.conf 42 BLAKE2B cc SHA512 dd\n\
DIST broken-line-without-size\n";

    #[test]
    fn parses_dist_entries_only() {
        let m = parse_manifest(SAMPLE);
        assert_eq!(m.size_of("linux-6.17.tar.xz"), Some(145234112));
        assert_eq!(m.size_of("genpatches-6.17-2.base.tar.xz"), Some(512000));
        assert_eq!(m.size_of("foo-1.0.ebuild"), None);
        assert_eq!(m.size_of("files/foo.conf"), None);
    }

    #[test]
    fn sum_files_counts_unknown() {
        let m = parse_manifest(SAMPLE);
        let (total, unknown) =
            m.sum_files(&["linux-6.17.tar.xz".to_string(), "fehlt.tar.gz".to_string()]);
        assert_eq!(total, 145234112);
        assert_eq!(unknown, 1);
    }

    #[test]
    fn missing_file_is_empty_manifest() {
        let d = tempfile::tempdir().unwrap();
        let m = load_manifest_file(d.path()).unwrap();
        assert!(m.dist_sizes.is_empty());
    }
}
