//! 04_binhost — Parser fuer die Gentoo-Binhost-`Packages`-Indexdatei.
//!
//! Genau eine Zustaendigkeit: `CPV`/`SIZE`/`USE`/`RDEPEND` aus Stanzas lesen.
//! `SIZE` ist der Runtime-Proxy (fertiges Binpkg statt riesigem Tarball).

use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::mod_01_types::{EngineError, EngineResult};

/// Ein Eintrag der Binhost-`Packages`-Datei.
#[derive(Debug, Clone, Default)]
pub struct BinpkgEntry {
    /// `kategorie/paket-version`.
    pub cpv: String,
    /// Komprimierte Paketgroesse (`SIZE`), Bytes.
    pub size: u64,
    /// USE-Flags, mit denen das Binpkg gebaut wurde.
    pub use_flags: HashSet<String>,
    /// Slot (`SLOT:`), falls angegeben.
    pub slot: String,
    /// Rohes `RDEPEND` (einzeilig zusammengefuegt).
    pub rdepend: String,
}

/// Index aller Binpkgs: CPV → Eintrag.
#[derive(Debug, Clone, Default)]
pub struct BinpkgIndex {
    /// Alle Eintraege.
    pub entries: HashMap<String, BinpkgEntry>,
}

impl BinpkgIndex {
    /// Leerer Index.
    pub fn empty() -> Self {
        Self::default()
    }

    /// Eintrag zu einem CPV.
    pub fn get(&self, cpv: &str) -> Option<&BinpkgEntry> {
        self.entries.get(cpv)
    }

    /// Anzahl Eintraege.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Ob leer.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Trefferquote: Anteil der `cpvs`, die einen Eintrag haben.
    pub fn hit_rate(&self, cpvs: &[String]) -> f64 {
        if cpvs.is_empty() {
            return 1.0;
        }
        let hits = cpvs
            .iter()
            .filter(|c| self.entries.contains_key(c.as_str()))
            .count();
        hits as f64 / cpvs.len() as f64
    }
}

/// Parst eine `Packages`-Datei (Stanzas durch Leerzeilen getrennt).
///
/// Format: `KEY: wert`, Fortsetzungszeilen beginnen mit Leerzeichen und
/// werden mit einem Leerzeichen angehaengt. Unbekannte Keys werden ignoriert.
pub fn parse_packages(text: &str) -> BinpkgIndex {
    let mut index = BinpkgIndex::empty();
    let mut fields: HashMap<String, String> = HashMap::new();
    let mut last_key = String::new();

    let mut flush = |fields: &mut HashMap<String, String>| {
        if let Some(cpv) = fields.remove("CPV") {
            let size = fields
                .remove("SIZE")
                .and_then(|s| {
                    s.split_whitespace()
                        .next()
                        .unwrap_or("0")
                        .parse::<u64>()
                        .ok()
                })
                .unwrap_or(0);
            let use_flags = fields
                .remove("USE")
                .map(|s| s.split_whitespace().map(str::to_string).collect())
                .unwrap_or_default();
            index.entries.insert(
                cpv.clone(),
                BinpkgEntry {
                    cpv,
                    size,
                    use_flags,
                    slot: fields.remove("SLOT").unwrap_or_default(),
                    rdepend: fields.remove("RDEPEND").unwrap_or_default(),
                },
            );
        }
        fields.clear();
    };

    for line in text.lines() {
        if line.trim().is_empty() {
            flush(&mut fields);
            last_key.clear();
            continue;
        }
        if line.starts_with(' ') || line.starts_with('\t') {
            if !last_key.is_empty() {
                let entry = fields.entry(last_key.clone()).or_default();
                entry.push(' ');
                entry.push_str(line.trim());
            }
            continue;
        }
        if let Some((key, value)) = line.split_once(':') {
            last_key = key.trim().to_string();
            fields.insert(last_key.clone(), value.trim().to_string());
        }
    }
    flush(&mut fields);
    index
}

/// Laedt eine `Packages`-Datei (fehlt sie, gilt leerer Index).
pub fn load_packages_file(path: &Path) -> EngineResult<BinpkgIndex> {
    match std::fs::read_to_string(path) {
        Ok(text) => Ok(parse_packages(&text)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(BinpkgIndex::empty()),
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
CPV: dev-libs/openssl-3.3.2\n\
SIZE: 5242880\n\
USE: ssl tls-compression -test\n\
SLOT: 0/3\n\
RDEPEND: dev-libs/openssl[ssl]\n\
\n\
CPV: app-misc/jq-1.7.1\n\
SIZE: 524288\n\
USE: abbrev oniguruma\n\
";

    #[test]
    fn parses_stanzas_with_use_and_slot() {
        let idx = parse_packages(SAMPLE);
        assert_eq!(idx.len(), 2);
        let ossl = idx.get("dev-libs/openssl-3.3.2").unwrap();
        assert_eq!(ossl.size, 5242880);
        assert!(ossl.use_flags.contains("ssl"));
        assert!(ossl.use_flags.contains("-test"));
        assert_eq!(ossl.slot, "0/3");
        assert!(ossl.rdepend.contains("dev-libs/openssl"));
    }

    #[test]
    fn continuation_lines_join() {
        let idx = parse_packages("CPV: a/b-1.0\nRDEPEND: x? ( a/b )\n y? ( c/d )\n\n");
        assert_eq!(idx.get("a/b-1.0").unwrap().rdepend, "x? ( a/b ) y? ( c/d )");
    }

    #[test]
    fn hit_rate_counts_matches() {
        let idx = parse_packages(SAMPLE);
        let rate = idx.hit_rate(&["dev-libs/openssl-3.3.2".into(), "x/y-9.9".into()]);
        assert!((rate - 0.5).abs() < f64::EPSILON);
        assert_eq!(idx.hit_rate(&[]), 1.0);
    }
}
