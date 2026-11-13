//! 05_repo_index — RepoIndex ueber `portage-metadata` + `portage-atom`.
//!
//! Genau eine Zustaendigkeit: md5-cache einlesen, Versionen waehlen,
//! Manifest-/Binhost-Groessen zuordnen. Reine Daten-Pipeline, kein Solver.
//! Paralleles Einlesen via `rayon` + `walkdir`.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::time::Instant;

use portage_atom::{Dep, Operator, SlotDep, Version};
use portage_metadata::{CacheEntry, Stability};
use rayon::prelude::*;
use walkdir::WalkDir;

use crate::mod_01_types::{EngineError, EngineResult};
use crate::mod_02_profile::Profile;
use crate::mod_03_manifest::{Manifest, load_manifest_file};
use crate::mod_04_binhost::{BinpkgIndex, load_packages_file};

/// Eine einzelne Ebuild-Version im Index.
pub struct PkgVersion {
    /// `kategorie/paket`.
    pub cpn: String,
    /// `kategorie/paket-version` (ohne Revision-Normalisierung).
    pub cpv: String,
    /// Versionsstring (`1.7.1-r1`).
    pub version_str: String,
    /// Geparste Version (Ordnung nach PMS Algorithmus 3.1).
    pub version: Version,
    /// Slot-Name (`0`, `3.12`, ...).
    pub slot: String,
    /// Ob `package.mask` greift.
    pub masked: bool,
    /// Ob Live-Ebuild (`9999*`, keine Manifest-/Binpkg-Daten).
    pub live: bool,
    /// IUSE-Defaults: Flag → an per Default.
    pub iuse_defaults: HashMap<String, bool>,
    /// Geparste Metadaten (faule `*DEPEND`-Listen inklusive).
    pub entry: CacheEntry,
    /// `Manifest`-DIST-Groessen dieses Pakets.
    pub manifest: Manifest,
    /// Binhost-`SIZE` des exakten CPV (`None` = kein Binpkg).
    pub binhost_size: Option<u64>,
}

/// Der Repo-Index: alle Versionen pro `cat/pkg`, sortiert (neueste zuerst).
pub struct RepoIndex {
    /// `cat/pkg` → Versionen, absteigend sortiert.
    pub versions: HashMap<String, Vec<PkgVersion>>,
    /// Anzahl geladener Cache-Dateien.
    pub cache_files: usize,
    /// Ladezeit in Millisekunden.
    pub load_ms: u128,
}

impl RepoIndex {
    /// Laedt `md5-cache` (+ `Manifest`, optional Binhost) aus `repo_root`.
    ///
    /// Erwartetes Layout: `<repo>/metadata/md5-cache/<kat>/<paket-version>`
    /// und `<repo>/<kat>/<pkg>/Manifest`. Filter: `KEYWORDS` (`profile.arch`,
    /// `ACCEPT_KEYWORDS`), `package.mask`, keine `9999`-Live-Ebuilds.
    pub fn load(repo_root: &Path, binhost: Option<&Path>, profile: &Profile) -> EngineResult<Self> {
        let start = Instant::now();
        let cache_dir = repo_root.join("metadata").join("md5-cache");
        let binhost_index = match binhost {
            Some(path) => load_packages_file(path)?,
            None => BinpkgIndex::empty(),
        };

        let mut files: Vec<PathBuf> = WalkDir::new(&cache_dir)
            .min_depth(2)
            .max_depth(2)
            .into_iter()
            .filter_map(|e| e.ok())
            .map(|e| e.into_path())
            .filter(|p| p.is_file())
            .collect();
        files.sort();
        let cache_files = files.len();

        let parsed: Vec<(String, PkgVersion)> = files
            .par_iter()
            .filter_map(|path| {
                parse_cache_file(repo_root, &cache_dir, path, &binhost_index, profile).ok()
            })
            .collect();

        let mut versions: HashMap<String, Vec<PkgVersion>> = HashMap::new();
        for (cpn, pkg) in parsed {
            versions.entry(cpn).or_default().push(pkg);
        }
        for list in versions.values_mut() {
            list.sort_by(|a, b| b.version.cmp(&a.version));
        }

        Ok(Self {
            versions,
            cache_files,
            load_ms: start.elapsed().as_millis(),
        })
    }

    /// Anzahl Kategorien (abgeleitet).
    pub fn categories(&self) -> usize {
        let mut cats = HashSet::new();
        for cpn in self.versions.keys() {
            if let Some((cat, _)) = cpn.split_once('/') {
                cats.insert(cat.to_string());
            }
        }
        cats.len()
    }

    /// Anzahl Paketnamen.
    pub fn packages(&self) -> usize {
        self.versions.len()
    }

    /// Trefferquote Binhost-`SIZE` ueber alle gewaehlten Top-Versionen.
    pub fn binhost_hit_rate(&self) -> f64 {
        let cpvs: Vec<String> = Vec::new();
        let _ = cpvs;
        let total = self.versions.len();
        if total == 0 {
            return 1.0;
        }
        let hits = self
            .versions
            .values()
            .filter(|l| l.first().is_some_and(|p| p.binhost_size.is_some()))
            .count();
        hits as f64 / total as f64
    }

    /// Waehlt die neueste sichtbare Version fuer `cat/pkg`.
    pub fn select(&self, cpn: &str) -> Option<&PkgVersion> {
        self.versions.get(cpn)?.first()
    }

    /// Waehlt die neueste Version, die ein `Dep`-Atom erfuellt.
    pub fn select_for_dep(&self, dep: &Dep) -> Option<&PkgVersion> {
        let cpn = dep.cpn.to_string();
        let list = self.versions.get(&cpn)?;
        list.iter().find(|p| dep_matches(dep, p))
    }
}

/// Prueft Atom gegen eine Version (CPN-Gleichheit ausserhalb geprueft).
fn dep_matches(dep: &Dep, pkg: &PkgVersion) -> bool {
    if let (Some(op), Some(want)) = (&dep.op, &dep.version)
        && !version_matches(*op, dep.glob, want, &pkg.version, &pkg.version_str)
    {
        return false;
    }
    if let Some(slot_dep) = &dep.slot_dep {
        match slot_dep {
            SlotDep::Slot {
                slot: Some(named), ..
            } => {
                if !slot_matches(named, &pkg.slot) {
                    return false;
                }
            }
            SlotDep::Slot { slot: None, .. } | SlotDep::Operator(_) => {}
        }
    }
    true
}

/// Slot-Abgleich: Slot-Name muss passen; ist im Atom ein Sub-Slot genannt
/// (`:0/1`), muss auch er passen (sonst wuerde `:0/1` faelschlich `0/2`
/// maskieren/treffen).
fn slot_matches(named: &portage_atom::Slot, pkg_full_slot: &str) -> bool {
    let (pkg_slot, pkg_sub) = match pkg_full_slot.split_once('/') {
        Some((s, sub)) => (s, Some(sub)),
        None => (pkg_full_slot, None),
    };
    if named.slot.as_str() != pkg_slot {
        return false;
    }
    match &named.subslot {
        Some(want) => pkg_sub == Some(want.as_str()),
        None => true,
    }
}

/// Versionsvergleich nach Operator (PMS 8.3.1).
fn version_matches(
    op: Operator,
    glob: bool,
    want: &Version,
    have: &Version,
    have_str: &str,
) -> bool {
    use std::cmp::Ordering;
    if glob && op == Operator::Equal {
        return have_str.starts_with(&want.to_string());
    }
    match op {
        Operator::Less => have.cmp(want) == Ordering::Less,
        Operator::LessOrEqual => have.cmp(want) != Ordering::Greater,
        Operator::Equal => have.cmp(want) == Ordering::Equal,
        Operator::Approximate => strip_revision(have) == strip_revision(want),
        Operator::GreaterOrEqual => have.cmp(want) != Ordering::Less,
        Operator::Greater => have.cmp(want) == Ordering::Greater,
    }
}

/// Versionsstring ohne `-rN`-Revision (fuer `~`-Vergleiche).
fn strip_revision(version: &Version) -> String {
    let rendered = version.to_string();
    match rendered.rsplit_once("-r") {
        Some((base, tail)) if tail.chars().all(|c| c.is_ascii_digit()) => base.to_string(),
        _ => rendered,
    }
}

/// Eine einzelne Cache-Datei parsen (+ Manifest + Binhost zuordnen).
fn parse_cache_file(
    repo_root: &Path,
    cache_dir: &Path,
    path: &Path,
    binhost: &BinpkgIndex,
    profile: &Profile,
) -> EngineResult<(String, PkgVersion)> {
    let rel = path
        .strip_prefix(cache_dir)
        .map_err(|_| EngineError::Parse {
            context: path.display().to_string(),
            message: "ausserhalb von md5-cache".into(),
        })?;
    let mut parts = rel.components();
    let category = parts
        .next()
        .map(|c| c.as_os_str().to_string_lossy().into_owned())
        .unwrap_or_default();
    let pf = parts
        .next()
        .map(|c| c.as_os_str().to_string_lossy().into_owned())
        .unwrap_or_default();
    // `<paket>-<version>` am letzten `-` mit Ziffer dahinter trennen
    // (Paketnamen enthalten nie eine solche Endung, Versionen immer).
    let split = pf
        .rmatch_indices('-')
        .map(|(i, _)| i)
        .find(|i| pf[i + 1..].starts_with(|c: char| c.is_ascii_digit()));
    let (package, version_str) = match split {
        Some(i) => (pf[..i].to_string(), pf[i + 1..].to_string()),
        None => {
            return Err(EngineError::Parse {
                context: path.display().to_string(),
                message: format!("kein Paketname-Version-Split: {pf}"),
            });
        }
    };
    let cpn = format!("{category}/{package}");

    let text = std::fs::read_to_string(path).map_err(|e| EngineError::Io {
        path: path.display().to_string(),
        source: e,
    })?;
    let entry = CacheEntry::parse(&text).map_err(|e| EngineError::Parse {
        context: path.display().to_string(),
        message: e.to_string(),
    })?;

    let live = version_str.starts_with("9999") || entry.metadata.is_live();
    let masked = !live && is_masked(&cpn, &version_str, &entry, profile);
    let keywords_ok = keywords_accepted(&entry, profile);
    if live || masked || !keywords_ok {
        return Err(EngineError::Parse {
            context: cpn.clone(),
            message: format!("Version {version_str} herausgefiltert"),
        });
    }

    let version = portage_atom::Version::parse(&version_str).map_err(|e| EngineError::Parse {
        context: path.display().to_string(),
        message: e.to_string(),
    })?;
    let cpv = format!("{cpn}-{version_str}");
    let slot = full_slot(&entry.metadata.slot);
    let mut iuse_defaults = HashMap::new();
    for flag in &entry.metadata.iuse {
        iuse_defaults.insert(flag.name().to_string(), flag.is_enabled_default());
    }
    let manifest = load_manifest_file(&repo_root.join(&category).join(&package))?;
    let binhost_size = binhost.get(&cpv).map(|e| e.size);

    Ok((
        cpn.clone(),
        PkgVersion {
            cpn,
            cpv,
            version_str,
            version,
            slot,
            masked: false,
            live: false,
            iuse_defaults,
            entry,
            manifest,
            binhost_size,
        },
    ))
}

/// `package.mask`-Treffer (Atome mit/ohne Versionsoperator).
fn is_masked(cpn: &str, version_str: &str, entry: &CacheEntry, profile: &Profile) -> bool {
    let version = match portage_atom::Version::parse(version_str) {
        Ok(v) => v,
        Err(_) => return false,
    };
    let slot = full_slot(&entry.metadata.slot);
    // Portage-Semantik: `-atom`-Zeilen entmaskieren fruehere Masken
    // (Profilstapel-Reihenfolge, letzte passende Zeile gewinnt).
    let mut masked = false;
    for line in &profile.package_mask {
        let raw = line.split_whitespace().next().unwrap_or("");
        if raw.is_empty() {
            continue;
        }
        let (atom, unmask) = match raw.strip_prefix('-') {
            Some(rest) if !rest.is_empty() => (rest, true),
            _ => (raw, false),
        };
        let Ok(dep) = Dep::parse(atom) else {
            continue;
        };
        if dep.cpn.to_string() != cpn {
            continue;
        }
        let probe = PkgVersionProbe {
            version: &version,
            version_str,
            slot: &slot,
        };
        if mask_dep_matches(&dep, probe) {
            masked = !unmask;
        }
    }
    masked
}

/// Voller Slot-String (`0/0` oder `0/2`): fehlender Sub-Slot defaultet auf
/// den Slot (PMS-Portage-Semantik, z. B. matcht `:0/0` ein `SLOT=0`-Paket).
fn full_slot(slot: &portage_atom::Slot) -> String {
    let base = slot.slot.as_str();
    match &slot.subslot {
        Some(sub) => format!("{base}/{sub}"),
        None => format!("{base}/{base}"),
    }
}

struct PkgVersionProbe<'a> {
    version: &'a Version,
    version_str: &'a str,
    slot: &'a str,
}

fn mask_dep_matches(dep: &Dep, probe: PkgVersionProbe<'_>) -> bool {
    if let (Some(op), Some(want)) = (&dep.op, &dep.version)
        && !version_matches(*op, dep.glob, want, probe.version, probe.version_str)
    {
        return false;
    }
    if let Some(slot_dep) = &dep.slot_dep {
        match slot_dep {
            SlotDep::Slot {
                slot: Some(named), ..
            } => {
                if !slot_matches(named, probe.slot) {
                    return false;
                }
            }
            SlotDep::Slot { slot: None, .. } | SlotDep::Operator(_) => {}
        }
    }
    true
}

/// KEYWORDS-Filter: stabil immer, Testing nur mit `~arch` in ACCEPT_KEYWORDS.
fn keywords_accepted(entry: &CacheEntry, profile: &Profile) -> bool {
    let keywords = &entry.metadata.keywords;
    if keywords.is_empty() {
        return true;
    }
    let testing_key = format!("~{}", profile.arch);
    for kw in keywords {
        let arch = kw.arch.as_str();
        match kw.stability {
            Stability::Stable => {
                if arch == profile.arch || profile.accept_keywords.contains(arch) {
                    return true;
                }
            }
            Stability::Testing => {
                if arch == profile.arch && profile.accept_keywords.contains(&testing_key) {
                    return true;
                }
            }
            Stability::Disabled => {
                if arch == profile.arch {
                    return false;
                }
            }
            Stability::DisabledAll => {}
        }
    }
    // Nichts Passendes (ggf. nur `-*`) → maskiert.
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn test_profile() -> Profile {
        Profile::test_default()
    }

    fn write_cache(repo: &Path, cat: &str, pf: &str, body: &str) {
        // Echtes Layout: `<kat>/<paket-version>` (zweistufig, kein pkg-Dir).
        let dir = repo.join("metadata").join("md5-cache").join(cat);
        std::fs::create_dir_all(&dir).unwrap();
        let mut f = std::fs::File::create(dir.join(pf)).unwrap();
        f.write_all(body.as_bytes()).unwrap();
    }

    const ENTRY: &str = "\
EAPI=8\n\
DESCRIPTION=test\n\
SLOT=0\n\
KEYWORDS=amd64\n\
IUSE=ssl\n\
RDEPEND=dev-libs/openssl\n";

    #[test]
    fn selects_highest_stable_and_skips_live() {
        let d = tempfile::tempdir().unwrap();
        let repo = d.path();
        write_cache(repo, "app-misc", "jq-1.7.1", ENTRY);
        write_cache(repo, "app-misc", "jq-1.8.0", ENTRY);
        write_cache(repo, "app-misc", "jq-9999", ENTRY);
        let idx = RepoIndex::load(repo, None, &test_profile()).unwrap();
        assert_eq!(idx.cache_files, 3);
        let sel = idx.select("app-misc/jq").unwrap();
        assert_eq!(sel.version_str, "1.8.0");
    }

    #[test]
    fn mask_and_keywords_filter() {
        let d = tempfile::tempdir().unwrap();
        let repo = d.path();
        write_cache(repo, "app-misc", "jq-1.8.0", ENTRY);
        let unstable = ENTRY.replace("KEYWORDS=amd64", "KEYWORDS=~amd64");
        write_cache(repo, "app-misc", "jq-1.9.0", &unstable);
        let mut prof = test_profile();
        prof.package_mask = vec!["=app-misc/jq-1.8.0".into()];
        let idx = RepoIndex::load(repo, None, &prof).unwrap();
        // 1.9.0 Testing ohne ~amd64 raus, 1.8.0 maskiert → nichts waehlbar.
        assert!(idx.select("app-misc/jq").is_none());
        prof.accept_keywords.insert("~amd64".into());
        prof.package_mask.clear();
        let idx = RepoIndex::load(repo, None, &prof).unwrap();
        assert_eq!(idx.select("app-misc/jq").unwrap().version_str, "1.9.0");
    }

    #[test]
    fn dash_unmask_reverts_earlier_mask() {
        // base maskiert, spaeteres `-atom` (Linux-Profil) entmaskiert wieder.
        let d = tempfile::tempdir().unwrap();
        let repo = d.path();
        write_cache(
            repo,
            "sys-libs",
            "libseccomp-2.6.1",
            &ENTRY.replace("IUSE=ssl", "IUSE="),
        );
        let mut prof = test_profile();
        prof.package_mask = vec!["sys-libs/libseccomp".into(), "-sys-libs/libseccomp".into()];
        let idx = RepoIndex::load(repo, None, &prof).unwrap();
        assert!(idx.select("sys-libs/libseccomp").is_some());
        prof.package_mask = vec!["sys-libs/libseccomp".into()];
        let idx = RepoIndex::load(repo, None, &prof).unwrap();
        assert!(idx.select("sys-libs/libseccomp").is_none());
    }

    #[test]
    fn slot_subslot_matches_exactly() {
        // `:0/1` maskiert nur Sub-Slot 1, nicht `0/2`.
        let d = tempfile::tempdir().unwrap();
        let repo = d.path();
        let slot2 = ENTRY.replace("SLOT=0\n", "SLOT=0/2\n");
        let slot1 = ENTRY.replace("SLOT=0\n", "SLOT=0/1\n");
        write_cache(repo, "virtual", "libcrypt-2", &slot2);
        write_cache(repo, "virtual", "libcrypt-1-r2", &slot1);
        let mut prof = test_profile();
        prof.package_mask = vec!["virtual/libcrypt:0/1".into()];
        let idx = RepoIndex::load(repo, None, &prof).unwrap();
        let sel = idx.select("virtual/libcrypt").unwrap();
        assert_eq!(sel.version_str, "2");
    }

    #[test]
    fn version_operators_match() {
        let v = |s: &str| portage_atom::Version::parse(s).unwrap();
        let want = v("1.8.0");
        assert!(version_matches(
            Operator::GreaterOrEqual,
            false,
            &want,
            &v("1.9.0"),
            "1.9.0"
        ));
        assert!(!version_matches(
            Operator::Greater,
            false,
            &want,
            &v("1.8.0"),
            "1.8.0"
        ));
        assert!(version_matches(
            Operator::Equal,
            true,
            &v("1.8"),
            &v("1.8.3"),
            "1.8.3"
        ));
        assert!(version_matches(
            Operator::Approximate,
            false,
            &v("1.8.0"),
            &v("1.8.0-r1"),
            "1.8.0-r1"
        ));
    }
}
