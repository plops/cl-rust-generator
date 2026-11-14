//! 05_repo_index — RepoIndex ueber `portage-metadata` + `portage-atom`.
//!
//! Genau eine Zustaendigkeit: md5-cache einlesen, Versionen waehlen,
//! Manifest-/Binhost-Groessen zuordnen. Reine Daten-Pipeline, kein Solver.
//! Paralleles Einlesen via `rayon` + `walkdir`; Manifeste werden vorab
//! lock-frei geladen (`preload_manifests`), `package.mask`-Treffer laufen
//! ueber den vorberechneten Profil-Index (kein `Dep::parse` pro Datei).

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::time::Instant;

use portage_atom::{Dep, Version};
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
    /// IUSE-Defaults: Flag → an per Default.
    pub iuse_defaults: HashMap<String, bool>,
    /// Geparste Metadaten (faule `*DEPEND`-Listen inklusive).
    pub entry: CacheEntry,
    /// `Manifest`-DIST-Groessen dieses Pakets.
    pub manifest: Manifest,
    /// Binhost-`SIZE` des exakten CPV (`None` = kein Binpkg).
    pub binhost_size: Option<u64>,
    /// Gecachte Profil-Baseline-USE (ohne Overrides).
    pub base_use: std::sync::OnceLock<HashSet<String>>,
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
        let _timer = crate::mod_00_perf::ScopeTimer::index_load();
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
            // Nur Ebuild-Eintraege: Kategorie-`Manifest*`-Dateien sind keine
            // Versionen und wuerden sonst als Warnung enden.
            .filter(|p| {
                p.is_file()
                    && !p
                        .file_name()
                        .is_some_and(|n| n.to_string_lossy().starts_with("Manifest"))
            })
            .collect();
        files.sort();
        let cache_files = files.len();

        // Manifeste vorab parallel einlesen (ein `Manifest` pro
        // `(kategorie, paket)`, T03): danach read-only ohne `Mutex`
        // an `parse_cache_file` uebergeben (review3 Task 3.2).
        let manifests = preload_manifests(repo_root, &cache_dir, &files);
        let parsed: Vec<(String, PkgVersion)> = files
            .par_iter()
            .filter_map(|path| {
                match parse_cache_file(
                    repo_root,
                    &cache_dir,
                    path,
                    &binhost_index,
                    profile,
                    &manifests,
                ) {
                    Ok(Some(v)) => Some(v),
                    Ok(None) => None,
                    // Echte Fehler (I/O/Parse) nicht mit Filtern vermischen:
                    // sichtbar loggen statt still schlucken.
                    Err(e) => {
                        eprintln!("warnung: {path:?} uebersprungen: {e}");
                        None
                    }
                }
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

    /// Ob irgendein Top-Paket Binhost-Daten hat (fuer `bin: n/a`-Hinweis).
    pub fn has_any_binhost(&self) -> bool {
        self.versions
            .values()
            .any(|l| l.first().is_some_and(|p| p.binhost_size.is_some()))
    }

    /// Trefferquote Binhost-`SIZE` ueber alle gewaehlten Top-Versionen.
    pub fn binhost_hit_rate(&self) -> f64 {
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
        list.iter().find(|p| {
            crate::mod_02_profile::dep_matches_version_and_slot(
                dep,
                &p.version,
                &p.version_str,
                &p.slot,
            )
        })
    }

    /// Exakte CPV-Auswahl (`cat/pkg-version`); Fallback auf neueste Version
    /// nur bei leerem `cpv` (vermeidet stille Versions-Verluste, Task 1.5).
    pub fn select_cpv(&self, cpn: &str, cpv: &str) -> Option<&PkgVersion> {
        let list = self.versions.get(cpn)?;
        if cpv.is_empty() {
            return list.first();
        }
        list.iter().find(|p| p.cpv == cpv).or_else(|| list.first())
    }
}

/// `<paket>-<version>` am letzten `-` mit Ziffer dahinter trennen
/// (Paketnamen enthalten nie eine solche Endung, Versionen immer).
fn split_pf(pf: &str) -> Option<(&str, &str)> {
    let split = pf
        .rmatch_indices('-')
        .map(|(i, _)| i)
        .find(|i| pf[i + 1..].starts_with(|c: char| c.is_ascii_digit()))?;
    Some((&pf[..split], &pf[split + 1..]))
}

/// Manifeste fuer alle eindeutigen `(kategorie, paket)`-Paare vorab parallel
/// einlesen (lock-frei, review3 Task 3.2).
///
/// Fehlende/fehlerhafte Manifeste fehlen in der Map: `parse_cache_file`
/// laedt sie dann einzeln nach (identisches Fehlerverhalten wie frueher).
fn preload_manifests(
    repo_root: &Path,
    cache_dir: &Path,
    files: &[PathBuf],
) -> HashMap<String, crate::mod_03_manifest::Manifest> {
    let mut pairs: Vec<(String, PathBuf)> = Vec::new();
    for path in files {
        let rel = match path.strip_prefix(cache_dir) {
            Ok(rel) => rel,
            Err(_) => continue,
        };
        let mut parts = rel.components();
        let category = parts
            .next()
            .map(|c| c.as_os_str().to_string_lossy().into_owned());
        let pf = path.file_name().map(|n| n.to_string_lossy().into_owned());
        let (Some(category), Some(pf)) = (category, pf) else {
            continue;
        };
        let Some((package, _)) = split_pf(&pf) else {
            continue;
        };
        pairs.push((
            format!("{category}/{package}"),
            repo_root.join(&category).join(package),
        ));
    }
    pairs.sort();
    pairs.dedup();
    pairs
        .into_par_iter()
        .filter_map(|(key, dir)| {
            load_manifest_file(&dir)
                .ok()
                .map(|manifest| (key, manifest))
        })
        .collect()
}

/// Eine einzelne Cache-Datei parsen (+ Manifest + Binhost zuordnen).
///
/// `Ok(None)`: gefilterte Version (live/maskiert/unkeyworded, kein Fehler).
/// `Err`: echter I/O-/Parse-Fehler (wird im Caller geloggt, Task 1.5).
fn parse_cache_file(
    repo_root: &Path,
    cache_dir: &Path,
    path: &Path,
    binhost: &BinpkgIndex,
    profile: &Profile,
    manifests: &HashMap<String, crate::mod_03_manifest::Manifest>,
) -> EngineResult<Option<(String, PkgVersion)>> {
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
    // (Paketnamen enthalten nie eine solche Endung, Versionen immer;
    // siehe `split_pf`).
    let (package, version_str) = match split_pf(&pf) {
        Some((package, version_str)) => (package.to_string(), version_str.to_string()),
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
    let slot_probe = full_slot(&entry.metadata.slot);
    let keywords_ok = keywords_accepted(&entry, profile, &cpn, &version_str, &slot_probe);
    if live || masked || !keywords_ok {
        return Ok(None);
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
    let cache_key = format!("{category}/{package}");
    // Vorab geladen (lock-frei); fehlt der Eintrag (defektes/fehlendes
    // Manifest), einzeln nachladen wie frueher.
    let manifest = match manifests.get(&cache_key) {
        Some(m) => m.clone(),
        None => load_manifest_file(&repo_root.join(&category).join(&package))?,
    };
    let binhost_size = binhost.get(&cpv).map(|e| e.size);

    Ok(Some((
        cpn.clone(),
        PkgVersion {
            cpn,
            cpv,
            version_str,
            version,
            slot,
            iuse_defaults,
            entry,
            manifest,
            binhost_size,
            base_use: std::sync::OnceLock::new(),
        },
    )))
}

/// `package.mask`-Treffer (Atome mit/ohne Versionsoperator).
///
/// Nutzt `package_mask_index` (`sync_pattern_caches`): Zeilenreihenfolge
/// bleibt (letzte passende Zeile gewinnt), aber kein `Dep::parse` mehr
/// pro Cache-Datei (review3 Task 3.1).
fn is_masked(cpn: &str, version_str: &str, entry: &CacheEntry, profile: &Profile) -> bool {
    crate::mod_00_perf::perf_count_mask_check();
    let version = match portage_atom::Version::parse(version_str) {
        Ok(v) => v,
        Err(_) => return false,
    };
    let slot = full_slot(&entry.metadata.slot);
    // Portage-Semantik: `-atom`-Zeilen entmaskieren fruehere Masken
    // (Profilstapel-Reihenfolge, letzte passende Zeile gewinnt).
    let mut masked = false;
    for rule in &profile.package_mask_index {
        let Some(dep) = &rule.dep else {
            continue;
        };
        if rule.cpn != cpn {
            continue;
        }
        let probe = PkgVersionProbe {
            version: &version,
            version_str,
            slot: &slot,
        };
        if mask_dep_matches(dep, probe) {
            masked = !rule.unmask;
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
    crate::mod_02_profile::dep_matches_version_and_slot(
        dep,
        probe.version,
        probe.version_str,
        probe.slot,
    )
}

/// KEYWORDS-Filter: stabil immer, Testing nur mit `~arch` in ACCEPT_KEYWORDS.
///
/// Leere `KEYWORDS` werden nur via explizitem `package.accept_keywords` akzeptiert.
fn keywords_accepted(
    entry: &CacheEntry,
    profile: &Profile,
    cpn: &str,
    version_str: &str,
    slot: &str,
) -> bool {
    if let Some(rules) = profile.package_accept_keywords.get(cpn) {
        let have = portage_atom::Version::parse(version_str).ok();
        for (dep, kws) in rules {
            let matches = match (dep, &have) {
                (Some(dep), Some(have)) => {
                    crate::mod_02_profile::dep_matches_version_and_slot(
                        dep,
                        have,
                        version_str,
                        slot,
                    )
                }
                (Some(_), None) => false,
                (None, _) => true,
            };
            if !matches {
                continue;
            }
            if kws.contains("**") || kws.contains("*") {
                return true;
            }
            for kw in &entry.metadata.keywords {
                let rendered = kw.to_string();
                if kws.contains(&rendered) {
                    return true;
                }
            }
        }
    }
    let keywords = &entry.metadata.keywords;
    if keywords.is_empty() {
        return false;
    }
    let testing_key = format!("~{}", profile.arch);
    for kw in keywords {
        let arch = kw.arch.as_str();
        match kw.stability {
            Stability::Stable => {
                if arch == profile.arch
                    || profile.accept_keywords.contains(arch)
                    || profile.accept_keywords.contains("*")
                {
                    return true;
                }
            }
            Stability::Testing => {
                if arch == profile.arch
                    && (profile.accept_keywords.contains(&testing_key)
                        || profile.accept_keywords.contains("~*"))
                {
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
#[path = "05_repo_index/tests.rs"]
mod tests;
