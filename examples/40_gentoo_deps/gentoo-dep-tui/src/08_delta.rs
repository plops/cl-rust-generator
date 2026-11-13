//! 08_delta — Marginal-Delta-Engine und Big-Levers-Aggregation.
//!
//! Genau eine Zustaendigkeit: Was aendert ein Flag-Toggle *wirklich* an der
//! Gesamtgroesse (Reference-Counting steckt implizit in der Neuaufloesung)?
//! Jedes Delta wird durch Simulation (Toggle → Kaskade → Re-Resolve) exakt
//! berechnet, nicht geschaetzt.

use std::collections::{BTreeSet, HashMap, HashSet};

use crate::mod_01_types::{BigLever, EngineResult, FlagDelta, SizeMetrics};
use crate::mod_06_graph::{Engine, resolve};
use crate::mod_07_cascade::resolve_with_cascade;

/// Marginal-Delta eines einzelnen USE-Flag-Toggles auf einem Paket.
///
/// Simuliert den Toggle (inkl. Kaskaden aus `07_cascade`) und vergleicht die
/// Gesamtgroessen vorher/nachher. Shared Dependencies ergeben exakt `0 B`,
/// solange ein anderes Paket sie weiterhin braucht.
pub fn flag_delta(
    engine: &Engine<'_>,
    world: &[String],
    package: &str,
    flag: &str,
) -> EngineResult<FlagDelta> {
    let current = current_flags(engine, package);
    let toggled_on = !current.contains(flag);

    // Nur der Toggle ist expliziter Intent (keine Defaults kopieren —
    // sonst wuerde jeder Default zum scheinbaren User-Intent).
    let mut sim = engine.fork();
    sim.overrides
        .entry(package.to_string())
        .or_default()
        .insert(flag.to_string(), toggled_on);
    let outcome = resolve_with_cascade(&mut sim, world)?;
    let new_totals = outcome.graph.totals();

    let before = resolve_with_cascade(&mut engine.fork(), world)?
        .graph
        .totals();
    let delta = SizeMetrics {
        binpkg_bytes: new_totals.binpkg_bytes - before.binpkg_bytes,
        download_bytes: new_totals.download_bytes - before.download_bytes,
    };
    Ok(FlagDelta {
        package: package.to_string(),
        flag: flag.to_string(),
        current: current.contains(flag),
        delta,
        cascade_note: cascade_note(engine, package, flag, toggled_on),
        conflict: outcome.conflicts.into_iter().next(),
    })
}

/// Ob ein Flag ein Plattform-Fakt ist (ARCH/ELIBC/KERNEL, nicht toggelbar).
fn is_platform_flag(profile: &crate::mod_02_profile::Profile, flag: &str) -> bool {
    flag == profile.arch.to_lowercase()
        || (!profile.elibc.is_empty() && flag == format!("elibc_{}", profile.elibc.to_lowercase()))
        || (!profile.kernel.is_empty()
            && flag == format!("kernel_{}", profile.kernel.to_lowercase()))
}

/// Aktuelle effektive Flags (Defaults + explizite Overrides).
fn current_flags(engine: &Engine<'_>, package: &str) -> HashSet<String> {
    match engine.index.select(package) {
        Some(pkg) => engine.effective_use(pkg),
        None => HashSet::new(),
    }
}

/// Kurzer Kaskaden-Hinweis (erzwungene weitere Flags am selben Paket).
fn cascade_note(
    engine: &Engine<'_>,
    package: &str,
    flag: &str,
    toggled_on: bool,
) -> Option<String> {
    let pkg = engine.index.select(package)?;
    let required = pkg.entry.metadata.required_use.as_ref()?;
    let text = format!("{required:?}");
    if text.contains(flag) {
        Some(format!(
            "REQUIRED_USE beruehrt {flag} (Toggle {}), Kaskade moeglich",
            if toggled_on { "an" } else { "aus" }
        ))
    } else {
        None
    }
}

/// Globale Big Levers: jedes USE-Flag ueber alle aktiven Pakete.
///
/// Deaktiviert das Flag global (pro Paket einzeln simuliert als ein Lauf:
/// alle Pakete, die es anhaben, schalten es aus) und aggregiert Gesamt-Delta,
/// Paketanzahl und Konfliktanzahl. Sortiert nach Binpkg-Ersparnis.
pub fn big_levers(engine: &Engine<'_>, world: &[String]) -> EngineResult<Vec<BigLever>> {
    let base_graph = resolve(engine, world)?;
    let base_totals = base_graph.totals();

    // Alle (Paket, Flag-an)-Paare einsammeln.
    let mut flag_packages: HashMap<String, Vec<String>> = HashMap::new();
    for (cpn, node) in &base_graph.nodes {
        for flag in &node.use_flags {
            flag_packages
                .entry(flag.clone())
                .or_default()
                .push(cpn.clone());
        }
    }

    let mut levers = Vec::new();
    let mut flags: Vec<String> = flag_packages.keys().cloned().collect();
    flags.sort();
    for flag in flags {
        let packages = &flag_packages[&flag];
        let mut sim = engine.fork();
        for cpn in packages {
            sim.overrides
                .entry(cpn.clone())
                .or_default()
                .insert(flag.clone(), false);
        }
        let outcome = resolve_with_cascade(&mut sim, world)?;
        let totals = outcome.graph.totals();
        levers.push(BigLever {
            flag: flag.clone(),
            packages: packages.len(),
            total: SizeMetrics {
                binpkg_bytes: totals.binpkg_bytes - base_totals.binpkg_bytes,
                download_bytes: totals.download_bytes - base_totals.download_bytes,
            },
            conflicts: outcome.conflicts.len(),
        });
    }
    levers.sort_by_key(|l| l.total.binpkg_bytes);
    Ok(levers)
}

/// Alle Flags eines Pakets mit Deltas (fuer `deltas <cat/pkg>`).
pub fn package_deltas(
    engine: &Engine<'_>,
    world: &[String],
    package: &str,
) -> EngineResult<Vec<FlagDelta>> {
    // Kandidaten: IUSE des Pakets + derzeit aktive Flags (sortiert, stabil).
    let mut candidates: BTreeSet<String> = BTreeSet::new();
    if let Some(pkg) = engine.index.select(package) {
        candidates.extend(pkg.iuse_defaults.keys().cloned());
    }
    candidates.extend(current_flags(engine, package));
    // Profil-maskierte Flags sind nie schaltbar.
    candidates.retain(|f| !engine.profile.is_masked(f));
    // Plattform-Fakten (ARCH/ELIBC/KERNEL) sind keine schaltbaren USE-Flags.
    candidates.retain(|f| !is_platform_flag(engine.profile, f));

    let mut out = Vec::new();
    for flag in candidates {
        out.push(flag_delta(engine, world, package, &flag)?);
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mod_02_profile::Profile;
    use crate::mod_03_manifest::parse_manifest;
    use crate::mod_05_repo_index::{PkgVersion, RepoIndex};
    use portage_metadata::CacheEntry;
    use std::collections::HashMap;

    fn pkg(cpn: &str, version: &str, iuse: &str, rdepend: &str, binpkg: u64) -> PkgVersion {
        let text = format!(
            "EAPI=8\nDESCRIPTION=t\nSLOT=0\nKEYWORDS=amd64\nIUSE={iuse}\n\
             RDEPEND={rdepend}\nSRC_URI=http://x/a.tar.gz\n"
        );
        let entry = CacheEntry::parse(&text).unwrap();
        let mut iuse_defaults = HashMap::new();
        for flag in &entry.metadata.iuse {
            iuse_defaults.insert(flag.name().to_string(), flag.is_enabled_default());
        }
        PkgVersion {
            cpn: cpn.into(),
            cpv: format!("{cpn}-{version}"),
            version_str: version.into(),
            version: portage_atom::Version::parse(version).unwrap(),
            slot: "0".into(),
            masked: false,
            live: false,
            iuse_defaults,
            entry,
            manifest: parse_manifest("DIST a.tar.gz 1000\n"),
            binhost_size: Some(binpkg),
        }
    }

    fn fixture() -> (RepoIndex, Profile) {
        let mut map: HashMap<String, Vec<PkgVersion>> = HashMap::new();
        for p in [
            pkg("app/a", "1.0", "+ssl", "ssl? ( dev-libs/openssl )", 100),
            pkg("app/b", "1.0", "+ssl", "ssl? ( dev-libs/openssl )", 100),
            pkg("dev-libs/openssl", "1.0", "+ssl", "", 800),
        ] {
            map.entry(p.cpn.clone()).or_default().push(p);
        }
        (
            RepoIndex {
                versions: map,
                cache_files: 3,
                load_ms: 0,
            },
            Profile::test_default(),
        )
    }

    #[test]
    fn shared_dep_toggle_saves_zero_until_last_user() {
        let (index, profile) = fixture();
        let engine = Engine::new(&index, &profile);
        let world = vec!["app/a".to_string(), "app/b".to_string()];
        // Nur bei A abschalten: openssl bleibt ueber B → exakt 0 B Binpkg.
        let d = flag_delta(&engine, &world, "app/a", "ssl").unwrap();
        assert_eq!(d.delta.binpkg_bytes, 0);
        // Global-Hebel ssl: beide aus → -800 B.
        let levers = big_levers(&engine, &world).unwrap();
        let ssl = levers.iter().find(|l| l.flag == "ssl").unwrap();
        assert_eq!(ssl.total.binpkg_bytes, -800);
        assert_eq!(ssl.packages, 3);
    }

    #[test]
    fn package_deltas_lists_all_flags() {
        let (index, profile) = fixture();
        let engine = Engine::new(&index, &profile);
        let world = vec!["app/a".to_string(), "app/b".to_string()];
        let deltas = package_deltas(&engine, &world, "app/a").unwrap();
        assert_eq!(deltas.len(), 1);
        assert!(deltas[0].current);
    }
}
