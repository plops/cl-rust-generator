//! 11_cli/context — Profil/Index/Welt laden (owned, kein Leak).

use anyhow::Context;

use crate::mod_02_profile::{self, Profile};
use crate::mod_05_repo_index::RepoIndex;
use crate::mod_06_graph::Engine;

use super::args::CommonArgs;

pub fn load_profile(args: &CommonArgs) -> anyhow::Result<Profile> {
    let profiles_dir = args
        .profile_dir
        .clone()
        .unwrap_or_else(|| args.repo.join("profiles"));
    // Ohne `--profile`: `/etc/portage/make.profile`-Symlink aufloesen (T09).
    let profile_name = args.profile.clone().unwrap_or_else(|| {
        resolve_make_profile(&profiles_dir)
            .unwrap_or_else(|| "default/linux/amd64/23.0".to_string())
    });
    let mut profile =
        mod_02_profile::load_profile_stack(&profiles_dir.join(&profile_name), &profiles_dir)
            .with_context(|| "Profilstapel laden")?;
    profile.profile_name = profile_name;
    if profile.arch.is_empty() {
        profile.arch = "amd64".into();
    }
    if profile.accept_keywords.is_empty() {
        profile.accept_keywords.insert(profile.arch.clone());
    }
    // make.conf: USE + ACCEPT_KEYWORDS einmischen (inkrementell, T09).
    // Profil-`USE` bleibt erhalten, wenn make.conf kein `${USE}` traegt
    // (Portage-Verhalten); `-*`/`-flag` werden via `apply_use_tokens`
    // deterministisch aufgeloest statt als `-`-Eintraege zu lagern.
    if let Ok(kv) = mod_02_profile::read_key_value(&args.make_conf) {
        if let Some(use_line) = kv.get("USE") {
            profile.apply_use_tokens(&use_line.replace("${USE}", ""));
        }
        if let Some(keywords) = kv.get("ACCEPT_KEYWORDS") {
            profile
                .accept_keywords
                .extend(mod_02_profile::parse_use_tokens(keywords));
        }
        if let Some(arch) = kv.get("ARCH") {
            profile.arch = arch.clone();
        }
        // USE_EXPAND-Werte aus make.conf haben Vorrang vor Profil-Defaults.
        // Auch Variablen ohne Profil-Defaults (nur in make.conf gesetzt,
        // z. B. `VIDEO_CARDS`, `L10N`) werden uebernommen.
        let mut expand_vars: Vec<String> = profile.use_expand.iter().cloned().collect();
        expand_vars.extend(profile.expand_defaults.keys().cloned());
        expand_vars.sort();
        expand_vars.dedup();
        for var in expand_vars {
            if let Some(line) = kv.get(&var) {
                profile.use_expand.insert(var.clone());
                profile
                    .expand_defaults
                    .insert(var, line.split_whitespace().map(str::to_string).collect());
            }
        }
    }
    // Hinweis: `load_profile_stack` laedt bereits den kompletten Stapel
    // (`use.mask`/`use.force`/`package.mask`); kein Nachladen noetig.
    profile.package_use = mod_02_profile::read_package_use(&args.package_use).unwrap_or_default();
    profile.sync_pattern_caches();
    Ok(profile)
}

/// `/etc/portage/make.profile`-Symlink auf Profilnamen aufloesen.
///
/// Gibt `None` zurueck, wenn kein Symlink existiert (Caller nutzt Fallback).
/// Pfade innerhalb von `profiles_dir` werden relativ zurueckgegeben.
fn resolve_make_profile(profiles_dir: &std::path::Path) -> Option<String> {
    resolve_make_profile_from(
        std::path::Path::new("/etc/portage/make.profile"),
        profiles_dir,
    )
}

/// Testbare Kernlogik: Symlink-Ziel lexikalisch normalisieren (`..`/`./`)
/// und relativ zu `profiles_dir` kuerzen (Task 1.4).
pub(super) fn resolve_make_profile_from(
    link: &std::path::Path,
    profiles_dir: &std::path::Path,
) -> Option<String> {
    let target = std::fs::read_link(link).ok()?;
    let abs = if target.is_absolute() {
        target
    } else {
        link.parent()
            .unwrap_or(std::path::Path::new("/"))
            .join(target)
    };
    // Beide Seiten lexikalisch normalisieren (relative Symlinks wie
    // `../../var/db/repos/gentoo/profiles/...` enthalten `..`-Segmente).
    let norm_abs =
        mod_02_profile::normalize_join(std::path::Path::new("/"), &abs.display().to_string());
    let norm_base = mod_02_profile::normalize_join(
        std::path::Path::new("/"),
        &profiles_dir.display().to_string(),
    );
    let profiles_canonical = norm_base.components().collect::<Vec<_>>();
    let abs_comps = norm_abs.components().collect::<Vec<_>>();
    if abs_comps.len() > profiles_canonical.len()
        && abs_comps[..profiles_canonical.len()] == profiles_canonical[..]
    {
        let rest: std::path::PathBuf = abs_comps[profiles_canonical.len()..].iter().collect();
        let s = rest.display().to_string();
        if !s.is_empty() {
            return Some(s);
        }
    }
    // Fallback: letzter Pfadanteil als Hinweis (kein gueltiges Profil).
    None
}

/// Welt lesen (fehlende Datei ist ein Fehler mit Hinweis, T09).
pub fn load_world(args: &CommonArgs) -> anyhow::Result<Vec<String>> {
    mod_02_profile::read_world(&args.world).with_context(|| {
        format!(
            "world-Datei nicht gefunden unter {} — bitte mit --world <PFAD> angeben",
            args.world.display()
        )
    })
}

/// Engine-Zugehoer aus Common-Argumenten aufbauen (owned, kein Leak).
pub struct LoadedContext {
    /// Repo-Index (owned).
    pub index: RepoIndex,
    /// Profil + make.conf (owned).
    pub profile: Profile,
    /// Welt-Atome.
    pub world: Vec<String>,
}

impl LoadedContext {
    /// Engine-Bundle auf owned Daten (lebt solange wie der Context).
    pub fn engine(&self) -> Engine<'_> {
        Engine::new(&self.index, &self.profile)
    }
}

/// Profil, Index und Welt aus Common-Argumenten laden.
pub fn load_context(args: &CommonArgs) -> anyhow::Result<LoadedContext> {
    let profile = load_profile(args)?;
    let world = load_world(args)?;
    let index = RepoIndex::load(&args.repo, args.binhost.as_deref(), &profile)
        .with_context(|| "Repo-Index laden")?;
    Ok(LoadedContext {
        index,
        profile,
        world,
    })
}
