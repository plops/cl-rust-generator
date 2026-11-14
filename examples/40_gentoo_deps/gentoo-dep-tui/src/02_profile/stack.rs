//! 02_profile/stack — Profilstapel-Lader und Flag-Dateien.
//!
//! Genau eine Zustaendigkeit: `parent`-Ketten, use.mask/use.force,
//! package.mask, make.defaults, package.use (+ Pfad-Normalisierung).

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::mod_01_types::{EngineError, EngineResult};

use super::parse::{read_key_value, read_to_string, strip_comment};
use super::{Profile, apply_use_token_set, parse_use_tokens};

pub(crate) fn normalize_join(base: &Path, rel: &str) -> PathBuf {
    let mut out = base.to_path_buf();
    for comp in Path::new(rel).components() {
        use std::path::Component;
        match comp {
            Component::ParentDir => {
                out.pop();
            }
            Component::CurDir => {}
            Component::Normal(c) => out.push(c),
            Component::RootDir | Component::Prefix(_) => out.push(comp.as_os_str()),
        }
    }
    out
}

/// Laedt den Profilstapel: folgt `parent`-Dateien ab `profile_path`.
///
/// Sammelt `use.mask`, `use.force`, `package.mask` und `make.defaults`
/// (Kind stapelt sich, Eltern zuerst). Fehlende Dateien sind OK.
pub fn load_profile_stack(profile_path: &Path, profiles_dir: &Path) -> EngineResult<Profile> {
    // Tiefensuche ueber ALLE `parent`-Zeilen (Portage erlaubt mehrere Eltern),
    // Eltern zuerst (Post-Order), Zyklen-/Duplikat-sicher.
    let mut chain: Vec<PathBuf> = Vec::new();
    let mut visited: HashSet<PathBuf> = HashSet::new();
    let mut stack: Vec<(PathBuf, bool)> = vec![(profile_path.to_path_buf(), false)];
    let mut depth = 0usize;
    while let Some((dir, expanded)) = stack.pop() {
        if expanded {
            chain.push(dir);
            continue;
        }
        if !visited.insert(dir.clone()) {
            continue;
        }
        depth += 1;
        if depth > 256 {
            break;
        }
        stack.push((dir.clone(), true));
        // Eltern in umgekehrter Reihenfolge pushen → Originalreihenfolge.
        let mut parents: Vec<PathBuf> = Vec::new();
        if let Ok(text) = std::fs::read_to_string(dir.join("parent")) {
            for line in text.lines().map(str::trim) {
                if line.is_empty() || line.starts_with('#') {
                    continue;
                }
                // Relativ zum Verzeichnis der `parent`-Datei (Portage-Semantik).
                parents.push(normalize_join(&dir, line));
            }
        }
        for parent in parents.into_iter().rev() {
            stack.push((parent, false));
        }
    }

    let mut profile = Profile {
        profiles_dir: profiles_dir.to_path_buf(),
        ..Profile::default()
    };
    let mut expand_vars: HashSet<String> = HashSet::new();
    for dir in &chain {
        let name = dir
            .strip_prefix(profiles_dir)
            .map(|p| p.display().to_string())
            .unwrap_or_default();
        if !name.is_empty() {
            profile.profile_name = name;
        }
        apply_flag_list(&mut profile.use_mask, &dir.join("use.mask"))?;
        apply_flag_list(&mut profile.use_force, &dir.join("use.force"))?;
        extend_atom_lines(&mut profile.package_mask, &dir.join("package.mask"))?;
        if let Ok(kv) = read_key_value(&dir.join("make.defaults")) {
            if let Some(arch) = kv.get("ARCH") {
                profile.arch = arch.clone();
            }
            if let Some(keywords) = kv.get("ACCEPT_KEYWORDS") {
                profile.accept_keywords.extend(parse_use_tokens(keywords));
            }
            if let Some(elibc) = kv.get("ELIBC") {
                profile.elibc = elibc.clone();
            }
            if let Some(kernel) = kv.get("KERNEL") {
                profile.kernel = kernel.clone();
            }
            // `USE` aus make.defaults akkumulieren (Kind erweitert Eltern;
            // `${USE}` steht fuer den bisherigen Wert, `-flag` entfernt).
            if let Some(use_line) = kv.get("USE") {
                let previous = profile.global_use_tokens();
                let expanded = use_line.replace("${USE}", &previous);
                for token in expanded.split_whitespace() {
                    apply_use_token_set(&mut profile.global_use, token);
                }
            }
            // USE_EXPAND-Namen merken (Werte im zweiten Durchgang, da
            // Liste und Werte in verschiedenen Dateien stehen koennen).
            if let Some(use_expand) = kv.get("USE_EXPAND") {
                let vars: Vec<String> = use_expand.split_whitespace().map(str::to_string).collect();
                profile.use_expand.extend(vars.iter().cloned());
                expand_vars.extend(vars);
            }
            // Versteckte Expand-Prefixe (review3 Task 1.1): z. B.
            // `USE_EXPAND_HIDDEN="ABI_X86 ELIBC KERNEL"`.
            if let Some(hidden) = kv.get("USE_EXPAND_HIDDEN") {
                profile
                    .use_expand_hidden
                    .extend(hidden.split_whitespace().map(str::to_string));
            }
        }
    }
    // Zweiter Durchgang: Werte je Variable (Kind ueberschreibt Eltern).
    for dir in &chain {
        if let Ok(kv) = read_key_value(&dir.join("make.defaults")) {
            for var in &expand_vars {
                if let Some(values) = kv.get(var) {
                    profile.expand_defaults.insert(
                        var.clone(),
                        values.split_whitespace().map(str::to_string).collect(),
                    );
                }
            }
        }
    }
    // `${ARCH}` expandieren (Portage setzt ARCH aus make.defaults ein;
    // z. B. ACCEPT_KEYWORDS="${ARCH}" → {"amd64"}).
    if profile.accept_keywords.remove("${ARCH}") {
        profile.accept_keywords.insert(profile.arch.clone());
    }
    if profile.accept_keywords.remove("~${ARCH}") {
        profile.accept_keywords.insert(format!("~{}", profile.arch));
    }
    // Mask-/Use-Regeln einmal parsen (Hot Loops nutzen nur den Index).
    profile.sync_pattern_caches();
    Ok(profile)
}

/// Haengt Atom-Zeilen einer Datei an (Kommentare/Leerzeilen raus).
fn extend_atom_lines(target: &mut Vec<String>, path: &Path) -> EngineResult<()> {
    let text = match std::fs::read_to_string(path) {
        Ok(t) => t,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(e) => {
            return Err(EngineError::Io {
                path: path.display().to_string(),
                source: e,
            });
        }
    };
    // use.mask/use.force enthalten USE-Flags (keine Atome) — hier generisch.
    for line in text.lines() {
        let line = strip_comment(line).trim();
        if line.is_empty() {
            continue;
        }
        // `atom: flag1 flag2`-Form auf reines Flag kuerzen, falls noetig.
        target.push(line.to_string());
    }
    Ok(())
}

/// Liest `use.mask`/`use.force` (reine Flag-Listen, optional `atom: flags`).
///
/// Portage-Semantik: `-flag` hebt eine fruehere Maskierung/Erzwingung auf.
/// Da der Profilstapel Eltern zuerst verarbeitet, entfernt `-flag` den Eintrag
/// aus der akkumulierten Menge statt ihn hinzuzufuegen.
pub fn read_flag_list(path: &Path) -> EngineResult<HashSet<String>> {
    let mut out = HashSet::new();
    apply_flag_list(&mut out, path)?;
    Ok(out)
}

/// Wendet eine `use.mask`-/`use.force`-Datei auf eine akkumulierte Menge an
/// (`-flag` entfernt, `+flag`/`flag` fuegt hinzu).
pub fn apply_flag_list(target: &mut HashSet<String>, path: &Path) -> EngineResult<()> {
    let text = match std::fs::read_to_string(path) {
        Ok(t) => t,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(e) => {
            return Err(EngineError::Io {
                path: path.display().to_string(),
                source: e,
            });
        }
    };
    for line in text.lines() {
        let line = strip_comment(line).trim();
        if line.is_empty() {
            continue;
        }
        // Form `cat/pkg: flag1 -flag2` oder reine Flags.
        let flags = line.split(':').next_back().unwrap_or(line);
        for token in flags.split_whitespace() {
            if let Some(flag) = token.strip_prefix('-') {
                target.remove(flag);
            } else if let Some(flag) = token.strip_prefix('+') {
                target.insert(flag.to_string());
            } else if !token.contains('/') {
                target.insert(token.to_string());
            }
        }
    }
    Ok(())
}

/// Liest reine Atom-Zeilen (Datei oder Verzeichnis, `#`-Kommentare raus).
pub fn read_atom_lines(path: &Path) -> Vec<String> {
    let mut files = Vec::new();
    if path.is_dir() {
        if let Ok(entries) = std::fs::read_dir(path) {
            let mut sorted: Vec<PathBuf> = entries
                .filter_map(|e| e.ok().map(|e| e.path()))
                .filter(|p| p.is_file())
                .collect();
            sorted.sort();
            files = sorted;
        }
    } else if path.is_file() {
        files.push(path.to_path_buf());
    } else {
        return Vec::new();
    }
    let mut out = Vec::new();
    for file in files {
        if let Ok(text) = std::fs::read_to_string(&file) {
            for line in text.lines() {
                let line = strip_comment(line).trim();
                if !line.is_empty() {
                    out.push(line.to_string());
                }
            }
        }
    }
    out
}

/// Liest `package.use` (Datei oder Verzeichnis): `atom flag1 -flag2`.
pub fn read_package_use(path: &Path) -> EngineResult<HashMap<String, Vec<String>>> {
    let mut out: HashMap<String, Vec<String>> = HashMap::new();
    let mut files = Vec::new();
    if path.is_dir() {
        let mut entries: Vec<PathBuf> = std::fs::read_dir(path)
            .map_err(|e| EngineError::Io {
                path: path.display().to_string(),
                source: e,
            })?
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| p.is_file())
            .collect();
        entries.sort();
        files = entries;
    } else if path.is_file() {
        files.push(path.to_path_buf());
    } else {
        return Ok(out);
    }
    for file in files {
        let text = read_to_string(&file)?;
        for line in text.lines() {
            let line = strip_comment(line).trim();
            if line.is_empty() {
                continue;
            }
            let mut parts = line.split_whitespace();
            if let Some(atom) = parts.next() {
                let flags: Vec<String> = parts.map(str::to_string).collect();
                out.entry(atom.to_string()).or_default().extend(flags);
            }
        }
    }
    Ok(out)
}
