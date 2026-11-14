//! 02_profile — Gentoo-Profilstapel, make.conf, world-Reader.
//!
//! Genau eine Zustaendigkeit: alles Einlesen von *Konfiguration*
//! (kein Ebuild-Parsing). Reihenfolge nach `pkgcore/domain`:
//! Profil-Defaults → `use.mask`/`use.force` → globale `USE` → `package.use`.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

/// Ausgewaehlte Architektur-/Keyword-Konfiguration.
#[derive(Debug, Clone, Default)]
pub struct Profile {
    /// Basisverzeichnis des Repo-Profils (z. B. `.../profiles`).
    pub profiles_dir: PathBuf,
    /// Aktives Profil (z. B. `default/linux/amd64/23.0/no-multilib`).
    pub profile_name: String,
    /// Ziel-Architektur (z. B. `amd64`).
    pub arch: String,
    /// Akzeptierte Keywords: stabil-Key + Testing-Key (z. B. `amd64`, `~amd64`).
    pub accept_keywords: HashSet<String>,
    /// Maskierte Flags (duerfen nicht an sein).
    pub use_mask: HashSet<String>,
    /// Erzwungene Flags (immer an).
    pub use_force: HashSet<String>,
    /// Maskierte Paket-Atome (eine Zeile pro Eintrag, Kommentare entfernt).
    pub package_mask: Vec<String>,
    /// Globale USE-Flags aus `make.conf` (mit `-`-Negation).
    pub global_use: HashSet<String>,
    /// Paket-spezifische USE-Flags aus `package.use` (`atom -> {flags}`).
    pub package_use: HashMap<String, Vec<String>>,
    /// USE_EXPAND-Variablen (`PYTHON_TARGETS` → Werte, z. B. `["python3_14"]`).
    pub expand_defaults: HashMap<String, Vec<String>>,
    /// Bekannte USE_EXPAND-Namen (auch ohne Defaults, z. B. nur in `make.conf`).
    pub use_expand: HashSet<String>,
    /// ELIBC-Wert (`glibc` → Flag `elibc_glibc`).
    pub elibc: String,
    /// KERNEL-Wert (`linux` → Flag `kernel_linux`).
    pub kernel: String,
}

impl Profile {
    /// Minimalprofil fuer Tests (amd64, stabil).
    pub fn test_default() -> Self {
        Self {
            arch: "amd64".into(),
            accept_keywords: ["amd64".to_string()].into_iter().collect(),
            ..Self::default()
        }
    }

    /// Prueft, ob ein USE-Flag effektiv erzwungen ist.
    pub fn is_forced(&self, flag: &str) -> bool {
        self.use_force.contains(flag)
    }

    /// Prueft, ob ein USE-Flag maskiert (verboten) ist.
    pub fn is_masked(&self, flag: &str) -> bool {
        self.use_mask.contains(flag)
    }

    /// Alle `package.use`-Tokens, die auf ein konkretes Paket zutreffen.
    ///
    /// Exakte `cat/pkg`-Schluessel plus versionierte/slottierte Atome
    /// (`>=cat/pkg-1.0`, `cat/pkg:0`), gematcht gegen Version und Slot.
    pub fn package_use_for(&self, cpn: &str, version_str: &str, slot: &str) -> Vec<String> {
        let mut out = Vec::new();
        // Sortiert: deterministische Token-Reihenfolge unabhaengig von
        // HashMap-Iteration (sonst wuerde z. B. `-x`/`x` zufaellig gewinnen).
        let mut atoms: Vec<&String> = self.package_use.keys().collect();
        atoms.sort();
        for atom in atoms {
            let tokens = &self.package_use[atom];
            if atom == cpn {
                out.extend(tokens.iter().cloned());
                continue;
            }
            let Ok(dep) = portage_atom::Dep::parse(atom) else {
                continue;
            };
            if dep.cpn.to_string() != cpn {
                continue;
            }
            if !Self::dep_matches_version(&dep, version_str, slot) {
                continue;
            }
            out.extend(tokens.iter().cloned());
        }
        out
    }

    /// Versions-/Slot-Abgleich eines `package.use`-Atoms.
    fn dep_matches_version(dep: &portage_atom::Dep, version_str: &str, slot: &str) -> bool {
        let Ok(have) = portage_atom::Version::parse(version_str) else {
            return dep.op.is_none() && dep.slot_dep.is_none();
        };
        dep_matches_version_and_slot(dep, &have, version_str, slot)
    }

    /// Gespeicherte globale USE-Tokens als String (fuer `${USE}`-Expansion).
    pub fn global_use_tokens(&self) -> String {
        let mut tokens: Vec<&String> = self.global_use.iter().collect();
        tokens.sort();
        tokens
            .into_iter()
            .map(String::as_str)
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Wendet Profil + globale USE auf einen IUSE-Defaultsatz an.
    ///
    /// Exakte Portage-Reihenfolge (siehe `effective_defaults`).
    pub fn apply_use_defaults(
        &self,
        iuse_defaults: &HashMap<String, bool>,
        package: &str,
    ) -> HashSet<String> {
        self.effective_defaults(iuse_defaults, package, "", "")
    }

    /// Effektive USE-Flags mit exakter Portage-Praezedenz (Task 1.3).
    ///
    /// Reihenfolge:
    /// 1) IUSE-Defaults (`+flag`)
    /// 2) `ARCH`, `ELIBC`, `KERNEL`
    /// 3) `USE_EXPAND`-Defaults (nur Flags aus `iuse_defaults`)
    /// 4) globale `USE`
    /// 5) `package.use` (exakt + versioniert/slottiert, in Map-Reihenfolge)
    /// 6) `use.force`
    /// 7) `use.mask` (Maske gewinnt immer)
    pub fn effective_defaults(
        &self,
        iuse_defaults: &HashMap<String, bool>,
        cpn: &str,
        version_str: &str,
        slot: &str,
    ) -> HashSet<String> {
        let mut active: HashSet<String> = iuse_defaults
            .iter()
            .filter_map(|(f, on)| if *on { Some(f.clone()) } else { None })
            .collect();
        // Implizite Plattform-Flags (Portage setzt ARCH/ELIBC/KERNEL immer).
        if !self.arch.is_empty() {
            active.insert(self.arch.to_lowercase());
        }
        if !self.elibc.is_empty() {
            active.insert(format!("elibc_{}", self.elibc.to_lowercase()));
        }
        if !self.kernel.is_empty() {
            active.insert(format!("kernel_{}", self.kernel.to_lowercase()));
        }
        // USE_EXPAND-Defaults, aber nur fuer Flags aus IUSE des Pakets
        // (sonst wuerde z. B. jedes Paket `python_targets_*` tragen).
        for (var, values) in &self.expand_defaults {
            let prefix = var.to_lowercase();
            for value in values {
                let flag = format!("{prefix}_{}", value.to_lowercase());
                if iuse_defaults.contains_key(&flag) {
                    active.insert(flag);
                }
            }
        }
        // Deterministische Reihenfolge statt HashSet-Iteration.
        let mut global: Vec<&String> = self.global_use.iter().collect();
        global.sort();
        for token in global {
            apply_use_token(&mut active, token);
        }
        for token in self.package_use_for(cpn, version_str, slot) {
            apply_use_token(&mut active, &token);
        }
        for flag in &self.use_force {
            active.insert(flag.clone());
        }
        for flag in &self.use_mask {
            active.remove(flag);
        }
        active
    }

    /// Wendet `USE`-Tokens inkrementell auf `global_use` an (Task 1.2).
    ///
    /// `-*` leert zuerst, `-flag`/`flag` danach sequentiell. `global_use`
    /// enthaelt dadurch nie `-`-Eintraege und bleibt deterministisch.
    pub fn apply_use_tokens(&mut self, tokens: &str) {
        for token in tokens.split_whitespace() {
            apply_use_token_set(&mut self.global_use, token);
        }
    }
}

/// Gemeinsamer PMS Versions-/Slot-Abgleich (Task 1.1).
///
/// `have`/`have_str` beschreiben die konkrete Paketversion, `slot` den vollen
/// Slot (`0` oder `0/2`). `Op::Equal` mit `glob` ist reiner String-Prefix
/// auf dem Versionsstring (PMS 8.3.1 `=*`, z. B. matcht `=1*` auch `10`).
pub(crate) fn dep_matches_version_and_slot(
    dep: &portage_atom::Dep,
    have: &portage_atom::Version,
    have_str: &str,
    slot: &str,
) -> bool {
    if let (Some(op), Some(want)) = (&dep.op, &dep.version)
        && !version_matches(*op, dep.glob, want, have, have_str)
    {
        return false;
    }
    if let Some(slot_dep) = &dep.slot_dep {
        match slot_dep {
            portage_atom::SlotDep::Slot {
                slot: Some(named), ..
            } => {
                if !slot_matches(named, slot) {
                    return false;
                }
            }
            portage_atom::SlotDep::Slot { slot: None, .. } | portage_atom::SlotDep::Operator(_) => {
            }
        }
    }
    true
}

/// Slot-Abgleich: Slot-Name muss passen; ist im Atom ein Sub-Slot genannt
/// (`:0/1`), muss auch er passen.
pub(crate) fn slot_matches(named: &portage_atom::Slot, pkg_full_slot: &str) -> bool {
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
pub(crate) fn version_matches(
    op: portage_atom::Operator,
    glob: bool,
    want: &portage_atom::Version,
    have: &portage_atom::Version,
    have_str: &str,
) -> bool {
    use portage_atom::Operator as Op;
    use std::cmp::Ordering as Ord;
    if glob && op == Op::Equal {
        return have_str.starts_with(&want.to_string());
    }
    match op {
        Op::Less => have.cmp(want) == Ord::Less,
        Op::LessOrEqual => have.cmp(want) != Ord::Greater,
        Op::Equal => have.cmp(want) == Ord::Equal,
        Op::Approximate => strip_revision(have) == strip_revision(want),
        Op::GreaterOrEqual => have.cmp(want) != Ord::Less,
        Op::Greater => have.cmp(want) == Ord::Greater,
    }
}

/// Versionsstring ohne `-rN`-Revision (fuer `~`-Vergleiche).
pub(crate) fn strip_revision(version: &portage_atom::Version) -> String {
    let rendered = version.to_string();
    match rendered.rsplit_once("-r") {
        Some((base, tail)) if tail.chars().all(|c| c.is_ascii_digit()) => base.to_string(),
        _ => rendered,
    }
}

/// Wendet ein Token auf eine gespeicherte USE-Menge an.
///
/// `-*` leert die Menge, `-flag` entfernt, `flag` (oder `+flag`) fuegt hinzu.
/// `global_use` enthaelt dadurch nie `-`-Eintraege.
pub(super) fn apply_use_token_set(set: &mut HashSet<String>, token: &str) {
    let token = token.trim();
    if token == "-*" {
        set.clear();
    } else if let Some(flag) = token.strip_prefix('-') {
        set.remove(flag);
    } else if let Some(flag) = token.strip_prefix('+') {
        set.insert(flag.to_string());
    } else {
        set.insert(token.to_string());
    }
}

/// Wendet ein einzelnes USE-Token (`flag`/`-flag`) an.
fn apply_use_token(active: &mut HashSet<String>, token: &str) {
    let token = token.trim();
    if token == "-*" {
        active.clear();
    } else if let Some(flag) = token.strip_prefix('-') {
        active.remove(flag);
    } else if let Some(flag) = token.strip_prefix('+') {
        active.insert(flag.to_string());
    } else {
        active.insert(token.to_string());
    }
}

#[path = "02_profile/parse.rs"]
mod parse;
#[path = "02_profile/stack.rs"]
mod stack;
#[cfg(test)]
#[path = "02_profile/tests.rs"]
mod tests;

pub use parse::{parse_key_value, parse_use_tokens, read_key_value, read_world};
pub(crate) use stack::normalize_join;
pub use stack::{apply_flag_list, load_profile_stack, read_flag_list, read_package_use};
