//! 02_profile — Gentoo-Profilstapel, make.conf, world-Reader.
//!
//! Genau eine Zustaendigkeit: alles Einlesen von *Konfiguration*
//! (kein Ebuild-Parsing). Reihenfolge nach `pkgcore/domain`:
//! Profil-Defaults → `use.mask`/`use.force` → globale `USE` → `package.use`.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::mod_01_types::{EngineError, EngineResult};

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

    /// Wendet Profil + globale USE auf einen IUSE-Defaultsatz an.
    ///
    /// Reihenfolge: IUSE-Defaults (`+flag`) → globale `USE` → `use.force` →
    /// `use.mask` (Maske gewinnt immer, wie in Portage).
    pub fn apply_use_defaults(
        &self,
        iuse_defaults: &HashMap<String, bool>,
        package: &str,
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
        for token in &self.global_use {
            apply_use_token(&mut active, token);
        }
        if let Some(tokens) = self.package_use.get(package) {
            for token in tokens {
                apply_use_token(&mut active, token);
            }
        }
        for flag in &self.use_force {
            active.insert(flag.clone());
        }
        for flag in &self.use_mask {
            active.remove(flag);
        }
        active
    }
}

/// Wendet ein einzelnes USE-Token (`flag`/`-flag`) an.
fn apply_use_token(active: &mut HashSet<String>, token: &str) {
    let token = token.trim();
    if let Some(flag) = token.strip_prefix('-') {
        active.remove(flag);
    } else {
        active.insert(token.to_string());
    }
}

/// Liest eine `world`-Datei (ein Atom pro Zeile, `#`-Kommentare).
pub fn read_world(path: &Path) -> EngineResult<Vec<String>> {
    let text = read_to_string(path)?;
    Ok(text
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(str::to_string)
        .collect())
}

/// Liest `KEY=VALUE`-Dateien (`make.conf`, `make.defaults`).
pub fn read_key_value(path: &Path) -> EngineResult<HashMap<String, String>> {
    let text = match std::fs::read_to_string(path) {
        Ok(t) => t,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(HashMap::new()),
        Err(e) => {
            return Err(EngineError::Io {
                path: path.display().to_string(),
                source: e,
            });
        }
    };
    Ok(parse_key_value(&text))
}

/// Parst `KEY="a b" # kommentar`-Zuweisungen (einfache Shell-Syntax).
pub fn parse_key_value(text: &str) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for line in text.lines() {
        let line = strip_comment(line).trim();
        if line.is_empty() {
            continue;
        }
        if let Some((k, v)) = line.split_once('=') {
            map.insert(k.trim().to_string(), unquote(v.trim()));
        }
    }
    map
}

/// Entfernt Anführungszeichen um einen Wert.
fn unquote(value: &str) -> String {
    let value = value.trim();
    if value.len() >= 2
        && ((value.starts_with('"') && value.ends_with('"'))
            || (value.starts_with('\'') && value.ends_with('\'')))
    {
        value[1..value.len() - 1].to_string()
    } else {
        value.to_string()
    }
}

/// Schneidet `#`-Kommentare ab (ausserhalb von Anfuehrungszeichen).
fn strip_comment(line: &str) -> &str {
    let mut in_single = false;
    let mut in_double = false;
    for (i, c) in line.char_indices() {
        match c {
            '\'' if !in_double => in_single = !in_single,
            '"' if !in_single => in_double = !in_double,
            '#' if !in_single && !in_double => return &line[..i],
            _ => {}
        }
    }
    line
}

/// Parst `USE`-Tokens (`"ssl -doc"` → Menge, Negation bleibt erhalten).
pub fn parse_use_tokens(value: &str) -> HashSet<String> {
    value.split_whitespace().map(str::to_string).collect()
}

/// Pfad lexikalisch normalisieren (`.`/`..` aufloesen, ohne FS-Zugriff).
fn normalize_join(base: &Path, rel: &str) -> PathBuf {
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
        profile
            .use_mask
            .extend(read_flag_list(&dir.join("use.mask"))?);
        profile
            .use_force
            .extend(read_flag_list(&dir.join("use.force"))?);
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
            // USE_EXPAND-Namen merken (Werte im zweiten Durchgang, da
            // Liste und Werte in verschiedenen Dateien stehen koennen).
            if let Some(use_expand) = kv.get("USE_EXPAND") {
                expand_vars.extend(use_expand.split_whitespace().map(str::to_string));
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
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        // `atom: flag1 flag2`-Form auf reines Flag kuerzen, falls noetig.
        target.push(line.to_string());
    }
    Ok(())
}

/// Liest `use.mask`/`use.force` (reine Flag-Listen, optional `atom: flags`).
pub fn read_flag_list(path: &Path) -> EngineResult<HashSet<String>> {
    let text = match std::fs::read_to_string(path) {
        Ok(t) => t,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(HashSet::new()),
        Err(e) => {
            return Err(EngineError::Io {
                path: path.display().to_string(),
                source: e,
            });
        }
    };
    let mut out = HashSet::new();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        // Form `cat/pkg: flag1 -flag2` oder reine Flags.
        let flags = line.split(':').next_back().unwrap_or(line);
        for token in flags.split_whitespace() {
            if token.starts_with('-') || token.starts_with('+') {
                out.insert(token[1..].to_string());
            } else if !token.contains('/') {
                out.insert(token.to_string());
            }
        }
    }
    Ok(out)
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

/// Datei lesen mit Engine-Fehler.
fn read_to_string(path: &Path) -> EngineResult<String> {
    std::fs::read_to_string(path).map_err(|e| EngineError::Io {
        path: path.display().to_string(),
        source: e,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn tmpdir() -> tempfile::TempDir {
        tempfile::tempdir().unwrap()
    }

    #[test]
    fn world_skips_comments_and_blanks() {
        let d = tmpdir();
        let p = d.path().join("world");
        let mut f = std::fs::File::create(&p).unwrap();
        writeln!(f, "# kommentar\n\napp-misc/jq\n\n  dev-libs/openssl  ").unwrap();
        let w = read_world(&p).unwrap();
        assert_eq!(w, vec!["app-misc/jq", "dev-libs/openssl"]);
    }

    #[test]
    fn key_value_parses_quoted_and_comment() {
        let kv = parse_key_value("USE=\"ssl -doc\" # kommentar\nARCH=amd64\n");
        assert_eq!(kv.get("USE").unwrap(), "ssl -doc");
        assert_eq!(kv.get("ARCH").unwrap(), "amd64");
    }

    #[test]
    fn use_expand_seeds_effective_flags() {
        let d = tmpdir();
        let profiles = d.path().join("profiles");
        let base = profiles.join("base");
        std::fs::create_dir_all(&base).unwrap();
        std::fs::write(
            base.join("make.defaults"),
            "ARCH=amd64\nELIBC=glibc\nKERNEL=linux\nUSE_EXPAND=\"PYTHON_TARGETS ABI_X86\"\nPYTHON_TARGETS=\"python3_14\"\nABI_X86=\"64\"\n",
        )
        .unwrap();
        let prof = load_profile_stack(&base, &profiles).unwrap();
        let mut iuse = HashMap::new();
        for f in [
            "python_targets_python3_13",
            "python_targets_python3_14",
            "abi_x86_64",
            "ssl",
        ] {
            iuse.insert(f.to_string(), false);
        }
        let active = prof.apply_use_defaults(&iuse, "dev-python/jaraco-context");
        // Expand-Defaults greifen (nur fuer IUSE-Flags), Plattform immer.
        assert!(active.contains("python_targets_python3_14"));
        assert!(!active.contains("python_targets_python3_13"));
        assert!(active.contains("abi_x86_64"));
        assert!(active.contains("amd64"));
        assert!(active.contains("elibc_glibc"));
        assert!(active.contains("kernel_linux"));
        assert!(!active.contains("ssl"));
    }

    #[test]
    fn arch_variable_expands_in_accept_keywords() {
        let d = tmpdir();
        let profiles = d.path().join("profiles");
        let base = profiles.join("base");
        std::fs::create_dir_all(&base).unwrap();
        std::fs::write(
            base.join("make.defaults"),
            "ARCH=amd64\nACCEPT_KEYWORDS=\"${ARCH}\"\n",
        )
        .unwrap();
        let prof = load_profile_stack(&base, &profiles).unwrap();
        assert_eq!(prof.arch, "amd64");
        assert!(prof.accept_keywords.contains("amd64"));
        assert!(!prof.accept_keywords.contains("${ARCH}"));
    }

    #[test]
    fn profile_stack_inherits_parent() {
        let d = tmpdir();
        let profiles = d.path().join("profiles");
        let base = profiles.join("base");
        let child = profiles.join("default/linux/amd64/23.0");
        std::fs::create_dir_all(&child).unwrap();
        std::fs::create_dir_all(&base).unwrap();
        std::fs::write(base.join("use.mask"), "wayland\n").unwrap();
        std::fs::write(base.join("make.defaults"), "ARCH=amd64\n").unwrap();
        std::fs::write(child.join("parent"), "../../../../../base\n").unwrap();
        std::fs::write(child.join("use.mask"), "systemd\n").unwrap();
        let mut prof = load_profile_stack(&child, &profiles).unwrap();
        prof.use_mask = read_flag_list(&base.join("use.mask")).unwrap();
        let child_mask = read_flag_list(&child.join("use.mask")).unwrap();
        prof.use_mask.extend(child_mask);
        assert!(prof.use_mask.contains("wayland"));
        assert!(prof.use_mask.contains("systemd"));
    }

    #[test]
    fn apply_use_defaults_order() {
        let mut p = Profile::test_default();
        p.global_use = ["ssl".into(), "-doc".into()].into_iter().collect();
        p.use_force = ["gui".into()].into_iter().collect();
        p.use_mask = ["ssl".into()].into_iter().collect();
        let mut iuse = HashMap::new();
        iuse.insert("doc".into(), true);
        iuse.insert("ssl".into(), false);
        let active = p.apply_use_defaults(&iuse, "app-misc/jq");
        // Maske gewinnt ueber global USE.
        assert!(!active.contains("ssl"));
        assert!(active.contains("gui"));
        assert!(!active.contains("doc"));
    }

    #[test]
    fn package_use_dir_reads_all_files() {
        let d = tmpdir();
        let dir = d.path().join("package.use");
        std::fs::create_dir(&dir).unwrap();
        std::fs::write(dir.join("00-base"), "dev-libs/openssl -ssl\n").unwrap();
        std::fs::write(dir.join("10-extra"), "# c\napp-misc/jq abbrev\n").unwrap();
        let pu = read_package_use(&dir).unwrap();
        assert_eq!(
            pu.get("dev-libs/openssl").unwrap(),
            &vec!["-ssl".to_string()]
        );
        assert_eq!(pu.get("app-misc/jq").unwrap(), &vec!["abbrev".to_string()]);
    }
}
