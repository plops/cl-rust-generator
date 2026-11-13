//! 10_export_verify — Portage-Export und `emerge --pretend`-Verifikation.
//!
//! Genau eine Zustaendigkeit: aus Plan-Zustand gueltige Portage-Dateien
//! schreiben und das echte Portage-Orakel (`emerge -p`) parsen.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::mod_01_types::{EngineError, EngineResult};

/// Exportierte Portage-Dateien.
#[derive(Debug, Clone, Default)]
pub struct Exported {
    /// Geschriebene `world`-Zeilen.
    pub world: Vec<String>,
    /// Geschriebene `package.use`-Zeilen.
    pub package_use: Vec<String>,
    /// Geschriebene `package.accept_keywords`-Zeilen (i. d. R. leer).
    pub accept_keywords: Vec<String>,
}

/// Exportiert Welt + USE-Flags in ein Zielverzeichnis.
///
/// Schreibt `world`, `package.use` und (falls noetig)
/// `package.accept_keywords` in Portage-Syntax.
pub fn export_portage(
    world: &[String],
    overrides: &HashMap<String, std::collections::HashSet<String>>,
    baseline_use: &HashMap<String, std::collections::HashSet<String>>,
    out_dir: &Path,
) -> EngineResult<Exported> {
    std::fs::create_dir_all(out_dir).map_err(|e| EngineError::Io {
        path: out_dir.display().to_string(),
        source: e,
    })?;
    let mut exported = Exported {
        world: world.to_vec(),
        ..Exported::default()
    };

    // package.use: nur Abweichungen gegenueber der Baseline schreiben.
    let mut use_lines: Vec<String> = Vec::new();
    let mut atoms: Vec<&String> = overrides.keys().collect();
    atoms.sort();
    for atom in atoms {
        let current = &overrides[atom];
        let base = baseline_use.get(atom.as_str());
        let mut tokens: Vec<String> = Vec::new();
        // Flags, die an sind, aber in der Baseline aus (oder unbekannt).
        let mut cur: Vec<&String> = current.iter().collect();
        cur.sort();
        for flag in cur {
            let in_base = base.is_some_and(|b| b.contains(flag.as_str()));
            if !in_base {
                tokens.push(flag.to_string());
            }
        }
        // Flags, die in der Baseline an, jetzt aber aus sind.
        if let Some(b) = base {
            let mut bflags: Vec<&String> = b.iter().collect();
            bflags.sort();
            for flag in bflags {
                if !current.contains(flag.as_str()) {
                    tokens.push(format!("-{flag}"));
                }
            }
        }
        if !tokens.is_empty() {
            use_lines.push(format!("{atom} {}", tokens.join(" ")));
            exported
                .package_use
                .push(format!("{atom} {}", tokens.join(" ")));
        }
    }

    std::fs::write(out_dir.join("world"), exported.world.join("\n") + "\n").map_err(|e| {
        EngineError::Io {
            path: out_dir.join("world").display().to_string(),
            source: e,
        }
    })?;
    std::fs::write(out_dir.join("package.use"), use_lines.join("\n") + "\n").map_err(|e| {
        EngineError::Io {
            path: out_dir.join("package.use").display().to_string(),
            source: e,
        }
    })?;
    Ok(exported)
}

/// Ergebnis eines `emerge --pretend`-Laufs.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct VerifyOutcome {
    /// Erfolg (Exit 0, keine Blocker/Slot-Konflikte im Output).
    pub success: bool,
    /// Erkannte Paketliste (`[ebuild ...]`-Zeilen, gekuerzt auf CPV).
    pub packages: Vec<String>,
    /// Fehler-/Konfliktzeilen (falls `!success`).
    pub problems: Vec<String>,
    /// Roh-Output (gekürzt, Diagnose).
    pub raw_tail: String,
}

/// Parst `emerge --pretend --emptytree --verbose`-Output.
pub fn parse_emerge_pretend(output: &str) -> VerifyOutcome {
    let mut packages = Vec::new();
    let mut problems = Vec::new();
    for line in output.lines() {
        let t = line.trim();
        if t.starts_with("[ebuild") {
            // Format: `[ebuild   R   ] cat/pkg-ver ...` — CPV steht hinter `]`.
            if let Some(after) = t.split(']').nth(1)
                && let Some(cpv) = after.split_whitespace().next()
            {
                packages.push(cpv.to_string());
            }
        } else if t.starts_with("[blocks") {
            problems.push(t.to_string());
        } else {
            let low = t.to_lowercase();
            if low.contains("slot conflict")
                || low.contains("required_use")
                || low.contains("required use")
                || low.contains("unsatisfiable")
                || low.contains("conflict")
            {
                problems.push(t.to_string());
            }
        }
    }
    let tail: Vec<&str> = output.lines().rev().take(20).collect();
    VerifyOutcome {
        success: problems.is_empty(),
        packages,
        problems,
        raw_tail: tail.into_iter().rev().collect::<Vec<_>>().join("\n"),
    }
}

/// Konfigurierbarer Verify-Runner (lokal oder via `sh -c`-Befehl).
#[derive(Debug, Clone)]
pub struct VerifyRunner {
    /// Befehlstemplate, z. B. `emerge --pretend --emptytree --verbose @world`.
    /// Wird via `sh -c` ausgefuehrt (ermoeglicht ssh-Prefixe im HIL).
    pub command: String,
    /// Arbeitsverzeichnis (dort liegen `world`/`package.use` bzw. Configroot).
    pub workdir: PathBuf,
}

impl VerifyRunner {
    /// Standard-Runner gegen ein Configroot-Verzeichnis.
    pub fn against_configroot(configroot: &Path) -> Self {
        Self {
            command: format!(
                "emerge --pretend --emptytree --verbose --config-root {} @world",
                configroot.display()
            ),
            workdir: configroot.to_path_buf(),
        }
    }

    /// Ausfuehren und parsen (Timeout: Portage-Aufruf, kein harter Abbruch).
    pub fn run(&self) -> EngineResult<VerifyOutcome> {
        let out = Command::new("sh")
            .arg("-c")
            .arg(&self.command)
            .current_dir(&self.workdir)
            .output()
            .map_err(|e| EngineError::Io {
                path: self.command.clone(),
                source: e,
            })?;
        let mut text = String::from_utf8_lossy(&out.stdout).into_owned();
        text.push_str(&String::from_utf8_lossy(&out.stderr));
        let mut outcome = parse_emerge_pretend(&text);
        if !out.status.success() && outcome.success {
            outcome.success = false;
            outcome
                .problems
                .push(format!("exit status: {}", out.status));
        }
        Ok(outcome)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CLEAN: &str = "\
These are the packages that would be merged:\n\
\n\
[ebuild   R   ] dev-libs/openssl-3.3.2\n\
[ebuild  N    ] app-misc/jq-1.7.1  USE=\"abbrev\"\n";

    const CONFLICT: &str = "\
[ebuild  N    ] dev-libs/libX-1.0  USE=\"foo\"\n\
[blocks B     ] dev-libs/libX[-foo] (is blocking app/c)\n\
!!! slot conflict detected\n";

    #[test]
    fn parses_clean_pretend() {
        let o = parse_emerge_pretend(CLEAN);
        assert!(o.success);
        assert_eq!(
            o.packages,
            vec!["dev-libs/openssl-3.3.2", "app-misc/jq-1.7.1"]
        );
    }

    #[test]
    fn detects_blocks_and_slot_conflicts() {
        let o = parse_emerge_pretend(CONFLICT);
        assert!(!o.success);
        assert!(o.problems.len() >= 2);
    }

    #[test]
    fn export_writes_portage_syntax() {
        let d = tempfile::tempdir().unwrap();
        let mut overrides = HashMap::new();
        overrides.insert(
            "dev-libs/openssl".to_string(),
            ["ssl".to_string()].into_iter().collect(),
        );
        let mut baseline = HashMap::new();
        baseline.insert(
            "dev-libs/openssl".to_string(),
            ["ssl".to_string(), "tls-compression".to_string()]
                .into_iter()
                .collect(),
        );
        let exp = export_portage(
            &["dev-libs/openssl".into()],
            &overrides,
            &baseline,
            d.path(),
        )
        .unwrap();
        assert_eq!(exp.package_use, vec!["dev-libs/openssl -tls-compression"]);
        let text = std::fs::read_to_string(d.path().join("package.use")).unwrap();
        assert!(text.contains("dev-libs/openssl -tls-compression"));
    }
}
