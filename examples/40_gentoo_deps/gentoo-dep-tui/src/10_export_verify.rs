//! 10_export_verify — Portage-Export und `emerge --pretend`-Verifikation.
//!
//! Genau eine Zustaendigkeit: aus Plan-Zustand gueltige Portage-Dateien
//! schreiben und das echte Portage-Orakel (`emerge -p`) parsen. Der Export
//! ergaenzt Host-Konfiguration (`supplement_host_portage_config`), Verify
//! prueft Configroot/`emerge` vorab (kein `ENOENT` mehr).

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

    let written = write_export_files(out_dir, exported.world.clone(), use_lines).map_err(|e| {
        // anyhow → EngineError zurueckfuehren (Dateiname geht verloren, egal).
        EngineError::Config(e.to_string())
    })?;
    exported.world = written.world;
    exported.package_use = written.package_use;
    Ok(exported)
}

/// Export-Dateien schreiben (Top-Level + Portage-Standardlayout).
///
/// Schreibt `world`/`package.use` sowohl menschenlesbar auf Top-Level als
/// auch unter `var/lib/portage/world` / `etc/portage/package.use`, sodass
/// `VerifyRunner::against_configroot(out_dir)` direkt funktioniert.
/// Gemeinsame Schreiblogik fuer `export_portage` und `export_plan` (Task 3.2).
pub fn write_export_files(
    out_dir: &Path,
    world: Vec<String>,
    package_use: Vec<String>,
) -> anyhow::Result<Exported> {
    let world_text = if world.is_empty() {
        String::new()
    } else {
        world.join("\n") + "\n"
    };
    let use_text = if package_use.is_empty() {
        String::new()
    } else {
        package_use.join("\n") + "\n"
    };
    std::fs::create_dir_all(out_dir)?;
    std::fs::write(out_dir.join("world"), &world_text)?;
    std::fs::write(out_dir.join("package.use"), &use_text)?;
    // Portage-Standardlayout fuer `emerge --root … --config-root …`.
    let var_lib = out_dir.join("var/lib/portage");
    std::fs::create_dir_all(&var_lib)?;
    std::fs::write(var_lib.join("world"), &world_text)?;
    let etc = out_dir.join("etc/portage");
    std::fs::create_dir_all(&etc)?;
    std::fs::write(etc.join("package.use"), &use_text)?;
    Ok(Exported {
        world,
        package_use,
        accept_keywords: Vec::new(),
    })
}

/// Ob der Befehl ein lokales `emerge` ist (HIL-ssh-Prefixe ausgenommen).
fn is_local_emerge_command(command: &str) -> bool {
    command.trim_start().starts_with("emerge")
}

/// Ob `emerge` im `PATH` liegt.
fn emerge_in_path() -> bool {
    std::env::var_os("PATH")
        .as_deref()
        .is_some_and(find_emerge_in)
}

/// `emerge`-Binary in einer `PATH`-Liste suchen (testbar, review3 Task 5.3).
fn find_emerge_in(paths: &std::ffi::OsStr) -> bool {
    std::env::split_paths(paths).any(|dir| dir.join("emerge").is_file())
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

/// Host-Portage-Anbindung fuer `emerge --config-root` (review3 Task 5.3).
///
/// Ergaenzt `make.profile`, `make.conf` und `repos.conf` in
/// `<out>/etc/portage/` aus dem laufenden System, sofern dort vorhanden und
/// im Export noch nicht enthalten. Ausserhalb von Gentoo no-op. Generierte
/// Dateien (insb. `package.use`) werden nie ueberschrieben, sodass
/// `emerge --root <out> --config-root <out> @world` auf einem Gentoo-Host
/// direkt gegen die laufenden Paketdefinitionen funktioniert.
pub fn supplement_host_portage_config(out_dir: &Path) -> EngineResult<()> {
    supplement_from(Path::new("/etc/portage"), &out_dir.join("etc/portage"))
}

/// Wie `supplement_host_portage_config`, aber mit explizitem Quellverzeichnis
/// (testbar ohne Host-Dateien).
pub fn supplement_from(host_etc: &Path, out_etc: &Path) -> EngineResult<()> {
    for name in ["make.profile", "make.conf", "repos.conf"] {
        let (src, dst) = (host_etc.join(name), out_etc.join(name));
        if dst.exists() {
            continue;
        }
        copy_path_if_present(&src, &dst)?;
    }
    Ok(())
}

/// Datei, Symlink oder Verzeichnis kopieren (fehlende Quelle = no-op).
fn copy_path_if_present(src: &Path, dst: &Path) -> EngineResult<()> {
    let meta = match std::fs::symlink_metadata(src) {
        Ok(m) => m,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(e) => {
            return Err(EngineError::Io {
                path: src.display().to_string(),
                source: e,
            });
        }
    };
    let io = |e: std::io::Error| EngineError::Io {
        path: src.display().to_string(),
        source: e,
    };
    if meta.file_type().is_symlink() {
        let target = std::fs::read_link(src).map_err(io)?;
        if let Some(parent) = dst.parent() {
            std::fs::create_dir_all(parent).map_err(io)?;
        }
        std::os::unix::fs::symlink(&target, dst).map_err(io)?;
    } else if meta.is_dir() {
        copy_dir_all(src, dst).map_err(io)?;
    } else if meta.is_file() {
        if let Some(parent) = dst.parent() {
            std::fs::create_dir_all(parent).map_err(io)?;
        }
        std::fs::copy(src, dst).map_err(io)?;
    }
    Ok(())
}

/// Verzeichnis rekursiv kopieren (fuer `repos.conf/` u. ae.).
fn copy_dir_all(src: &Path, dst: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let (s, d) = (entry.path(), dst.join(entry.file_name()));
        if entry.file_type()?.is_dir() {
            copy_dir_all(&s, &d)?;
        } else {
            std::fs::copy(&s, &d)?;
        }
    }
    Ok(())
}

impl VerifyRunner {
    /// Standard-Runner gegen ein Export-Verzeichnis.
    ///
    /// Das Verzeichnis enthaelt das Portage-Standardlayout (siehe
    /// `write_export_files`): `var/lib/portage/world` + `etc/portage/…`.
    /// `--config-root` steuert nur `/etc/portage` (package.use/make.conf),
    /// `--root` steuert das Welt- und System-Root — beide zeigen hier auf
    /// dasselbe Verzeichnis, damit `@world` dort aufgeloest wird.
    pub fn against_configroot(configroot: &Path) -> Self {
        Self {
            command: format!(
                "emerge --pretend --emptytree --verbose --root {} --config-root {} @world",
                configroot.display(),
                configroot.display()
            ),
            workdir: configroot.to_path_buf(),
        }
    }

    /// Ausfuehren und parsen (Timeout: Portage-Aufruf, kein harter Abbruch).
    ///
    /// Vorab: Configroot muss existieren (sonst zuerst `[E] Export`), und
    /// lokale `emerge`-Befehle brauchen `emerge` im `PATH` (HIL-Befehle via
    /// ssh-Prefix sind ausgenommen, review3 Task 5.3).
    pub fn run(&self) -> EngineResult<VerifyOutcome> {
        if !self.workdir.is_dir() {
            return Err(EngineError::Config(format!(
                "Configroot fehlt: {} — zuerst [E] Export ausfuehren \
                 (oder `gentoo-dep-tui export`), siehe README",
                self.workdir.display()
            )));
        }
        if is_local_emerge_command(&self.command) && !emerge_in_path() {
            return Err(EngineError::Config(
                "emerge nicht im PATH — Verify braucht ein Gentoo-System \
                 (oder HIL-Befehl via --command)"
                    .into(),
            ));
        }
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
    fn verify_fails_without_configroot() {
        // Fehlendes Verzeichnis → klare Meldung statt `ENOENT` (review3 5.3).
        let missing = PathBuf::from("/tmp/gentoo-dep-tui-definitiv-nicht-da-12345");
        let _ = std::fs::remove_dir_all(&missing);
        let err = VerifyRunner::against_configroot(&missing)
            .run()
            .unwrap_err();
        assert!(
            format!("{err:#}").contains("Export"),
            "Hinweis fehlt: {err:#}"
        );
    }

    #[test]
    fn verify_rejects_missing_emerge() {
        // Existierender Configroot + lokales emerge ohne emerge im Suchpfad
        // → Hinweis statt spaetem emerge-Fehler (helperweise, ohne PATH).
        assert!(!find_emerge_in(std::ffi::OsStr::new(
            "/tmp/gentoo-dep-tui-leer-12345"
        )));
        let d = tempfile::tempdir().unwrap();
        let bin = d.path().join("bin");
        std::fs::create_dir(&bin).unwrap();
        std::fs::write(bin.join("emerge"), "#!/bin/sh\n").unwrap();
        assert!(find_emerge_in(bin.as_os_str()));
        assert!(is_local_emerge_command("emerge --pretend @world"));
        assert!(!is_local_emerge_command("ssh hil emerge --pretend @world"));
    }

    #[test]
    fn supplement_copies_host_config_without_touching_package_use() {
        // Fake-Host: Symlink-Profil, Datei-conf, Verzeichnis-repos.
        let host = tempfile::tempdir().unwrap();
        std::fs::create_dir(host.path().join("real-profile")).unwrap();
        #[cfg(unix)]
        std::os::unix::fs::symlink("real-profile", host.path().join("make.profile")).unwrap();
        std::fs::write(host.path().join("make.conf"), "USE=\"ssl\"\n").unwrap();
        let repos = host.path().join("repos.conf");
        std::fs::create_dir(&repos).unwrap();
        std::fs::write(repos.join("gentoo.conf"), "[gentoo]\n").unwrap();
        // Export mit vorhandenem package.use (darf nicht angefasst werden).
        let out = tempfile::tempdir().unwrap();
        let etc = out.path().join("etc/portage");
        std::fs::create_dir_all(&etc).unwrap();
        std::fs::write(etc.join("package.use"), "a/b x\n").unwrap();
        supplement_from(host.path(), &etc).unwrap();
        assert_eq!(
            std::fs::read_to_string(etc.join("package.use")).unwrap(),
            "a/b x\n"
        );
        assert_eq!(
            std::fs::read_to_string(etc.join("make.conf")).unwrap(),
            "USE=\"ssl\"\n"
        );
        assert_eq!(
            std::fs::read_to_string(etc.join("repos.conf/gentoo.conf")).unwrap(),
            "[gentoo]\n"
        );
        #[cfg(unix)]
        assert!(
            std::fs::symlink_metadata(etc.join("make.profile"))
                .unwrap()
                .file_type()
                .is_symlink()
        );
        // Fehlende Quelle = no-op, kein Fehler.
        supplement_from(&PathBuf::from("/tmp/gentoo-dep-tui-nichts-12345"), &etc).unwrap();
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
