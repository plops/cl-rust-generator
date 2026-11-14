//! 02_profile/descriptions — Offline-USE-Flag-Beschreibungen.
//!
//! Genau eine Zustaendigkeit: `use.desc`, `use.local.desc` und `desc/*.desc`
//! aus `<repo>/profiles` laden und abfragen (lokal vor global, review3 4.2).

use std::collections::HashMap;
use std::path::Path;

/// Offline-Beschreibungen zu USE-Flags (tolerant geladen: fehlende oder
/// unlesbare Dateien ergeben leere Maps statt Fehler).
#[derive(Debug, Clone, Default)]
pub struct UseDescriptions {
    /// Globale Flags: `flag` → Beschreibung (aus `use.desc`, `desc/*.desc`).
    global: HashMap<String, String>,
    /// Paket-lokale Flags: `cat/pkg` → `flag` → Beschreibung.
    local: HashMap<String, HashMap<String, String>>,
}

impl UseDescriptions {
    /// Alles laden, was existiert (jede Datei einzeln tolerant).
    pub fn load(profiles_dir: &Path) -> Self {
        let mut out = Self::default();
        out.global
            .extend(parse_desc_file(&profiles_dir.join("use.desc"), None));
        // `desc/*.desc`: nur fehlende Eintraege ergaenzen.
        let desc_dir = profiles_dir.join("desc");
        if let Ok(entries) = std::fs::read_dir(&desc_dir) {
            let mut files: Vec<_> = entries.filter_map(|e| e.ok().map(|e| e.path())).collect();
            files.sort();
            for file in files {
                if file.is_file() {
                    for (flag, text) in parse_desc_file(&file, None) {
                        out.global.entry(flag).or_insert(text);
                    }
                }
            }
        }
        for (cpn, flag, text) in parse_local_desc_file(&profiles_dir.join("use.local.desc")) {
            out.local.entry(cpn).or_default().insert(flag, text);
        }
        out
    }

    /// Beschreibung eines Flags: paket-lokal vor global (review3 Task 4.2).
    pub fn describe(&self, cpn: Option<&str>, flag: &str) -> Option<&str> {
        if let Some(cpn) = cpn
            && let Some(text) = self.local.get(cpn).and_then(|m| m.get(flag))
        {
            return Some(text);
        }
        self.global.get(flag).map(String::as_str)
    }
}

/// `use.desc`-Format parsen: `flag - Beschreibung` je Zeile.
///
/// `cpn_filter`: nur Zeilen dieses Pakets behalten (`None` = alle).
/// Rueckgabe: `(flag, beschreibung)`; ungueltige Zeilen entfallen still.
fn parse_desc_file(path: &Path, cpn_filter: Option<&str>) -> Vec<(String, String)> {
    let text = match std::fs::read_to_string(path) {
        Ok(t) => t,
        Err(_) => return Vec::new(),
    };
    let mut out = Vec::new();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        // Paket-Prefix (`cat/pkg:flag - ...`) bei Bedarf abstreifen.
        let mut rest = line;
        if let Some((head, tail)) = line.split_once(':')
            && head.contains('/')
        {
            if cpn_filter.is_some_and(|c| c != head.trim()) {
                continue;
            }
            rest = tail.trim();
        } else if cpn_filter.is_some() {
            continue;
        }
        let Some((flag, desc)) = rest.split_once(" - ") else {
            continue;
        };
        let flag = flag.trim();
        let desc = desc.trim();
        if flag.is_empty() || desc.is_empty() || !is_flag_token(flag) {
            continue;
        }
        out.push((flag.to_string(), desc.to_string()));
    }
    out
}

/// `use.local.desc` parsen: `cat/pkg:flag - Beschreibung` je Zeile.
fn parse_local_desc_file(path: &Path) -> Vec<(String, String, String)> {
    let text = match std::fs::read_to_string(path) {
        Ok(t) => t,
        Err(_) => return Vec::new(),
    };
    let mut out = Vec::new();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let Some((head, tail)) = line.split_once(':') else {
            continue;
        };
        let (head, tail) = (head.trim(), tail.trim());
        if !head.contains('/') {
            continue;
        }
        let Some((flag, desc)) = tail.split_once(" - ") else {
            continue;
        };
        let (flag, desc) = (flag.trim(), desc.trim());
        if flag.is_empty() || desc.is_empty() || !is_flag_token(flag) {
            continue;
        }
        out.push((head.to_string(), flag.to_string(), desc.to_string()));
    }
    out
}

/// Grober Flag-Check (keine Leerzeichen/Doppelpunkte im Namen).
fn is_flag_token(flag: &str) -> bool {
    !flag.is_empty() && !flag.chars().any(|c| c.is_whitespace() || c == ':')
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write(dir: &std::path::Path, name: &str, body: &str) {
        std::fs::write(dir.join(name), body).unwrap();
    }

    #[test]
    fn local_beats_global_and_missing_files_are_empty() {
        let d = tempfile::tempdir().unwrap();
        write(
            d.path(),
            "use.desc",
            "# Kommentar\n\ngtk - GIMP Tool Kit\nungültig ohne trenner\n",
        );
        write(
            d.path(),
            "use.local.desc",
            "app-editors/emacs:gtk - GTK für Emacs\n",
        );
        let desc = d.path().join("desc");
        std::fs::create_dir(&desc).unwrap();
        write(&desc, "video_cards.desc", "intel - Intel-Karten\n");
        let ud = UseDescriptions::load(d.path());
        // Lokal vor global.
        assert_eq!(
            ud.describe(Some("app-editors/emacs"), "gtk"),
            Some("GTK für Emacs")
        );
        assert_eq!(ud.describe(Some("app-x"), "gtk"), Some("GIMP Tool Kit"));
        assert_eq!(ud.describe(None, "intel"), Some("Intel-Karten"));
        assert_eq!(ud.describe(None, "unbekannt"), None);
        // Fehlendes Verzeichnis → leer, kein Fehler.
        let empty = UseDescriptions::load(&d.path().join("nichts"));
        assert_eq!(empty.describe(None, "gtk"), None);
    }
}
