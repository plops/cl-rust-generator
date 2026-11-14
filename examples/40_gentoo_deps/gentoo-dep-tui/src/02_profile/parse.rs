//! 02_profile/parse — KEY=VALUE-Parser, world-Reader, Tokenizer.
//!
//! Genau eine Zustaendigkeit: Shell-nahe `make.conf`/`make.defaults`-Syntax
//! (mehrzeilige Quotes, `\`-Fortsetzungen, Kommentare) und Atom-Reader.

use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::mod_01_types::{EngineError, EngineResult};

/// Datei lesen mit Engine-Fehler.
pub(super) fn read_to_string(path: &Path) -> EngineResult<String> {
    std::fs::read_to_string(path).map_err(|e| EngineError::Io {
        path: path.display().to_string(),
        source: e,
    })
}

/// Liest eine `world`-Datei (ein Atom pro Zeile, `#`-Kommentare).
pub fn read_world(path: &Path) -> EngineResult<Vec<String>> {
    let text = read_to_string(path)?;
    Ok(text
        .lines()
        .map(|l| strip_comment(l).trim())
        .filter(|l| !l.is_empty())
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
///
/// Unterstuetzt mehrzeilige quoted Zuweisungen (`KEY="...\n..."`,
/// `KEY='...\n...'`) und `\`-Fortsetzungen am Zeilenende. `#`-Kommentare
/// werden ausserhalb von Quotes je logischer Zeile entfernt.
pub fn parse_key_value(text: &str) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for line in logical_lines(text) {
        let line = strip_comment(&line).trim();
        if line.is_empty() {
            continue;
        }
        if let Some((k, v)) = line.split_once('=') {
            map.insert(k.trim().to_string(), unquote(v.trim()));
        }
    }
    map
}

/// Teilt Text in logische Zeilen: `\`-Fortsetzung klebt, offene Quotes
/// (einfach/doppelt) laufen ueber Zeilengrenzen weiter.
fn logical_lines(text: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    for line in text.lines() {
        cur.push_str(line);
        // Quote-Zustand ueber den bisherigen Puffer berechnen.
        let (in_single, in_double) = quote_state(&cur);
        // `\`-Fortsetzung am Ende klebt die naechste Zeile an — auch in
        // Double-Quotes (Shell entfernt `\<newline>` dort, ganz ohne Zeilen-
        // umbruch). In Single-Quotes ist `\` literal, dort keine Fortsetzung.
        let continued = !in_single && continues(&cur);
        if continued {
            // `\` entfernen, naechste Zeile per Leerzeichen ankleben.
            let trimmed = cur.trim_end();
            cur.truncate(trimmed.len().saturating_sub(1));
            cur.push(' ');
            continue;
        }
        if in_single || in_double {
            // Offener Quote: echte Zeilengrenze gehoert zum Wert.
            cur.push('\n');
            continue;
        }
        out.push(std::mem::take(&mut cur));
    }
    if !cur.trim().is_empty() {
        out.push(cur);
    }
    out
}

/// Quote-Zustand am Pufferende: `(in_single, in_double)`.
fn quote_state(buf: &str) -> (bool, bool) {
    let mut in_single = false;
    let mut in_double = false;
    for c in buf.chars() {
        match c {
            '\'' if !in_double => in_single = !in_single,
            '"' if !in_single => in_double = !in_double,
            _ => {}
        }
    }
    (in_single, in_double)
}

/// Endet der Puffer auf unquotetem `\` (Kommentar bereits egal: `#` nach
/// `\` gehoert zur Fortsetzung, shellnah genug fuer make.conf-Zwecke)?
fn continues(buf: &str) -> bool {
    let t = buf.trim_end();
    // Anzahl trailing `\`: ungerade = Fortsetzung (gerade = escaped).
    let n = t.len() - t.trim_end_matches('\\').len();
    n % 2 == 1
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
pub(super) fn strip_comment(line: &str) -> &str {
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
