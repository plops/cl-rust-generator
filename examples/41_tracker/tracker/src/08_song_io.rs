//! Human-readable song format (diff-friendly, editable with any text editor).
//!
//! ```text
//! # dnb-tracker song v1
//! bpm 174
//! speed 6
//! order 00 01 01 02
//! pan 0 0 0.35 0 0 -0.3 0.25 -0.35
//! instr 00 kind=kick attack=0.001 decay=0.45 … name=Kick
//! pattern 01
//! 00 | C-4 00 40 ... | --- .. .. C60 | …
//! ```
//! Only patterns with content are written; missing rows are empty.

use crate::types::{
    CHANNELS, Cell, Kind, MAX_INSTR, MAX_ORDER, MAX_PATTERNS, Patch, ROWS, Song, note_name,
    parse_note,
};
use anyhow::{Context, Result, anyhow, bail};
use std::fmt::Write as _;
use std::path::Path;

/// Fixed-width (13 chars) text of a cell: `C-4 00 40 C60`.
pub fn cell_text(c: Cell) -> String {
    let hex = |v: u8| {
        if v == 0xFF {
            "..".to_string()
        } else {
            format!("{v:02X}")
        }
    };
    let fx = if c.fx == 0 {
        "...".to_string()
    } else {
        format!("{}{:02X}", c.fx as char, c.arg)
    };
    format!(
        "{} {} {} {}",
        note_name(c.note),
        hex(c.instr),
        hex(c.vol),
        fx
    )
}

pub fn parse_cell(s: &str) -> Option<Cell> {
    let mut it = s.split_whitespace();
    let note = parse_note(it.next()?)?;
    let hex = |t: &str| {
        if t == ".." {
            Some(0xFF)
        } else {
            u8::from_str_radix(t, 16).ok()
        }
    };
    let instr = hex(it.next()?)?;
    let vol = hex(it.next()?)?;
    let fxs = it.next()?;
    let (fx, arg) = if fxs == "..." {
        (0, 0)
    } else {
        let (c, a) = fxs.split_at_checked(1)?;
        (c.as_bytes()[0], u8::from_str_radix(a, 16).ok()?)
    };
    it.next().is_none().then_some(Cell {
        note,
        instr,
        vol,
        fx,
        arg,
    })
}

fn pattern_used(song: &Song, p: usize) -> bool {
    song.order[..song.order_len].contains(&(p as u8))
        || song.patterns[p].iter().flatten().any(|c| *c != Cell::EMPTY)
}

pub fn to_text(song: &Song) -> String {
    let mut s = String::from("# dnb-tracker song v1\n");
    let _ = writeln!(s, "bpm {}\nspeed {}", song.bpm, song.speed);
    let order: Vec<String> = song.order[..song.order_len]
        .iter()
        .map(|o| format!("{o:02X}"))
        .collect();
    let _ = writeln!(s, "order {}", order.join(" "));
    let pan: Vec<String> = song.pan.iter().map(|p| p.to_string()).collect();
    let _ = writeln!(s, "pan {}", pan.join(" "));
    for (i, p) in song.instruments.iter().enumerate() {
        let _ = write!(
            s,
            "instr {i:02X} kind={} attack={} decay={} sustain={} release={} cutoff={} reso={} \
             drive={} pitch_env={} pitch_decay={} detune={} volume={}",
            p.kind.name(),
            p.attack,
            p.decay,
            p.sustain,
            p.release,
            p.cutoff,
            p.reso,
            p.drive,
            p.pitch_env,
            p.pitch_decay,
            p.detune,
            p.volume
        );
        if !p.sample_path.is_empty() {
            let _ = write!(s, " sample={}", p.sample_path);
        }
        let _ = writeln!(s, " name={}", p.name.replace(' ', "_"));
    }
    for p in (0..MAX_PATTERNS).filter(|&p| pattern_used(song, p)) {
        let _ = writeln!(s, "pattern {p:02X}");
        for (r, row) in song.patterns[p].iter().enumerate() {
            let cells: Vec<String> = row.iter().map(|&c| cell_text(c)).collect();
            let _ = writeln!(s, "{r:02X} | {} |", cells.join(" | "));
        }
    }
    s
}

fn hex_index(t: &str, max: usize, what: &str) -> Result<usize> {
    let v = usize::from_str_radix(t, 16).with_context(|| format!("bad {what} {t:?}"))?;
    if v >= max {
        bail!("{what} {v} out of range");
    }
    Ok(v)
}

fn apply_param(p: &mut Patch, key: &str, val: &str) -> Result<()> {
    let num = || -> Result<f32> { val.parse().with_context(|| format!("{key}={val}")) };
    match key {
        "kind" => p.kind = Kind::parse(val).ok_or_else(|| anyhow!("unknown kind {val}"))?,
        "name" => p.name = val.to_string(),
        "sample" => p.sample_path = val.to_string(),
        "attack" => p.attack = num()?,
        "decay" => p.decay = num()?,
        "sustain" => p.sustain = num()?,
        "release" => p.release = num()?,
        "cutoff" => p.cutoff = num()?,
        "reso" => p.reso = num()?,
        "drive" => p.drive = num()?,
        "pitch_env" => p.pitch_env = num()?,
        "pitch_decay" => p.pitch_decay = num()?,
        "detune" => p.detune = num()?,
        "volume" => p.volume = num()?,
        _ => bail!("unknown instrument key {key}"),
    }
    Ok(())
}

pub fn from_text(text: &str) -> Result<Song> {
    let mut song = Song::new();
    let mut pat: Option<usize> = None;
    for (ln, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let ctx = || format!("line {}: {line}", ln + 1);
        let (head, rest) = line.split_once(' ').unwrap_or((line, ""));
        match head {
            "bpm" => song.bpm = rest.trim().parse().with_context(ctx)?,
            "speed" => song.speed = rest.trim().parse().with_context(ctx)?,
            "order" => {
                let items: Vec<&str> = rest.split_whitespace().collect();
                if items.is_empty() || items.len() > MAX_ORDER {
                    bail!("{}: order needs 1..={MAX_ORDER} entries", ctx());
                }
                for (i, t) in items.iter().enumerate() {
                    song.order[i] = hex_index(t, MAX_PATTERNS, "pattern").with_context(ctx)? as u8;
                }
                song.order_len = items.len();
            }
            "pan" => {
                for (i, t) in rest.split_whitespace().take(CHANNELS).enumerate() {
                    song.pan[i] = t.parse().with_context(ctx)?;
                }
            }
            "instr" => {
                let mut it = rest.split_whitespace();
                let idx = hex_index(it.next().unwrap_or(""), MAX_INSTR, "instrument")
                    .with_context(ctx)?;
                let pairs = it
                    .map(|kv| kv.split_once('=').ok_or_else(|| anyhow!("{}", ctx())))
                    .collect::<Result<Vec<_>>>()?;
                // Defaults depend on the kind, so it must be known before other keys apply.
                let kind = match pairs.iter().find(|(k, _)| *k == "kind") {
                    Some((_, v)) => Kind::parse(v).ok_or_else(|| anyhow!("{}: kind", ctx()))?,
                    None => Kind::Lead,
                };
                let mut p = Patch::new("", kind);
                for (k, v) in pairs {
                    apply_param(&mut p, k, v).with_context(ctx)?;
                }
                song.instruments[idx] = p;
            }
            "pattern" => pat = Some(hex_index(rest.trim(), MAX_PATTERNS, "pattern")?),
            _ => {
                let p = pat.ok_or_else(|| anyhow!("{}: row outside pattern", ctx()))?;
                let mut parts = line.split('|');
                let row = hex_index(parts.next().unwrap_or("").trim(), ROWS, "row")?;
                for (ch, cell) in parts.filter(|c| !c.trim().is_empty()).enumerate() {
                    if ch >= CHANNELS {
                        bail!("{}: too many channels", ctx());
                    }
                    let c = parse_cell(cell).ok_or_else(|| anyhow!("{}: bad cell", ctx()))?;
                    song.set_cell(p, row, ch, c);
                }
            }
        }
    }
    Ok(song)
}

/// Load a song and its sample WAVs (paths relative to the song file).
/// Returns the song plus warnings for samples that could not be loaded.
pub fn load(path: &Path) -> Result<(Song, Vec<String>)> {
    let text = std::fs::read_to_string(path).with_context(|| format!("{path:?}"))?;
    let mut song = from_text(&text)?;
    let base = path.parent().unwrap_or(Path::new("."));
    let mut warnings = Vec::new();
    for p in song
        .instruments
        .iter_mut()
        .filter(|p| !p.sample_path.is_empty())
    {
        match crate::wav::load_wav(&base.join(&p.sample_path)) {
            Ok((data, sr)) => {
                p.sample = data;
                p.sample_rate = sr;
            }
            Err(e) => warnings.push(format!("{}: {e:#}", p.sample_path)),
        }
    }
    Ok((song, warnings))
}

pub fn save(path: &Path, song: &Song) -> Result<()> {
    std::fs::write(path, to_text(song)).with_context(|| format!("{path:?}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::demo_songs::{demo_dnb, demo_phonk};

    #[test]
    fn cell_text_roundtrip() {
        let c = Cell::note(parse_note("F#3").unwrap(), 0x0A)
            .vol(0x20)
            .fx(b'C', 0x7F);
        assert_eq!(cell_text(c), "F#3 0A 20 C7F");
        assert_eq!(parse_cell(&cell_text(c)), Some(c));
        assert_eq!(cell_text(Cell::EMPTY), "--- .. .. ...");
        assert_eq!(parse_cell("--- .. .. ..."), Some(Cell::EMPTY));
        assert_eq!(parse_cell("--- .. .."), None);
    }

    #[test]
    fn demo_songs_roundtrip() {
        for song in [demo_dnb(), demo_phonk()] {
            let text = to_text(&song);
            let back = from_text(&text).unwrap();
            assert_eq!(back, song);
            assert_eq!(to_text(&back), text);
        }
    }

    #[test]
    fn missing_keys_use_defaults_of_the_kind() {
        let s = from_text("instr 02 kind=kick name=K").unwrap();
        let mut want = Patch::new("K", Kind::Kick);
        want.name = "K".into();
        assert_eq!(s.instruments[2], want);
    }

    #[test]
    fn errors_name_the_line() {
        let err = from_text("bpm 170\npattern 00\n00 | X-9 .. .. ... |").unwrap_err();
        assert!(format!("{err:#}").contains("line 3"), "{err:#}");
    }
}
