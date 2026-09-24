//! `09_canvas` — räumliches Text-Abbild der ROI fürs Terminal (TUI).
//!
//! Statt einer Box-Tabelle malt `render_spatial` jeden erkannten Text
//! ungefähr dorthin, wo er im Fenster steht (skaliert auf die
//! Terminal-Größe). Reine Funktion, ohne Terminal testbar. Die
//! Tabellen-Form (`08_tui::render`) bleibt für den Batch-Modus.

use crate::tui::{Dashboard, render};

/// Stellt das Terminal per `Drop` wieder her (Raw-Mode aus, Cursor an).
pub struct TuiGuard;

impl TuiGuard {
    /// Schaltet Raw-Mode + Cursor aus; Rückkehr läuft per `Drop`.
    pub fn enter() -> Result<Self, std::io::Error> {
        use crossterm::{cursor, execute};
        use std::io::stdout;
        crossterm::terminal::enable_raw_mode()?;
        execute!(stdout(), cursor::Hide)?;
        Ok(Self)
    }
}

impl Drop for TuiGuard {
    fn drop(&mut self) {
        use crossterm::{cursor, execute};
        use std::io::stdout;
        let _ = crossterm::terminal::disable_raw_mode();
        let _ = execute!(stdout(), cursor::Show);
    }
}

/// Rendert Tabelle (Batch, grepbar) oder Abbild (TUI, positionsgetreu).
#[must_use]
pub fn render_frame(d: &Dashboard<'_>, spatial: bool, term: (u16, u16)) -> String {
    if spatial {
        render_spatial(d, term.0, term.1)
    } else {
        render(d)
    }
}

/// Baut die Terminal-Bytefolge für einen Frame: erst voll löschen, dann
/// malen. Reine Funktion, damit die Reihenfolge testbar bleibt —
/// umgekehrt (malen, dann löschen) bliebe der Schirm schwarz.
#[must_use]
pub fn frame_bytes(text: &str) -> Vec<u8> {
    use crossterm::{cursor, execute, style, terminal};
    let mut buf = Vec::new();
    execute!(
        &mut buf,
        terminal::Clear(terminal::ClearType::All),
        cursor::MoveTo(0, 0),
        style::Print(text)
    )
    .expect("in Speicher schreiben kann nicht fehlschlagen");
    buf
}

/// Kopfzeilen über der Leinwand (Titel, Status, Tasten, letzte Aktion).
pub const HEADER_ROWS: usize = 4;

/// Malt das ROI-Abbild: Kopf + Text an skalierter Position.
/// `term_w`/`term_h` = Terminal-Größe in Zeichen; zu kleine Werte werden
/// auf ein Minimum geclampt. Volle Zeichenzahl je Zeile (kein Umbruch),
/// letzte Zeile ohne `\n` (kein Scrollen).
#[must_use]
pub fn render_spatial(d: &Dashboard<'_>, term_w: u16, term_h: u16) -> String {
    let w = (term_w as usize).max(20);
    let h = (term_h as usize).max(HEADER_ROWS + 2);
    let canvas_h = h - HEADER_ROWS;
    let (rx, ry, rs) = d.roi;
    let rs = rs.max(1) as usize;

    let status = if d.changed { "SUCHE" } else { "IDLE" };
    let auto = if d.automation { "SCHARF" } else { "AUS(a)" };
    let mut out = String::with_capacity(w * h);
    push_line(
        &mut out,
        &format!(
            "OCR {}x{}@({rx},{ry}) {status} {auto} {} Boxen",
            d.roi.2,
            d.roi.2,
            d.rows.len()
        ),
        w,
    );
    push_line(
        &mut out,
        &format!(
            "Det {:.0}ms Rec {:.0}ms Skip {}",
            d.det_ms, d.rec_ms, d.skipped
        ),
        w,
    );
    push_line(&mut out, "Pfeile=Pan 1/2=Zoom a=scharf q=Ende", w);
    push_line(&mut out, d.log.last().map_or("", String::as_str), w);

    // Leinwand: nur über Leerzeichen schreiben (kein Übereinander).
    let mut grid = vec![vec![' '; w]; canvas_h];
    for row in d.rows {
        if row.text.trim().is_empty() {
            continue;
        }
        let (sx, sy, _, _) = row.rect;
        let r = ((sy.max(ry) - ry) as usize * canvas_h / rs).min(canvas_h - 1);
        let c = ((sx.max(rx) - rx) as usize * w / rs).min(w.saturating_sub(1));
        let mut cc = c;
        for ch in row.text.chars() {
            let adv = cell_width(ch).max(1);
            // Breites Zeichen an der letzten Spalte würde umbrechen.
            if cc + adv > w {
                break;
            }
            if grid[r][cc] == ' ' {
                grid[r][cc] = ch;
                // Breite Zeichen (CJK etc., Unifont) belegen zwei Zellen;
                // die zweite wird mit unsichtbarer Markierung reserviert.
                if adv == 2 {
                    grid[r][cc + 1] = '\u{200b}';
                }
            }
            cc += adv;
        }
    }
    for (i, row) in grid.iter().enumerate() {
        out.extend(row.iter());
        if i + 1 < canvas_h {
            out.push('\n');
        }
    }
    out
}

/// Zellbreite: 2 für weite ostasiatische Zeichen (Unifont rendert sie
/// doppelbreit), sonst 1. Kombinierende Zeichen zählen als 0.
/// Bereiche nach Unicode East Asian Width (W/F), vereinfacht.
fn cell_width(ch: char) -> usize {
    let n = ch as u32;
    if (0x1100..=0x115f).contains(&n)
        || (0x2e80..=0xa4cf).contains(&n)
        || (0xac00..=0xd7a3).contains(&n)
        || (0xf900..=0xfaff).contains(&n)
        || (0xfe30..=0xfe4f).contains(&n)
        || (0xff00..=0xff60).contains(&n)
        || (0xffe0..=0xffe6).contains(&n)
        || (0x20000..=0x3fffd).contains(&n)
    {
        2
    } else if (0x300..=0x36f).contains(&n)
        || (0x1ab0..=0x1aff).contains(&n)
        || (0x1dc0..=0x1dff).contains(&n)
        || (0x20d0..=0x20ff).contains(&n)
        || (0xfe20..=0xfe2f).contains(&n)
    {
        0
    } else {
        1
    }
}

/// Eine Kopfzeile, exakt `w` Zeichen breit (abschneiden/auffüllen).
fn push_line(out: &mut String, s: &str, w: usize) {
    let mut n = 0;
    for ch in s.chars() {
        if n >= w {
            break;
        }
        out.push(ch);
        n += 1;
    }
    while n < w {
        out.push(' ');
        n += 1;
    }
    out.push('\n');
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tui::TableRow;

    fn dash<'a>(rows: &'a [TableRow], log: &'a [String]) -> Dashboard<'a> {
        Dashboard {
            roi: (0, 0, 640),
            screen: (1920, 1080),
            changed: true,
            automation: false,
            det_ms: 10.0,
            rec_ms: 20.0,
            skipped: 0,
            rows,
            log,
        }
    }

    fn canvas_lines(s: &str) -> Vec<&str> {
        s.lines().skip(HEADER_ROWS).collect()
    }

    #[test]
    fn text_lands_at_scaled_position() {
        let rows = [
            TableRow {
                rect: (0, 0, 100, 20),
                text: "AB".into(),
            },
            TableRow {
                rect: (320, 320, 100, 20),
                text: "CD".into(),
            },
        ];
        let log: Vec<String> = vec![];
        let s = render_spatial(&dash(&rows, &log), 80, 24);
        let c = canvas_lines(&s);
        assert_eq!(c.len(), 20);
        assert!(c[0].starts_with("AB"));
        assert_eq!(&c[10][40..42], "CD");
    }

    #[test]
    fn empty_and_clipped_text() {
        let rows = [
            TableRow {
                rect: (10, 10, 50, 10),
                text: "   ".into(),
            },
            TableRow {
                rect: (600, 600, 200, 20),
                text: "WEIT".into(),
            },
        ];
        let log: Vec<String> = vec![];
        // 40 Spalten: x=600 -> Spalte 37, "WEIT" wird zu "WEI" geclippt.
        let s = render_spatial(&dash(&rows, &log), 40, 10);
        for line in canvas_lines(&s) {
            assert!(line.chars().count() <= 40);
        }
        assert!(s.contains("WEI"));
        assert!(!s.contains("WEIT"));
    }

    #[test]
    fn overlap_skips_occupied_cells() {
        let rows = [
            TableRow {
                rect: (0, 0, 200, 20),
                text: "AAAA".into(),
            },
            TableRow {
                rect: (32, 0, 200, 20),
                text: "BB".into(),
            },
        ];
        let log: Vec<String> = vec![];
        // 640 px auf 64 Spalten: Box 2 startet bei Spalte 3 (in "AAAA"):
        // erstes B entfällt (belegt), zweites landet auf Spalte 4.
        let s = render_spatial(&dash(&rows, &log), 64, 10);
        let first = canvas_lines(&s)[0];
        assert!(first.starts_with("AAAAB"));
    }

    #[test]
    fn frame_clears_before_painting() {
        let b = frame_bytes("AB");
        let s = String::from_utf8_lossy(&b);
        let clear = s.find("\u{1b}[2J").expect("Clear-All fehlt");
        let home = s.find("\u{1b}[1;1H").expect("MoveTo fehlt");
        let text = s.find("AB").expect("Text fehlt");
        assert!(
            clear < home && home < text,
            "erst löschen, dann malen (sonst schwarzer Schirm)"
        );
    }

    #[test]
    fn wide_chars_take_two_cells() {
        let rows = [TableRow {
            rect: (0, 0, 200, 20),
            text: "A\u{4e2d}B".into(), // A + 中 (doppelbreit) + B
        }];
        let log: Vec<String> = vec![];
        let s = render_spatial(&dash(&rows, &log), 80, 10);
        let first = canvas_lines(&s)[0];
        // A(0) 中(1+2) B(3): B steht auf Spalte 3 statt 2.
        assert_eq!(
            first.chars().take(4).collect::<String>(),
            "A\u{4e2d}\u{200b}B"
        );
    }
}
