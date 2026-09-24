//! `08_tui` — Terminal-Dashboard ohne Fenster (S5).
//!
//! `render` baut die komplette Anzeige als String (ohne Terminal testbar);
//! `map_key` übersetzt Tastendrücke in Aktionen; `TuiGuard` stellt das
//! Terminal beim Verlassen wieder her (auch im Fehlerpfad, per `Drop`).

use crossterm::event::{KeyCode, KeyModifiers};

/// Max. Tabellenzeilen im Dashboard.
pub const TABLE_ROWS: usize = 12;
/// Max. Log-Zeilen im Dashboard.
pub const LOG_ROWS: usize = 3;

/// Anzeige-Zeile: `rect` = Screen-Rechteck `(x, y, w, h)`, `text` = OCR-Text.
#[derive(Debug, Clone)]
pub struct TableRow {
    pub rect: (i32, i32, u32, u32),
    pub text: String,
}

/// Eingaben für `render`: ROI/Screen/Status/Zeiten/Skip + Zeilen + Log.
pub struct Dashboard<'a> {
    pub roi: (i32, i32, u32),
    pub screen: (i32, i32),
    pub changed: bool,
    pub automation: bool,
    pub det_ms: f64,
    pub rec_ms: f64,
    pub skipped: u64,
    pub rows: &'a [TableRow],
    pub log: &'a [String],
}

/// Baut das Dashboard als String (Tabelle max. `TABLE_ROWS`, Log die
/// letzten `LOG_ROWS` Einträge).
#[must_use]
pub fn render(d: &Dashboard<'_>) -> String {
    let mut out = String::with_capacity(2048);
    let (rx, ry, rs) = d.roi;
    let scale = rs as f64 / 640.0;
    out.push_str("=== X11 OCR Automation (TUI) ===\n");
    out.push_str(&format!(
        "ROI: {rs}x{rs} @ ({rx},{ry}) | Skala: {scale:.2}x | \
         Screen: {}x{}\n",
        d.screen.0, d.screen.1
    ));
    let status = if d.changed { "SUCHE" } else { "IDLE" };
    let auto = if d.automation {
        "SCHARF"
    } else {
        "AUS (Taste 'a')"
    };
    out.push_str(&format!(
        "Status: {status} | Automation: [{auto}] | Inferenz: {:.1}ms \
         (Det {:.1} + Rec {:.1}) | Skip: {}\n",
        d.det_ms + d.rec_ms,
        d.det_ms,
        d.rec_ms,
        d.skipped,
    ));
    out.push_str(
        "Tasten: [Pfeile] Pan | [1/2] Zoom | [a] scharf/unscharf | \
         [q] Ende\n\n",
    );
    out.push_str(&format!("--- Textboxen ({} gefunden) ---\n", d.rows.len()));
    out.push_str("#   | Screen (X,Y)     | Groesse  | Text\n");
    out.push_str("----+------------------+----------+----------------\n");
    for (i, r) in d.rows.iter().take(TABLE_ROWS).enumerate() {
        let (sx, sy, sw, sh) = r.rect;
        out.push_str(&format!(
            "{:<3} | ({sx:>4},{sy:>4})     | {sw:>4}x{sh:<4}  | {}\n",
            i + 1,
            r.text
        ));
    }
    out.push_str("\n--- Aktionen ---\n");
    let start = d.log.len().saturating_sub(LOG_ROWS);
    for line in &d.log[start..] {
        out.push_str(line);
        out.push('\n');
    }
    out
}

/// Tasten-Aktion (reine Abbildung, ohne Terminal testbar).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyAction {
    /// Beenden (`q`, Esc, Strg+C).
    Quit,
    Pan(i32, i32),
    ZoomIn,
    ZoomOut,
    ToggleAutomation,
    /// Keine belegte Taste.
    None,
}

/// Übersetzt Tastendruck in Aktion.
#[must_use]
pub fn map_key(code: KeyCode, mods: KeyModifiers) -> KeyAction {
    if mods.contains(KeyModifiers::CONTROL) && matches!(code, KeyCode::Char('c')) {
        return KeyAction::Quit;
    }
    match code {
        KeyCode::Esc | KeyCode::Char('q') => KeyAction::Quit,
        KeyCode::Left => KeyAction::Pan(-1, 0),
        KeyCode::Right => KeyAction::Pan(1, 0),
        KeyCode::Up => KeyAction::Pan(0, -1),
        KeyCode::Down => KeyAction::Pan(0, 1),
        KeyCode::Char('1') | KeyCode::Char('-') => KeyAction::ZoomIn,
        KeyCode::Char('2') | KeyCode::Char('+') => KeyAction::ZoomOut,
        KeyCode::Char('a') => KeyAction::ToggleAutomation,
        _ => KeyAction::None,
    }
}

/// Ausgabe-Ziel für Frames: Terminal oder Batch.
pub trait FrameDisplay {
    /// Zeigt Dashboard-Text (`changed`: neuer Inhalt).
    fn show(&mut self, text: &str, changed: bool) -> Result<(), String>;
    /// Wahr bei genug Frames (Batch) bzw. Tastatur lesen (Terminal).
    fn done(&self) -> bool {
        false
    }
    fn interactive(&self) -> bool {
        true
    }
    /// Wahr für räumliches Abbild statt Tabelle (nur Terminal-TUI).
    fn spatial(&self) -> bool {
        false
    }
}

/// Terminal-Anzeige (Cursor positionieren + voll löschen).
pub struct TuiDisplay {
    out: std::io::Stdout,
}

impl TuiDisplay {
    pub fn new() -> Self {
        Self {
            out: std::io::stdout(),
        }
    }
}

impl FrameDisplay for TuiDisplay {
    fn spatial(&self) -> bool {
        true
    }

    fn show(&mut self, text: &str, _changed: bool) -> Result<(), String> {
        use crossterm::{cursor, execute, style, terminal};
        // Voll löschen (nicht ab Cursor): Die Frame-Höhe variiert, Reste
        // alter Frames würden sonst als Müll stehen bleiben.
        execute!(
            self.out,
            cursor::MoveTo(0, 0),
            style::Print(text),
            terminal::Clear(terminal::ClearType::All),
            cursor::MoveTo(0, 0)
        )
        .map_err(|e| format!("Terminal: {e}"))?;
        use std::io::Write;
        self.out.flush().map_err(|e| format!("Terminal: {e}"))
    }
}

/// Batch-Anzeige (pty-frei, CI/Smoke): nur geänderte Frames als Klartext.
pub struct BatchDisplay {
    /// Noch zu zeigende geänderte Frames.
    pub remaining: usize,
}

impl FrameDisplay for BatchDisplay {
    fn show(&mut self, text: &str, changed: bool) -> Result<(), String> {
        if changed && self.remaining > 0 {
            println!("{text}\n--- FRAME ---\n");
            self.remaining -= 1;
        }
        Ok(())
    }

    fn done(&self) -> bool {
        self.remaining == 0
    }

    fn interactive(&self) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dash<'a>(rows: &'a [TableRow], log: &'a [String]) -> Dashboard<'a> {
        Dashboard {
            roi: (0, 0, 640),
            screen: (1920, 1080),
            changed: true,
            automation: true,
            det_ms: 12.0,
            rec_ms: 34.0,
            skipped: 0,
            rows,
            log,
        }
    }

    #[test]
    fn render_shows_all_states() {
        let rows = [TableRow {
            rect: (10, 20, 100, 30),
            text: "Hallo".into(),
        }];
        let log = ["[r] Klick".to_string()];
        let s = render(&dash(&rows, &log));
        assert!(s.contains("SUCHE"));
        assert!(s.contains("SCHARF"));
        assert!(s.contains("Hallo"));
        assert!(s.contains("[r] Klick"));
        assert!(s.contains("46.0ms"));
    }

    #[test]
    fn render_idle_and_off() {
        let d = Dashboard {
            changed: false,
            automation: false,
            rows: &[],
            log: &[],
            ..dash(&[], &[])
        };
        let s = render(&d);
        assert!(s.contains("IDLE"));
        assert!(s.contains("AUS"));
    }

    #[test]
    fn render_truncates_table_and_log() {
        let rows: Vec<TableRow> = (0..50)
            .map(|i| TableRow {
                rect: (i, i, 10, 10),
                text: format!("t{i}"),
            })
            .collect();
        let log: Vec<String> = (0..50).map(|i| format!("log{i}")).collect();
        let s = render(&dash(&rows, &log));
        assert!(!s.contains("t49")); // über TABLE_ROWS hinaus
        assert!(!s.contains("log0")); // nur letzte LOG_ROWS
        assert!(s.contains("log49"));
    }

    #[test]
    fn batch_display_counts_changed_frames() {
        let mut b = BatchDisplay { remaining: 2 };
        assert!(!b.done());
        b.show("a", false).unwrap();
        assert!(!b.done());
        b.show("b", true).unwrap();
        assert!(!b.done());
        b.show("c", true).unwrap();
        assert!(b.done());
    }

    #[test]
    fn key_mapping_covers_controls() {
        let plain = KeyModifiers::empty();
        assert_eq!(map_key(KeyCode::Char('q'), plain), KeyAction::Quit);
        assert_eq!(map_key(KeyCode::Esc, plain), KeyAction::Quit);
        assert_eq!(
            map_key(KeyCode::Char('c'), KeyModifiers::CONTROL),
            KeyAction::Quit
        );
        assert_eq!(map_key(KeyCode::Left, plain), KeyAction::Pan(-1, 0));
        assert_eq!(map_key(KeyCode::Char('1'), plain), KeyAction::ZoomIn);
        assert_eq!(map_key(KeyCode::Char('2'), plain), KeyAction::ZoomOut);
        assert_eq!(
            map_key(KeyCode::Char('a'), plain),
            KeyAction::ToggleAutomation
        );
        assert_eq!(map_key(KeyCode::Enter, plain), KeyAction::None);
    }
}
