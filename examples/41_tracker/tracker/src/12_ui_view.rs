//! Drawing with ratatui. Pure function of [`App`] → frame.

use crate::song_io::cell_text;
use crate::types::{CHANNELS, Cell, ROWS, Sources};
use crate::ui_state::App;
use ratatui::Frame;
use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Paragraph};

/// Characters per channel column: separator + 13-char cell.
const CH_W: u16 = 14;
const SIDE_W: u16 = 26;

/// Character range of each cursor column inside a cell's text.
const COL_RANGE: [(usize, usize); 8] = [
    (0, 3),
    (4, 5),
    (5, 6),
    (7, 8),
    (8, 9),
    (10, 11),
    (11, 12),
    (12, 13),
];

fn segment_color(i: usize) -> Color {
    match i {
        0..=2 => Color::Cyan,
        4..=5 => Color::Yellow,
        7..=8 => Color::Green,
        _ => Color::Magenta,
    }
}

fn cell_spans(c: Cell, base: Style, cursor: Option<(usize, usize)>) -> Vec<Span<'static>> {
    let text: Vec<char> = cell_text(c).chars().collect();
    let style_at = |i: usize| {
        let ch = text[i];
        let mut s = if ch == '.' || ch == '-' && i < 3 {
            base.fg(Color::DarkGray)
        } else if ch == ' ' {
            base
        } else {
            base.fg(segment_color(i))
        };
        if cursor.is_some_and(|(a, b)| (a..b).contains(&i)) {
            s = s.add_modifier(Modifier::REVERSED);
        }
        s
    };
    let mut spans: Vec<Span> = Vec::new();
    let mut run = String::new();
    let mut run_style = style_at(0);
    for (i, &ch) in text.iter().enumerate() {
        let s = style_at(i);
        if s != run_style && !run.is_empty() {
            spans.push(Span::styled(std::mem::take(&mut run), run_style));
        }
        run_style = s;
        run.push(ch);
    }
    spans.push(Span::styled(run, run_style));
    spans
}

/// First visible channel so that the cursor channel is on screen.
pub fn channel_window(width: u16, cursor_ch: usize) -> (usize, usize) {
    let vis = (width.saturating_sub(3) / CH_W).clamp(1, CHANNELS as u16) as usize;
    let first = cursor_ch.saturating_sub(vis - 1).min(CHANNELS - vis);
    (first, vis)
}

fn header(app: &App) -> Line<'static> {
    let st = &app.status;
    let play = if st.playing {
        Span::styled(" ▶ PLAY ", Style::new().fg(Color::Black).bg(Color::Green))
    } else {
        Span::styled(" ■ STOP ", Style::new().fg(Color::Black).bg(Color::Gray))
    };
    let edit = if app.edit {
        Span::styled(" EDIT ", Style::new().fg(Color::Black).bg(Color::Red))
    } else {
        Span::raw(" jam ")
    };
    let ins = &app.song.instruments[app.instr as usize];
    let src = app.song.sources();
    let badge_bg = match src {
        Sources::Synth => Color::Cyan,
        Sources::Samples => Color::Magenta,
        Sources::Mixed => Color::LightMagenta,
    };
    Line::from(vec![
        Span::styled(" dnb-tracker ", Style::new().add_modifier(Modifier::BOLD)),
        play,
        edit,
        Span::styled(
            format!(" {} ", src.label()),
            Style::new().fg(Color::Black).bg(badge_bg),
        ),
        Span::raw(format!(" {} │", app.song.title)),
        Span::raw(format!(
            " BPM {} │ Pat {:02X} │ Oct {} │ Ins {:02X} {} │ Step {}",
            app.song.bpm, app.pattern, app.octave, app.instr, ins.name, app.step
        )),
    ])
}

fn order_line(app: &App) -> Line<'static> {
    let mut spans = vec![Span::raw(" Order ")];
    for i in 0..app.song.order_len {
        let s = format!("{:02X}", app.song.order[i]);
        let style = if i == app.order_pos {
            Style::new().fg(Color::Black).bg(Color::Yellow)
        } else if app.status.playing && i == app.status.order_pos as usize {
            Style::new().fg(Color::Green)
        } else {
            Style::new()
        };
        spans.push(Span::styled(s, style));
        spans.push(Span::raw(" "));
    }
    Line::from(spans)
}

fn draw_pattern(f: &mut Frame, app: &App, area: Rect) {
    let block = Block::bordered().title(format!(" Pattern {:02X} ", app.pattern));
    let inner = block.inner(area);
    f.render_widget(block, area);
    let (first, vis) = channel_window(inner.width, app.ch);
    let rows_vis = inner.height.saturating_sub(1) as usize;
    let top = app
        .row
        .saturating_sub(rows_vis / 2)
        .min(ROWS.saturating_sub(rows_vis));
    let mut lines = Vec::with_capacity(rows_vis + 1);
    let mut head = vec![Span::raw("  ")];
    for ch in first..first + vis {
        let lvl = app.status.levels[ch];
        let bar = "▮".repeat((lvl * 6.0).ceil().min(6.0) as usize);
        let label = format!(" Ch{ch}{} {bar:<6}", if app.mute[ch] { "M" } else { " " });
        let style = if app.mute[ch] {
            Style::new().fg(Color::Red)
        } else {
            Style::new().fg(Color::Gray)
        };
        head.push(Span::styled(format!("{label:<14}"), style));
    }
    lines.push(Line::from(head));
    let playing_here = app.status.playing && app.status.pattern as usize == app.pattern;
    for row in top..(top + rows_vis).min(ROWS) {
        let mut base = Style::new();
        if row % 16 == 0 {
            base = base.bg(Color::Rgb(40, 40, 52));
        } else if row % 4 == 0 {
            base = base.bg(Color::Rgb(28, 28, 36));
        }
        if playing_here && row == app.status.row as usize {
            base = base.bg(Color::Rgb(20, 60, 110));
        }
        let num_style = if row == app.row {
            base.fg(Color::White).add_modifier(Modifier::BOLD)
        } else {
            base.fg(Color::DarkGray)
        };
        let mut spans = vec![Span::styled(format!("{row:02X}"), num_style)];
        for ch in first..first + vis {
            spans.push(Span::styled(" ", base));
            let cursor = (row == app.row && ch == app.ch).then(|| COL_RANGE[app.col]);
            spans.extend(cell_spans(
                app.song.cell(app.pattern, row, ch),
                base,
                cursor,
            ));
        }
        lines.push(Line::from(spans));
    }
    f.render_widget(Paragraph::new(lines), inner);
}

fn draw_side(f: &mut Frame, app: &App, area: Rect) {
    let [ins_area, meter_area] =
        Layout::vertical([Constraint::Min(4), Constraint::Length(4)]).areas(area);
    let lines: Vec<Line> = app
        .song
        .instruments
        .iter()
        .enumerate()
        .map(|(i, p)| {
            let text = format!("{i:02X} {:<6} {}", p.kind.name(), p.name);
            if i == app.instr as usize {
                Line::styled(text, Style::new().fg(Color::Black).bg(Color::Yellow))
            } else {
                Line::raw(text)
            }
        })
        .collect();
    let skip = (app.instr as usize).saturating_sub(ins_area.height.saturating_sub(3) as usize);
    f.render_widget(
        Paragraph::new(lines)
            .scroll((skip as u16, 0))
            .block(Block::bordered().title(" Instruments ")),
        ins_area,
    );
    let w = meter_area.width.saturating_sub(5) as f32;
    let bar = |v: f32| {
        let n = (v.clamp(0.0, 1.0) * w) as usize;
        let color = if v > 0.95 { Color::Red } else { Color::Green };
        Span::styled("█".repeat(n), Style::new().fg(color))
    };
    let meters = vec![
        Line::from(vec![Span::raw("L "), bar(app.status.peak_l)]),
        Line::from(vec![Span::raw("R "), bar(app.status.peak_r)]),
    ];
    f.render_widget(
        Paragraph::new(meters).block(Block::bordered().title(" Master ")),
        meter_area,
    );
}

pub fn draw(f: &mut Frame, app: &App) {
    let [top, order, main, foot] = Layout::vertical([
        Constraint::Length(1),
        Constraint::Length(1),
        Constraint::Min(5),
        Constraint::Length(1),
    ])
    .areas(f.area());
    f.render_widget(Paragraph::new(header(app)), top);
    f.render_widget(Paragraph::new(order_line(app)), order);
    let side = if main.width >= 3 + CH_W * 4 + SIDE_W {
        SIDE_W
    } else {
        0
    };
    let [pat, side_area] =
        Layout::horizontal([Constraint::Min(10), Constraint::Length(side)]).areas(main);
    draw_pattern(f, app, pat);
    if side > 0 {
        draw_side(f, app, side_area);
    }
    f.render_widget(
        Paragraph::new(Line::styled(
            format!(" {}", app.message),
            Style::new().fg(Color::Gray),
        )),
        foot,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::demo_songs::demo_dnb;
    use ratatui::{Terminal, backend::TestBackend};
    use std::path::PathBuf;

    fn screen(w: u16, h: u16, app: &App) -> String {
        let mut t = Terminal::new(TestBackend::new(w, h)).unwrap();
        t.draw(|f| draw(f, app)).unwrap();
        let buf = t.backend().buffer();
        (0..h)
            .map(|y| (0..w).map(|x| buf[(x, y)].symbol()).collect::<String>() + "\n")
            .collect()
    }

    #[test]
    fn renders_small_terminal() {
        let mut app = App::new(demo_dnb(), PathBuf::from("x"));
        app.pattern = 1;
        let s = screen(100, 24, &app);
        assert!(s.contains("BPM 174"), "{s}");
        assert!(s.contains(" synth  Two-Step Reese"), "badge + title:\n{s}");
        assert!(s.contains("Pattern 01"));
        assert!(s.contains("C-4 00 40"), "kick on row 0:\n{s}");
        assert!(s.contains("Order 00 01 01 02"));
    }

    #[test]
    fn renders_wide_terminal_with_side_panel() {
        let mut app = App::new(demo_dnb(), PathBuf::from("x"));
        app.instr = 3;
        let s = screen(150, 40, &app);
        assert!(s.contains("Instruments"));
        assert!(s.contains("03 reese  Reese"));
        assert!(s.contains("Master"));
        assert!(s.contains("Ch7"), "all 8 channels fit:\n{s}");
    }

    #[test]
    fn cursor_channel_stays_visible() {
        assert_eq!(channel_window(80, 0), (0, 5));
        assert_eq!(channel_window(80, 7), (3, 5));
        assert_eq!(channel_window(200, 7), (0, 8));
        assert_eq!(channel_window(5, 4), (4, 1));
    }
}
