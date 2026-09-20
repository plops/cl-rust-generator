//! dnb_tui — Transport- und Sektions-Anzeige (nutzt lib, kein Logik-Duplikat).
//! Ohne TTY (z.B. CI): Sektions-Tabelle auf stdout, Exit 0.

use std::io::IsTerminal;

use anyhow::Result;
use clap::Parser;
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Style},
    widgets::{Block, Borders, Gauge, List, ListItem, Paragraph},
    Terminal,
};
use source0::{section_at, section_gain, Preset, PRESET_VER};

#[derive(Parser, Debug)]
#[command(name = "dnb_tui", about = "dark DnB transport UI")]
struct Args {
    /// Takte fuer Render-Vorschau
    #[arg(long, default_value_t = 52)]
    bars: u32,
    /// Preset-TOML laden
    #[arg(long)]
    preset: Option<String>,
}

fn sections_table() -> String {
    let mut out = String::from("bar section gain\n");
    for bar in [0, 4, 20, 28, 32, 48] {
        let s = section_at(bar);
        out.push_str(&format!("{bar:>3} {s:?} {:.2}\n", section_gain(s)));
    }
    out
}

fn main() -> Result<()> {
    let args = Args::parse();
    if let Some(path) = &args.preset {
        let p = Preset::load(path)?;
        println!(
            "preset ver={} bpm={} bars={} gain_db={}",
            p.ver, p.bpm, p.bars, p.gain_db
        );
    } else {
        println!("preset ver={PRESET_VER} (defaults)");
    }
    print!("{}.", sections_table());

    if !std::io::stdout().is_terminal() {
        return Ok(());
    }
    run_tty(args.bars)
}

fn run_tty(bars: u32) -> Result<()> {
    use crossterm::{
        event::{self, Event, KeyCode},
        execute,
        terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    };

    enable_raw_mode()?;
    let mut stdout = std::io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let mut term = Terminal::new(CrosstermBackend::new(stdout))?;

    // Render-Vorschau mit Fortschritt (4 Render-Schritte, R = erneut).
    let mut progress = 0u16;
    let mut rendered = false;
    loop {
        term.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(9),
                    Constraint::Length(3),
                    Constraint::Min(1),
                ])
                .split(f.area());
            let items: Vec<ListItem> = [0, 4, 20, 28, 32, 48]
                .iter()
                .map(|b| {
                    ListItem::new(format!(
                        "bar {b:>3}: {:?} (gain {:.2})",
                        section_at(*b),
                        section_gain(section_at(*b))
                    ))
                })
                .collect();
            f.render_widget(
                List::new(items).block(Block::default().borders(Borders::ALL).title("Sections")),
                chunks[0],
            );
            f.render_widget(
                Gauge::default()
                    .block(Block::default().borders(Borders::ALL).title("Render"))
                    .gauge_style(Style::default().fg(Color::Green))
                    .percent(progress),
                chunks[1],
            );
            f.render_widget(
                Paragraph::new(if rendered {
                    format!("{bars} bars gerendert (R = erneut, Q = quit)")
                } else {
                    "R = render, Q = quit".to_string()
                }),
                chunks[2],
            );
        })?;
        if event::poll(std::time::Duration::from_millis(50))? {
            if let Event::Key(k) = event::read()? {
                match k.code {
                    KeyCode::Char('q') | KeyCode::Char('Q') => break,
                    KeyCode::Char('r') | KeyCode::Char('R') => {
                        for p in [25, 50, 75, 100] {
                            let _ = source0::render_bars(174.0, bars.min(4), 8000.0);
                            progress = p;
                        }
                        rendered = true;
                    }
                    _ => {}
                }
            }
        }
    }

    disable_raw_mode()?;
    execute!(term.backend_mut(), LeaveAlternateScreen)?;
    Ok(())
}
