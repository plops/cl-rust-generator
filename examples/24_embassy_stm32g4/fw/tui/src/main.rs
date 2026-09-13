//! G474 instrument TUI (host, std): tabbed control for all five modes.
//! Wiring only — comms in `01_comms`, state/actions in `02_app`, drawing in
//! `03_ui`. Usage: `g474-tui /dev/ttyACM1` (device USB) or the STLink VCP.

use std::io;
use std::time::Duration;

use crossterm::event::{self, Event, KeyCode};
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use crossterm::ExecutableCommand;
use ratatui::backend::CrosstermBackend;
use ratatui::Terminal;

#[path = "02_app.rs"]
mod app_02;
#[path = "01_comms.rs"]
mod comms_01;
#[path = "03_ui.rs"]
mod ui_03;

use app_02::{App, Screen};

fn main() -> Result<(), String> {
    let mut args = std::env::args().skip(1);
    let port = args.next().unwrap_or_else(|| {
        eprintln!("usage: g474-tui /dev/ttyACMx [--probe]");
        std::process::exit(2);
    });
    let mut app = App::new(&port)?;
    // Headless self-check: status + one freq gate, print the log, exit.
    // Lets CI exercise the TUI comms path without a terminal.
    if args.any(|a| a == "--probe") {
        app.screen = Screen::Freq;
        app.action();
        for line in &app.log {
            println!("{}", line);
        }
        return Ok(());
    }

    enable_raw_mode().map_err(|e| e.to_string())?;
    let mut stdout = io::stdout();
    stdout
        .execute(EnterAlternateScreen)
        .map_err(|e| e.to_string())?;
    let backend = CrosstermBackend::new(stdout);
    let mut term = Terminal::new(backend).map_err(|e| e.to_string())?;

    let out = run(&mut term, &mut app);

    disable_raw_mode().map_err(|e| e.to_string())?;
    term.backend_mut()
        .execute(LeaveAlternateScreen)
        .map_err(|e| e.to_string())?;
    term.show_cursor().map_err(|e| e.to_string())?;
    out
}

fn run(term: &mut Terminal<CrosstermBackend<io::Stdout>>, app: &mut App) -> Result<(), String> {
    loop {
        term.draw(|f| ui_03::draw(f, app))
            .map_err(|e| e.to_string())?;
        if !event::poll(Duration::from_millis(100)).map_err(|e| e.to_string())? {
            continue;
        }
        let Event::Key(key) = event::read().map_err(|e| e.to_string())? else {
            continue;
        };
        match key.code {
            KeyCode::Char('q') | KeyCode::Esc => return Ok(()),
            KeyCode::Char(' ') => app.action(),
            KeyCode::Char(c) if ('1'..='6').contains(&c) => {
                let i = (c as u8 - b'1') as usize;
                app.screen = [
                    Screen::Status,
                    Screen::Freq,
                    Screen::Scope,
                    Screen::Awg,
                    Screen::Vna,
                    Screen::Cap,
                ][i];
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::app_02::Screen;

    #[test]
    fn screens_have_distinct_titles_and_hints() {
        let screens = [
            Screen::Status,
            Screen::Freq,
            Screen::Scope,
            Screen::Awg,
            Screen::Vna,
            Screen::Cap,
        ];
        let mut titles = std::collections::HashSet::new();
        for s in screens {
            assert!(!s.title().is_empty());
            assert!(!s.hint().is_empty());
            assert!(titles.insert(s.title()), "duplicate title");
        }
    }
}
