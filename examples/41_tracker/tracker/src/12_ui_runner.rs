//! Terminal setup and the UI event loop.

use crate::audio_out;
use crate::protocol::{Command, UiLink};
use crate::song_io;
use crate::ui_state::App;
use crate::ui_view;
use anyhow::Result;
use crossterm::event::{self, Event, KeyEventKind};
use std::time::Duration;

/// ~60 fps redraw while idle; key events wake the loop immediately.
const FRAME: Duration = Duration::from_millis(16);

pub fn run(mut app: App, mut link: UiLink) -> Result<()> {
    let mut terminal = ratatui::init();
    let result = (|| -> Result<()> {
        while !app.quit {
            if let Some(st) = link.poll() {
                app.on_status(st);
            }
            let errors = audio_out::error_count();
            if errors > 0 && !app.message.starts_with("audio") {
                app.message = format!("audio: {errors} stream errors/xruns");
            }
            terminal.draw(|f| ui_view::draw(f, &app))?;
            if !event::poll(FRAME)? {
                continue;
            }
            if let Event::Key(k) = event::read()?
                && k.kind != KeyEventKind::Release
            {
                for cmd in app.handle_key(k) {
                    send(&mut link, &mut app, cmd);
                }
            }
            if app.save_requested {
                app.save_requested = false;
                app.message = match song_io::save(&app.path, &app.song) {
                    Ok(()) => format!("saved {}", app.path.display()),
                    Err(e) => format!("save failed: {e:#}"),
                };
            }
        }
        Ok(())
    })();
    ratatui::restore();
    result
}

fn send(link: &mut UiLink, app: &mut App, cmd: Command) {
    if !link.send(cmd) {
        app.message = "audio queue full – edit not sent".into();
    }
}
