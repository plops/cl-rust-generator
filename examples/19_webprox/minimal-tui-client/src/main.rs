#[allow(dead_code)]
mod connection;
#[allow(dead_code)]
mod echo;
mod input;
mod renderer;
mod state;

use connection::ServerConnection;
use echo::{LocalEcho, StatusBar};
use input::{InputAction, InputHandler, InputMode, HitBoxMap};
use renderer::TerminalRenderer;
use state::PageState;

use crossterm::{
    cursor,
    event::EnableMouseCapture,
    execute,
    terminal::{self, EnterAlternateScreen, LeaveAlternateScreen},
};
use futures_util::StreamExt;
use proto_definitions::browser::*;
use std::io;
use std::time::Duration;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "http://[::1]:50051".to_string());

    // Enter raw mode and alternate screen
    terminal::enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture, cursor::Hide)?;

    let result = run_app(&addr).await;

    // Restore terminal
    execute!(
        stdout,
        LeaveAlternateScreen,
        crossterm::event::DisableMouseCapture,
        cursor::Show
    )?;
    terminal::disable_raw_mode()?;

    if let Err(e) = result {
        eprintln!("Error: {}", e);
    }
    Ok(())
}

async fn run_app(addr: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut page_state = PageState::new();
    let renderer = TerminalRenderer::new();
    let mut input_handler = InputHandler::new();
    let mut hitbox_map = HitBoxMap::new();
    let mut local_echo = LocalEcho::new();
    let mut status_bar = StatusBar::new();
    let mut scroll_offset: usize = 0;

    let (_term_width, term_height) = terminal::size()?;

    let mut connection = ServerConnection::new(addr.to_string());

    // Initial connection
    status_bar.set_message("Connecting...");
    render_all(&renderer, &page_state, &status_bar, scroll_offset, term_height, &mut hitbox_map);

    let (interaction_tx, mut update_stream) = match connection.connect().await {
        Ok(pair) => {
            status_bar.set_connection("Connected");
            pair
        }
        Err(e) => {
            status_bar.set_message(format!("Connection failed: {}", e));
            status_bar.set_connection("Offline");
            render_all(&renderer, &page_state, &status_bar, scroll_offset, term_height, &mut hitbox_map);

            // Wait for quit
            loop {
                let action = input_handler.poll(Duration::from_millis(100), &hitbox_map);
                if matches!(action, InputAction::Quit) {
                    return Ok(());
                }
            }
        }
    };

    status_bar.set_message("Press 'g' to enter URL, 'q' to quit");
    render_all(&renderer, &page_state, &status_bar, scroll_offset, term_height, &mut hitbox_map);

    // Main event loop: poll input and server updates
    loop {
        // Check for server updates (non-blocking)
        tokio::select! {
            biased;

            update = update_stream.next() => {
                match update {
                    Some(Ok(page_update)) => {
                        handle_page_update(
                            page_update,
                            &mut page_state,
                            &mut local_echo,
                            &mut status_bar,
                            &mut connection,
                        );
                        let (_, h) = terminal::size().unwrap_or((80, 24));
                        render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);
                    }
                    Some(Err(e)) => {
                        status_bar.set_message(format!("Stream error: {}", e));
                        status_bar.set_connection("Disconnected");
                        let (_, h) = terminal::size().unwrap_or((80, 24));
                        render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);
                        break;
                    }
                    None => {
                        status_bar.set_connection("Disconnected");
                        let (_, h) = terminal::size().unwrap_or((80, 24));
                        render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);
                        break;
                    }
                }
            }

            _ = tokio::time::sleep(Duration::from_millis(10)) => {
                let action = input_handler.poll(Duration::from_millis(1), &hitbox_map);
                let (_, h) = terminal::size().unwrap_or((80, 24));

                match action {
                    InputAction::Quit => return Ok(()),
                    InputAction::Navigate(url) => {
                        connection.set_last_url(url.clone());
                        status_bar.set_message(format!("Loading {}...", url));
                        render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);
                        let _ = interaction_tx.send(Interaction {
                            r#type: Some(interaction::Type::Navigate(NavigateRequest {
                                url,
                                session_id: String::new(),
                            })),
                        }).await;
                    }
                    InputAction::Click(link_id) => {
                        local_echo.mark_loading(link_id);
                        if let Some(url) = page_state.resolve_link(link_id) {
                            let url = url.to_string();
                            connection.set_last_url(url.clone());
                            let _ = interaction_tx.send(Interaction {
                                r#type: Some(interaction::Type::Navigate(NavigateRequest {
                                    url,
                                    session_id: String::new(),
                                })),
                            }).await;
                        }
                    }
                    InputAction::TypeText(text) => {
                        local_echo.echo_char(text.chars().next().unwrap_or(' '));
                        let _ = interaction_tx.send(Interaction {
                            r#type: Some(interaction::Type::Input(InputRequest {
                                text,
                                session_id: String::new(),
                                is_submit: false,
                            })),
                        }).await;
                    }
                    InputAction::Submit => {
                        local_echo.clear_input();
                        let _ = interaction_tx.send(Interaction {
                            r#type: Some(interaction::Type::Input(InputRequest {
                                text: String::new(),
                                session_id: String::new(),
                                is_submit: true,
                            })),
                        }).await;
                    }
                    InputAction::ScrollUp => {
                        scroll_offset = scroll_offset.saturating_sub(3);
                        render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);
                    }
                    InputAction::ScrollDown => {
                        scroll_offset += 3;
                        render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);
                    }
                    InputAction::ToggleInputMode => {
                        let mode_str = match input_handler.mode {
                            InputMode::Normal => "NORMAL",
                            InputMode::TextInput => "INPUT",
                            InputMode::UrlBar => "URL",
                        };
                        status_bar.set_mode(mode_str);
                        render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);
                    }
                    InputAction::None => {}
                }

                // Show URL bar input if in UrlBar mode
                if input_handler.mode == InputMode::UrlBar {
                    status_bar.set_message(format!("URL: {}_", input_handler.url_buffer));
                    render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);
                }
            }
        }
    }

    Ok(())
}

fn handle_page_update(
    update: PageUpdate,
    page_state: &mut PageState,
    local_echo: &mut LocalEcho,
    status_bar: &mut StatusBar,
    connection: &mut ServerConnection,
) {
    match update.content {
        Some(page_update::Content::FullPage(page)) => {
            page_state.apply_full_page(
                page.markdown,
                page.link_map,
                page.title,
                page.url.clone(),
            );
            local_echo.clear_loading();
            connection.set_last_url(page.url);
            status_bar.set_message(format!("{}", page_state.title));
        }
        Some(page_update::Content::Patch(_delta)) => {
            // TODO: decompress and apply DOM delta
            status_bar.set_message("Received incremental update");
        }
        Some(page_update::Content::Status(msg)) => {
            status_bar.set_message(&msg.message);
        }
        None => {}
    }
}

fn render_all(
    renderer: &TerminalRenderer,
    page_state: &PageState,
    status_bar: &StatusBar,
    scroll_offset: usize,
    term_height: u16,
    hitbox_map: &mut crate::input::HitBoxMap,
) {
    renderer.render(page_state, scroll_offset, term_height, hitbox_map);
    renderer.render_status_bar(&status_bar.message, &status_bar.connection_status, term_height);
}
