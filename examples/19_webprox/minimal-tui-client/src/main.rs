mod connection;
mod echo;
mod input;
mod renderer;
mod state;

use connection::ServerConnection;
use echo::{LocalEcho, StatusBar};
use input::{InputAction, InputHandler, InputMode, HitBoxMap};
use renderer::TerminalRenderer;
use state::PageState;

use clap::Parser;
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

#[derive(Parser, Debug)]
#[command(name = "minimal-tui-client")]
#[command(about = "Remote browser TUI client", long_about = None)]
struct Args {
    /// Server address
    #[arg(short, long, default_value = "http://[::1]:50051")]
    server: String,

    /// URL to navigate to on startup
    #[arg(short, long)]
    url: Option<String>,

    /// Path to a custom TLS CA certificate (PEM) to trust
    #[arg(long)]
    tls_ca: Option<String>,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    terminal::enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture, cursor::Hide)?;

    let result = run_app(&args.server, args.url, args.tls_ca).await;

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

async fn run_app(addr: &str, initial_url: Option<String>, tls_ca: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let mut page_state = PageState::new();
    let renderer = TerminalRenderer::new();
    let mut input_handler = InputHandler::new();
    let mut hitbox_map = HitBoxMap::new();
    let mut local_echo = LocalEcho::new();
    let mut status_bar = StatusBar::new();
    let mut scroll_offset: usize = 0;

    let (_term_width, term_height) = terminal::size()?;
    let mut connection = ServerConnection::new(addr.to_string(), tls_ca);

    // Initial connection
    status_bar.set_message("Connecting...");
    render_all(&renderer, &page_state, &status_bar, scroll_offset, term_height, &mut hitbox_map);

    let (mut interaction_tx, mut update_stream) = match connection.connect().await {
        Ok(pair) => {
            status_bar.set_connection("Connected");
            pair
        }
        Err(e) => {
            status_bar.set_message(format!("Connection failed: {}", e));
            status_bar.set_connection("Offline");
            render_all(&renderer, &page_state, &status_bar, scroll_offset, term_height, &mut hitbox_map);
            loop {
                let action = input_handler.poll(Duration::from_millis(100), &hitbox_map);
                if matches!(action, InputAction::Quit) {
                    return Ok(());
                }
            }
        }
    };

    // Navigate to initial URL if provided
    if let Some(url) = initial_url {
        connection.set_last_url(url.clone());
        status_bar.set_message(format!("Loading {}...", url));
        render_all(&renderer, &page_state, &status_bar, scroll_offset, term_height, &mut hitbox_map);
        let _ = interaction_tx.send(Interaction {
            r#type: Some(interaction::Type::Navigate(NavigateRequest {
                url,
                session_id: String::new(),
            })),
        }).await;
    } else {
        status_bar.set_message("Press 'g' to enter URL, 'q' to quit");
        render_all(&renderer, &page_state, &status_bar, scroll_offset, term_height, &mut hitbox_map);
    }

    // Main event loop with reconnection
    loop {
        let should_reconnect = run_session_loop(
            &mut interaction_tx,
            &mut update_stream,
            &mut page_state,
            &renderer,
            &mut input_handler,
            &mut hitbox_map,
            &mut local_echo,
            &mut status_bar,
            &mut scroll_offset,
            &mut connection,
        )
        .await;

        if !should_reconnect {
            return Ok(()); // User quit
        }

        // Attempt reconnection
        status_bar.set_message("Reconnecting...");
        status_bar.set_connection(connection.status_text());
        let (_, h) = terminal::size().unwrap_or((80, 24));
        render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);

        match connection.reconnect().await {
            Ok((tx, stream)) => {
                interaction_tx = tx;
                update_stream = stream;
                status_bar.set_connection("Connected");
                status_bar.set_message("Reconnected");
                let (_, h) = terminal::size().unwrap_or((80, 24));
                render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);
            }
            Err(_) => {
                status_bar.set_connection("Offline");
                status_bar.set_message("Reconnection failed. Press 'q' to quit.");
                let (_, h) = terminal::size().unwrap_or((80, 24));
                render_all(&renderer, &page_state, &status_bar, scroll_offset, h, &mut hitbox_map);
                loop {
                    let action = input_handler.poll(Duration::from_millis(100), &hitbox_map);
                    if matches!(action, InputAction::Quit) {
                        return Ok(());
                    }
                }
            }
        }
    }
}

/// Run the main session loop. Returns true if we should reconnect, false if user quit.
async fn run_session_loop(
    interaction_tx: &mut tokio::sync::mpsc::Sender<Interaction>,
    update_stream: &mut tonic::Streaming<PageUpdate>,
    page_state: &mut PageState,
    renderer: &TerminalRenderer,
    input_handler: &mut InputHandler,
    hitbox_map: &mut HitBoxMap,
    local_echo: &mut LocalEcho,
    status_bar: &mut StatusBar,
    scroll_offset: &mut usize,
    connection: &mut ServerConnection,
) -> bool {
    loop {
        tokio::select! {
            biased;

            update = update_stream.next() => {
                match update {
                    Some(Ok(page_update)) => {
                        handle_page_update(page_update, page_state, local_echo, status_bar, connection);
                        let (_, h) = terminal::size().unwrap_or((80, 24));
                        render_all(renderer, page_state, status_bar, *scroll_offset, h, hitbox_map);
                    }
                    Some(Err(e)) => {
                        status_bar.set_message(format!("Stream error: {}", e));
                        status_bar.set_connection("Disconnected");
                        let (_, h) = terminal::size().unwrap_or((80, 24));
                        render_all(renderer, page_state, status_bar, *scroll_offset, h, hitbox_map);
                        return true; // reconnect
                    }
                    None => {
                        status_bar.set_connection("Disconnected");
                        let (_, h) = terminal::size().unwrap_or((80, 24));
                        render_all(renderer, page_state, status_bar, *scroll_offset, h, hitbox_map);
                        return true; // reconnect
                    }
                }
            }

            _ = tokio::time::sleep(Duration::from_millis(10)) => {
                let action = input_handler.poll(Duration::from_millis(1), hitbox_map);
                let (_, h) = terminal::size().unwrap_or((80, 24));

                match action {
                    InputAction::Quit => return false,
                    InputAction::Navigate(url) => {
                        connection.set_last_url(url.clone());
                        status_bar.set_message(format!("Loading {}...", url));
                        render_all(renderer, page_state, status_bar, *scroll_offset, h, hitbox_map);
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
                        *scroll_offset = scroll_offset.saturating_sub(3);
                        render_all(renderer, page_state, status_bar, *scroll_offset, h, hitbox_map);
                    }
                    InputAction::ScrollDown => {
                        *scroll_offset += 3;
                        render_all(renderer, page_state, status_bar, *scroll_offset, h, hitbox_map);
                    }
                    InputAction::ToggleInputMode => {
                        let mode_str = match input_handler.mode {
                            InputMode::Normal => "NORMAL",
                            InputMode::TextInput => "INPUT",
                            InputMode::UrlBar => "URL",
                            InputMode::Follow => "FOLLOW",
                        };
                        status_bar.set_mode(mode_str);
                        render_all(renderer, page_state, status_bar, *scroll_offset, h, hitbox_map);
                    }
                    InputAction::None => {}
                }

                if input_handler.mode == InputMode::UrlBar {
                    status_bar.set_message(format!("URL: {}_", input_handler.url_buffer));
                    render_all(renderer, page_state, status_bar, *scroll_offset, h, hitbox_map);
                } else if input_handler.mode == InputMode::Follow {
                    status_bar.set_message(format!("Link ID: {}_", input_handler.text_buffer));
                    render_all(renderer, page_state, status_bar, *scroll_offset, h, hitbox_map);
                }
            }
        }
    }
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
        Some(page_update::Content::Patch(delta)) => {
            if page_state.apply_patch(&delta.compressed_patch) {
                status_bar.set_message("Incremental update applied");
            } else {
                status_bar.set_message("Patch apply failed, awaiting full refresh");
            }
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
    hitbox_map: &mut HitBoxMap,
) {
    renderer.render(page_state, scroll_offset, term_height, hitbox_map);
    renderer.render_status_bar(&status_bar.message, &status_bar.connection_status, term_height);
}
