//! Gemeinsame Test-Helfer. Der Fake-Gemini lebt in `fake.rs` (ein Ort,
//! auch vom Beispiel `examples/fake_gemini.rs` eingebunden).

pub mod fake;

use tokio::net::TcpListener;

/// Startet den Fake auf `127.0.0.1:0` und liefert die `ws://…`-Basis-URL.
pub async fn spawn_fake_gemini() -> String {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(fake::serve(listener));
    format!("ws://{addr}")
}
