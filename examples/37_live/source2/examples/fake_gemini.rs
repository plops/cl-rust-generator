//! `fake_gemini` — deterministischer Gemini-Ersatz fuer Browser-Tests.
//!
//! Lauscht auf `--listen ADDR` (Default `127.0.0.1:8900`) und spricht das
//! Live-Protokoll wie der Test-Fake: `setupComplete` auf `setup`, auf jedes
//! `realtimeInput` 100 ms Sinus-PCM24 plus In-/Out-Transkript.
//!
//! Der Protokoll-Code lebt in `tests/common/fake.rs` (kein Duplikat).

#[path = "../tests/common/fake.rs"]
mod fake;

use tokio::net::TcpListener;

#[tokio::main]
async fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut listen = "127.0.0.1:8900".to_owned();
    let mut it = args.iter().skip(1);
    while let Some(a) = it.next() {
        if a == "--listen"
            && let Some(v) = it.next()
        {
            listen = v.clone();
        }
    }
    let listener = TcpListener::bind(&listen)
        .await
        .unwrap_or_else(|e| panic!("bind {listen}: {e}"));
    eprintln!("fake_gemini hoert auf {listen}");
    fake::serve(listener).await;
}
