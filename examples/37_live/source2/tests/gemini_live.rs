//! Live-Test gegen echtes Gemini (`#[ignore]`).
//!
//! Key aus Env `LIVE_KEY_FILE` (Default api_key.txt). Erwartet `Ready`
//! innerhalb 10 s. Ausfuehren: `cargo test --test gemini_live -- --ignored`.

use live_silk::{config::GEMINI_URL, gemini::Event};
use std::time::Duration;
use tokio::sync::mpsc;

#[tokio::test]
#[ignore = "braucht echten Key und Netz"]
async fn live_setup_complete() {
    let path = std::env::var("LIVE_KEY_FILE")
        .unwrap_or_else(|_| "/workspace/src/cl-rust-generator/api_key.txt".to_owned());
    let key = std::fs::read_to_string(&path)
        .expect("key-datei lesbar")
        .trim()
        .to_owned();

    let (etx, mut erx) = mpsc::channel::<Event>(32);
    let setup = live_silk::gemini::translate_setup_json("zh-Hans", false);
    let _session = live_silk::gemini::connect(GEMINI_URL, &key, &setup, false, etx)
        .await
        .expect("connect gegen echtes gemini");

    let ev = tokio::time::timeout(Duration::from_secs(10), erx.recv())
        .await
        .expect("ready innerhalb 10 s")
        .expect("event-kanal lebt");
    assert_eq!(ev, Event::Ready, "erstes event ist setupComplete");
}
