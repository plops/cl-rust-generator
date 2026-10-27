use futures_util::{SinkExt, StreamExt};
use tokio_tungstenite::{connect_async, tungstenite::Message};
#[tokio::main]
async fn main() {
    let key = std::fs::read_to_string("/workspace/src/cl-rust-generator/api_key.txt").unwrap();
    let url = format!("wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1beta.GenerativeService.BidiGenerateContent?key={}", key.trim());
    let (mut ws, _) = connect_async(url).await.expect("connect");
    ws.send(Message::Text(r#"{"setup":{"model":"models/gemini-3.5-live-translate-preview","generationConfig":{"responseModalities":["AUDIO"],"translationConfig":{"targetLanguageCode":"zh-Hans"}}}}"#.into())).await.unwrap();
    let m = ws.next().await.unwrap().unwrap();
    println!("got {:?}", m.into_text().map(|t| t.chars().take(80).collect::<String>()));
}
