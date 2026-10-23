//! Relay-Broadcast ueber echte WebSockets: A→B ja, A→A nein, Raumtrennung ja.

use std::sync::Arc;

use futures_util::{SinkExt, StreamExt};
use live_relay::{serve::router, state::Rooms};
use tokio_tungstenite::{connect_async, tungstenite::Message};

async fn spawn_server() -> String {
    let rooms = Arc::new(Rooms::new(8));
    let app = router(rooms, "../source1/public");
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        axum::serve(listener, app).await.unwrap();
    });
    format!("ws://{addr}")
}

async fn connect(
    base: &str,
    room: &str,
) -> tokio_tungstenite::WebSocketStream<tokio_tungstenite::MaybeTlsStream<tokio::net::TcpStream>> {
    let (ws, _) = connect_async(format!("{base}/ws?room={room}"))
        .await
        .unwrap();
    ws
}

#[tokio::test]
async fn partner_bekommt_frame_sender_nicht() {
    let base = spawn_server().await;
    let (mut a, _) = connect_async(format!("{base}/ws?room=r1")).await.unwrap();
    let mut b = connect(&base, "r1").await;
    // kurz warten, bis beide registriert sind
    tokio::time::sleep(std::time::Duration::from_millis(200)).await;

    a.send(Message::Binary(vec![1u8, 2, 3].into()))
        .await
        .unwrap();

    let msg = tokio::time::timeout(std::time::Duration::from_secs(3), b.next())
        .await
        .expect("partner bekommt frame")
        .expect("stream lebt")
        .unwrap();
    assert_eq!(msg.into_data().to_vec(), vec![1u8, 2, 3]);

    // kein Echo an den Sender (kurzes Fenster, muss leer bleiben)
    let echo = tokio::time::timeout(std::time::Duration::from_millis(300), a.next()).await;
    assert!(echo.is_err(), "sender darf kein echo bekommen");
}

#[tokio::test]
async fn raeume_sind_isoliert() {
    let base = spawn_server().await;
    let (mut a, _) = connect_async(format!("{base}/ws?room=raum-a"))
        .await
        .unwrap();
    let mut c = connect(&base, "raum-b").await;
    tokio::time::sleep(std::time::Duration::from_millis(200)).await;

    a.send(Message::Binary(vec![9u8].into())).await.unwrap();

    let got = tokio::time::timeout(std::time::Duration::from_millis(300), c.next()).await;
    assert!(got.is_err(), "fremder raum hoert nichts");
}

#[tokio::test]
async fn healthz_und_rooms_antworten() {
    let rooms = Arc::new(Rooms::new(8));
    let app = router(rooms, "../source1/public");
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        axum::serve(listener, app).await.unwrap();
    });
    let health = reqwest::get(format!("http://{addr}/healthz"))
        .await
        .unwrap()
        .text()
        .await
        .unwrap();
    assert_eq!(health, "ok");
    let rooms_body = reqwest::get(format!("http://{addr}/api/rooms"))
        .await
        .unwrap()
        .text()
        .await
        .unwrap();
    assert_eq!(rooms_body.trim(), "{}");
    let modes = reqwest::get(format!("http://{addr}/api/modes"))
        .await
        .unwrap()
        .text()
        .await
        .unwrap();
    for id in ["de-to-zh", "zh-to-de", "en-to-zh", "zh-to-en"] {
        assert!(modes.contains(id), "modes enthaelt {id}");
    }
}
