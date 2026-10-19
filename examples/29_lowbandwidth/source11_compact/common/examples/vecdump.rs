//! Gibt kanonische v5-Body-Vektoren als Hex aus (für gen_sample.py).

use lbw_common::{ClientMsg, Rect, ServerMsg, TextItem, encode_msg};

fn hex(b: &[u8]) -> String {
    b.iter().map(|x| format!("{x:02x}")).collect()
}

fn main() {
    let item = TextItem {
        id: 7,
        rect: Rect::new(1, 2, 300, 16),
        fg: [0, 0, 0],
        bg: [255, 255, 250],
        text: "n€u".into(),
    };
    let nonce: [u8; 16] = *b"0123456789abcdef";
    let mac: [u8; 32] = core::array::from_fn(|i| i as u8);
    let msgs: &[(&str, Vec<u8>)] = &[
        (
            "S_CHALLENGE",
            encode_msg(&ServerMsg::Challenge { version: 5, nonce }).unwrap(),
        ),
        ("S_HELLO", encode_msg(&ServerMsg::Hello).unwrap()),
        ("S_CLEAR", encode_msg(&ServerMsg::ClearText).unwrap()),
        ("S_NEW", encode_msg(&ServerMsg::NewText(item)).unwrap()),
        (
            "S_MOVE",
            encode_msg(&ServerMsg::MoveText {
                id: 7,
                rect: Rect::new(10, 200, 100, 16),
            })
            .unwrap(),
        ),
        ("S_RM", encode_msg(&ServerMsg::RemoveText(9)).unwrap()),
        (
            "S_TILE",
            encode_msg(&ServerMsg::Tile {
                x: 10,
                y: 20,
                w: 128,
                h: 64,
                data: vec![1, 2, 3, 4],
            })
            .unwrap(),
        ),
        ("S_UNKNOWN", vec![9, 0xff]),
        ("S_TRAIL", {
            let mut v = encode_msg(&ServerMsg::Hello).unwrap();
            v.push(0xaa);
            v
        }),
        ("C_AUTH", encode_msg(&ClientMsg::Auth { mac }).unwrap()),
        (
            "C_HELLO",
            encode_msg(&ClientMsg::Hello { version: 5 }).unwrap(),
        ),
        (
            "C_MOVE",
            encode_msg(&ClientMsg::MouseMove { x: 100, y: 200 }).unwrap(),
        ),
        (
            "C_BTN",
            encode_msg(&ClientMsg::Button {
                button: 1,
                down: true,
            })
            .unwrap(),
        ),
        ("C_TEXT", encode_msg(&ClientMsg::Text("hi".into())).unwrap()),
        (
            "C_KEY",
            encode_msg(&ClientMsg::Key {
                key: "Enter".into(),
                down: false,
            })
            .unwrap(),
        ),
        ("C_RESYNC", encode_msg(&ClientMsg::ResyncText).unwrap()),
        ("C_QUALITY", encode_msg(&ClientMsg::QualityTile).unwrap()),
    ];
    for (name, body) in msgs {
        println!("\"{name}\": \"{}\"", hex(body));
    }
}
