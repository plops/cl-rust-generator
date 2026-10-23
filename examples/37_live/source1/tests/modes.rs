//! Modi-Tabelle: 4 Sprachpaare, stabile IDs, strikte Instruktionen.

use live_relay::config::{MODES, find_mode};

#[test]
fn vier_modi_mit_stabilen_ids() {
    let ids: Vec<_> = MODES.iter().map(|m| m.id).collect();
    assert_eq!(ids, vec!["de-to-zh", "zh-to-de", "en-to-zh", "zh-to-en"]);
}

#[test]
fn jede_instruktion_verbietet_plaudern() {
    for m in MODES {
        assert!(
            m.instruction_en.contains("ONLY"),
            "Modus {} muss ONLY enthalten",
            m.id
        );
        assert!(!m.label_de.is_empty(), "Modus {} braucht Label", m.id);
    }
}

#[test]
fn stimmen_aus_erlaubter_menge() {
    let ok = ["Aoede", "Puck", "Charon", "Fenrir", "Kore"];
    for m in MODES {
        assert!(ok.contains(&m.voice), "Modus {}: Stimme {}", m.id, m.voice);
    }
}

#[test]
fn find_mode_findet_alle_unbekannt_ist_none() {
    for m in MODES {
        assert_eq!(find_mode(m.id), Some(m));
    }
    assert_eq!(find_mode("fr-to-de"), None);
}
