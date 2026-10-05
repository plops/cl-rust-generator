//! Echo-Auswahl: abbildende Echos des stärksten Beams — ohne Limit.
//!
//! Die `copernicus-radar`-Main begrenzt gespeicherte Echos per Default auf
//! 512 (`stored_echoes`); dieser Pfad nutzt nur die Dekodier-Bibliothek und
//! wählt bewusst ALLE abbildenden Echos (S6 VV: 44.901). Der Test unten mit
//! 600 synthetischen Echos (> 512) schützt vor einem einsickernden Cap.

use copernicus_radar::header::PacketHeader;
use std::collections::HashMap;

/// Paket-Indizes der abbildenden Echos des quad-stärksten Beams.
///
/// Filter: `meta::is_imaging_echo` (FDBAQ, Signaltyp Echo, kein Kalibrier-
/// paket), danach Mehrheits-Beam. Keine Begrenzung der Anzahl.
pub fn select_beam_echoes(headers: &[PacketHeader]) -> Vec<usize> {
    let mut beams: HashMap<u32, u64> = HashMap::new();
    for h in headers {
        if crate::meta::is_imaging_echo(h) {
            *beams.entry(h.elevation()).or_insert(0) += u64::from(h.number_of_quads);
        }
    }
    let beam = beams.iter().max_by_key(|&(_, n)| n).map(|(&b, _)| b);
    let Some(beam) = beam else {
        return Vec::new();
    };
    headers
        .iter()
        .enumerate()
        .filter(|(_, h)| crate::meta::is_imaging_echo(h) && h.elevation() == beam)
        .map(|(i, _)| i)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Synthetischer Echo-Header (FDBAQ 12, Signaltyp 0, Beam wählbar).
    fn echo_header(beam: u8, signal: u8, baq: u8) -> PacketHeader {
        let mut p = [0u8; 68];
        p[37] = baq;
        p[60] = beam << 4;
        p[63] = signal << 4;
        p[65] = 0x26;
        p[66] = 0xF7; // 9975 Quads
        PacketHeader::parse(&p).unwrap()
    }

    #[test]
    fn keine_512_begrenzung() {
        // 600 Echos Beam 5 + Störer (Rauschen, Fremd-Beam, Festraten-BAQ).
        let mut headers = Vec::new();
        for _ in 0..600 {
            headers.push(echo_header(5, 0, 12));
        }
        for _ in 0..20 {
            headers.push(echo_header(5, 1, 12)); // Rauschen
        }
        for _ in 0..10 {
            headers.push(echo_header(6, 0, 12)); // Fremd-Beam
        }
        for _ in 0..10 {
            headers.push(echo_header(5, 0, 5)); // Festraten-BAQ
        }
        let sel = select_beam_echoes(&headers);
        assert_eq!(sel.len(), 600); // alle 600, kein Cap bei 512
        assert!(sel.iter().all(|&i| headers[i].elevation() == 5));
    }

    #[test]
    fn ohne_echos_leer() {
        let headers = vec![echo_header(5, 1, 12); 3];
        assert!(select_beam_echoes(&headers).is_empty());
    }
}
