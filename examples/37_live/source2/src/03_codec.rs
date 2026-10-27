//! 03 — Opus-SILK-Codec fuer den Sparmodus.
//!
//! Zwei Richtungen, beide auf 60-ms-Blöcke fixiert:
//! - `DownlinkEncoder`: PCM 24 kHz (die Gemini-Uebersetzung) → Opus-SILK
//!   mit 12 kbit/s, Bandbreite auf Wideband gedeckelt (erzwingt SILK-only).
//!   Eingang sind PCM16-LE-Bytes beliebiger Laenge; gepuffert wird, bis ein
//!   voller Block zusammenkommt. Ziel: ~11 statt ~384 kbit/s zum Mobil-Peer.
//! - `UplinkDecoder`: Opus-Paket vom Mobil-Peer → PCM 16 kHz i16, direkt die
//!   Abtastrate, die der Gemini-Eingang erwartet (kein Resampler noetig).
//!
//! Spike-Messung (3 s Sprachsignal): 50/50 Pakete SilkOnly, 10,7 kbit/s.

use opus_pure::{Application, Bandwidth, OpusDecoder, OpusEncoder, Signal};

/// Blocklaenge in Millisekunden (ein WS-Frame = ein Opus-Paket).
pub const FRAME_MS: usize = 60;
/// Encoder-Abtastrate (Gemini-Ausgabe ist PCM 24 kHz).
pub const DOWN_RATE: i32 = 24_000;
/// Decoder-Abtastrate (Gemini-Eingabe erwartet PCM 16 kHz).
pub const UP_RATE: i32 = 16_000;
/// Zielbitrate des Encoders.
pub const BITRATE: i32 = 12_000;

/// Samples je 60-ms-Block bei DOWN_RATE (24_000 * 60 / 1000 = 1440).
const DOWN_FRAME: usize = (DOWN_RATE as usize) * FRAME_MS / 1000;
/// Obergrenze fuer die Decoder-Ausgabe: 120 ms bei 48 kHz, grosszuegig.
const MAX_DECODE: usize = 5760;
/// Puffergroesse fuer ein kodiertes Paket.
const PACKET_CAP: usize = 1500;

/// PCM16-LE → i16-Sample-Vektor (Restbyte wird vom Aufrufer gepuffert).
fn le_to_i16(bytes: &[u8]) -> Vec<i16> {
    let (pairs, _rest) = bytes.as_chunks::<2>();
    pairs
        .iter()
        .map(|&[a, b]| i16::from_le_bytes([a, b]))
        .collect()
}

/// Downlink: PCM 24 kHz → Opus-SILK 60 ms.
pub struct DownlinkEncoder {
    enc: OpusEncoder,
    /// Noch nicht verbrauchte Samples (< ein Block).
    pending: Vec<i16>,
    /// Ungerades Restbyte aus dem letzten `push_pcm24` (PCM16-LE).
    odd: Option<u8>,
}

impl DownlinkEncoder {
    /// Baut den Encoder mit Sparmodus-Einstellungen (SILK-only erzwungen).
    pub fn new() -> Result<Self, String> {
        let mut enc =
            OpusEncoder::new(DOWN_RATE, 1, Application::Voip).map_err(|e| format!("enc: {e}"))?;
        enc.bitrate_bps = BITRATE;
        // Bandbreite auf Wideband deckeln ⇒ der Encoder bleibt im SILK-Modus.
        enc.max_bandwidth = Bandwidth::Wideband;
        enc.signal_type = Some(Signal::Voice);
        Ok(Self {
            enc,
            pending: Vec::new(),
            odd: None,
        })
    }

    /// Nimmt PCM16-LE-Bytes beliebiger Laenge entgegen und gibt fuer jeden
    /// vollen 60-ms-Block ein Opus-Paket zurueck. Ungerade Byte-Anzahl wird
    /// korrekt behandelt: das Restbyte wird bis zum naechsten Aufruf gemerkt.
    pub fn push_pcm24(&mut self, le_bytes: &[u8]) -> Vec<Vec<u8>> {
        // Restbyte vom letzten Aufruf voranstellen, damit die Paare stimmen.
        let mut buf: Vec<u8> = Vec::with_capacity(le_bytes.len() + 1);
        if let Some(b) = self.odd.take() {
            buf.push(b);
        }
        buf.extend_from_slice(le_bytes);
        // Neues (eventuell) ungerades Restbyte abspalten.
        if buf.len() % 2 == 1 {
            self.odd = buf.pop();
        }
        self.pending.extend(le_to_i16(&buf));

        let mut out = Vec::new();
        let mut scratch = vec![0u8; PACKET_CAP];
        while self.pending.len() >= DOWN_FRAME {
            let frame: Vec<i16> = self.pending.drain(..DOWN_FRAME).collect();
            match self.enc.encode_s16(&frame, DOWN_FRAME, &mut scratch) {
                Ok(n) => out.push(scratch[..n].to_vec()),
                // Ein fehlerhafter Block darf den Strom nicht abreissen lassen.
                Err(_) => continue,
            }
        }
        out
    }
}

/// Uplink: Opus-Paket (vom Mobil-Mikro) → PCM 16 kHz i16.
pub struct UplinkDecoder {
    dec: OpusDecoder,
}

impl UplinkDecoder {
    /// Baut den 16-kHz-Mono-Decoder (direkt die Gemini-Eingangsrate).
    pub fn new() -> Result<Self, String> {
        let dec = OpusDecoder::new(UP_RATE, 1).map_err(|e| format!("dec: {e}"))?;
        Ok(Self { dec })
    }

    /// Dekodiert ein Paket zu PCM 16 kHz mono. `Err` bei kaputtem Paket.
    pub fn decode(&mut self, pkt: &[u8]) -> Result<Vec<i16>, String> {
        if pkt.is_empty() {
            return Err("leeres paket".to_owned());
        }
        let mut out = vec![0i16; MAX_DECODE];
        let got = self
            .dec
            .decode_s16(pkt, MAX_DECODE, &mut out)
            .map_err(|e| format!("decode: {e}"))?;
        out.truncate(got);
        Ok(out)
    }
}

/// i16-Samples → PCM16-LE-Bytes (Gegenstueck zu `le_to_i16`).
pub fn i16_to_le(samples: &[i16]) -> Vec<u8> {
    let mut out = Vec::with_capacity(samples.len() * 2);
    for s in samples {
        out.extend_from_slice(&s.to_le_bytes());
    }
    out
}

/// PCM16-LE-Bytes → i16-Samples (oeffentliche Variante von `le_to_i16`; ein
/// evtl. ungerades Restbyte wird ignoriert). Vom Gemini-Segmentierer genutzt,
/// der die RMS je 100-ms-Block braucht.
pub fn le_to_i16_public(bytes: &[u8]) -> Vec<i16> {
    le_to_i16(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use opus_pure::{OpusMode, packet};

    /// 3 s amplitudenmoduliertes 220-Hz-Signal mit Obertoenen bei 24 kHz,
    /// als PCM16-LE-Bytes — grob sprachaehnlich, damit SILK greift.
    fn speech_like_le(secs: f32) -> Vec<u8> {
        let n = (DOWN_RATE as f32 * secs) as usize;
        let mut pcm = Vec::with_capacity(n);
        for i in 0..n {
            let t = i as f32 / DOWN_RATE as f32;
            let env = 0.5 + 0.5 * (2.0 * std::f32::consts::PI * 3.0 * t).sin();
            let base = (2.0 * std::f32::consts::PI * 220.0 * t).sin();
            let h2 = 0.4 * (2.0 * std::f32::consts::PI * 440.0 * t).sin();
            let h3 = 0.2 * (2.0 * std::f32::consts::PI * 660.0 * t).sin();
            let v = env * (base + h2 + h3) / 1.6 * 0.6;
            pcm.push((v * i16::MAX as f32) as i16);
        }
        i16_to_le(&pcm)
    }

    fn rms(samples: &[i16]) -> f32 {
        if samples.is_empty() {
            return 0.0;
        }
        let sum: f64 = samples.iter().map(|&s| (s as f64) * (s as f64)).sum();
        (sum / samples.len() as f64).sqrt() as f32
    }

    #[test]
    fn silk_only_und_bitrate() {
        let pcm = speech_like_le(3.0);
        let mut enc = DownlinkEncoder::new().unwrap();
        let pkts = enc.push_pcm24(&pcm);
        // 3 s / 60 ms = 50 Pakete.
        assert_eq!(pkts.len(), 50, "paketzahl");
        let mut bytes = 0usize;
        for p in &pkts {
            assert_eq!(packet::mode(p).unwrap(), OpusMode::SilkOnly, "nur SILK");
            bytes += p.len();
        }
        // Bitrate = Bytes * 8 / Dauer; Dauer = 50 * 60 ms = 3 s.
        let kbit = (bytes * 8) as f32 / 3.0 / 1000.0;
        assert!(kbit < 16.0, "bitrate {kbit} kbit/s >= 16");
    }

    #[test]
    fn framing_unabhaengig_von_stueckelung() {
        let pcm = speech_like_le(3.0);

        // Am Stueck.
        let whole = DownlinkEncoder::new().unwrap().push_pcm24(&pcm).len();

        // In krummen Chunks (1000, 333, 7 Bytes) — gleiche Paketzahl erwartet.
        for &chunk in &[1000usize, 333, 7] {
            let mut enc = DownlinkEncoder::new().unwrap();
            let mut count = 0;
            for part in pcm.chunks(chunk) {
                count += enc.push_pcm24(part).len();
            }
            assert_eq!(count, whole, "chunk {chunk}: {count} != {whole}");
        }
    }

    #[test]
    fn roundtrip_energie() {
        let pcm = speech_like_le(3.0);
        let in_samples = le_to_i16(&pcm);
        let in_rms = rms(&in_samples);

        let mut enc = DownlinkEncoder::new().unwrap();
        let mut dec = UplinkDecoder::new().unwrap();
        let pkts = enc.push_pcm24(&pcm);

        let mut all_out: Vec<i16> = Vec::new();
        for p in &pkts {
            let got = dec.decode(p).unwrap();
            // 60 ms bei 16 kHz = 960 Samples je Paket.
            assert_eq!(got.len(), 960, "samples je paket");
            all_out.extend(got);
        }
        // Erste zwei Pakete als Einschwingen verwerfen.
        let out_rms = rms(&all_out[1920..]);
        assert!(
            out_rms > 0.3 * in_rms,
            "ausgangs-RMS {out_rms} <= 30% von {in_rms}"
        );
    }

    #[test]
    fn kaputtes_paket_ohne_panik() {
        let mut dec = UplinkDecoder::new().unwrap();
        // Zufaelliger Muell und ein leeres Paket: Err, aber kein Panic.
        let _ = dec.decode(&[0xff, 0x01, 0x02, 0x03, 0x04]);
        assert!(dec.decode(&[]).is_err());
    }
}
