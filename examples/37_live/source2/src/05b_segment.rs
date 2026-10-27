//! 05b — Eigene Segmentierung fuer das Live-Modell `gemini-3.8-live`.
//!
//! Warum: Bei DURCHGEHENDER Sprache ohne Pause (>= 0,3 s) uebersetzt das
//! Live-Modell mit Server-VAD gar nichts (gemessen: 0,0 s Audio, egal ob
//! Default-VAD, `NO_INTERRUPTION` oder `END_SENSITIVITY_HIGH`). Erst wenn der
//! CLIENT die Sprecherwechsel selbst markiert — VAD aus
//! (`automaticActivityDetection.disabled`), `activityHandling`
//! `NO_INTERRUPTION`, und alle paar Sekunden ein `activityEnd`/`activityStart`
//! — liefert das Modell Uebersetzungen (Spike `vad_probe.py`: 8–11 s Audio,
//! erstes nach 2,7–6,2 s).
//!
//! Dieser Segmentierer ist eine REINE Struktur ohne I/O: er bekommt PCM16-
//! 16-kHz-Chunks hineingereicht und gibt zurueck, WELCHE Steuernachrichten
//! (`activityStart`/`activityEnd`) davor/danach an Gemini gehen muessen. Das
//! Senden erledigt die Session in `05_gemini.rs`. So laesst sich die Logik
//! deterministisch unit-testen (kein Netz, keine Zeit).
//!
//! Regel (siehe Zustandsdiagramm in `walkthrough.md` 2.14): Ein Segment
//! beginnt mit `activityStart`; es endet am ersten leisen 100-ms-Block (RMS
//! unter Schwelle) NACHDEM mindestens `MIN_SEGMENT_MS` Audio im Segment
//! liegen, spaetestens aber nach `MAX_SEGMENT_MS`. Beim Ende sofort
//! `activityEnd` + `activityStart` (naechstes Segment). Beim Schliessen der
//! Sitzung ein abschliessendes `activityEnd`.

/// Mindestlaenge eines Segments, bevor eine leise Stelle es beenden darf (ms).
/// Kuerzer waere die Uebersetzung zu zerstueckelt (abgehackte Satzfetzen).
pub const MIN_SEGMENT_MS: u32 = 1_500;

/// Hoechstlaenge eines Segments (ms). Spaetestens hier wird — auch ohne leise
/// Stelle — geschnitten, damit bei durchgehender Sprache ueberhaupt etwas
/// uebersetzt wird (Server-VAD liefert dann 0,0 s). Nach echter HIL-Messung
/// bei 3,0 s belassen: der Spike (`vad_probe.py`) mass bei 3,0 s das MEISTE
/// uebersetzte Audio (11,3 s) gegenueber 2,0 s (9,6 s) und 5,0 s (8,2 s); ein
/// Gegentest mit 2,0 s im echten HIL verschlechterte die A→B-Richtung von
/// 1,6 s auf 0,0 s. 3,0 s ist also das Optimum.
pub const MAX_SEGMENT_MS: u32 = 3_000;

/// Blocklaenge fuer die RMS-Pruefung (ms). Audio wird in 100-ms-Bloecke
/// zerlegt; ein Block gilt als leise, wenn sein RMS unter `QUIET_RMS_I16`
/// liegt.
pub const BLOCK_MS: u32 = 100;

/// Stille-Schwelle als i16-RMS (entspricht ~0,01 normiert: 0,01 * 32768 ≈ 328).
/// Unter diesem Wert gilt ein 100-ms-Block als „leise".
pub const QUIET_RMS_I16: f32 = 330.0;

/// Abtastrate des Eingangs (PCM16 mono): Gemini-Live erwartet 16 kHz.
pub const SAMPLE_RATE: u32 = 16_000;

/// Samples je 100-ms-Block bei 16 kHz = 1600.
const BLOCK_SAMPLES: usize = (SAMPLE_RATE * BLOCK_MS / 1000) as usize;

/// Was der Segmentierer nach einem `push`/`finish` an Steuernachrichten
/// verlangt. Die Session sendet sie in dieser Reihenfolge VOR bzw. NACH dem
/// eigentlichen Audio-Chunk an Gemini.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Control {
    /// Vor dem Audio ein `activityStart` senden (Beginn eines Segments).
    pub start: bool,
    /// Nach dem Audio ein `activityEnd` senden (Ende eines Segments).
    pub end: bool,
}

impl Control {
    /// Keine Steuernachricht noetig.
    const NONE: Self = Self {
        start: false,
        end: false,
    };
}

/// Zustand des Segmentierers.
///
/// `offen` ist `true`, sobald ein `activityStart` gesendet wurde und noch kein
/// `activityEnd` folgte. `ms_im_segment` zaehlt die bisher in das aktuelle
/// Segment geschobene Audio-Dauer (ms). `block_rest` puffert Samples, bis ein
/// voller 100-ms-Block fuer die RMS-Pruefung zusammenkommt.
#[derive(Debug, Default)]
pub struct Segmenter {
    offen: bool,
    ms_im_segment: u32,
    block_rest: Vec<i16>,
}

impl Segmenter {
    /// Neuer Segmentierer; noch kein Segment offen.
    pub fn new() -> Self {
        Self::default()
    }

    /// Ist aktuell ein Segment offen (Start ohne folgendes Ende)?
    pub fn is_open(&self) -> bool {
        self.offen
    }

    /// Einen PCM16-16-kHz-Chunk verarbeiten und die noetigen Steuernachrichten
    /// zurueckgeben.
    ///
    /// Bedeutung von `Control`: `start` = ein `activityStart` VOR dem Audio
    /// dieses Chunks senden; `end` = ein `activityEnd` NACH dem Audio senden.
    ///
    /// - Ist noch kein Segment offen, kommt `start = true` (auch vor dem ersten
    ///   Audio ueberhaupt).
    /// - Dann wird 100-ms-blockweise der RMS geprueft. Liegt ein Block unter
    ///   der Schwelle UND sind bereits >= `MIN_SEGMENT_MS` im Segment, oder
    ///   erreicht das Segment `MAX_SEGMENT_MS`, wird `end = true` gesetzt: das
    ///   aktuelle Segment endet NACH diesem Chunk. Das Folgesegment oeffnet
    ///   der NAECHSTE `push` (dann wieder `start = true`) — so bleibt die
    ///   Reihenfolge am Netz sauber start→audio→end, start→audio→…
    ///
    /// Es wird hoechstens EIN Schnitt je `push` gemeldet.
    pub fn push(&mut self, samples: &[i16]) -> Control {
        let mut ctrl = Control::NONE;
        // Segment oeffnen (auch vor dem ersten Audio ueberhaupt).
        if !self.offen {
            self.offen = true;
            ctrl.start = true;
        }

        // Samples 100-ms-blockweise durchlaufen (Rest aus letztem Aufruf voran).
        let mut puffer = std::mem::take(&mut self.block_rest);
        puffer.extend_from_slice(samples);

        let mut idx = 0;
        while idx + BLOCK_SAMPLES <= puffer.len() {
            let block = &puffer[idx..idx + BLOCK_SAMPLES];
            idx += BLOCK_SAMPLES;
            self.ms_im_segment += BLOCK_MS;

            let leise = rms_i16(block) < QUIET_RMS_I16;
            let lang_genug = self.ms_im_segment >= MIN_SEGMENT_MS;
            let zu_lang = self.ms_im_segment >= MAX_SEGMENT_MS;

            // Schneiden bei leiser Stelle (ab Mindestlaenge) oder Maximallaenge:
            // dieses Segment endet NACH diesem Chunk, der naechste push oeffnet
            // das Folgesegment.
            if (leise && lang_genug) || zu_lang {
                ctrl.end = true;
                self.offen = false;
                self.ms_im_segment = 0;
                // Restliche Samples dieses Chunks fuer den naechsten Aufruf
                // (sie gehoeren schon zum Folgesegment).
                self.block_rest = puffer[idx..].to_vec();
                return ctrl;
            }
        }

        // Nicht verbrauchte Samples (< ein Block) fuer den naechsten Aufruf.
        self.block_rest = puffer[idx..].to_vec();
        ctrl
    }

    /// Sitzung schliessen: ein letztes `activityEnd`, falls ein Segment offen
    /// ist. Danach ist der Segmentierer wieder geschlossen.
    pub fn finish(&mut self) -> Control {
        if self.offen {
            self.offen = false;
            self.ms_im_segment = 0;
            self.block_rest.clear();
            Control {
                start: false,
                end: true,
            }
        } else {
            Control::NONE
        }
    }
}

/// Untertitel-Artefakte herausfiltern: Tokens in spitzen `<...>` oder
/// geschweiften `{...}` Klammern (z. B. `<no speech>`, `{pause}`) sowie das
/// Platzhalter-Wort `translation pending`. Uebrig bleibt der eigentliche Text,
/// getrimmt. Ist danach nichts mehr da, gibt die Funktion `None` zurueck
/// (leere Untertitel nicht senden).
///
/// Die Transkription des Live-Modells enthaelt bei Stille/Rauschen solche
/// Marker; ungefiltert erschienen sie als Untertitel beim Partner.
pub fn clean_subtitle(text: &str) -> Option<String> {
    let mut out = String::with_capacity(text.len());
    let mut tiefe_eckig = 0u32;
    let mut tiefe_geschweift = 0u32;
    for c in text.chars() {
        match c {
            '<' => tiefe_eckig += 1,
            '>' => tiefe_eckig = tiefe_eckig.saturating_sub(1),
            '{' => tiefe_geschweift += 1,
            '}' => tiefe_geschweift = tiefe_geschweift.saturating_sub(1),
            _ if tiefe_eckig == 0 && tiefe_geschweift == 0 => out.push(c),
            _ => {}
        }
    }
    // Platzhalter-Wendung (unabhaengig von Klammern) entfernen.
    let gefiltert = out.replace("translation pending", "");
    // Mehrfach-Leerzeichen zu einem; Raender trimmen.
    let zusammen: String = gefiltert.split_whitespace().collect::<Vec<_>>().join(" ");
    (!zusammen.is_empty()).then_some(zusammen)
}

/// RMS eines i16-Blocks (0.0, wenn leer).
fn rms_i16(block: &[i16]) -> f32 {
    if block.is_empty() {
        return 0.0;
    }
    let sum: f64 = block.iter().map(|&s| (s as f64) * (s as f64)).sum();
    (sum / block.len() as f64).sqrt() as f32
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `n` Samples mit konstanter Amplitude `amp` (fuer RMS-Tests).
    fn ton(n: usize, amp: i16) -> Vec<i16> {
        vec![amp; n]
    }

    /// Samples fuer `ms` Millisekunden lauten Tons.
    fn laut_ms(ms: u32) -> Vec<i16> {
        ton((SAMPLE_RATE * ms / 1000) as usize, 8_000)
    }

    /// Samples fuer `ms` Millisekunden Stille (unter der Schwelle).
    fn leise_ms(ms: u32) -> Vec<i16> {
        ton((SAMPLE_RATE * ms / 1000) as usize, 0)
    }

    #[test]
    fn erster_chunk_oeffnet_segment() {
        let mut s = Segmenter::new();
        assert!(!s.is_open());
        let c = s.push(&laut_ms(100));
        assert!(c.start, "erster chunk → activityStart");
        assert!(!c.end, "noch kein ende");
        assert!(s.is_open());
    }

    #[test]
    fn leise_vor_mindestlaenge_schneidet_nicht() {
        let mut s = Segmenter::new();
        // 500 ms laut, dann 200 ms leise — noch unter MIN_SEGMENT_MS (1500).
        s.push(&laut_ms(500));
        let c = s.push(&leise_ms(200));
        assert!(!c.end, "vor 1,5 s darf eine Pause nicht schneiden");
    }

    #[test]
    fn leise_nach_mindestlaenge_schneidet() {
        let mut s = Segmenter::new();
        // Genau MIN_SEGMENT_MS laut, dann ein leiser 100-ms-Block → Schnitt.
        s.push(&laut_ms(MIN_SEGMENT_MS));
        let c = s.push(&leise_ms(100));
        assert!(
            c.end,
            "leise Stelle ab Mindestlaenge schneidet (end nach Chunk)"
        );
        assert!(!c.start, "kein Start auf dem Schnitt-Chunk");
        assert!(!s.is_open(), "nach Schnitt geschlossen");
        // Der naechste push oeffnet das Folgesegment wieder.
        let c2 = s.push(&laut_ms(100));
        assert!(c2.start, "Folgesegment startet beim naechsten push");
        assert!(s.is_open());
    }

    #[test]
    fn maximallaenge_schneidet_auch_ohne_pause() {
        let mut s = Segmenter::new();
        // 3,0 s durchgehend laut → Schnitt bei MAX_SEGMENT_MS, obwohl nie leise.
        let c = s.push(&laut_ms(3_000));
        assert!(c.end, "nach MAX wird auch ohne Pause geschnitten");
        assert!(!s.is_open(), "nach Maximalschnitt geschlossen");
        // Folgesegment beim naechsten push.
        assert!(
            s.push(&laut_ms(100)).start,
            "neues Segment nach Maximalschnitt"
        );
    }

    #[test]
    fn nur_ein_schnitt_je_push() {
        let mut s = Segmenter::new();
        // 7 s durchgehend laut in EINEM push: nur ein Schnitt (end) gemeldet,
        // Rest bleibt fuer den naechsten push gepuffert.
        let c = s.push(&laut_ms(7_000));
        assert!(c.start && c.end, "erster push: start davor, end am Schnitt");
        assert!(!s.is_open());
        // Der naechste push oeffnet neu und schneidet erneut (Rest > MAX).
        let c2 = s.push(&laut_ms(100));
        assert!(c2.start, "Folgesegment startet");
        assert!(c2.end, "zweiter Schnitt im Folgesegment");
    }

    #[test]
    fn chunk_grenzen_unabhaengig() {
        // Dieselben 1,6 s laut + 100 ms leise, einmal am Stueck, einmal in
        // 50-ms-Haeppchen: beide schneiden genau einmal.
        fn schnitte(chunk_ms: u32) -> usize {
            let mut s = Segmenter::new();
            let mut cuts = 0;
            let laut = laut_ms(1_600);
            let block = (SAMPLE_RATE * chunk_ms / 1000) as usize;
            for teil in laut.chunks(block.max(1)) {
                if s.push(teil).end {
                    cuts += 1;
                }
            }
            if s.push(&leise_ms(100)).end {
                cuts += 1;
            }
            cuts
        }
        assert_eq!(schnitte(1_600), schnitte(50), "schnittzahl unabhaengig");
    }

    #[test]
    fn finish_schliesst_offenes_segment() {
        let mut s = Segmenter::new();
        s.push(&laut_ms(500));
        let c = s.finish();
        assert!(c.end, "finish schliesst mit activityEnd");
        assert!(!c.start, "kein neuer Start beim Schliessen");
        assert!(!s.is_open());
        // Zweimal finish → nichts mehr.
        assert_eq!(s.finish(), Control::NONE);
    }

    #[test]
    fn finish_ohne_segment_ist_leer() {
        let mut s = Segmenter::new();
        assert_eq!(s.finish(), Control::NONE);
    }

    #[test]
    fn rms_schwelle_grob() {
        // Leiser Block (amp 100 < Schwelle 330) vs. lauter Block (amp 8000).
        assert!(rms_i16(&ton(1_600, 100)) < QUIET_RMS_I16);
        assert!(rms_i16(&ton(1_600, 8_000)) > QUIET_RMS_I16);
    }

    #[test]
    fn clean_subtitle_filtert_artefakte() {
        // Spitze und geschweifte Klammern raus, Platzhalter raus.
        assert_eq!(clean_subtitle("<no speech>"), None);
        assert_eq!(clean_subtitle("{pause}"), None);
        assert_eq!(clean_subtitle("  <noise> {pause} "), None);
        assert_eq!(clean_subtitle("translation pending"), None);
        // Echter Text bleibt (ohne die Marker, getrimmt).
        assert_eq!(
            clean_subtitle("<pause>Guten Morgen{noise}").as_deref(),
            Some("Guten Morgen")
        );
        assert_eq!(clean_subtitle("你好 <no speech>").as_deref(), Some("你好"));
        // Mehrfach-Leerzeichen werden zu einem.
        assert_eq!(clean_subtitle("a   b").as_deref(), Some("a b"));
    }
}
