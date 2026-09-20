//! 06_backend: Offline-Render (ohne Hardware testbar) + CPAL-ALSA-Stream.
//! Render-Pfad ist das Test-Gate; ALSA-Pfad folgt dem kanonischen
//! fundsp-`beep.rs`-Muster (Format-Match F32/I16/U16).

use anyhow::{Context, Result};
use cpal::traits::{DeviceTrait, HostTrait};

use crate::{
    bar_secs, build_reese, build_reese_mid, decay, drum_active, hat_sample, kick_sample, mix,
    pattern_for, reese_sample, section_at, section_gain, siren_sample, snare_sample, step_index,
    sub_808_sample, Master, Noise, Section,
};

/// Rendere `bars` Takte bei `bpm`/`sample_rate` nach Stereo (f32, -1..1).
/// Deterministisch (Noise-Seed 42): zwei Renders sind bitgleich.
pub fn render_bars(bpm: f64, bars: u32, sample_rate: f64) -> Vec<(f32, f32)> {
    let total_frames = (bar_secs(bpm) * sample_rate).round() as u64 * bars as u64;
    let mut out = Vec::with_capacity(total_frames as usize);
    let mut reese = build_reese();
    reese.set_sample_rate(sample_rate);
    reese.allocate();
    let mut reese_mid = build_reese_mid();
    reese_mid.set_sample_rate(sample_rate);
    reese_mid.allocate();
    let mut noise = Noise::new(42);
    let step_frames = (sample_rate / ((bpm / 60.0) * 4.0)).round().max(1.0) as u64;
    let bar_frames = (bar_secs(bpm) * sample_rate).round().max(1.0) as u64;
    let mut master_l = Master::new(sample_rate as f32, -1.0);
    let mut master_r = Master::new(sample_rate as f32, -1.0);
    let mut prev_noise = 0.0f32;
    // Absoluter Trigger-Frame des letzten Kicks (fuer 808-Tail).
    let mut last_kick_frame: Option<u64> = None;

    for frame in 0..total_frames {
        let t = frame as f64 / sample_rate;
        let bar = (frame / bar_frames) as u32;
        let section = section_at(bar);
        let step = step_index(frame, bpm, sample_rate);
        let frame_in_step = frame % step_frames;
        let phase = frame_in_step as f64 / step_frames as f64;
        let env = decay(phase);
        // Sub-Reese plus Mid-Oktave (0.35) fuer kleine Lautsprecher.
        let bass = reese_sample(&mut reese) + reese_mid.get_mono() * 0.14;

        let mut drums = 0.0f32;
        if drum_active(section, step) {
            let (trig, vel) = pattern_for(bar, step);
            match trig {
                1 => {
                    drums += kick_sample(t, phase, env) * vel;
                    // 808 folgt Kick nur am Step-Anfang (ein Trigger pro Step).
                    if frame_in_step == 0 {
                        last_kick_frame = Some(frame);
                    }
                }
                2 => drums += snare_sample(noise.next_sample(), t, env) * vel,
                _ => {}
            }
        }
        // Offbeat-Hats (Steps 2/6/10/14), eigene kurze Huellkurve.
        if step % 4 == 2 {
            let hat_env = (-phase * 24.0).exp();
            let n = noise.next_sample();
            drums += hat_sample(n, prev_noise, hat_env);
            prev_noise = n;
        }
        // 808-Sub-Tail seit letztem Kick.
        let sub = match last_kick_frame {
            Some(k) => sub_808_sample(t, (frame - k) as f64 / sample_rate),
            None => 0.0,
        };
        // Rave-Sirene: Riser-Uplift + Tearout-Stabs.
        let siren = match section {
            Section::Riser => {
                let p = ((bar - 28).min(3) as f64 * bar_frames as f64
                    + (frame % bar_frames) as f64)
                    / (4.0 * bar_frames as f64);
                siren_sample(t, p as f32)
            }
            Section::Tearout if step % 8 == 6 => {
                siren_sample(t, 0.85) * (-phase * 6.0).exp() as f32
            }
            _ => 0.0,
        };

        let pre = mix(bass + sub, drums + siren, section_gain(section), -6.0);
        out.push((master_l.process(pre), master_r.process(pre)));
    }
    out
}

/// Schreibe Stereo-f32-Frames als 16-bit WAV.
pub fn write_wav(path: &str, frames: &[(f32, f32)], sample_rate: u32) -> Result<()> {
    let spec = hound::WavSpec {
        channels: 2,
        sample_rate,
        bits_per_sample: 16,
        sample_format: hound::SampleFormat::Int,
    };
    let mut w =
        hound::WavWriter::create(path, spec).with_context(|| format!("wav create {path}"))?;
    for (l, r) in frames {
        w.write_sample((l.clamp(-1.0, 1.0) * 32767.0) as i16)?;
        w.write_sample((r.clamp(-1.0, 1.0) * 32767.0) as i16)?;
    }
    w.finalize()?;
    Ok(())
}

/// Liste ALSA-Ausgabegeraete (fuer `--list-devices`), ohne Stream zu oeffnen.
/// cpal 0.18: Gerätename via `description()` (kein `Device::name` mehr).
pub fn list_devices() -> Result<Vec<String>> {
    let host = cpal::default_host();
    let mut names = Vec::new();
    if let Some(d) = host.default_output_device() {
        let label = d
            .description()
            .map(|desc| desc.name().to_string())
            .unwrap_or_default();
        names.push(format!("default: {label}"));
    }
    for d in host.output_devices().context("output devices")? {
        let label = d
            .description()
            .map(|desc| desc.name().to_string())
            .unwrap_or_default();
        if !names.iter().any(|n| n == &label) {
            names.push(label);
        }
    }
    Ok(names)
}

/// Treffer-Pruefung fuer `--device`: case-insensitiver Substring.
/// Ein mpv-artiges `alsa/sysdefault:CARD=Generic_1` wird auf den
/// Kern `sysdefault:CARD=Generic_1` reduziert; `CARD=Foo` matcht auch
/// Beschreibungen, die nur `Foo` enthalten.
pub fn device_matches(name: &str, query: &str) -> bool {
    let name = name.to_lowercase();
    let q = query.strip_prefix("alsa/").unwrap_or(query).to_lowercase();
    if name.contains(&q) {
        return true;
    }
    // Fallback: `CARD=X`-Kern aus mpv-Schreibweise extrahieren.
    if let Some(card) = q.split("card=").nth(1) {
        let card = card.trim_matches(|c| c == '"' || c == '\'' || c == ' ');
        if !card.is_empty() && name.contains(card) {
            return true;
        }
    }
    false
}

/// Ausgabegeraet waehlen: `--device`-Substring oder Default.
/// Gibt (Device, Anzeigename) zurueck; bei Trefferlosigkeit mit
/// Geraeteliste als Hilfetext (mpv-Paritaet zu `--audio-device`).
pub fn find_output_device(query: Option<&str>) -> Result<(cpal::Device, String)> {
    let host = cpal::default_host();
    if let Some(q) = query {
        let mut names = Vec::new();
        for d in host.output_devices().context("output devices")? {
            let label = d
                .description()
                .map(|desc| desc.name().to_string())
                .unwrap_or_default();
            names.push(label.clone());
            if device_matches(&label, q) {
                return Ok((d, label));
            }
        }
        names.sort();
        names.dedup();
        anyhow::bail!(
            "no output device matches {q:?}; available: {} (see --list-devices)",
            names.join(", ")
        );
    }
    let device = host
        .default_output_device()
        .context("no default ALSA output device (try --list-devices + --device)")?;
    let label = device
        .description()
        .map(|desc| desc.name().to_string())
        .unwrap_or_else(|_| "<default>".to_string());
    Ok((device, label))
}

/// Spiele `bars` Takte live auf dem gewaehlten ALSA-Geraet.
/// Format-agnostisch (F32/I16/U16, s. plan.md Kap. 7); Fehler -> Err.
pub fn play_live(bpm: f64, bars: u32, gain_db: f32, device_query: Option<&str>) -> Result<()> {
    let (device, label) = find_output_device(device_query)?;
    eprintln!("output device: {label}");
    let config = device.default_output_config().with_context(|| {
        format!("device {label:?} has no default output config (try --list-devices + --device)")
    })?;
    match config.sample_format() {
        cpal::SampleFormat::F32 => run_stream::<f32>(&device, &config.into(), bpm, bars, gain_db),
        cpal::SampleFormat::I16 => run_stream::<i16>(&device, &config.into(), bpm, bars, gain_db),
        cpal::SampleFormat::U16 => run_stream::<u16>(&device, &config.into(), bpm, bars, gain_db),
        f => anyhow::bail!("unsupported ALSA sample format: {f:?}"),
    }
}

/// Ein Stereo-Frame auf beliebig viele Geraete-Kanaele legen
/// (Mono/Stereo/Surround-sicher: L R L R …). Reine Funktion, testbar.
pub fn push_frame<T>(out: &mut [T], l: f32, r: f32)
where
    T: cpal::SizedSample + cpal::FromSample<f32>,
{
    for (ch, s) in out.iter_mut().enumerate() {
        *s = T::from_sample(if ch % 2 == 0 { l } else { r });
    }
}

/// Einen Callback-Puffer aus dem Render fuellen (Stereo -> N Kanaele).
fn pump<T>(
    data: &mut [T],
    channels: usize,
    cur: &std::sync::Mutex<usize>,
    frames: &[(f32, f32)],
    gain: f32,
) where
    T: cpal::SizedSample + cpal::FromSample<f32>,
{
    let mut i = cur.lock().unwrap();
    for frame in data.chunks_mut(channels.max(1)) {
        let (l, r) = if *i < frames.len() {
            let (left, right) = frames[*i];
            *i += 1;
            (left * gain, right * gain)
        } else {
            (0.0, 0.0)
        };
        push_frame(frame, l, r);
    }
}

fn run_stream<T>(
    device: &cpal::Device,
    config: &cpal::StreamConfig,
    bpm: f64,
    bars: u32,
    gain_db: f32,
) -> Result<()>
where
    T: cpal::SizedSample + cpal::FromSample<f32>,
{
    use cpal::traits::StreamTrait;
    use std::sync::{Arc, Mutex};

    // cpal 0.18: `StreamConfig.sample_rate` ist `SampleRate = u32`.
    let frames = Arc::new(render_bars(bpm, bars, config.sample_rate as f64));
    let total = frames.len();
    // Render ist bereits bei -6 dB gemischt; Gain-Delta anwenden.
    let gain = crate::gain_linear(gain_db.min(crate::MAX_GAIN_DB)) / crate::gain_linear(-6.0);
    let cursor = Arc::new(Mutex::new(0usize));

    // Grosszuegiger ALSA-Puffer gegen Underruns; bei Ablehnung Default.
    // `build_output_stream` nimmt `StreamConfig` by value (cpal 0.18).
    let channels = config.channels as usize;
    let mut wanted = *config;
    wanted.buffer_size = cpal::BufferSize::Fixed(8192);
    let stream = match device.build_output_stream(
        wanted,
        {
            let cur = Arc::clone(&cursor);
            let frames = Arc::clone(&frames);
            move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
                pump(data, channels, &cur, &frames, gain);
            }
        },
        |err| eprintln!("ALSA stream error: {err}"),
        None,
    ) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("fixed buffer rejected ({e}); retry with device default");
            let cur = Arc::clone(&cursor);
            *cur.lock().unwrap() = 0;
            device.build_output_stream(
                *config,
                move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
                    pump(data, channels, &cur, &frames, gain);
                },
                |err| eprintln!("ALSA stream error: {err}"),
                None,
            )?
        }
    };
    stream.play()?;
    while *cursor.lock().unwrap() < total {
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_render_is_empty() {
        assert!(render_bars(174.0, 0, 44100.0).is_empty());
    }

    #[test]
    fn push_frame_maps_any_channel_count() {
        let mut stereo = [0.0f32; 2];
        push_frame(&mut stereo, 0.5, -0.5);
        assert_eq!(stereo, [0.5, -0.5]);
        let mut mono = [0.0f32; 1];
        push_frame(&mut mono, 0.5, -0.5);
        assert_eq!(mono, [0.5]);
        let mut surround = [0.0f32; 6];
        push_frame(&mut surround, 0.5, -0.5);
        assert_eq!(surround, [0.5, -0.5, 0.5, -0.5, 0.5, -0.5]);
    }

    #[test]
    fn device_query_matches_mpv_style() {
        assert!(device_matches("HD-Audio Generic, ALC287", "generic"));
        assert!(device_matches("HDA NVidia, HDMI 0", "hdmi 0"));
        // mpv-Alias aus dem Bug-Report.
        assert!(device_matches(
            "HD-Audio Generic, ALC287 Analog",
            "alsa/sysdefault:CARD=Generic"
        ));
        assert!(device_matches("HD-Audio Generic", "CARD=Generic"));
        assert!(!device_matches("HDA NVidia, HDMI 0", "Generic"));
        assert!(!device_matches("HD-Audio Generic", "hdmi"));
    }

    #[test]
    fn render_two_renders_bit_equal() {
        let a = render_bars(174.0, 1, 8000.0);
        let b = render_bars(174.0, 1, 8000.0);
        assert_eq!(a.len(), b.len());
        assert_eq!(a[0].0.to_bits(), b[0].0.to_bits());
    }
}
