//! Sound card output via cpal. The callback owns the [`Engine`].

use crate::engine::Engine;
use crate::protocol::AudioLink;
use crate::types::Song;
use anyhow::{Context, Result, anyhow};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{FromSample, SampleFormat, SizedSample, Stream, StreamConfig};
use std::sync::atomic::{AtomicU32, Ordering};

/// Frames rendered per inner chunk (stack buffer, no allocation in the callback).
const CHUNK: usize = 256;
/// Requested device buffer (frames).
const LATENCY_FRAMES: u32 = 512;

/// Stream errors (mostly xruns); counted instead of printed so the TUI stays intact.
static ERRORS: AtomicU32 = AtomicU32::new(0);

pub fn error_count() -> u32 {
    ERRORS.load(Ordering::Relaxed)
}

pub fn device_label(d: &cpal::Device) -> String {
    let id = d.id().map(|i| i.to_string()).unwrap_or_default();
    match d.description() {
        Ok(desc) => format!("{id}  {desc}"),
        Err(_) => id,
    }
}

pub fn list_devices() -> Result<Vec<String>> {
    let host = cpal::default_host();
    Ok(host.output_devices()?.map(|d| device_label(&d)).collect())
}

/// Pick a device: substring match on `want`, else prefer an analog/hardware
/// output over HDMI, else the host default.
fn pick_device(want: Option<&str>) -> Result<cpal::Device> {
    let host = cpal::default_host();
    let devices: Vec<_> = host.output_devices()?.collect();
    let label = |d: &cpal::Device| device_label(d).to_lowercase();
    if let Some(w) = want {
        let w = w.to_lowercase();
        return devices
            .into_iter()
            .find(|d| label(d).contains(&w))
            .ok_or_else(|| anyhow!("no output device matches {w:?} (see --list-devices)"));
    }
    let is_analog = |d: &cpal::Device| {
        let l = label(d);
        l.contains("analog") && !l.contains("hdmi")
    };
    // `plughw` lets ALSA convert rate/format, raw `hw` often rejects f32.
    let analog = devices
        .iter()
        .position(|d| is_analog(d) && label(d).contains("plughw"))
        .or_else(|| devices.iter().position(is_analog));
    match analog {
        Some(i) => Ok(devices.into_iter().nth(i).expect("index valid")),
        None => host
            .default_output_device()
            .context("no default output device"),
    }
}

pub struct AudioOut {
    pub stream: Stream,
    pub sample_rate: u32,
    pub device: String,
}

pub fn start(song: Song, mut link: AudioLink, device: Option<&str>) -> Result<AudioOut> {
    let dev = pick_device(device)?;
    let supported = dev.default_output_config()?;
    let mut config: StreamConfig = supported.into();
    // ALSA defaults to ~340 ms; ask for ~10 ms so key previews feel immediate.
    if let cpal::SupportedBufferSize::Range { min, max } = supported.buffer_size() {
        config.buffer_size = cpal::BufferSize::Fixed(LATENCY_FRAMES.clamp(*min, *max));
    }
    let sr = config.sample_rate;
    let engine = Engine::new(Box::new(song), sr as f32);
    let stream = match supported.sample_format() {
        SampleFormat::F32 => build::<f32>(&dev, config, engine, link),
        SampleFormat::I16 => build::<i16>(&dev, config, engine, link),
        SampleFormat::I32 => build::<i32>(&dev, config, engine, link),
        SampleFormat::U16 => build::<u16>(&dev, config, engine, link),
        f => {
            // Drain the link so the caller sees a clean error rather than a hang.
            while link.cmd.pop().is_ok() {}
            Err(anyhow!("unsupported sample format {f}"))
        }
    }?;
    stream.play()?;
    Ok(AudioOut {
        stream,
        sample_rate: sr,
        device: device_label(&dev),
    })
}

fn build<T>(
    dev: &cpal::Device,
    config: StreamConfig,
    mut engine: Engine,
    mut link: AudioLink,
) -> Result<Stream>
where
    T: SizedSample + FromSample<f32>,
{
    let channels = config.channels as usize;
    let mut buf = [0.0f32; CHUNK * 2];
    let stream = dev.build_output_stream(
        config,
        move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
            for out in data.chunks_mut(CHUNK * channels) {
                let frames = out.len() / channels;
                let stereo = &mut buf[..frames * 2];
                engine.process(stereo, &mut link);
                for (frame, lr) in out.chunks_mut(channels).zip(stereo.as_chunks::<2>().0) {
                    for (k, s) in frame.iter_mut().enumerate() {
                        let v = match (channels, k) {
                            (1, _) => (lr[0] + lr[1]) * 0.5,
                            (_, 0) => lr[0],
                            (_, 1) => lr[1],
                            _ => 0.0,
                        };
                        *s = T::from_sample(v);
                    }
                }
            }
        },
        |_err| {
            ERRORS.fetch_add(1, Ordering::Relaxed);
        },
        None,
    )?;
    Ok(stream)
}
