//! `01_cli` — Kommandozeilen-Flags und Validierung (S0).
//!
//! Alle Laufzeit-Parameter des Viewers als `clap`-derive-Struct plus
//! reine Validierungs-/Clamp-Funktionen (ohne X11/GPU/Modell testbar).

use anyhow::{bail, Result};
use clap::Parser;
use std::path::PathBuf;

/// Default-Modell: YOLOv8m aus dem ort-Referenzbeispiel.
pub const YOLOV8M_URL: &str = "https://github.com/ultralytics/assets/releases/download/v8.3.0/yolo11n.onnx"; // "https://cdn.pyke.io/0/pyke:ort-rs/example-models@0.0.0/yolov8m.onnx";

/// Standard-Framerate, wenn `--fps` nicht angegeben ist.
pub const DEFAULT_FPS: u32 = 5;
/// Untere/obere Schranke für `--fps` (Ressourcen-Limit, s. Plan Kap. 3.2).
pub const MIN_FPS: u32 = 1;
pub const MAX_FPS: u32 = 30;

/// Kommandozeile des X11-YOLO-Live-Viewers.
#[derive(Parser, Debug, Clone)]
#[command(
    name = "x11_yolo_viewer",
    about = "X11-Region capturen, YOLOv8-Inferenz, Boxen anzeigen"
)]
pub struct Args {
    /// Linke Kante der Capture-Region, monitor-relativ in px
    #[arg(long, default_value_t = 0)]
    pub x: i32,
    /// Obere Kante der Capture-Region, monitor-relativ in px
    #[arg(long, default_value_t = 0)]
    pub y: i32,
    /// Breite der Capture-Region in px (> 0)
    #[arg(long, default_value_t = 800)]
    pub w: u32,
    /// Hoehe der Capture-Region in px (> 0)
    #[arg(long, default_value_t = 600)]
    pub h: u32,
    /// Monitor-Index aus `Monitor::all()` (0 = erster)
    #[arg(long, default_value_t = 0)]
    pub monitor: usize,
    /// Ganzzahliger Zoom-Faktor des Anzeigebildes
    #[arg(long, default_value_t = 1)]
    pub zoom: u32,
    /// Fensterbreite (Alternative zu `--zoom`)
    #[arg(long)]
    pub win_w: Option<u32>,
    /// Fensterhoehe (Alternative zu `--zoom`)
    #[arg(long)]
    pub win_h: Option<u32>,
    /// Capture-Framerate (wird auf 1..=30 geclampt)
    #[arg(long, default_value_t = DEFAULT_FPS)]
    pub fps: u32,
    /// ONNX-Modell: lokale Datei oder URL (Default YOLOv8m)
    #[arg(long, default_value = YOLOV8M_URL)]
    pub model: String,
    /// Confidence-Schwelle 0..=1
    #[arg(long, default_value_t = 0.5)]
    pub conf: f32,
    /// NMS-IoU-Schwelle 0..=1
    #[arg(long, default_value_t = 0.7)]
    pub nms: f32,
    /// Kein Fenster: nur Inferenz + Log (CI-/Server-Betrieb)
    #[arg(long, default_value_t = false)]
    pub headless: bool,
    /// Einen annotierten Frame als PNG schreiben und beenden
    #[arg(long)]
    pub save_frame: Option<PathBuf>,
}

impl Args {
    /// Prueft die Flags auf Konsistenz. Gibt bei Fehler eine
    /// menschenlesbare `anyhow`-Meldung zurueck (Exit ≠ 0 im Caller).
    pub fn validate(&self) -> Result<()> {
        if self.w == 0 || self.h == 0 {
            bail!(
                "capture region must be non-empty (got w={} h={})",
                self.w,
                self.h
            );
        }
        if self.zoom == 0 {
            bail!("--zoom must be >= 1");
        }
        if !(0.0..=1.0).contains(&self.conf) {
            bail!("--conf must be in 0..=1 (got {})", self.conf);
        }
        if !(0.0..=1.0).contains(&self.nms) {
            bail!("--nms must be in 0..=1 (got {})", self.nms);
        }
        if self.model.trim().is_empty() {
            bail!("--model must not be empty");
        }
        Ok(())
    }

    /// Effektive Framerate nach Clamp auf `MIN_FPS..=MAX_FPS`.
    #[must_use]
    pub fn effective_fps(&self) -> u32 {
        self.fps.clamp(MIN_FPS, MAX_FPS)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn default_args() -> Args {
        Args::try_parse_from(["x11_yolo_viewer"]).expect("defaults must parse")
    }

    #[test]
    fn defaults_are_sane() {
        let a = default_args();
        assert_eq!((a.x, a.y), (0, 0));
        assert_eq!((a.w, a.h), (800, 600));
        assert_eq!(a.monitor, 0);
        assert_eq!(a.zoom, 1);
        assert_eq!(a.fps, DEFAULT_FPS);
        assert_eq!(a.model, YOLOV8M_URL);
        assert!((a.conf - 0.5).abs() < f32::EPSILON);
        assert!((a.nms - 0.7).abs() < f32::EPSILON);
        assert!(!a.headless);
        assert!(a.save_frame.is_none());
        a.validate().expect("defaults must validate");
    }

    #[test]
    fn empty_region_rejected() {
        let mut a = default_args();
        a.w = 0;
        assert!(a.validate().is_err());
        let mut b = default_args();
        b.h = 0;
        assert!(b.validate().is_err());
    }

    #[test]
    fn zero_zoom_rejected() {
        let mut a = default_args();
        a.zoom = 0;
        assert!(a.validate().is_err());
    }

    #[test]
    fn thresholds_out_of_range_rejected() {
        let mut a = default_args();
        a.conf = 1.5;
        assert!(a.validate().is_err());
        let mut b = default_args();
        b.nms = -0.1;
        assert!(b.validate().is_err());
    }

    #[test]
    fn empty_model_rejected() {
        let mut a = default_args();
        a.model = "  ".to_string();
        assert!(a.validate().is_err());
    }

    #[test]
    fn fps_is_clamped() {
        let mut a = default_args();
        a.fps = 999;
        assert_eq!(a.effective_fps(), MAX_FPS);
        a.fps = 0;
        assert_eq!(a.effective_fps(), MIN_FPS);
        a.fps = 12;
        assert_eq!(a.effective_fps(), 12);
    }

    #[test]
    fn flags_parse() {
        let a = Args::try_parse_from([
            "x11_yolo_viewer",
            "--x",
            "10",
            "--y",
            "20",
            "--w",
            "320",
            "--h",
            "240",
            "--fps",
            "12",
            "--zoom",
            "2",
            "--headless",
        ])
        .expect("flags must parse");
        assert_eq!((a.x, a.y, a.w, a.h), (10, 20, 320, 240));
        assert_eq!(a.effective_fps(), 12);
        assert_eq!(a.zoom, 2);
        assert!(a.headless);
        a.validate().expect("parsed flags must validate");
    }
}
