//! `01_view` — ROI-Zustand: Pan-/Zoom-Arithmetik mit Screen-Clamp (S1).
//!
//! Rein und ohne X11/GPU/Modell testbar. Die Detektion sieht immer ein
//! `MODEL_SIZE`×`MODEL_SIZE`-Bild; `size` ist die Kantenlänge der
//! quadratischen Bildschirm-ROI.

/// Kantenlänge, die das Det-Modell frisst (1:1-Fast-Path, keine Interpolation).
pub const MODEL_SIZE: u32 = 640;

/// Zoom-Stufen (ROI-Kanten): `1` geht eine Stufe herunter (hinein),
/// `2` eine Stufe herauf (heraus).
pub const ROI_STEPS: &[u32] = &[320, 480, 640, 960, 1280];

/// Index in `ROI_STEPS` für den Start (= heutiges Verhalten, 640 oben links).
pub const DEFAULT_STEP_INDEX: usize = 2;

/// Bildschirmgröße in px (aus dem x11rb-Setup).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Screen {
    /// Breite in px.
    pub w: i32,
    /// Höhe in px.
    pub h: i32,
}

/// Capture-Region: linke/obere Kante plus quadratische Kantenlänge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct View {
    /// Linke Kante in px.
    pub x: i32,
    /// Obere Kante in px.
    pub y: i32,
    /// Kantenlänge in px (immer ein Wert aus `ROI_STEPS`).
    pub size: u32,
}

impl View {
    /// Start-ROI: 640×640 oben links (= bisheriges Verhalten).
    #[must_use]
    pub fn default() -> Self {
        Self {
            x: 0,
            y: 0,
            size: ROI_STEPS[DEFAULT_STEP_INDEX],
        }
    }

    /// Pan-Schritt in px: mit der ROI mitwachsend, mindestens 8.
    #[must_use]
    pub fn pan_step(&self) -> i32 {
        ((self.size / 16).max(8)) as i32
    }

    /// Verschiebt um `pan_step()` in die Richtungen (`dx`/`dy` ∈ -1..=1)
    /// und clampt auf den Screen. Gibt zurück, ob sich etwas geändert hat.
    pub fn pan(&mut self, dx: i32, dy: i32, screen: Screen) -> bool {
        let step = self.pan_step();
        let size = self.size as i32;
        let nx = (self.x + dx * step).clamp(0, (screen.w - size).max(0));
        let ny = (self.y + dy * step).clamp(0, (screen.h - size).max(0));
        let changed = nx != self.x || ny != self.y;
        self.x = nx;
        self.y = ny;
        changed
    }

    /// Eine Stufe hineinzoomen (kleinere ROI). Gibt zurück, ob gewechselt wurde.
    pub fn zoom_in(&mut self, screen: Screen) -> bool {
        self.goto_step(self.step_index().saturating_sub(1), screen)
    }

    /// Eine Stufe herauszoomen (größere ROI). Gibt zurück, ob gewechselt wurde.
    pub fn zoom_out(&mut self, screen: Screen) -> bool {
        self.goto_step(self.step_index() + 1, screen)
    }

    /// Wahr, wenn kein Resize nötig ist (Capture = Modell-Input).
    #[must_use]
    pub fn is_native(&self) -> bool {
        self.size == MODEL_SIZE
    }

    /// Skalierung ROI→Anzeige für `out`-px Ausgabegröße (1:1 = 1.0).
    #[must_use]
    pub fn display_scale(&self, out: u32) -> f32 {
        out as f32 / self.size as f32
    }

    fn step_index(&self) -> usize {
        ROI_STEPS
            .iter()
            .position(|s| *s == self.size)
            .unwrap_or(DEFAULT_STEP_INDEX)
    }

    fn goto_step(&mut self, index: usize, screen: Screen) -> bool {
        let Some(&size) = ROI_STEPS.get(index) else {
            return false;
        };
        // Große ROI passt ggf. nicht auf kleinen Screen: dann größte
        // passende Stufe nehmen (niemals größer als der Screen).
        let mut size = size;
        while size as i32 > screen.w.min(screen.h) && size > ROI_STEPS[0] {
            let i = ROI_STEPS.iter().position(|s| *s == size).unwrap_or(1);
            size = ROI_STEPS[i - 1];
        }
        if size == self.size {
            return false;
        }
        self.size = size;
        // Kante nachziehen, damit die ROI auf dem Screen bleibt.
        let max_x = (screen.w - size as i32).max(0);
        let max_y = (screen.h - size as i32).max(0);
        self.x = self.x.min(max_x);
        self.y = self.y.min(max_y);
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SCREEN: Screen = Screen { w: 1920, h: 1080 };

    #[test]
    fn default_is_legacy_region() {
        let v = View::default();
        assert_eq!(v.size, MODEL_SIZE);
        assert_eq!((v.x, v.y), (0, 0));
        assert!(v.is_native());
        assert!((v.display_scale(640) - 1.0).abs() < f32::EPSILON);
    }

    #[test]
    fn pan_moves_by_step_and_reports_change() {
        let mut v = View::default();
        let step = v.pan_step();
        assert!(v.pan(1, 1, SCREEN));
        assert_eq!((v.x, v.y), (step, step));
    }

    #[test]
    fn pan_clamps_at_screen_edges() {
        let mut v = View::default();
        for _ in 0..200 {
            v.pan(-1, -1, SCREEN);
        }
        assert_eq!((v.x, v.y), (0, 0));
        for _ in 0..200 {
            v.pan(1, 1, SCREEN);
        }
        assert_eq!((v.x, v.y), (1920 - 640, 1080 - 640));
    }

    #[test]
    fn pan_reports_no_change_at_edge() {
        let mut v = View::default();
        assert!(!v.pan(-1, 0, SCREEN));
        assert!(!v.pan(0, -1, SCREEN));
    }

    #[test]
    fn zoom_steps_in_and_out() {
        let mut v = View::default();
        assert!(v.zoom_in(SCREEN));
        assert_eq!(v.size, 480);
        assert!(!v.is_native());
        assert!(v.zoom_out(SCREEN));
        assert_eq!(v.size, 640);
        assert!(v.is_native());
    }

    #[test]
    fn zoom_stops_at_limits() {
        let mut v = View::default();
        for _ in 0..10 {
            v.zoom_in(SCREEN);
        }
        assert_eq!(v.size, 320);
        assert!(!v.zoom_in(SCREEN));
        for _ in 0..10 {
            v.zoom_out(SCREEN);
        }
        // 1280 passt nicht auf 1080 Höhe -> größte passende Stufe.
        assert!(v.size <= 1080);
        assert!(!v.zoom_out(SCREEN));
    }

    #[test]
    fn zoom_keeps_roi_on_screen() {
        let mut v = View {
            x: 1500,
            y: 800,
            size: 320,
        };
        assert!(v.zoom_out(SCREEN));
        assert!(v.x + v.size as i32 <= SCREEN.w);
        assert!(v.y + v.size as i32 <= SCREEN.h);
    }

    #[test]
    fn zoom_out_clamped_by_small_screen() {
        let small = Screen { w: 800, h: 600 };
        let mut v = View::default();
        assert!(v.zoom_out(small));
        assert!(v.size <= 600);
        assert!(v.x + v.size as i32 <= 800);
    }
}
