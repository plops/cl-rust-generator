//! `01_view` — ROI-Zustand plus Detektions- → Screen-Projektion (S1).
//!
//! Rein und ohne X11/GPU/Modell testbar. Die Detektion sieht immer ein
//! `MODEL_SIZE`×`MODEL_SIZE`-Bild; `size` ist die Kantenlänge der
//! quadratischen Bildschirm-ROI. `to_screen_rect` bildet Boxen aus dem
//! Detektions-Raum auf absolute Bildschirm-Pixel ab (Klick-Koordinaten).
//! Die Pan-Formel (`step_divisor`, `step_min_px`) kommt aus der
//! TOML-Konfiguration (`main` setzt die Felder nach dem Laden).

/// Kantenlänge, die das Det-Modell frisst (1:1-Fast-Path, keine Interpolation).
pub const MODEL_SIZE: u32 = 640;

/// Zoom-Stufen (ROI-Kanten): `1` geht eine Stufe herunter (hinein),
/// `2` eine Stufe herauf (heraus).
pub const ROI_STEPS: &[u32] = &[320, 480, 640, 960, 1280];

/// Index in `ROI_STEPS` für den Start (= source5-Verhalten, 640 oben links).
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
    /// Pan-Schritt = `size / step_divisor` (aus `[pan]`, Default 16).
    pub step_divisor: u32,
    /// Pan-Schritt mindestens `step_min_px` (aus `[pan]`, Default 8).
    pub step_min_px: u32,
}

impl View {
    /// Start-View aus `[pan]`-Werten (`default_size` muss aus `ROI_STEPS`
    /// stammen — prüft der Aufrufer, sonst gilt `MODEL_SIZE`).
    #[must_use]
    pub fn from_pan(default_size: u32, step_divisor: u32, step_min_px: u32) -> Self {
        let size = if ROI_STEPS.contains(&default_size) {
            default_size
        } else {
            MODEL_SIZE
        };
        Self {
            x: 0,
            y: 0,
            size,
            step_divisor,
            step_min_px,
        }
    }

    /// Pan-Schritt in px: mit der ROI mitwachsend, mindestens `step_min_px`.
    #[must_use]
    pub fn pan_step(&self) -> i32 {
        ((self.size / self.step_divisor.max(1)).max(self.step_min_px)) as i32
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

    /// Bildet eine Box aus dem Detektions-Raum (`MODEL_SIZE`×`MODEL_SIZE`)
    /// auf absolute Bildschirm-Pixel ab: `(linke Kante, obere Kante,
    /// Breite, Höhe)`. Mindestgröße 1 px, auf den Screen geclampt.
    #[must_use]
    pub fn to_screen_rect(
        self,
        bx: f32,
        by: f32,
        bw: f32,
        bh: f32,
        screen: Screen,
    ) -> (i32, i32, u32, u32) {
        let scale = self.size as f32 / MODEL_SIZE as f32;
        let sx = (self.x + (bx * scale).round() as i32).clamp(0, screen.w);
        let sy = (self.y + (by * scale).round() as i32).clamp(0, screen.h);
        let sw = ((bw * scale).round().max(1.0) as i32).clamp(1, screen.w);
        let sh = ((bh * scale).round().max(1.0) as i32).clamp(1, screen.h);
        (sx, sy, sw as u32, sh as u32)
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
    fn projection_is_identity_at_native() {
        let v = View::from_pan(640, 16, 8);
        assert_eq!(
            v.to_screen_rect(10.0, 20.0, 100.0, 30.0, SCREEN),
            (10, 20, 100, 30)
        );
    }

    #[test]
    fn projection_scales_with_roi() {
        let mut v = View::from_pan(320, 16, 8);
        v.x = 100;
        v.y = 50;
        // scale 0.5: (200,100,60,40) -> (200,100,30,20).
        assert_eq!(
            v.to_screen_rect(200.0, 100.0, 60.0, 40.0, SCREEN),
            (200, 100, 30, 20)
        );
        let v = View::from_pan(1280, 16, 8);
        // scale 2.0.
        assert_eq!(
            v.to_screen_rect(10.0, 20.0, 100.0, 30.0, SCREEN),
            (20, 40, 200, 60)
        );
    }

    #[test]
    fn projection_clamps_to_screen() {
        let mut v = View::from_pan(320, 16, 8);
        v.x = 1800;
        v.y = 1000;
        let (sx, sy, sw, sh) = v.to_screen_rect(600.0, 600.0, 200.0, 200.0, SCREEN);
        assert!(sx <= SCREEN.w && sy <= SCREEN.h);
        assert!(sw >= 1 && sh >= 1);
    }

    #[test]
    fn pan_step_follows_toml_formula() {
        let mut v = View::from_pan(640, 16, 8);
        assert_eq!(v.pan_step(), 640 / 16);
        v.step_divisor = 8;
        v.step_min_px = 100;
        // 640/8 = 80 < 100 -> Minimum greift.
        assert_eq!(v.pan_step(), 100);
        let step = v.pan_step();
        assert!(v.pan(1, 0, SCREEN));
        assert_eq!(v.x, step);
    }

    #[test]
    fn from_pan_takes_valid_size_or_falls_back() {
        let v = View::from_pan(320, 16, 8);
        assert_eq!((v.size, v.step_divisor, v.step_min_px), (320, 16, 8));
        let v = View::from_pan(999, 16, 8);
        assert_eq!(v.size, MODEL_SIZE);
    }

    #[test]
    fn pan_and_zoom_match_source5() {
        let mut v = View::from_pan(640, 16, 8);
        let step = v.pan_step();
        assert!(v.pan(1, 1, SCREEN));
        assert_eq!((v.x, v.y), (step, step));
        assert!(v.zoom_in(SCREEN));
        assert_eq!(v.size, 480);
        assert!(v.zoom_out(SCREEN));
        assert_eq!(v.size, 640);
    }
}
