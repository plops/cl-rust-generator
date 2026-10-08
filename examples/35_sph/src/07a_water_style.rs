//! Wasser-Farbstil: Palette, Gischt-Erkennung, Stilkonstanten.
//!
//! Reine Funktionen (unit-getestet); der Renderer (`07_renderer.rs`) ruft
//! sie pro Partikel auf. Aquatische Rampe (tief → türkis → gischtweiß)
//! statt technischer Wärmebildkamera.

use macroquad::prelude::*;

/// Hintergrund (Tiefsee-Schwarzblau), auch Fade-Farbe für Trails.
pub const WATER_BG: (u8, u8, u8) = (8, 10, 18);
/// Fade-Alpha pro Frame bei Trails (110 ≈ 43 % → ~4 Frames Schweif,
/// HUD-Schrift bleibt lesbar, da sie jeden Frame frisch übermalt wird).
pub const TRAIL_FADE_ALPHA: u8 = 110;
/// Gischt-Schwelle: Dichte unter 70 % von ρ₀ wirkt als Spray.
pub const FOAM_DENSITY_RATIO: f32 = 0.7;
/// Gischt-Schwelle: Aufwärtsgeschwindigkeit über 2,5 m/s wirkt als Spray.
pub const FOAM_RISE_VEL: f32 = 2.5;
/// Partikelradius als Bruchteil des Anfangsabstands (1,5-fache Überlappung
/// lässt dichte Regionen optisch zur Fläche verschmelzen).
pub const PARTICLE_R_FACTOR: f32 = 0.75;
/// Deckkraft des Wasserkörpers (Überlappung akkumuliert zur Fläche).
pub const WATER_ALPHA: u8 = 220;

/// Tiefes Ozeanblau (ruhiges/tiefes Wasser).
pub fn deep_color() -> Color {
    Color::from_rgba(10, 45, 90, WATER_ALPHA)
}

/// Türkis/Cyan (bewegtes Flachwasser).
pub fn mid_color() -> Color {
    Color::from_rgba(0, 150, 210, WATER_ALPHA)
}

/// Gischtweiß (schnell/dünn).
pub fn foam_color() -> Color {
    Color::from_rgba(210, 240, 255, 255)
}

/// Lineare Farbmischung inkl. Alpha (t = 0 → a, t = 1 → b).
pub fn mix(a: Color, b: Color, t: f32) -> Color {
    Color::new(
        a.r + (b.r - a.r) * t,
        a.g + (b.g - a.g) * t,
        a.b + (b.b - a.b) * t,
        a.a + (b.a - a.a) * t,
    )
}

/// Wasserfarbe über Geschwindigkeitsbetrag (0–5 m/s): tief → türkis → weiß.
pub fn water_color(speed: f32) -> Color {
    let t = (speed / 5.0).clamp(0.0, 1.0);
    if t < 0.6 {
        mix(deep_color(), mid_color(), t / 0.6)
    } else {
        mix(mid_color(), foam_color(), (t - 0.6) / 0.4)
    }
}

/// Aquatische Dichtefarbe über ρ/ρ₀: tief (0,3ρ₀) → türkis (ρ₀) → weiß.
pub fn water_density_color(rho: f32, rho0: f32) -> Color {
    let t = ((rho / rho0 - 0.3) / 0.8).clamp(0.0, 1.0);
    if t < 0.875 {
        mix(deep_color(), mid_color(), t / 0.875)
    } else {
        mix(mid_color(), foam_color(), (t - 0.875) / 0.125)
    }
}

/// Gischt? Dünne Regionen oder aufsteigende Spritzer wirken als Spray.
pub fn is_foam(density: f32, rest_density: f32, vy: f32) -> bool {
    density < FOAM_DENSITY_RATIO * rest_density || vy > FOAM_RISE_VEL
}

/// Kantenlänge des Soft-Sprites in px (radialer Verlauf, weiß).
pub const SPRITE_SIZE: usize = 64;

/// Weiches Partikel-Sprite: weißer Radialverlauf (Mitte deckend, Rand
/// transparent), vom Renderer per `draw_texture` eingefärbt und skaliert.
///
/// Ein texturierter Quad kostet gleich viele Vertices wie das alte Rechteck
/// (2 Dreiecke) und batcht identisch — Kreise aus Dreiecksfächern wären
/// ~20× teurer in der Draw-Schleife.
pub fn soft_sprite_image() -> Image {
    let s = SPRITE_SIZE;
    let mut bytes = vec![0u8; s * s * 4];
    for y in 0..s {
        for x in 0..s {
            let dx = (x as f32 + 0.5) / s as f32 * 2.0 - 1.0;
            let dy = (y as f32 + 0.5) / s as f32 * 2.0 - 1.0;
            let d2 = dx * dx + dy * dy;
            // Glatter Falloff: opaker Kern, weiche Kante, transparent außen.
            let a = (1.0 - d2).max(0.0).powf(1.5);
            let o = (y * s + x) * 4;
            bytes[o] = 255;
            bytes[o + 1] = 255;
            bytes[o + 2] = 255;
            bytes[o + 3] = (a * 255.0) as u8;
        }
    }
    Image {
        bytes,
        width: s as u16,
        height: s as u16,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn valid(c: Color) {
        assert!([c.r, c.g, c.b, c.a].iter().all(|x| (0.0..=1.0).contains(x)));
    }

    fn approx_color(a: Color, b: Color) {
        // mix(.., 1.0) rechnet a+(b−a) und darf 1 ulp neben b landen.
        for (x, y) in [(a.r, b.r), (a.g, b.g), (a.b, b.b), (a.a, b.a)] {
            assert!((x - y).abs() < 1e-6, "{a:?} ≈ {b:?}");
        }
    }

    #[test]
    fn wasser_rampe_ist_gueltig_und_erreicht_endpunkte() {
        for v in [0.0, 1.5, 3.0, 5.0, 20.0] {
            valid(water_color(v));
        }
        approx_color(water_color(0.0), deep_color());
        approx_color(water_color(99.0), foam_color());
        for r in [0.0, 300.0, 1000.0, 1100.0, 5000.0] {
            valid(water_density_color(r, 1000.0));
        }
        approx_color(water_density_color(0.0, 1000.0), deep_color());
        approx_color(water_density_color(99_000.0, 1000.0), foam_color());
    }

    #[test]
    fn gischt_regel_erkennt_spray() {
        assert!(is_foam(600.0, 1000.0, 0.0)); // dünn
        assert!(is_foam(1000.0, 1000.0, 3.0)); // aufsteigend
        assert!(!is_foam(1000.0, 1000.0, 0.0)); // Volumenwasser
        assert!(!is_foam(1000.0, 1000.0, -5.0)); // fallend, aber dicht
        valid(foam_color());
    }

    #[test]
    fn sprite_ist_radial_weich() {
        let img = soft_sprite_image();
        assert_eq!((img.width, img.height), (64, 64));
        assert_eq!(img.bytes.len(), 64 * 64 * 4);
        let alpha = |x: usize, y: usize| img.bytes[(y * 64 + x) * 4 + 3];
        // Mitte (nahezu) deckend, Ecken transparent, monotoner Falloff.
        assert!(alpha(32, 32) >= 254, "Kern: {}", alpha(32, 32));
        assert_eq!(alpha(0, 0), 0);
        assert_eq!(alpha(63, 0), 0);
        let mut prev = 255u8;
        for d in 0..32 {
            let a = alpha(32, 32 + d.min(31));
            assert!(a <= prev, "d={d}: {a} <= {prev}");
            prev = a;
        }
        // RGB überall weiß (Einfärbung übernimmt der Renderer).
        assert!(
            img.bytes
                .as_chunks::<4>()
                .0
                .iter()
                .all(|p| p[0] == 255 && p[1] == 255 && p[2] == 255)
        );
    }

    #[test]
    fn mischung_blendet_auch_alpha() {
        let c = mix(deep_color(), foam_color(), 0.5);
        assert!((c.a - (deep_color().a + foam_color().a) * 0.5).abs() < 1e-6);
    }
}
