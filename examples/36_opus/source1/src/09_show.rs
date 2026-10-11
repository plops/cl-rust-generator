//! X11-Fenster via `minifb` (nur Feature `show`).
//!
//! Zeigt das fertige Bild; ESC oder Schliessen beendet. `max_frames`
//! begrenzt die Event-Schleife (Test-Haken fuer xvfb-Smoke).

use minifb::{Key, Window, WindowOptions};

use crate::render::Image;
use crate::types::Error;

/// Bild ins Fenster bringen (blockiert bis ESC/schliessen/Limit).
pub fn show(img: &Image, title: &str, max_frames: u64) -> Result<(), Error> {
    let (w, h) = (img.width as usize, img.height as usize);
    let mut window = Window::new(title, w, h, WindowOptions::default())
        .map_err(|e| Error::Show(format!("oeffnen: {e}")))?;
    window.set_target_fps(30);
    let buf = pack(img);
    let mut frames = 0;
    while window.is_open()
        && !window.is_key_down(Key::Escape)
        && frames < max_frames
    {
        window
            .update_with_buffer(&buf, w, h)
            .map_err(|e| Error::Show(format!("anzeige: {e}")))?;
        frames += 1;
    }
    Ok(())
}

/// RGB-Bytes → `0x00RRGGBB`-Puffer (minifb-Format, rein/testbar).
fn pack(img: &Image) -> Vec<u32> {
    img.rgb
        .chunks_exact(3)
        .map(|p| {
            (u32::from(p[0]) << 16) | (u32::from(p[1]) << 8) | u32::from(p[2])
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pack_orders_rgb_channels() {
        let img = Image {
            width: 2,
            height: 1,
            rgb: vec![255, 0, 0, 0, 17, 34],
        };
        assert_eq!(pack(&img), vec![0x00ff_0000, 0x0000_1122]);
    }
}
