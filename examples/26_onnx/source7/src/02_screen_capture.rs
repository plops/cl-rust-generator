//! `02_screen_capture` — X11-Root-Grab 640×640 Z-Pixmap → RGB-Puffer.
//!
//! Nur `capture_rgb` braucht ein Display; `bgra_to_rgb` ist rein und ohne
//! X11 testbar. Single-Pass wie in `source2` (kein Resize: nativ 640).

use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

/// Capture-Kante in px (SCRFD-Nativegröße, kein Resize nötig).
pub const CAPTURE_SIZE: usize = 640;
/// Pixel im Capture (`640*640`).
pub const CAPTURE_PLANE: usize = CAPTURE_SIZE * CAPTURE_SIZE;

/// Wandelt X11-BGRA (`4 B/px`) in RGB (`3 B/px`) um; Alpha entfällt.
pub fn bgra_to_rgb(bgra: &[u8], out_rgb: &mut [u8]) {
    debug_assert_eq!(out_rgb.len(), bgra.len() / 4 * 3);
    let (dst, _) = out_rgb.as_chunks_mut::<3>();
    let (src, _) = bgra.as_chunks::<4>();
    for (d, px) in dst.iter_mut().zip(src.iter()) {
        d[0] = px[2];
        d[1] = px[1];
        d[2] = px[0];
    }
}

/// X11-Verbindung plus Root-Fenster für wiederholte Grabs.
pub struct ScreenCapture {
    /// Offene X11-Verbindung.
    conn: x11rb::rust_connection::RustConnection,
    /// Root-Window-ID.
    root: u32,
}

impl ScreenCapture {
    /// Verbindet zum X-Server (`DISPLAY` aus der Umgebung).
    pub fn connect() -> Result<Self, Box<dyn std::error::Error>> {
        let (conn, screen_idx) = x11rb::connect(None)?;
        let root = conn.setup().roots[screen_idx].root;
        Ok(Self { conn, root })
    }

    /// Liest 640×640 oben links als RGB-Bytes (`640*640*3`).
    pub fn capture_rgb(&self) -> Vec<u8> {
        let reply = xproto::get_image(
            &self.conn,
            ImageFormat::Z_PIXMAP,
            self.root,
            0,
            0,
            CAPTURE_SIZE as u16,
            CAPTURE_SIZE as u16,
            u32::MAX,
        )
        .unwrap()
        .reply()
        .unwrap();
        let mut rgb = vec![0u8; CAPTURE_PLANE * 3];
        let n = CAPTURE_PLANE.min(reply.data.len() / 4);
        bgra_to_rgb(&reply.data[..n * 4], &mut rgb[..n * 3]);
        rgb
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bgra_to_rgb_swaps_channels_drops_alpha() {
        let bgra = vec![10u8, 20, 30, 255, 1, 2, 3, 0];
        let mut rgb = vec![0u8; 6];
        bgra_to_rgb(&bgra, &mut rgb);
        assert_eq!(rgb, vec![30, 20, 10, 3, 2, 1]);
    }

    #[test]
    fn capture_plane_matches_size() {
        assert_eq!(CAPTURE_PLANE, 640 * 640);
        assert_eq!(CAPTURE_PLANE * 3, 640 * 640 * 3);
    }
}
