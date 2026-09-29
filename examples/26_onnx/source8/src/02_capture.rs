//! `02_capture` — X11-Root-Window-Grab (Z-Pixmap) → `Rgb`.
//! Einziger X11-Code; Test über Xvfb (`gui_detect grab`).

use crate::image::Rgb;
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{ConnectionExt, ImageFormat};

/// Offene X11-Verbindung plus Root-Fenster und Bildschirmgröße.
pub struct Screen {
    conn: x11rb::rust_connection::RustConnection,
    root: u32,
    pub w: usize,
    pub h: usize,
}

impl Screen {
    /// Verbindet zu `$DISPLAY`.
    pub fn open() -> Result<Self, String> {
        let (conn, idx) = x11rb::connect(None).map_err(|e| format!("X11: {e}"))?;
        let s = &conn.setup().roots[idx];
        let (root, w, h) = (
            s.root,
            s.width_in_pixels as usize,
            s.height_in_pixels as usize,
        );
        Ok(Self { conn, root, w, h })
    }

    /// Ganzer Bildschirm als RGB (24/32-bpp-TrueColor vorausgesetzt).
    pub fn grab(&self) -> Result<Rgb, String> {
        let reply = self
            .conn
            .get_image(
                ImageFormat::Z_PIXMAP,
                self.root,
                0,
                0,
                self.w as u16,
                self.h as u16,
                u32::MAX,
            )
            .map_err(|e| format!("GetImage: {e}"))?
            .reply()
            .map_err(|e| format!("GetImage: {e}"))?;
        if reply.data.len() < self.w * self.h * 4 {
            return Err(format!(
                "unerwartete Bildgröße {} (Tiefe {})",
                reply.data.len(),
                reply.depth
            ));
        }
        Ok(Rgb::from_bgra(self.w, self.h, &reply.data))
    }
}
