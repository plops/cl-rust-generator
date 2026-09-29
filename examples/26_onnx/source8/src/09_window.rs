//! `09_window` — minimales X11-Ausgabefenster über x11rb (keine GUI-Crate):
//! RGB-Frame per `PutImage` in Streifen zeigen, HUD-Zeile mit dem
//! Server-Font `fixed`, Ende per Escape/q oder Fenster-Schließen.

use crate::image::Rgb;
use x11rb::connection::{Connection, RequestConnection};
use x11rb::protocol::Event;
use x11rb::protocol::xproto::{
    AtomEnum, ConnectionExt, CreateGCAux, CreateWindowAux, EventMask, ImageFormat, PropMode,
    WindowClass,
};
use x11rb::rust_connection::RustConnection;
use x11rb::wrapper::ConnectionExt as _;

/// Keycodes (evdev/Xvfb-Standardbelegung): Escape und `q`.
const KEY_ESCAPE: u8 = 9;
const KEY_Q: u8 = 24;

pub struct Window {
    conn: RustConnection,
    win: u32,
    gc: u32,
    depth: u8,
    wm_delete: u32,
    has_font: bool,
    /// Wiederverwendeter BGRX-Puffer für `PutImage`.
    bgrx: Vec<u8>,
}

/// RGB → BGRX (Z-Pixmap, 32 bpp) in einen wiederverwendeten Puffer.
pub fn to_bgrx(img: &Rgb, out: &mut Vec<u8>) {
    out.clear();
    let (px, _) = img.data.as_chunks::<3>();
    for p in px {
        out.extend_from_slice(&[p[2], p[1], p[0], 0]);
    }
}

impl Window {
    /// Öffnet ein `w×h`-Fenster bei `(x, y)` (ohne Window-Manager exakt dort,
    /// mit WM nur ein Hinweis).
    pub fn open(title: &str, x: i16, y: i16, w: u16, h: u16) -> Result<Self, String> {
        let err = |e: &dyn std::fmt::Display| format!("X11-Fenster: {e}");
        let (conn, idx) = x11rb::connect(None).map_err(|e| err(&e))?;
        let screen = &conn.setup().roots[idx];
        let (root, depth, visual) = (screen.root, screen.root_depth, screen.root_visual);
        let bpp = conn
            .setup()
            .pixmap_formats
            .iter()
            .find(|f| f.depth == depth)
            .map_or(0, |f| f.bits_per_pixel);
        if bpp != 32 {
            return Err(format!(
                "brauche 32-bpp-TrueColor, Server hat Tiefe {depth} mit {bpp} bpp"
            ));
        }
        let win = conn.generate_id().map_err(|e| err(&e))?;
        let aux = CreateWindowAux::new()
            .background_pixel(screen.black_pixel)
            .event_mask(EventMask::EXPOSURE | EventMask::KEY_PRESS);
        conn.create_window(
            depth,
            win,
            root,
            x,
            y,
            w,
            h,
            0,
            WindowClass::INPUT_OUTPUT,
            visual,
            &aux,
        )
        .map_err(|e| err(&e))?;
        conn.change_property8(
            PropMode::REPLACE,
            win,
            AtomEnum::WM_NAME,
            AtomEnum::STRING,
            title.as_bytes(),
        )
        .map_err(|e| err(&e))?;

        // Schließen-Knopf des Window-Managers als ClientMessage empfangen.
        let atom = |name: &[u8]| -> Result<u32, String> {
            Ok(conn
                .intern_atom(false, name)
                .map_err(|e| err(&e))?
                .reply()
                .map_err(|e| err(&e))?
                .atom)
        };
        let (protocols, wm_delete) = (atom(b"WM_PROTOCOLS")?, atom(b"WM_DELETE_WINDOW")?);
        conn.change_property32(
            PropMode::REPLACE,
            win,
            protocols,
            AtomEnum::ATOM,
            &[wm_delete],
        )
        .map_err(|e| err(&e))?;

        // HUD-Font: `fixed` ist im X-Server eingebaut; fehlt er, ohne HUD weiter.
        let font = conn.generate_id().map_err(|e| err(&e))?;
        let has_font = conn
            .open_font(font, b"fixed")
            .map_err(|e| err(&e))?
            .check()
            .is_ok();
        let gc = conn.generate_id().map_err(|e| err(&e))?;
        let mut gc_aux = CreateGCAux::new().foreground(0x00ff_ffff).background(0);
        if has_font {
            gc_aux = gc_aux.font(font);
        }
        conn.create_gc(gc, win, &gc_aux).map_err(|e| err(&e))?;
        conn.map_window(win).map_err(|e| err(&e))?;
        conn.flush().map_err(|e| err(&e))?;
        Ok(Self {
            conn,
            win,
            gc,
            depth,
            wm_delete,
            has_font,
            bgrx: Vec::new(),
        })
    }

    /// Zeigt das Bild bei (0,0) plus eine HUD-Zeile oben links.
    pub fn show(&mut self, img: &Rgb, hud: &str) -> Result<(), String> {
        let err = |e: &dyn std::fmt::Display| format!("X11-Ausgabe: {e}");
        to_bgrx(img, &mut self.bgrx);
        // Streifenweise senden: ein 640×640-Frame (1,6 MB) kann über der
        // maximalen Request-Größe liegen (ohne BIG-REQUESTS 256 KiB).
        let row = img.w * 4;
        let rows = ((self.conn.maximum_request_bytes() - 64) / row).clamp(1, img.h);
        for (i, chunk) in self.bgrx.chunks(rows * row).enumerate() {
            let y = (i * rows) as i16;
            let h = (chunk.len() / row) as u16;
            self.conn
                .put_image(
                    ImageFormat::Z_PIXMAP,
                    self.win,
                    self.gc,
                    img.w as u16,
                    h,
                    0,
                    y,
                    0,
                    self.depth,
                    chunk,
                )
                .map_err(|e| err(&e))?;
        }
        if self.has_font {
            let text = &hud.as_bytes()[..hud.len().min(255)];
            self.conn
                .image_text8(self.win, self.gc, 4, 14, text)
                .map_err(|e| err(&e))?;
        }
        self.conn.flush().map_err(|e| err(&e))
    }

    /// Verarbeitet anstehende Events; `true` = Benutzer will beenden.
    pub fn quit_requested(&self) -> Result<bool, String> {
        while let Some(ev) = self
            .conn
            .poll_for_event()
            .map_err(|e| format!("X11-Event: {e}"))?
        {
            match ev {
                Event::KeyPress(k) if k.detail == KEY_ESCAPE || k.detail == KEY_Q => {
                    return Ok(true);
                }
                Event::ClientMessage(m) if m.data.as_data32()[0] == self.wm_delete => {
                    return Ok(true);
                }
                _ => {}
            }
        }
        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bgrx_swizzle_and_reuse() {
        let img = Rgb {
            w: 2,
            h: 1,
            data: vec![1, 2, 3, 4, 5, 6],
        };
        let mut buf = vec![9; 100];
        to_bgrx(&img, &mut buf);
        assert_eq!(buf, vec![3, 2, 1, 0, 6, 5, 4, 0]);
    }
}
