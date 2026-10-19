//! `02_av1` — AV1-Decoder (rav1d, reines Rust) für Still-Picture-Kacheln.
//!
//! Aus `source6/client/02_av1.rs` übernommen: rav1d exportiert die
//! dav1d-C-API als Rust-Funktionen; dieser Wrapper kapselt das `unsafe`
//! an einer Stelle und liefert RGBA8.

use std::ptr::NonNull;

use lbw_common::yuv::yuv420_to_rgba;
use rav1d::include::dav1d::data::Dav1dData;
use rav1d::include::dav1d::dav1d::{Dav1dContext, Dav1dSettings};
use rav1d::include::dav1d::headers::DAV1D_PIXEL_LAYOUT_I420;
use rav1d::include::dav1d::picture::Dav1dPicture;
use rav1d::src::lib::{
    dav1d_close, dav1d_data_create, dav1d_data_unref, dav1d_default_settings, dav1d_get_picture,
    dav1d_open, dav1d_picture_unref, dav1d_send_data,
};

/// Dekodiertes Bild.
#[derive(Debug)]
pub struct Rgba {
    pub w: usize,
    pub h: usize,
    pub data: Vec<u8>,
}

/// Skaliert RGBA8 per Nearest-Neighbor auf (`w`, `h`) hoch (oder runter).
/// Für `--tile-scale`-Kacheln: Der Server kodiert klein, der Client stellt
/// den vollen Fußabdruck wieder her. Gleich groß = unverändert zurück.
#[must_use]
pub fn upscale_rgba(src: Rgba, w: usize, h: usize) -> Rgba {
    if src.w == w && src.h == h || w == 0 || h == 0 || src.data.len() < src.w * src.h * 4 {
        return src;
    }
    let mut data = vec![0u8; w * h * 4];
    for y in 0..h {
        let sy = (y * src.h / h).min(src.h.saturating_sub(1));
        for x in 0..w {
            let sx = (x * src.w / w).min(src.w.saturating_sub(1));
            let s = (sy * src.w + sx) * 4;
            let d = (y * w + x) * 4;
            data[d..d + 4].copy_from_slice(&src.data[s..s + 4]);
        }
    }
    Rgba { w, h, data }
}

/// `-EAGAIN` der dav1d-API (Linux: EAGAIN = 11).
const EAGAIN: i32 = -11;

/// Einmal geöffneter Decoder; wird für jede Kachel wiederverwendet.
pub struct Decoder {
    ctx: Option<Dav1dContext>,
}

// SAFETY: Der Kontext wird nur über `&mut self` benutzt (kein geteilter Zugriff).
unsafe impl Send for Decoder {}

impl Decoder {
    /// Öffnet rav1d (1 Thread genügt für 720p-Stills).
    pub fn new() -> Result<Self, String> {
        let mut s = std::mem::MaybeUninit::<Dav1dSettings>::uninit();
        // SAFETY: `s` ist gültig beschreibbar; danach initialisiert.
        let mut s = unsafe {
            dav1d_default_settings(NonNull::new(s.as_mut_ptr()).unwrap());
            s.assume_init()
        };
        s.n_threads = 1;
        s.max_frame_delay = 1;
        let mut ctx: Option<Dav1dContext> = None;
        // SAFETY: Zeiger auf lokale, gültige Werte.
        let r = unsafe { dav1d_open(Some(NonNull::from(&mut ctx)), Some(NonNull::from(&mut s))) };
        if r.0 != 0 || ctx.is_none() {
            return Err(format!("dav1d_open: {}", r.0));
        }
        Ok(Self { ctx })
    }

    /// Dekodiert eine Kachel (rohe OBUs eines Still-Pictures) zu RGBA8.
    pub fn decode(&mut self, obu: &[u8]) -> Result<Rgba, String> {
        if obu.is_empty() {
            return Err("leere Kachel".into());
        }
        // `Dav1dContext` ist ein `Copy`-Handle (roher Arc-Zeiger).
        let ctx = self.ctx;
        let mut data = Dav1dData::default();
        // SAFETY: `data` ist gültig beschreibbar; Puffer hat `obu.len()` Byte.
        unsafe {
            let p = dav1d_data_create(Some(NonNull::from(&mut data)), obu.len());
            if p.is_null() {
                return Err("dav1d_data_create".into());
            }
            std::ptr::copy_nonoverlapping(obu.as_ptr(), p, obu.len());
        }
        let mut pic = Dav1dPicture::default();
        let mut got = false;
        // Senden bis alles verbraucht ist; dazwischen Bilder abholen.
        for _ in 0..16 {
            if data.sz > 0 {
                // SAFETY: `ctx` stammt aus `dav1d_open`; `data` ist gültig.
                let r = unsafe { dav1d_send_data(ctx, Some(NonNull::from(&mut data))) };
                if r.0 != 0 && r.0 != EAGAIN {
                    // SAFETY: `data` ist gültig.
                    unsafe { dav1d_data_unref(Some(NonNull::from(&mut data))) };
                    return Err(format!("dav1d_send_data: {}", r.0));
                }
            }
            // SAFETY: `ctx` gültig, `pic` beschreibbar.
            let r = unsafe { dav1d_get_picture(ctx, Some(NonNull::from(&mut pic))) };
            if r.0 == 0 {
                got = true;
                break;
            }
            if r.0 != EAGAIN {
                // SAFETY: `data` ist gültig.
                unsafe { dav1d_data_unref(Some(NonNull::from(&mut data))) };
                return Err(format!("dav1d_get_picture: {}", r.0));
            }
            if data.sz == 0 {
                break;
            }
        }
        // SAFETY: `data` ist gültig (evtl. schon leer).
        unsafe { dav1d_data_unref(Some(NonNull::from(&mut data))) };
        if !got {
            return Err("kein Bild dekodiert".into());
        }
        let out = picture_to_rgba(&pic);
        // SAFETY: `pic` wurde von `dav1d_get_picture` gefüllt.
        unsafe { dav1d_picture_unref(Some(NonNull::from(&mut pic))) };
        out
    }
}

fn picture_to_rgba(pic: &Dav1dPicture) -> Result<Rgba, String> {
    let (w, h) = (pic.p.w as usize, pic.p.h as usize);
    if pic.p.bpc != 8 || pic.p.layout != DAV1D_PIXEL_LAYOUT_I420 {
        return Err(format!(
            "nicht unterstützt: bpc {} layout {}",
            pic.p.bpc, pic.p.layout
        ));
    }
    let (ys, cs) = (pic.stride[0] as usize, pic.stride[1] as usize);
    let (ch, cw) = (h.div_ceil(2), w.div_ceil(2));
    let plane = |i: usize, len: usize| -> Result<&[u8], String> {
        let p = pic.data[i].ok_or("fehlende Ebene")?;
        // SAFETY: dav1d garantiert `stride * Zeilen` gültige Bytes je Ebene.
        Ok(unsafe { std::slice::from_raw_parts(p.as_ptr() as *const u8, len) })
    };
    let y = plane(0, ys * (h - 1) + w)?;
    let u = plane(1, cs * (ch - 1) + cw)?;
    let v = plane(2, cs * (ch - 1) + cw)?;
    let mut data = vec![0u8; w * h * 4];
    yuv420_to_rgba(y, ys, u, v, cs, w, h, &mut data);
    Ok(Rgba { w, h, data })
}

impl Drop for Decoder {
    fn drop(&mut self) {
        // SAFETY: `ctx` stammt aus `dav1d_open` und wird hier genau einmal geschlossen.
        unsafe { dav1d_close(Some(NonNull::from(&mut self.ctx))) };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn garbage_is_an_error_not_a_crash() {
        let mut d = Decoder::new().unwrap();
        assert!(d.decode(&[]).is_err());
        assert!(d.decode(&[0x12, 0x00, 0xff, 0xff, 0x01]).is_err());
    }

    fn rgba2x2() -> Rgba {
        // rot | grün / blau | weiß
        Rgba {
            w: 2,
            h: 2,
            data: vec![
                255, 0, 0, 255, 0, 255, 0, 255, //
                0, 0, 255, 255, 255, 255, 255, 255,
            ],
        }
    }

    #[test]
    fn upscale_doubles_each_pixel_to_a_block() {
        let out = upscale_rgba(rgba2x2(), 4, 4);
        assert_eq!((out.w, out.h), (4, 4));
        let px = |x: usize, y: usize| &out.data[(y * 4 + x) * 4..][..4];
        assert_eq!(px(0, 0), &[255, 0, 0, 255]);
        assert_eq!(px(1, 0), &[255, 0, 0, 255]);
        assert_eq!(px(2, 0), &[0, 255, 0, 255]);
        assert_eq!(px(0, 2), &[0, 0, 255, 255]);
        assert_eq!(px(3, 3), &[255, 255, 255, 255]);
    }

    #[test]
    fn upscale_same_size_and_broken_input_pass_through() {
        let src = rgba2x2();
        let out = upscale_rgba(
            Rgba {
                w: 2,
                h: 2,
                data: src.data.clone(),
            },
            2,
            2,
        );
        assert_eq!(out.data, src.data);
        // Kaputt (zu wenig Bytes) oder Zielfläche 0: liegen lassen statt
        // zu paniken — blit() ignoriert das dann.
        let broken = Rgba {
            w: 2,
            h: 2,
            data: vec![1, 2, 3],
        };
        assert_eq!(upscale_rgba(broken, 4, 4).data.len(), 3);
    }
}
