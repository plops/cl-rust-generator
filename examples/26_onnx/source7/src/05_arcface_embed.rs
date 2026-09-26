//! `05_arcface_embed` — ArcFace-Session, 112-Preprocessing, 512D-L2-Embedding.
//!
//! Input: alignter 112×112-RGB-Crop. Norm `(x−127.5)/127.5` wie im
//! Referenzcode, Output zwingend L2-normalisiert (Cosine = Dot).

use crate::types::{CROP_SIZE, Embedding512};
use ort::session::Session;

/// ArcFace-Embedder mit Session.
pub struct ArcfaceEmbed {
    session: Session,
    input_name: String,
}

impl ArcfaceEmbed {
    /// Lädt das Modell (`w600k_mbf.onnx`) mit EP-Fallback.
    pub fn open(path: &str) -> Self {
        // Gleicher Fallback wie SCRFD; Provider-Name trägt die Engine.
        let (session, _provider) =
            crate::scrfd::session_with_fallback(path, [1, 3, CROP_SIZE, CROP_SIZE]);
        let input_name = session.inputs()[0].name().to_string();
        Self {
            session,
            input_name,
        }
    }

    /// Extrahiert das L2-normalisierte Embedding eines 112-Crops.
    pub fn embed(&mut self, crop112_rgb: &[u8]) -> Embedding512 {
        use ort::value::TensorRef;
        let input = preprocess_112(crop112_rgb);
        let outputs = self
            .session
            .run(ort::inputs![self.input_name.as_str() =>
                TensorRef::from_array_view(([1, 3, CROP_SIZE, CROP_SIZE], &input[..])).unwrap()])
            .unwrap();
        let (_, d) = outputs[0].try_extract_tensor::<f32>().unwrap();
        let mut e = Embedding512 {
            v: d.try_into().unwrap_or_else(|_| Embedding512::zeros().v),
        };
        e.normalize();
        e
    }
}

/// `(x−127.5)/127.5`, RGB-planar für 112×112.
pub fn preprocess_112(crop: &[u8]) -> Vec<f32> {
    let plane = CROP_SIZE * CROP_SIZE;
    let mut out = vec![0.0f32; 3 * plane];
    let (r, rest) = out.split_at_mut(plane);
    let (g, b) = rest.split_at_mut(plane);
    let (px3, _) = crop.as_chunks::<3>();
    for (i, px) in px3.iter().take(plane).enumerate() {
        r[i] = (f32::from(px[0]) - 127.5) / 127.5;
        g[i] = (f32::from(px[1]) - 127.5) / 127.5;
        b[i] = (f32::from(px[2]) - 127.5) / 127.5;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preprocess_maps_black_white_symmetrically() {
        let mut crop = vec![0u8; 112 * 112 * 3];
        crop[0..3].copy_from_slice(&[0, 127, 255]);
        let p = preprocess_112(&crop);
        let plane = 112 * 112;
        assert!((p[0] + 1.0).abs() < 1e-6); // schwarz → −1
        assert!((p[plane] - (127.0f32 - 127.5) / 127.5).abs() < 1e-6);
        assert!((p[2 * plane] - 1.0).abs() < 1e-6); // weiß → +1
    }

    #[test]
    fn cosine_equals_dot_for_normalized() {
        let mut a = Embedding512 { v: [1.0; 512] };
        let mut b = Embedding512 { v: [1.0; 512] };
        a.normalize();
        b.normalize();
        assert!((a.dot(&b) - 1.0).abs() < 1e-4);
    }
}
