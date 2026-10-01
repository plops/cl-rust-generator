//! Dateityp-Farben: Extension-Mapping mit Hash-Fallback.
//!
//! Port des macroquad-MVP: Quelltext grün, Bilder blau, Medien violett,
//! Archive rot, alles andere deterministisch aus dem Dateinamen gehasht.

use std::ffi::OsStr;
use std::path::Path;

use crate::types::Rgb;

/// Hintergrundfarbe für Verzeichnisrahmen.
pub const DIR_COLOR: Rgb = Rgb::new(0.15, 0.17, 0.22);

const CODE_COLOR: Rgb = Rgb::new(0.20, 0.75, 0.45);
const IMAGE_COLOR: Rgb = Rgb::new(0.20, 0.65, 0.95);
const MEDIA_COLOR: Rgb = Rgb::new(0.75, 0.35, 0.85);
const ARCHIVE_COLOR: Rgb = Rgb::new(0.90, 0.30, 0.25);

/// Farbe für `path` anhand der Dateiendung.
pub fn color_for_path(path: &Path) -> Rgb {
    let ext = path
        .extension()
        .and_then(OsStr::to_str)
        .unwrap_or("")
        .to_ascii_lowercase();
    match ext.as_str() {
        "rs" | "c" | "cpp" | "h" | "hpp" | "py" | "js" | "ts" | "txt" | "md" | "lisp" | "lsp"
        | "clj" | "go" | "java" | "json" | "toml" | "xml" | "html" | "css" => CODE_COLOR,
        "png" | "jpg" | "jpeg" | "svg" | "webp" | "gif" | "bmp" | "avif" => IMAGE_COLOR,
        "mp4" | "mkv" | "mov" | "mp3" | "flac" | "wav" | "ogg" => MEDIA_COLOR,
        "zip" | "tar" | "gz" | "7z" | "rar" | "bz2" | "xz" => ARCHIVE_COLOR,
        _ => hash_color(path),
    }
}

/// Deterministische Pastellfarbe aus dem Dateinamen (Fallback).
fn hash_color(path: &Path) -> Rgb {
    let name = path.file_name().and_then(OsStr::to_str).unwrap_or("");
    let hash: u32 = name
        .bytes()
        .fold(0, |acc, b| acc.wrapping_add(u32::from(b)));
    Rgb::bytes(
        (hash.wrapping_mul(37) % 160 + 80) as u8,
        (hash.wrapping_mul(59) % 160 + 80) as u8,
        (hash.wrapping_mul(83) % 160 + 80) as u8,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_extensions_map() {
        assert_eq!(color_for_path(Path::new("main.rs")), CODE_COLOR);
        assert_eq!(color_for_path(Path::new("doc.TXT")), CODE_COLOR);
        assert_eq!(color_for_path(Path::new("pic.png")), IMAGE_COLOR);
        assert_eq!(color_for_path(Path::new("film.mkv")), MEDIA_COLOR);
        assert_eq!(color_for_path(Path::new("data.tar.gz")), ARCHIVE_COLOR);
    }

    #[test]
    fn unknown_extension_is_stable_and_bright() {
        let a = color_for_path(Path::new("mystery.zzz9"));
        let b = color_for_path(Path::new("mystery.zzz9"));
        assert_eq!(a, b);
        // Pastellbereich 80..=239 -> normiert >= 0.3.
        assert!(a.r >= 0.3 && a.g >= 0.3 && a.b >= 0.3);
    }
}
