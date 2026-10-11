//! Minimaler PNG-Writer: 8-Bit-RGB, unkomprimierte Deflate-Bloecke.
//!
//! Kein Filter, keine Interlace — jeder Standard-Reader akzeptiert das.
//! Reine `std`-Implementierung (CRC32-Tabelle + Adler32 inline).

/// PNG-Magie (8 Bytes).
pub const MAGIC: [u8; 8] = [137, 80, 78, 71, 13, 10, 26, 10];

/// Encodiert RGB-Zeilen (`rgb.len() == w*h*3`) zu PNG-Bytes.
#[must_use]
pub fn encode_rgb(width: u32, height: u32, rgb: &[u8]) -> Vec<u8> {
    assert_eq!(rgb.len(), width as usize * height as usize * 3);
    let mut out = Vec::with_capacity(rgb.len() + 128);
    out.extend_from_slice(&MAGIC);
    let mut ihdr = [0u8; 13];
    ihdr[0..4].copy_from_slice(&width.to_be_bytes());
    ihdr[4..8].copy_from_slice(&height.to_be_bytes());
    ihdr[8] = 8; // Bit-Tiefe.
    ihdr[9] = 2; // Truecolor RGB.
    chunk(&mut out, b"IHDR", &ihdr);
    // IDAT: zlib-Header + stored Blocks + Adler32.
    let mut raw = Vec::with_capacity(rgb.len() + height as usize);
    let stride = width as usize * 3;
    for row in rgb.chunks_exact(stride) {
        raw.push(0); // Filtertyp 0 = None.
        raw.extend_from_slice(row);
    }
    let mut zlib = vec![0x78, 0x01];
    for (i, block) in raw.chunks(65535).enumerate() {
        let last = i == raw.len().div_ceil(65535) - 1;
        zlib.push(u8::from(last)); // BFINAL + BTYPE 00.
        let len = block.len() as u16;
        zlib.extend_from_slice(&len.to_le_bytes());
        zlib.extend_from_slice(&(!len).to_le_bytes());
        zlib.extend_from_slice(block);
    }
    zlib.extend_from_slice(&adler32(&raw).to_be_bytes());
    chunk(&mut out, b"IDAT", &zlib);
    chunk(&mut out, b"IEND", &[]);
    out
}

/// Ein PNG-Chunk: Laenge + Typ + Daten + CRC32.
fn chunk(out: &mut Vec<u8>, typ: &[u8; 4], data: &[u8]) {
    out.extend_from_slice(&(data.len() as u32).to_be_bytes());
    out.extend_from_slice(typ);
    out.extend_from_slice(data);
    let crc = crc32_update(0xffff_ffff, typ);
    let crc = crc32_update(crc, data) ^ 0xffff_ffff;
    out.extend_from_slice(&crc.to_be_bytes());
}

/// CRC32-Tabelle (IEEE-Polynom), bei Bedarf frisch gebaut (billig).
fn crc_table() -> [u32; 256] {
    let mut table = [0u32; 256];
    for (i, slot) in table.iter_mut().enumerate() {
        let mut c = i as u32;
        for _ in 0..8 {
            c = if c & 1 == 1 { 0xedb8_8320 ^ (c >> 1) } else { c >> 1 };
        }
        *slot = c;
    }
    table
}

/// CRC32-Fortsetzung (Start `0xffff_ffff`, XOR am Ende).
fn crc32_update(mut crc: u32, data: &[u8]) -> u32 {
    let table = crc_table();
    for &b in data {
        let idx = ((crc ^ u32::from(b)) & 0xff) as usize;
        crc = table[idx] ^ (crc >> 8);
    }
    crc
}

/// Adler32 (zlib-Pruefsumme) ueber `data`.
fn adler32(data: &[u8]) -> u32 {
    let mut a = 1u32;
    let mut b = 0u32;
    for &byte in data {
        a = (a + u32::from(byte)) % 65521;
        b = (b + a) % 65521;
    }
    (b << 16) | a
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_reports_size_and_rgb8() {
        let png = encode_rgb(3, 2, &[1u8; 18]);
        assert_eq!(&png[..8], &MAGIC);
        assert_eq!(&png[12..16], b"IHDR");
        assert_eq!(u32::from_be_bytes(png[16..20].try_into().unwrap()), 3);
        assert_eq!(u32::from_be_bytes(png[20..24].try_into().unwrap()), 2);
        assert_eq!((png[24], png[25]), (8, 2)); // Tiefe, Truecolor.
    }

    #[test]
    fn chunk_crcs_verify() {
        let png = encode_rgb(5, 4, &[7u8; 60]);
        let mut pos = 8;
        let mut seen = 0;
        while pos < png.len() {
            let len = u32::from_be_bytes(png[pos..pos + 4].try_into().unwrap());
            let typ = &png[pos + 4..pos + 8];
            let data = &png[pos + 8..pos + 8 + len as usize];
            let want = u32::from_be_bytes(
                png[pos + 8 + len as usize..pos + 12 + len as usize]
                    .try_into()
                    .unwrap(),
            );
            let crc = crc32_update(0xffff_ffff, typ);
            let crc = crc32_update(crc, data) ^ 0xffff_ffff;
            assert_eq!(crc, want, "crc {typ:?}");
            pos += 12 + len as usize;
            seen += 1;
        }
        assert_eq!(seen, 3); // IHDR, IDAT, IEND.
    }

    #[test]
    fn stored_blocks_roundtrip_scanlines() {
        let rgb: Vec<u8> = (0..60).collect();
        let png = encode_rgb(5, 4, &rgb);
        // IDAT-Daten finden (einziger IDAT-Chunk).
        let at = png.windows(4).position(|w| w == b"IDAT").unwrap();
        let len = u32::from_be_bytes(png[at - 4..at].try_into().unwrap());
        let zlib = &png[at + 4..at + 4 + len as usize];
        assert_eq!(&zlib[..2], &[0x78, 0x01]);
        // Stored-Block entpacken (Test kennt das Format).
        assert_eq!(zlib[2], 1); // BFINAL, stored.
        let n = u16::from_le_bytes([zlib[3], zlib[4]]) as usize;
        assert_eq!(u16::from_le_bytes([zlib[5], zlib[6]]), !n as u16);
        let raw = &zlib[7..7 + n];
        assert_eq!(raw.len(), 4 * (1 + 15)); // Filter + 5 RGB-Pixel.
        let mut back = Vec::new();
        for row in raw.chunks_exact(16) {
            assert_eq!(row[0], 0);
            back.extend_from_slice(&row[1..]);
        }
        assert_eq!(back, rgb);
        // Adler32 pruefen.
        let adler = u32::from_be_bytes(
            zlib[7 + n..11 + n].try_into().unwrap(),
        );
        assert_eq!(adler, adler32(raw));
    }
}