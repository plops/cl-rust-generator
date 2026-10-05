//! End-to-end tests: build synthetic `.dat` files, run the binary, and check
//! the CSV reports and complex-sample (`.cf`) outputs.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::TempDir;

/// MSB-first bit writer into a byte buffer.
struct BitWriter {
    buf: Vec<u8>,
    bitpos: usize,
}

impl BitWriter {
    fn new(nbytes: usize) -> BitWriter {
        BitWriter {
            buf: vec![0u8; nbytes],
            bitpos: 0,
        }
    }

    fn put(&mut self, value: u32, n: u32) {
        for i in (0..n).rev() {
            if (value >> i) & 1 == 1 {
                self.buf[self.bitpos / 8] |= 1 << (7 - (self.bitpos % 8));
            }
            self.bitpos += 1;
        }
    }

    /// Align like `consume_padding_bits` (payload starts at an even offset).
    fn pad(&mut self) {
        let byte = self.bitpos / 8;
        let used = self.bitpos % 8;
        if byte.is_multiple_of(2) {
            if used != 0 {
                self.bitpos = (byte + 2) * 8;
            }
        } else {
            self.bitpos = (byte + 1) * 8;
        }
    }

    fn bytes(self) -> Vec<u8> {
        self.buf
    }
}

fn header(cal: bool, baq_mode: u8, quads: u16, ele: u8, swst: u32, sub_idx: u8) -> [u8; 68] {
    let mut p = [0u8; 68];
    p[12..16].copy_from_slice(&[0x35, 0x2E, 0xF8, 0x53]);
    p[65] = (quads >> 8) as u8;
    p[66] = quads as u8;
    p[37] = baq_mode;
    p[38] = 31;
    p[59] = if cal { 0x80 | (5 << 4) } else { 2 << 4 };
    p[60] = (ele << 4) | 0x02;
    p[61] = 0x34;
    p[63] = 0x00;
    p[64] = 3;
    p[53] = (swst >> 16) as u8;
    p[54] = (swst >> 8) as u8;
    p[55] = swst as u8;
    p[26] = sub_idx;
    p[27] = 0xAB;
    p[28] = 0xCD;
    p
}

/// Type-A/B payload: 10-bit sign-magnitude codes per channel.
fn type_ab_payload(channels: &[[u32; 2]; 4]) -> Vec<u8> {
    let mut w = BitWriter::new(24);
    for ch in channels {
        for &code in ch {
            w.put(code, 10);
        }
        w.pad();
    }
    w.bytes()
}

/// FDBAQ payload: BRC 0, magnitude codes, threshold index 10 (normal law).
fn fdbaq_payload(mcodes: &[u32]) -> Vec<u8> {
    fn huff0(m: u32) -> (u32, u32) {
        match m {
            0 => (0b0, 1),
            1 => (0b10, 2),
            2 => (0b110, 3),
            _ => (0b111, 3),
        }
    }
    let mut w = BitWriter::new(16);
    w.put(0, 3); // IE bit-rate code
    for &m in mcodes {
        let (code, n) = huff0(m);
        w.put(0, 1);
        w.put(code, n);
    }
    w.pad();
    for &m in mcodes {
        let (code, n) = huff0(m);
        w.put(0, 1);
        w.put(code, n);
    }
    w.pad();
    w.put(10, 8); // QE threshold index
    for &m in mcodes {
        let (code, n) = huff0(m);
        w.put(0, 1);
        w.put(code, n);
    }
    w.pad();
    for &m in mcodes {
        let (code, n) = huff0(m);
        w.put(0, 1);
        w.put(code, n);
    }
    w.bytes()
}

fn packet(mut header: [u8; 68], payload: &[u8]) -> Vec<u8> {
    let data_length = (68 + payload.len() - 7) as u16;
    header[4] = (data_length >> 8) as u8;
    header[5] = data_length as u8;
    let mut v = header.to_vec();
    v.extend_from_slice(payload);
    v
}

fn test_file(dir: &Path) -> PathBuf {
    let cal = packet(
        header(true, 0, 2, 4, 3548, 0),
        &type_ab_payload(&[[5, 0x200 | 7], [1, 2], [0, 511], [0x200 | 3, 4]]),
    );
    let sig1 = packet(header(false, 12, 2, 2, 3548, 1), &fdbaq_payload(&[1, 2]));
    let sig2 = packet(header(false, 12, 2, 2, 3548, 2), &fdbaq_payload(&[1, 2]));
    let path = dir.join("input.dat");
    let mut file = cal;
    file.extend(sig1);
    file.extend(sig2);
    fs::write(&path, file).unwrap();
    path
}

fn binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_copernicus-radar"))
}

fn read_cf_f32(path: &Path, n: usize) -> Vec<f32> {
    let bytes = fs::read(path).unwrap();
    assert!(bytes.len() >= n * 4, "short .cf file: {}", bytes.len());
    (0..n)
        .map(|i| f32::from_le_bytes(bytes[4 * i..4 * i + 4].try_into().unwrap()))
        .collect()
}

#[test]
fn decodes_synthetic_file() {
    let dir = TempDir::new().unwrap();
    let input = test_file(dir.path());
    let status = Command::new(binary())
        .arg(&input)
        .arg("--csv-dir")
        .arg(dir.path())
        .arg("--cf-dir")
        .arg(dir.path())
        .status()
        .unwrap();
    assert!(status.success(), "exit status: {status}");

    // Per-packet CSV reports: header + one row per decoded packet.
    let range = fs::read_to_string(dir.path().join("o_range.csv")).unwrap();
    let lines: Vec<&str> = range.lines().collect();
    assert_eq!(lines.len(), 3);
    assert!(lines[0].starts_with("azi,baq_n,baqmod,"));
    assert_eq!(lines[0].split(',').count(), lines[1].split(',').count());
    let cal = fs::read_to_string(dir.path().join("o_cal_range.csv")).unwrap();
    assert_eq!(cal.lines().count(), 2);
    assert!(cal.lines().next().unwrap().contains("cal_type"));
    // Only three ancillary words arrived; no complete block, no CSV.
    assert!(!dir.path().join("o_anxillary.csv").exists());

    // Range image: n0 = 3588 + 2*2 = 3592, 2 echoes.
    let sar_path = dir.path().join("o_range3592_echoes2.cf");
    assert_eq!(fs::metadata(&sar_path).unwrap().len(), 3592 * 2 * 8);
    // Normal law: NRL0[m] * SF[10], SF[10] = 6.27.
    let e1 = 1.09150 * 6.270;
    let e2 = 1.82080 * 6.270;
    // Interleaved per quad: (ie0,qe0), (io0,qo0), (ie1,qe1), (io1,qo1).
    let got = read_cf_f32(&sar_path, 8);
    for (i, v) in got.iter().enumerate() {
        let expected = if i < 4 { e1 } else { e2 };
        assert!((v - expected).abs() < 1e-3, "sample {i} = {v}");
    }

    // Calibration image: 6000 x 1.
    let cal_path = dir.path().join("o_cal_range6000_echoes1.cf");
    assert_eq!(fs::metadata(&cal_path).unwrap().len(), 6000 * 8);
    // Interleaved even/odd: (ie0,qe0), (io0,qo0) = (5,0), (1,-3).
    let got = read_cf_f32(&cal_path, 4);
    assert_eq!(got, [5.0, 0.0, 1.0, -3.0]);
}

#[test]
fn echo_cap_limits_image_but_not_reports() {
    let dir = TempDir::new().unwrap();
    let input = test_file(dir.path());
    let status = Command::new(binary())
        .arg(&input)
        .arg("--csv-dir")
        .arg(dir.path())
        .arg("--cf-dir")
        .arg(dir.path())
        .arg("--max-echoes")
        .arg("1")
        .status()
        .unwrap();
    assert!(status.success());
    // Image holds one echo...
    assert_eq!(
        fs::metadata(dir.path().join("o_range3592_echoes1.cf"))
            .unwrap()
            .len(),
        3592 * 8
    );
    // ...but both packets were decoded and reported.
    let range = fs::read_to_string(dir.path().join("o_range.csv")).unwrap();
    assert_eq!(range.lines().count(), 3);
}

#[test]
fn header_dump_and_export_work() {
    let dir = TempDir::new().unwrap();
    let input = test_file(dir.path());
    let out = Command::new(binary())
        .arg(&input)
        .arg("--csv-dir")
        .arg(dir.path())
        .arg("--cf-dir")
        .arg(dir.path())
        .arg("--dump-headers")
        .output()
        .unwrap();
    assert!(out.status.success());
    let text = String::from_utf8(out.stdout).unwrap();
    assert!(text.contains("sync-marker"));
    assert!(text.contains("352ef853"));

    let status = Command::new(binary())
        .arg(&input)
        .arg("--csv-dir")
        .arg(dir.path())
        .arg("--cf-dir")
        .arg(dir.path())
        .arg("--export-headers")
        .status()
        .unwrap();
    assert!(status.success());
    let exported = fs::read_to_string(dir.path().join("o_packet_header.csv")).unwrap();
    assert!(exported.lines().next().unwrap().contains("sync_marker"));
    assert_eq!(exported.lines().count(), 4); // header + 3 packets
}

#[test]
fn bad_sync_marker_fails() {
    let dir = TempDir::new().unwrap();
    let mut h = header(false, 12, 2, 2, 3548, 0);
    h[12] = 0x00; // corrupt the sync marker
    let path = dir.path().join("bad.dat");
    fs::write(&path, packet(h, &fdbaq_payload(&[0, 0]))).unwrap();
    let status = Command::new(binary())
        .arg(&path)
        .arg("--csv-dir")
        .arg(dir.path())
        .arg("--cf-dir")
        .arg(dir.path())
        .status()
        .unwrap();
    assert!(!status.success());
}

#[test]
fn missing_file_fails() {
    let dir = TempDir::new().unwrap();
    let status = Command::new(binary())
        .arg(dir.path().join("does-not-exist.dat"))
        .status()
        .unwrap();
    assert!(!status.success());
}
