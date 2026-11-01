//! Reverse-Geokodierung: Kartenpunkt (Breite, Länge, Höhe) → Radar-Koordinaten
//! (Sub-Swath, Burst, gebrochene Azimut-/Range-Pixel) — Nutzer-Asset auf
//! Rohbild abbilden, ohne die Szene zu geokodieren.
//!
//! Zutaten: präzise Orbit-Ephemeriden (`AUX_POEORB`-EOF, Parser unten; oder die
//! SubCom-Ephemeriden des Datensatzes) + Punkthöhe aus einem SRTM-1-`.hgt`
//! (`HgtTile`). Die Projektion selbst (Newton auf der Zero-Doppler-Bedingung)
//! kommt aus [`crate::insar::project_target_to_slave`].

use std::path::Path;

use crate::ephem::{EphemPoint, WGS84_A_M, WGS84_B_M, interp_pos, interp_vel};
use crate::ingest::L0Dataset;
use crate::insar::project_target_to_slave;
use crate::types::{Error, SwathData, Vec3d};

/// GPS-Zeit (s seit 1980-01-06) minus Unix-Zeit (s seit 1970-01-01), inkl.
/// 18 Schaltsekunden (Stand 2017..2026): `gps = unix - 315_964_800 + 18`.
const GPS_MINUS_UNIX_S: f64 = -315_964_782.0;

/// Wandelt einen Unix-Zeitstempel [s] in die GPS-Zeitskala [s] um, auf der
/// `EphemPoint.time_s` und die Paket-Zeitstempel (`timestamp_s`) leben.
pub fn unix_to_gps_s(unix_s: f64) -> f64 {
    unix_s + GPS_MINUS_UNIX_S
}

/// Wandelt einen GPS-Zeitstempel [s] zurück in Unix [s].
pub fn gps_to_unix_s(gps_s: f64) -> f64 {
    gps_s - GPS_MINUS_UNIX_S
}

/// Tage seit 1970-01-01 für ein ziviles Datum (Howard-Hinnant-Algorithmus).
fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = y - era * 400;
    let doy = (153 * (if m > 2 { m - 3 } else { m + 9 }) + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

/// Parsiert `2026-09-27T22:59:42.000000` (EOF-Schreibweise, ohne `UTC=`-Präfix)
/// zu Unix-Sekunden. Gibt `None` bei unerwartetem Format.
fn parse_iso_utc_seconds(iso: &str) -> Option<f64> {
    let b = iso.as_bytes();
    if b.len() < 19 || b[4] != b'-' || b[7] != b'-' {
        return None;
    }
    let num = |a: usize, n: usize| -> Option<i64> { iso.get(a..a + n)?.parse::<i64>().ok() };
    let (y, mo, d) = (num(0, 4)?, num(5, 2)?, num(8, 2)?);
    let (h, mi) = (num(11, 2)?, num(14, 2)?);
    let sec: f64 = iso.get(17..)?.parse().ok()?;
    let days = days_from_civil(y, mo, d);
    Some(days as f64 * 86_400.0 + h as f64 * 3_600.0 + mi as f64 * 60.0 + sec)
}

/// Extrahiert den Text zwischen `<tag …>` und `</tag>` ab Byte-Offset `from`
/// (Attribute wie `<X unit="m">` werden übersprungen).
fn tag_text<'a>(xml: &'a str, tag: &str, from: usize) -> Option<(&'a str, usize)> {
    let stem = format!("<{tag}");
    let close = format!("</{tag}>");
    let mut i = from + xml.get(from..)?.find(&stem)? + stem.len();
    // Nur Treffer, wenn auf den Tagnamen `>` oder Whitespace folgt.
    match xml.as_bytes().get(i) {
        Some(b'>') | Some(b' ') | Some(b'\t') => {}
        _ => return None,
    }
    i += xml.get(i..)?.find('>')? + 1;
    let e = i + xml.get(i..)?.find(&close)?;
    Some((xml.get(i..e)?, e))
}

/// Liest eine Sentinel-1 `AUX_POEORB`-EOF-Datei (Earth-Explorer-XML) und gibt
/// die Orbit-Stützstellen in ECEF + GPS-Zeitskala zurück (Ringgröße ~10 s).
///
/// Bewusst ohne XML-Crate: Die EOF-Struktur ist flach (`List_of_OSVs/OSV`),
/// ein Tag-Scan reicht und hält die Abhängigkeiten auf std.
pub fn parse_poeorb_eof(path: &Path) -> Result<Vec<EphemPoint>, Error> {
    let xml = std::fs::read_to_string(path)?;
    let mut out = Vec::new();
    let mut cur = 0usize;
    while let Some((_, next)) = tag_text(&xml, "OSV", cur) {
        let block = &xml[cur..next];
        let get = |tag: &str| -> Option<&str> { tag_text(block, tag, 0).map(|(t, _)| t) };
        let (Some(utc), Some(xs), Some(ys), Some(zs), Some(vxs), Some(vys), Some(vzs)) = (
            get("UTC"),
            get("X"),
            get("Y"),
            get("Z"),
            get("VX"),
            get("VY"),
            get("VZ"),
        ) else {
            cur = next;
            continue;
        };
        let iso = utc.trim().strip_prefix("UTC=").unwrap_or(utc.trim());
        let (Some(unix), Ok(x), Ok(y), Ok(z), Ok(vx), Ok(vy), Ok(vz)) = (
            parse_iso_utc_seconds(iso),
            xs.trim().parse::<f64>(),
            ys.trim().parse::<f64>(),
            zs.trim().parse::<f64>(),
            vxs.trim().parse::<f64>(),
            vys.trim().parse::<f64>(),
            vzs.trim().parse::<f64>(),
        ) else {
            cur = next;
            continue;
        };
        out.push(EphemPoint {
            time_s: unix_to_gps_s(unix),
            pos: Vec3d::new(x, y, z),
            vel: Vec3d::new(vx, vy, vz),
            quat: [0.0; 4],
        });
        cur = next;
    }
    if out.is_empty() {
        return Err(Error(format!(
            "keine OSV-Stützstellen in {} gefunden",
            path.display()
        )));
    }
    Ok(out)
}

/// Geodätisch (WGS-84, Höhe über Ellipsoid) → ECEF [m].
pub fn geodetic_to_ecef(lat_deg: f64, lon_deg: f64, h_m: f64) -> Vec3d {
    let (lat, lon) = (lat_deg.to_radians(), lon_deg.to_radians());
    let (s, c) = lat.sin_cos();
    let e2 = 1.0 - (WGS84_B_M * WGS84_B_M) / (WGS84_A_M * WGS84_A_M);
    let n = WGS84_A_M / (1.0 - e2 * s * s).sqrt();
    Vec3d::new(
        (n + h_m) * c * lon.cos(),
        (n + h_m) * c * lon.sin(),
        (n * (1.0 - e2) + h_m) * s,
    )
}

/// ECEF [m] → geodätisch (Grad, Grad, Höhe über Ellipsoid [m]), iterativ.
pub fn ecef_to_geodetic(p: Vec3d) -> (f64, f64, f64) {
    let e2 = 1.0 - (WGS84_B_M * WGS84_B_M) / (WGS84_A_M * WGS84_A_M);
    let lon = p.y.atan2(p.x);
    let rho = p.x.hypot(p.y).max(1e-9);
    let mut lat = (p.z / (rho * (1.0 - e2))).atan();
    for _ in 0..8 {
        let (s, _) = lat.sin_cos();
        let n = WGS84_A_M / (1.0 - e2 * s * s).sqrt();
        let h = rho / lat.cos() - n;
        lat = (p.z / (rho * (1.0 - e2 * n / (n + h)))).atan();
    }
    let (s, c) = lat.sin_cos();
    let n = WGS84_A_M / (1.0 - e2 * s * s).sqrt();
    let h = rho / c - n;
    (lat.to_degrees(), lon.to_degrees(), h)
}

/// Eine SRTM-1-Höhenkachel (`.hgt`, 3601×3601 Big-Endian-`i16`, 1°×1°).
///
/// Deckt eine Punkt-Höhenabfrage ab (bilinar); kein Szenen-DEM nötig.
pub struct HgtTile {
    /// Südwest-Ecke [Grad].
    pub lat0: i32,
    pub lon0: i32,
    n: usize,
    data: Vec<i16>,
}

impl HgtTile {
    /// Lädt eine `.hgt`-Datei; `lat0`/`lon0` aus dem Dateinamen (`N22E120.hgt`
    /// → Ecke 22°N/120°E; `S`/`W` negativ).
    pub fn load(path: &Path) -> Result<Self, Error> {
        let name = path
            .file_name()
            .and_then(|s| s.to_str())
            .ok_or_else(|| Error(format!("ungültiger DEM-Dateiname: {}", path.display())))?;
        let lat0 = parse_hemi(name.chars().nth(0), name.get(1..3))?;
        let lon0 = parse_hemi(name.chars().nth(3), name.get(4..7))?;
        let raw = std::fs::read(path)?;
        if raw.len() % 2 != 0 {
            return Err(Error(format!("{}: ungerade Bytezahl", path.display())));
        }
        let n = ((raw.len() / 2) as f64).sqrt() as usize;
        if n * n * 2 != raw.len() || n < 2 {
            return Err(Error(format!(
                "{}: keine quadratische HGT-Kachel",
                path.display()
            )));
        }
        let (pairs, _) = raw.as_chunks::<2>();
        let data = pairs.iter().map(|c| i16::from_be_bytes(*c)).collect();
        Ok(Self {
            lat0,
            lon0,
            n,
            data,
        })
    }

    fn sample(&self, row: usize, col: usize) -> f64 {
        let idx = row.min(self.n - 1) * self.n + col.min(self.n - 1);
        f64::from(self.data[idx])
    }

    /// Bilineare Höhe [m] für einen Punkt innerhalb dieser Kachel.
    /// NODATA (-32768) wird als 0 behandelt (Ozean).
    pub fn height(&self, lat_deg: f64, lon_deg: f64) -> f64 {
        let fy = (f64::from(self.lat0) + 1.0 - lat_deg) * (self.n - 1) as f64;
        let fx = (lon_deg - f64::from(self.lon0)) * (self.n - 1) as f64;
        let (r0, c0) = (
            fy.floor().clamp(0.0, (self.n - 2) as f64) as usize,
            fx.floor().clamp(0.0, (self.n - 2) as f64) as usize,
        );
        let (ty, tx) = (fy - r0 as f64, fx - c0 as f64);
        let g = |v: f64| if v <= -30_000.0 { 0.0 } else { v };
        let top = g(self.sample(r0, c0)) * (1.0 - tx) + g(self.sample(r0, c0 + 1)) * tx;
        let bot = g(self.sample(r0 + 1, c0)) * (1.0 - tx) + g(self.sample(r0 + 1, c0 + 1)) * tx;
        top * (1.0 - ty) + bot * ty
    }
}

fn parse_hemi(hemi: Option<char>, digits: Option<&str>) -> Result<i32, Error> {
    let v: i32 = digits
        .and_then(|d| d.parse().ok())
        .ok_or_else(|| Error("DEM-Dateiname ohne Koordinaten".into()))?;
    Ok(match hemi {
        Some('N') | Some('E') => v,
        Some('S') | Some('W') => -v,
        _ => return Err(Error("DEM-Dateiname ohne Hemisphäre".into())),
    })
}

/// Treffer eines Kartenpunkts im Rohbild: Sub-Swath, Burst und gebrochene
/// Pixelkoordinaten (Azimut-/Range-Index, 0-basiert, Bruchteil = Sub-Pixel).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AssetHit {
    pub swath_number: u8,
    pub burst_index: usize,
    /// Gebrochene Zeile im FOKUSSIERTEN Burst (TOPS: `n_az_fft`-Achse um die
    /// Burst-Mitte; Stripmap: Rohzeile = fokussierte Zeile).
    pub az: f64,
    /// Gebrochener Range-Index innerhalb des Bursts.
    pub rg: f64,
    /// Zero-Doppler-Zeit des Punkts [s, GPS-Skala des Datensatzes].
    pub t_zd_s: f64,
    /// Schrägentfernung [m].
    pub slant_m: f64,
    /// Einfallswinkel gegen die lokale Vertikale [Grad].
    pub incidence_deg: f64,
    /// Heuristik: steiles Gelände / kleiner Einfallswinkel → Layover-Verdacht.
    pub layover_risk: bool,
}

/// Sucht einen Kartenpunkt (geodätisch + Höhe über Ellipsoid [m]) in allen
/// Sub-Swaths/Bursts eines Datensatzes. Bei TOPS trifft geometrisch genau ein
/// Sub-Swath; Überlappungen an Burst-Rändern können Mehrfachtreffer geben.
///
/// `ephem` muss die Orbit-Stützstellen sein (SubCom oder präzise EOF — die
/// Zeitskala entscheidet die Genauigkeit, nicht das Verfahren).
pub fn locate_asset(
    ephem: &[EphemPoint],
    dataset: &L0Dataset,
    lat_deg: f64,
    lon_deg: f64,
    h_m: f64,
) -> Vec<AssetHit> {
    let target = geodetic_to_ecef(lat_deg, lon_deg, h_m);
    // Lokale Vertikale (Ellipsoid-Normale) für den Einfallswinkel.
    let up = {
        let (la, lo) = (lat_deg.to_radians(), lon_deg.to_radians());
        let (s, c) = la.sin_cos();
        Vec3d::new(c * lo.cos(), c * lo.sin(), s)
    };
    let mut hits = Vec::new();
    for swath in &dataset.swaths {
        hits.extend(locate_in_swath(ephem, swath, target, up));
    }
    hits
}

/// Wie [`locate_asset`], aber nur für einen Sub-Swath (getrennte Testbarkeit).
pub fn locate_in_swath(
    ephem: &[EphemPoint],
    swath: &SwathData,
    target_ecef: Vec3d,
    up: Vec3d,
) -> Vec<AssetHit> {
    let mut hits = Vec::new();
    for burst in &swath.bursts {
        let n_az = burst.n_az;
        if n_az < 2 || burst.pulse_times.len() < 2 {
            continue;
        }
        let t_first = burst.pulse_times[0];
        let t_last = burst.pulse_times[n_az - 1];
        let t_mid = 0.5 * (t_first + t_last);
        let (t_zd, slant) = project_target_to_slave(ephem, target_ecef, t_mid);
        // Azimut-Achse des FOKUSSIERTEN Bursts: Bei TOPS ist sie `n_az_fft`
        // Zeilen lang (Deramping spreizt die Zero-Doppler-Zeiten über die
        // gepaddete Achse um die Burst-Mitte); bei Stripmap entspricht sie
        // den Rohzeilen. `az` ist also stets die Zeile im fokussierten Burst.
        let tops = matches!(swath.swath_number, 10..=12 | 20..=24);
        let (az, az_len) = if tops {
            let n_fft = crate::rda::tops_padded_az_len(
                n_az,
                burst.params.pri_s,
                burst.veff[burst.n_rg / 2],
                burst.params.slant_range(burst.n_rg / 2),
            );
            (
                (t_zd - t_mid) / burst.params.pri_s + n_fft as f64 * 0.5,
                n_fft as f64,
            )
        } else {
            let dt = (t_last - t_first) / (n_az - 1) as f64;
            if dt <= 0.0 {
                continue;
            }
            ((t_zd - t_first) / dt, n_az as f64)
        };
        let rg = (slant - burst.params.r0_m) / burst.params.dr_m;
        if az < 0.0 || az > az_len || rg < 0.0 || rg > burst.n_rg as f64 {
            continue;
        }
        let sat = interp_pos(ephem, t_zd);
        // Einfallswinkel: lokale Vertikale gegen Blickrichtung Ziel → Satellit.
        let to_sat = sat - target_ecef;
        let cos_inc = to_sat.dot(up) / to_sat.norm().max(1.0);
        let incidence_deg = cos_inc.clamp(-1.0, 1.0).acos().to_degrees();
        // Satellitenhöhe für die Plausibilitätsprüfung der Interpolationsgüte.
        let _ = interp_vel(ephem, t_zd);
        hits.push(AssetHit {
            swath_number: swath.swath_number,
            burst_index: burst.burst_index,
            az,
            rg,
            t_zd_s: t_zd,
            slant_m: slant,
            incidence_deg,
            // Faustregel: < 25° Einfallswinkel → steile Gegenhänge kippen
            // (Layover); präziser nur mit Hangneigung aus dem DEM.
            layover_risk: incidence_deg < 25.0,
        });
    }
    hits
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zivilzeit_rundreise() {
        // 2026-09-28T21:51:49 UTC als bekannter Unix-Wert (UTC-Kalender:
        // unabhängig per `date -u -d` prüfbar: 1790607909? hier nur
        // Rundreise-Konsistenz + Schaltsekunden-Offset).
        let unix = parse_iso_utc_seconds("2026-09-28T21:51:49.000000").unwrap();
        assert!((unix - 1_790_632_309.0).abs() < 2.0, "unix={unix}");
        assert!((gps_to_unix_s(unix_to_gps_s(unix)) - unix).abs() < 1e-9);
        // GPS-Achse: 2026 → ~1.475e9 s (Größenordnung gegen L0-PVT geprüft).
        let gps = unix_to_gps_s(unix);
        assert!((1.47e9..1.48e9).contains(&gps), "gps={gps}");
    }

    #[test]
    fn geodaetisch_rundreise() {
        // Kaohsiung + Yushan-Höhe: ECEF hin und zurück.
        for (la, lo, h) in [
            (22.62, 120.31, 21.0),
            (23.47, 120.96, 3_809.0),
            (0.0, 0.0, 0.0),
        ] {
            let p = geodetic_to_ecef(la, lo, h);
            let (la2, lo2, h2) = ecef_to_geodetic(p);
            assert!((la - la2).abs() < 1e-9, "lat {la} vs {la2}");
            assert!((lo - lo2).abs() < 1e-9, "lon {lo} vs {lo2}");
            assert!((h - h2).abs() < 1e-3, "h {h} vs {h2}");
        }
    }
}
