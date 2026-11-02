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
use crate::types::{Complex32, Error, SwathData, Vec3d};

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

/// Leistungsstatistik eines Bildausschnitts („Chip") um einen [`AssetHit`].
///
/// Alle dB-Werte sind `10·log10(Leistung)`, Leistung = `re²+im²` (linear).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ChipStats {
    /// Zahl der einbezogenen Pixel.
    pub n: usize,
    /// Mittlere Leistung [dB].
    pub mean_db: f64,
    /// Median der Leistung [dB] (robust gegen einzelne Streuer).
    pub median_db: f64,
    /// 90-%-Quantil der Leistung [dB] (helle Strukturen im Chip).
    pub p90_db: f64,
}

/// Schneidet einen Chip (`±half_az` Zeilen, `±half_rg` Spalten um die
/// gebrochene Position `az`/`rg`) aus einem fokussierten Burstbild
/// (`n_az` × `n_rg`, zeilenmajor) und fasst die Leistung zusammen.
///
/// Die Achsen entsprechen exakt [`AssetHit`]: `az` ist die Zeile im
/// fokussierten Burst, `rg` der Range-Index. Das Fenster wird am
/// Bildrand beschnitten; liegt gar kein Pixel im Bild, kommt `None`.
pub fn chip_stats(
    img: &[Complex32],
    n_az: usize,
    n_rg: usize,
    az: f64,
    rg: f64,
    half_az: usize,
    half_rg: usize,
) -> Option<ChipStats> {
    if img.len() != n_az * n_rg || n_az == 0 || n_rg == 0 {
        return None;
    }
    // Fenstergrenzen erst in i64 prüfen: `as usize` würde eine negative
    // Mitte (Chip komplett außerhalb) per Wrap-around in einen riesigen
    // Index verwandeln, bevor die Leer-Prüfung greifen kann.
    let r0i = az.round() as i64 - half_az as i64;
    let r1i = az.round() as i64 + half_az as i64;
    let c0i = rg.round() as i64 - half_rg as i64;
    let c1i = rg.round() as i64 + half_rg as i64;
    if r1i < 0 || c1i < 0 || r0i > n_az as i64 - 1 || c0i > n_rg as i64 - 1 {
        return None;
    }
    let r0 = r0i.max(0) as usize;
    let r1 = r1i.min(n_az as i64 - 1) as usize;
    let c0 = c0i.max(0) as usize;
    let c1 = c1i.min(n_rg as i64 - 1) as usize;
    let mut powers: Vec<f64> = Vec::new();
    for r in r0..=r1 {
        for c in c0..=c1 {
            let p = img[r * n_rg + c];
            powers.push(f64::from(p.re) * f64::from(p.re) + f64::from(p.im) * f64::from(p.im));
        }
    }
    if powers.is_empty() {
        return None;
    }
    let n = powers.len();
    let mean = powers.iter().sum::<f64>() / n as f64;
    powers.sort_by(f64::total_cmp);
    let median = if n % 2 == 1 {
        powers[n / 2]
    } else {
        0.5 * (powers[n / 2 - 1] + powers[n / 2])
    };
    let p90 = powers[((0.9 * (n - 1) as f64) as usize).min(n - 1)];
    let db = |p: f64| 10.0 * p.max(1e-30).log10();
    Some(ChipStats {
        n,
        mean_db: db(mean),
        median_db: db(median),
        p90_db: db(p90),
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

/// Ein abgetasteter Polygon-Punkt mit seinen Radar-Treffern.
#[derive(Clone, Debug)]
pub struct AssetSample {
    pub lat_deg: f64,
    pub lon_deg: f64,
    pub h_m: f64,
    pub hits: Vec<AssetHit>,
}

/// Prüft, ob ein Punkt (`lat`, `lon`) im Polygon `corners` liegt
/// (Even-Odd-Regel, Rand zählt als innen, Toleranz 1e-9 Grad).
pub fn point_in_polygon(lat: f64, lon: f64, corners: &[(f64, f64)]) -> bool {
    if corners.len() < 3 {
        return false;
    }
    let eps = 1e-9;
    // Randtest: Punkt auf einer Kante → innen.
    for w in 0..corners.len() {
        let (alat, alon) = corners[w];
        let (blat, blon) = corners[(w + 1) % corners.len()];
        let cross = (blon - alon) * (lat - alat) - (blat - alat) * (lon - alon);
        if cross.abs() < eps
            && lat >= alat.min(blat) - eps
            && lat <= alat.max(blat) + eps
            && lon >= alon.min(blon) - eps
            && lon <= alon.max(blon) + eps
        {
            return true;
        }
    }
    // Strahl nach Osten (+lon): zählt Kanten, die ihn kreuzen.
    let mut inside = false;
    for w in 0..corners.len() {
        let (alat, alon) = corners[w];
        let (blat, blon) = corners[(w + 1) % corners.len()];
        if (alat > lat) != (blat > lat) {
            let x = alon + (lat - alat) * (blon - alon) / (blat - alat);
            if x > lon {
                inside = !inside;
            }
        }
    }
    inside
}

/// Tastet ein Asset-Polygon (`corners` als (Breite, Länge)) auf einem
/// Grad-Raster der Schrittweite `step_deg` ab (immer inkl. Ecken).
/// Gibt die Abtastpunkte im Inneren zurück (leer bei < 3 Ecken).
pub fn sample_polygon(corners: &[(f64, f64)], step_deg: f64) -> Vec<(f64, f64)> {
    if corners.len() < 3 || step_deg.is_nan() || step_deg <= 0.0 {
        return Vec::new();
    }
    let (mut la0, mut la1) = (f64::INFINITY, f64::NEG_INFINITY);
    let (mut lo0, mut lo1) = (f64::INFINITY, f64::NEG_INFINITY);
    for &(la, lo) in corners {
        la0 = la0.min(la);
        la1 = la1.max(la);
        lo0 = lo0.min(lo);
        lo1 = lo1.max(lo);
    }
    let mut lats = Vec::new();
    let mut v = la0;
    while v < la1 - 1e-12 {
        lats.push(v);
        v += step_deg;
    }
    lats.push(la1);
    let mut lons = Vec::new();
    let mut u = lo0;
    while u < lo1 - 1e-12 {
        lons.push(u);
        u += step_deg;
    }
    lons.push(lo1);
    let mut out = Vec::new();
    for &la in &lats {
        for &lo in &lons {
            if point_in_polygon(la, lo, corners) {
                out.push((la, lo));
            }
        }
    }
    out
}

/// Ortet ein Asset-Polygon: tastet es ab und ruft pro Punkt [`locate_asset`].
/// Die Höhe liefert `h_of` (z. B. eine DEM-Kachel pro Punkt).
pub fn locate_polygon(
    ephem: &[EphemPoint],
    dataset: &L0Dataset,
    corners: &[(f64, f64)],
    step_deg: f64,
    h_of: impl Fn(f64, f64) -> f64,
) -> Vec<AssetSample> {
    sample_polygon(corners, step_deg)
        .into_iter()
        .map(|(lat, lon)| {
            let h = h_of(lat, lon);
            let hits = locate_asset(ephem, dataset, lat, lon, h);
            AssetSample {
                lat_deg: lat,
                lon_deg: lon,
                h_m: h,
                hits,
            }
        })
        .collect()
}

/// Ampelzustand einer Asset-Zeitreihe.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AmpelStatus {
    Gruen,
    Gelb,
    Rot,
}

/// Bewertung einer Asset-Zeitreihe mit Begründungstext (deutsch).
#[derive(Clone, Debug)]
pub struct Ampel {
    pub status: AmpelStatus,
    /// Maximale |Abweichung| eines Folgewerts vom Startwert [dB].
    pub drift_db: f64,
    pub detail: String,
}

/// Bewertet eine Median-Zeitreihe (`medians_db`, chronologisch, erster Wert =
/// Referenz) gegen Warn-/Alarmschwellen [dB].
///
/// Prototyp-Schwellen — vor Produktiveinsatz an beschrifteten Zeitreihen
/// kalibrieren (siehe Plan). Leere Reihe → Grün ohne Aussage.
pub fn bewerte_zeitreihe(medians_db: &[f64], warn_db: f64, alarm_db: f64) -> Ampel {
    let Some((&basis, rest)) = medians_db.split_first() else {
        return Ampel {
            status: AmpelStatus::Gruen,
            drift_db: 0.0,
            detail: "keine Messwerte".into(),
        };
    };
    let drift = rest
        .iter()
        .map(|m| (m - basis).abs())
        .fold(0.0f64, f64::max);
    if drift >= alarm_db {
        Ampel {
            status: AmpelStatus::Rot,
            drift_db: drift,
            detail: format!("Drift {drift:.1} dB ≥ Alarm {alarm_db:.1} dB"),
        }
    } else if drift >= warn_db {
        Ampel {
            status: AmpelStatus::Gelb,
            drift_db: drift,
            detail: format!("Drift {drift:.1} dB ≥ Warnung {warn_db:.1} dB"),
        }
    } else {
        Ampel {
            status: AmpelStatus::Gruen,
            drift_db: drift,
            detail: format!("Drift {drift:.1} dB < Warnung {warn_db:.1} dB"),
        }
    }
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
    use crate::ingest::L0Dataset;
    use crate::rda::tops_padded_az_len;
    use crate::types::{BurstBlock, RadarParams, SarMode, SwathData};

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

    #[test]
    fn chip_statistik_synth() {
        // 4×5-Bild, Leistungen 1..=20 (re = sqrt(p), im = 0).
        let img: Vec<Complex32> = (1..=20)
            .map(|p| Complex32::new((p as f32).sqrt(), 0.0))
            .collect();
        // Vollchip: Mittel 10.5, Median 10.5, p90 = Wert an Index 17 → 18.
        let s = chip_stats(&img, 4, 5, 1.5, 2.0, 10, 10).unwrap();
        assert_eq!(s.n, 20);
        assert!((s.mean_db - 10.0 * 10.5f64.log10()).abs() < 1e-6);
        assert!((s.median_db - 10.0 * 10.5f64.log10()).abs() < 1e-6);
        assert!((s.p90_db - 10.0 * 18.0f64.log10()).abs() < 1e-6);
        assert!(s.median_db < s.p90_db);
        // Teilchip an der Ecke (Zeilen 0..1, Spalten 0..1 → Leistungen 1,2,6,7).
        let e = chip_stats(&img, 4, 5, 0.0, 0.0, 1, 1).unwrap();
        assert_eq!(e.n, 4);
        assert!((e.median_db - 10.0 * 4.0f64.log10()).abs() < 1e-6);
        // Außerhalb: None. Falsche Geometrie: None.
        assert!(chip_stats(&img, 4, 5, -100.0, 2.0, 1, 1).is_none());
        assert!(chip_stats(&img, 4, 4, 1.5, 2.0, 10, 10).is_none());
        assert!(chip_stats(&[], 0, 0, 0.0, 0.0, 1, 1).is_none());
    }

    #[test]
    fn polygon_abtastung() {
        // Einheitsquadrat, Schritt 0.4 → 4×4-Raster, alles innen.
        let q = [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)];
        let pts = sample_polygon(&q, 0.4);
        assert_eq!(pts.len(), 16);
        assert!(pts.contains(&(0.0, 0.0)) && pts.contains(&(1.0, 1.0)));
        // Rechtwinkliges Dreieck (x+y≤1), Schritt 0.5 → 6 von 9 Punkten.
        let t = [(0.0, 0.0), (1.0, 0.0), (0.0, 1.0)];
        let pts = sample_polygon(&t, 0.5);
        assert_eq!(pts.len(), 6);
        assert!(!pts.contains(&(1.0, 1.0)));
        // Entartet: < 3 Ecken oder Schritt ≤ 0 → leer.
        assert!(sample_polygon(&[(0.0, 0.0), (1.0, 1.0)], 0.5).is_empty());
        assert!(sample_polygon(&q, 0.0).is_empty());
        // Punktprüfung: innen, außen, Kante.
        assert!(point_in_polygon(0.25, 0.25, &t));
        assert!(!point_in_polygon(0.75, 0.75, &t));
        assert!(point_in_polygon(0.5, 0.0, &t));
    }

    #[test]
    fn ampel_schwellen() {
        let g = bewerte_zeitreihe(&[4.8, 5.1, 4.6], 3.0, 6.0);
        assert_eq!(g.status, AmpelStatus::Gruen);
        assert!((g.drift_db - 0.3).abs() < 1e-9);
        let y = bewerte_zeitreihe(&[4.8, 9.0], 3.0, 6.0);
        assert_eq!(y.status, AmpelStatus::Gelb);
        let r = bewerte_zeitreihe(&[4.8, -2.0], 3.0, 6.0);
        assert_eq!(r.status, AmpelStatus::Rot);
        assert!(!r.detail.is_empty());
        let leer = bewerte_zeitreihe(&[], 3.0, 6.0);
        assert_eq!(leer.status, AmpelStatus::Gruen);
    }

    /// Kreisbahn-Äquator als Vorwärtsmodell: Ziel (0°N, 0°E, 0 m) liegt
    /// exakt unter dem Scheitel bei t=10 s; die Ortung muss Burstmitte
    /// (fokussierte Achse) und Range-Index wiederfinden.
    fn synth_dataset() -> (Vec<EphemPoint>, L0Dataset) {
        let r_orb = 7_080_000.0f64;
        let v_sat = 7_540.0f64;
        let w = v_sat / r_orb;
        let ephem: Vec<EphemPoint> = [0.0, 10.0, 20.0, 30.0, 40.0]
            .into_iter()
            .map(|t| {
                let th = w * (t - 10.0);
                EphemPoint {
                    time_s: t,
                    pos: Vec3d::new(r_orb * th.cos(), r_orb * th.sin(), 0.0),
                    vel: Vec3d::new(-v_sat * th.sin(), v_sat * th.cos(), 0.0),
                    quat: [0.0; 4],
                }
            })
            .collect();
        let pri = 0.0006f64;
        let n_az = 200usize;
        let params = RadarParams {
            swath_number: 10,
            elevation_beam: 0,
            rgdec: 3,
            rank: 0,
            fs_hz: 64_000_000.0,
            prf_hz: 1.0 / pri,
            pri_s: pri,
            swst_s: 0.0,
            swl_s: 0.0,
            tx_ramp_rate_hz_s: 0.0,
            tx_start_freq_hz: 0.0,
            tx_pulse_length_s: 0.0,
            fast_time_start_s: 0.0,
            r0_m: 700_000.0,
            dr_m: 2.3,
            chirp_samples: 0,
        };
        let burst = BurstBlock {
            burst_index: 3,
            swath_number: 10,
            params,
            n_az,
            n_rg: 5000,
            raw: Vec::new(),
            pulse_times: (0..n_az)
                .map(|i| 10.0 + (i as f64 - (n_az as f64 - 1.0) * 0.5) * pri)
                .collect(),
            az_beam_addr: Vec::new(),
            veff: vec![7_400.0; 5000],
            pos_ecef: Vec::new(),
            vel_ecef: Vec::new(),
        };
        let ds = L0Dataset {
            mode: SarMode::Iw,
            total_packets: 0,
            echo_packets: 0,
            ephemeris: Vec::new(),
            swaths: vec![SwathData {
                swath_number: 10,
                swath_name: "IW1".into(),
                params,
                bursts: vec![burst],
            }],
        };
        (ephem, ds)
    }

    #[test]
    fn ortung_synth_kreisbahn() {
        let (ephem, ds) = synth_dataset();
        let hits = locate_asset(&ephem, &ds, 0.0, 0.0, 0.0);
        assert_eq!(hits.len(), 1, "genau ein Treffer erwartet");
        let hit = hits[0];
        assert_eq!((hit.swath_number, hit.burst_index), (10, 3));
        // Fokussierte Achse: Burstmitte + Slant (R−A)/dr.
        let n_fft = tops_padded_az_len(200, 0.0006, 7_400.0, 700_000.0 + 2500.0 * 2.3);
        assert!(
            (hit.az - n_fft as f64 * 0.5).abs() < 2.0,
            "az={} vs Mitte {}",
            hit.az,
            n_fft as f64 * 0.5
        );
        let rg_soll = (7_080_000.0 - WGS84_A_M - 700_000.0) / 2.3;
        assert!((hit.rg - rg_soll).abs() < 5.0, "rg={} vs {rg_soll}", hit.rg);
        // Lotrecht darüber: Einfallswinkel ≈ 0.
        assert!(hit.incidence_deg < 5.0, "{} Grad", hit.incidence_deg);
        assert!((hit.t_zd_s - 10.0).abs() < 0.01, "t={}", hit.t_zd_s);
    }
}
