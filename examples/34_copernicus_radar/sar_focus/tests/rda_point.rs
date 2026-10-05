//! RDA-Physiktest: simuliertes Punktziel muss exakt fokussieren.
//!
//! Prüft Peak-Lage, PSF-Breiten gegen Theorie und RCMC-Wirkung (mit/ohne).

mod common;

use common::{FS, J0, NAZ, NR, PRI, V, chirp, fwhm, peak, simulate_raw};
use sar_focus::meta::slant_range_vec;
use sar_focus::rda::{RdaParams, RdaProcessor};

fn processor(
    slant: &[f64],
    veff_range: &[f64],
    fdc_range: &[f64],
    apply_rcmc: bool,
) -> RdaProcessor {
    RdaProcessor::new(&RdaParams {
        chirp: chirp(),
        naz: NAZ,
        nrange: NR,
        pri_s: PRI,
        slant_m: slant,
        veff_range,
        fdc_range,
        apply_rcmc,
    })
}

#[test]
fn punktziel_fokussiert_exakt() {
    let (mut data, slant, _) = simulate_raw();
    // Sanity: slant_range_vec aus 02_meta reproduziert dieselbe Achse.
    let check = slant_range_vec(
        0,
        0.0,
        slant[0] * 2.0 / sar_focus::types::SPEED_OF_LIGHT,
        FS,
        3,
    );
    assert!((check[1] - check[0] - (slant[1] - slant[0])).abs() < 1e-9);
    let veff = vec![V; NR];
    let fdc = vec![0.0; NR];
    processor(&slant, &veff, &fdc, true).focus(&mut data);
    let (pa, pr, pv) = peak(&data, NR);
    // Peak exakt am Ziel-Pixel (512, J0).
    assert_eq!((pa, pr), (512, J0), "Peak bei ({pa}, {pr})");
    // Azimut-FWHM: Theorie v/B_d = 6,03 m ≈ 1,4 px (Spacing 4,27 m).
    // Gemessen 1 px (diskret); Defokus (falsche Stufe) gab 5 px.
    let row: Vec<f32> = (0..NAZ).map(|a| data[a * NR + pr].norm_sqr()).collect();
    let azw = fwhm(&row, pa);
    assert!((1..=2).contains(&azw), "Azimut-FWHM = {azw} px");
    // Range-FWHM: Theorie 0,89·fs/B ≈ 1,0 px.
    let col: Vec<f32> = (0..NR).map(|r| data[pa * NR + r].norm_sqr()).collect();
    let rw = fwhm(&col, pr);
    assert!((1..=2).contains(&rw), "Range-FWHM = {rw} px");
    // Nebenzipfel außerhalb 8-px-Radius.
    // Gemessen −26,7 dB (2D-Sinc-Fernfeld); Schranke mit Abstand.
    let mut side = 0.0f32;
    for a in 0..NAZ {
        for r in 0..NR {
            let d2 = (a as i32 - 512).pow(2) + (r as i32 - J0 as i32).pow(2);
            if d2 > 64 {
                side = side.max(data[a * NR + r].norm_sqr());
            }
        }
    }
    let db = 10.0 * (side / pv).log10();
    assert!(db < -20.0, "Nebenzipfel {db} dB");
}

#[test]
fn rcmc_verbessert_fokus() {
    // Migration am Aperturrand ≈ 0,8 Samples: mit RCMC schärfer als ohne.
    let (data, slant, _) = simulate_raw();
    let veff = vec![V; NR];
    let fdc = vec![0.0; NR];
    let mut mit = data.clone();
    processor(&slant, &veff, &fdc, true).focus(&mut mit);
    let mut ohne = data;
    processor(&slant, &veff, &fdc, false).focus(&mut ohne);
    let (pa1, pr1, pv1) = peak(&mit, NR);
    let (pa0, pr0, pv0) = peak(&ohne, NR);
    assert_eq!((pa1, pr1), (512, J0));
    assert_eq!((pa0, pr0), (512, J0));
    assert!(pv1 >= pv0, "mit {pv1} vs. ohne {pv0}");
}
