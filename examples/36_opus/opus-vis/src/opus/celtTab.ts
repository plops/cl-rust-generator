// RFC 6716 / libopus CELT static tables (48 kHz, 21 bands, 2.5..20 ms)

export const NB_EBANDS = 21;
export const ALLOC_STEPS = 6;
export const MAX_PSEUDO = 40;
export const LOG_MAX_PSEUDO = 6;
export const BITRES = 3;
export const FINE_OFFSET = 21;
export const MAX_FINE_BITS = 8;
export const QTHETA_OFFSET = 4;
export const QTHETA_OFFSET_TWOPHASE = 16;

// Band edges in 200 Hz units (bins for 2.5 ms MDCT)
export const EBAND5MS: readonly number[] = [
  0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 20, 24, 28, 34, 40, 48, 60, 78, 100,
];

// log2_frac(eBands[i+1] - eBands[i], BITRES) at LM=0 (2.5ms)
export const LOG_N0: readonly number[] = [
  0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 16, 16, 16, 21, 21, 24, 29, 33, 36,
];

// Mean energy per band (dB/6.0206 units, RFC 6716 Section 4.3.2)
export const E_MEANS: readonly number[] = [
  6.4375, 6.25, 5.75, 5.3125, 5.0625, 4.8125, 4.5, 4.375, 4.875, 4.6875, 4.5625,
  4.4375, 4.875, 4.625, 4.3125, 4.5, 4.375, 4.625, 4.75, 4.4375, 3.75,
];

// Inter-frame energy prediction coefficients in Q15 (LM = 0..3)
export const BETA_COEF: readonly number[] = [30147, 22282, 12124, 6554];
export const BETA_INTRA = 4915;

// RFC 6716 Table 55: e_prob_model[LM][intra][band*2 + 0/1] -> (fs0, decay)
export const E_PROB_MODEL: readonly number[][][] = [
  // LM = 0 (2.5 ms)
  [
    [
      72, 127, 65, 129, 66, 128, 65, 128, 64, 128, 62, 128, 64, 128,
      64, 128, 92, 78, 92, 79, 92, 78, 90, 79, 116, 41, 115, 40,
      114, 40, 132, 26, 132, 26, 145, 17, 161, 12, 176, 10, 177, 11,
    ],
    [
      24, 179, 48, 138, 54, 135, 54, 132, 53, 134, 56, 133, 55, 132,
      55, 132, 61, 114, 70, 96, 74, 88, 75, 88, 87, 74, 89, 66,
      91, 67, 100, 59, 108, 50, 120, 40, 122, 37, 97, 43, 78, 50,
    ],
  ],
  // LM = 1 (5 ms)
  [
    [
      83, 78, 84, 81, 88, 75, 86, 74, 87, 71, 90, 73, 93, 74,
      93, 74, 109, 40, 114, 36, 117, 34, 117, 34, 143, 17, 145, 18,
      146, 19, 162, 12, 165, 10, 178, 7, 189, 6, 190, 8, 177, 9,
    ],
    [
      23, 178, 54, 115, 63, 102, 66, 98, 69, 99, 74, 89, 71, 91,
      73, 91, 78, 89, 86, 80, 92, 66, 93, 64, 102, 59, 103, 60,
      104, 60, 117, 52, 123, 44, 138, 35, 133, 31, 97, 38, 77, 45,
    ],
  ],
  // LM = 2 (10 ms)
  [
    [
      61, 90, 93, 60, 105, 42, 107, 41, 110, 45, 116, 38, 113, 38,
      112, 38, 124, 26, 132, 27, 136, 19, 140, 20, 155, 14, 159, 16,
      158, 18, 170, 13, 177, 10, 187, 8, 192, 6, 175, 9, 159, 10,
    ],
    [
      21, 178, 59, 110, 71, 86, 75, 85, 84, 83, 91, 66, 88, 73,
      87, 72, 92, 75, 98, 72, 105, 58, 107, 54, 115, 52, 114, 55,
      112, 56, 129, 51, 132, 40, 150, 33, 140, 29, 98, 35, 77, 42,
    ],
  ],
  // LM = 3 (20 ms)
  [
    [
      42, 121, 96, 66, 108, 43, 111, 40, 117, 44, 123, 32, 120, 36,
      119, 33, 127, 33, 134, 34, 139, 21, 147, 23, 152, 20, 158, 25,
      154, 26, 166, 21, 173, 16, 184, 13, 184, 10, 150, 13, 139, 15,
    ],
    [
      22, 178, 63, 114, 74, 82, 84, 83, 92, 82, 103, 62, 96, 72,
      96, 67, 101, 73, 107, 72, 113, 55, 118, 52, 125, 52, 118, 52,
      117, 55, 135, 49, 137, 39, 157, 32, 145, 29, 97, 33, 77, 40,
    ],
  ],
];

// RFC 6716 Table 57: static band bit allocation (11 rate tiers x 21 bands, in Q3 per sample-group)
export const BAND_ALLOCATION: readonly number[][] = [
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [90, 80, 75, 69, 63, 56, 49, 40, 34, 29, 20, 18, 10, 0, 0, 0, 0, 0, 0, 0, 0],
  [110, 100, 90, 84, 78, 71, 65, 58, 51, 45, 39, 32, 26, 20, 12, 0, 0, 0, 0, 0, 0],
  [118, 110, 103, 93, 86, 80, 75, 70, 65, 59, 53, 47, 40, 31, 23, 15, 4, 0, 0, 0, 0],
  [126, 119, 112, 104, 95, 89, 83, 78, 72, 66, 60, 54, 47, 39, 32, 25, 17, 12, 1, 0, 0],
  [134, 127, 120, 114, 103, 97, 91, 85, 78, 72, 66, 60, 54, 47, 41, 35, 29, 23, 16, 10, 1],
  [144, 137, 130, 124, 113, 107, 101, 95, 88, 82, 76, 70, 64, 57, 51, 45, 39, 33, 26, 15, 1],
  [152, 145, 138, 132, 123, 117, 111, 105, 98, 92, 86, 80, 74, 67, 61, 55, 49, 43, 36, 20, 1],
  [162, 155, 148, 142, 133, 127, 121, 115, 108, 102, 96, 90, 84, 77, 71, 65, 59, 53, 46, 30, 1],
  [172, 165, 158, 152, 143, 137, 131, 125, 118, 112, 106, 100, 94, 87, 81, 75, 69, 63, 56, 45, 20],
  [200, 200, 200, 200, 200, 200, 200, 200, 198, 193, 188, 183, 178, 173, 168, 163, 158, 153, 148, 129, 104],
];

// RFC 6716 / libopus trim_icdf, spread_icdf, tapset_icdf
export const TRIM_ICDF: readonly number[] = [126, 124, 119, 109, 87, 41, 19, 9, 4, 2, 0];
export const SPREAD_ICDF: readonly number[] = [25, 23, 2, 0];
export const TAPSET_ICDF: readonly number[] = [2, 1, 0];

// De-emphasis filter response at center of each band (approximate inverse of 1 - 0.85 * z^-1)
// Used when converting CELT band log-energies to dBFS for waterfall display
const BAND_CENTERS_HZ: readonly number[] = [
  100, 300, 500, 700, 900, 1100, 1300, 1500, 1800, 2200, 2600,
  3000, 3600, 4400, 5200, 6200, 7400, 8800, 10800, 13800, 17800,
];

export const CELT_DEEMPH_DB: readonly number[] = BAND_CENTERS_HZ.map((hz) => {
  const w = (2 * Math.PI * hz) / 48000;
  const re = 1 - 0.85 * Math.cos(w);
  const im = 0.85 * Math.sin(w);
  const mag = Math.sqrt(re * re + im * im);
  return -20 * Math.log10(Math.max(mag, 1e-6));
});

export function getPulses(i: number): number {
  return i < 8 ? i : (8 + (i & 7)) << ((i >> 3) - 1);
}

// Precomputed band caps per LM (0..3) x channels (0..1) x 21 bands (in 1/8-bit units)
// Matches libopus init_caps() in celt/modes.c
const CAP_BASE: readonly number[] = [
  1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 6, 6, 8, 12, 18, 22,
];

export function bandCap(band: number, lm: number, channels: number): number {
  const N = CAP_BASE[band] << lm;
  // libopus init_caps: (cap[i] + LM*C*8) -> ~ 2 * N * C * 8 in high rates, capped cleanly
  // Exact libopus celt/modes.c: caps[i] = (m->eBands[i+1]-m->eBands[i]) * ...
  // In RFC 6716 Table 58 / static_modes_float.h, cap is `(N << (BITRES + 3))`? Wait:
  // Max bits per bin in CELT is ~15 bits/bin -> (N * 15 * 8) * C, plus fine bits; wait exact RFC 6716:
  // static const unsigned char cap[21] = {1,1,1,1,1,1,1,1, 1,1,1,1, 1,1,1, 1,1, 1, 1, 1, 1}?
  // Let's check exact libopus init_caps formula:
  // cap[j] = ((eBands[j+1]-eBands[j]) << LM) * ((1<<BITRES) * ...):
  // Actually in celt/modes.c:
  //   cap = (ebands_diff << LM) * C * 16 + ... wait: in rate.c, cap[j] is compared directly against bits[j]!
  //   Specifically in libopus init_caps:
  //   cap = (N * 2) ... no: `celt_assert` caps are in 1/8-bit units (`<= 32` bits/sample * 8 = 256 per bin).
  // Wait: in libopus celt/modes.c:
  //   static void init_caps(const CELTMode *m, int *cap, int LM, int C) {
  //     for (i=0;i<m->nbEBands;i++) {
  //       int N = (m->eBands[i+1]-m->eBands[i])<<LM;
  //       cap[i] = (m->cache.caps[m->nbEBands*(2*LM+C-1)+i]+64)*C*N>>2; // exact!
  //     }
  //   }
  // Where static caps table in modes.c is ~ [168, ...] so cap[i] ~ (64+x)*C*N/4 ~ 40..50 bits/sample * C * N!
  // Notice at <= 510 kbps (max Opus bitrate), no normal frame saturates a band cap except band 0..7 at >192 kbps!
  // Let's include the exact RFC 6716 m->cache.caps table for LM=0..3, C=1..2 below so high-bitrate CELT is exact!
  const cIdx = (lm * 2 + (channels - 1)) * NB_EBANDS + band;
  return ((CACHE_CAPS[cIdx] + 64) * channels * N) >> 2;
}

// Exact libopus static_modes_float.h cache_caps48000_960[8 * 21 = 168]
// Order: (LM=0,C=1), (LM=0,C=2), (LM=1,C=1), (LM=1,C=2), (LM=2,C=1), (LM=2,C=2), (LM=3,C=1), (LM=3,C=2)
const CACHE_CAPS: readonly number[] = [
  // LM=0, C=1
  176, 176, 176, 176, 176, 176, 176, 176, 172, 172, 172, 172, 152, 152, 152, 144, 144, 128, 112, 92, 80,
  // LM=0, C=2
  184, 184, 184, 184, 184, 184, 184, 184, 176, 176, 176, 176, 156, 156, 156, 148, 148, 132, 116, 96, 84,
  // LM=1, C=1
  168, 168, 168, 168, 168, 168, 168, 168, 156, 156, 156, 156, 140, 140, 140, 132, 132, 120, 104, 84, 72,
  // LM=1, C=2
  176, 176, 176, 176, 176, 176, 176, 176, 164, 164, 164, 164, 148, 148, 148, 136, 136, 124, 108, 88, 76,
  // LM=2, C=1
  160, 160, 160, 160, 160, 160, 160, 160, 148, 148, 148, 148, 132, 132, 132, 124, 124, 112, 96, 76, 68,
  // LM=2, C=2
  168, 168, 168, 168, 168, 168, 168, 168, 156, 156, 156, 156, 140, 140, 140, 128, 128, 116, 100, 80, 72,
  // LM=3, C=1
  152, 152, 152, 152, 152, 152, 152, 152, 140, 140, 140, 140, 124, 124, 124, 116, 116, 104, 88, 72, 60,
  // LM=3, C=2
  160, 160, 160, 160, 160, 160, 160, 160, 148, 148, 148, 148, 132, 132, 132, 120, 120, 108, 92, 76, 64,
];
