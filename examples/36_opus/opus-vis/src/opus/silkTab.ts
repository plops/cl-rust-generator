// RFC 6716 Section 4.2.7 SILK Probability Tables & NLSF->LPC Envelope Helpers

export const SILK_TYPE_OFFSET_NO_VAD_ICDF: readonly number[] = [230, 0];
export const SILK_TYPE_OFFSET_VAD_ICDF: readonly number[] = [232, 158, 10, 0];

export const SILK_UNIFORM3_ICDF: readonly number[] = [171, 85, 0];
export const SILK_UNIFORM4_ICDF: readonly number[] = [192, 128, 64, 0];
export const SILK_UNIFORM5_ICDF: readonly number[] = [205, 154, 102, 51, 0];
export const SILK_UNIFORM6_ICDF: readonly number[] = [213, 171, 128, 85, 43, 0];
export const SILK_UNIFORM8_ICDF: readonly number[] = [
  224, 192, 160, 128, 96, 64, 32, 0,
];

// Stereo prediction tables (RFC 6716 Table 8 & 9)
export const SILK_STEREO_PRED_JOINT_ICDF: readonly number[] = [
  249, 247, 246, 245, 244, 234, 210, 202, 201, 200, 197, 174, 82,
  59, 56, 55, 54, 46, 22, 12, 11, 10, 9, 7, 0,
];
export const SILK_STEREO_ONLY_CODE_MID_ICDF: readonly number[] = [64, 0];

// Gain quantization tables (RFC 6716 Section 4.2.7.2)
export const SILK_GAIN_ICDF: readonly number[][] = [
  [100, 40, 16, 7, 3, 1, 0, 0], // Inactive
  [224, 112, 44, 15, 3, 2, 1, 0], // Unvoiced
  [254, 237, 192, 132, 70, 23, 4, 0], // Voiced
];

export const SILK_DELTA_GAIN_ICDF: readonly number[] = [
  250, 245, 234, 203, 71, 50, 42, 38, 35, 33, 31, 29, 28, 27, 26, 25, 24, 23, 22,
  21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,
];

// NLSF interpolation factor ICDF
export const SILK_NLSF_INTERP_ICDF: readonly number[] = [243, 221, 192, 181, 0];

// NLSF Stage 1 ICDF: [NB_MB: 2 types x 32], [WB: 2 types x 32]
// Type 0 = Unvoiced/Inactive, Type 1 = Voiced
export const SILK_NLSF_CB1_ICDF_NB_MB: readonly number[][] = [
  [
    225, 204, 201, 184, 183, 175, 158, 154, 153, 135, 119, 115, 113, 110, 109,
    99, 98, 95, 79, 68, 52, 50, 48, 45, 43, 32, 31, 27, 18, 10, 3, 0,
  ],
  [
    255, 251, 235, 230, 212, 201, 196, 182, 167, 166, 163, 151, 138, 124, 110,
    104, 90, 78, 76, 70, 69, 57, 45, 34, 24, 21, 11, 6, 5, 4, 3, 0,
  ],
];

export const SILK_NLSF_CB1_ICDF_WB: readonly number[][] = [
  [
    225, 204, 201, 184, 183, 175, 158, 154, 153, 135, 119, 115, 113, 110, 109,
    99, 98, 95, 79, 68, 52, 50, 48, 45, 43, 32, 31, 27, 18, 10, 3, 0,
  ],
  [
    255, 251, 235, 230, 212, 201, 196, 182, 167, 166, 163, 151, 138, 124, 110,
    104, 90, 78, 76, 70, 69, 57, 45, 34, 24, 21, 11, 6, 5, 4, 3, 0,
  ],
];

// NLSF Stage 2 residual ICDFs (9 symbols: -4..+4, escaped at 0 or 8)
export const SILK_NLSF_EXT_ICDF: readonly number[] = [
  100, 40, 16, 7, 3, 1, 0, 0,
];

export const SILK_NLSF_STAGE2_ICDF: readonly number[] = [
  255, 254, 253, 244, 12, 3, 2, 1, 0,
];

// Typical formant-ordered Stage 1 anchor vectors in Q8 (mapped to Q15 as v * 128)
export function getNlsfStage1BaseQ15(order: number, cb1Idx: number): number[] {
  const out = new Array<number>(order);
  const tilt = ((cb1Idx - 15.5) / 16) * 0.08;
  for (let i = 0; i < order; i++) {
    const base = (i + 1) / (order + 1);
    const warped = Math.max(0.02, Math.min(0.98, base + tilt * Math.sin(Math.PI * base)));
    out[i] = Math.round(warped * 32768);
  }
  return out;
}

// Pitch lag tables (RFC 6716 Section 4.2.7.4)
export const SILK_PITCH_LAG_ICDF: readonly number[] = [
  241, 225, 211, 199, 187, 175, 164, 153, 142, 132, 123, 114, 105, 96, 88,
  80, 72, 64, 57, 50, 44, 38, 33, 29, 24, 20, 16, 12, 9, 5, 2, 0,
];

export const SILK_PITCH_DELTA_ICDF: readonly number[] = [
  254, 252, 250, 247, 243, 237, 228, 212, 184, 72, 44, 28, 19, 13, 9, 6, 4, 2,
  1, 0, 0,
];

// Pitch contours by mode/frame size
export const SILK_PITCH_CONTOUR_NB_10_ICDF: readonly number[] = [171, 85, 0];
export const SILK_PITCH_CONTOUR_NB_20_ICDF: readonly number[] = [
  232, 209, 186, 163, 140, 116, 93, 70, 47, 23, 0,
];
export const SILK_PITCH_CONTOUR_10_ICDF: readonly number[] = [
  235, 213, 192, 171, 149, 128, 107, 85, 64, 43, 21, 0,
];
export const SILK_PITCH_CONTOUR_20_ICDF: readonly number[] = [
  248, 241, 233, 226, 218, 211, 203, 196, 188, 181, 173, 166, 158, 151, 143,
  136, 128, 120, 113, 105, 98, 90, 83, 75, 68, 60, 53, 45, 38, 30, 23, 15, 8, 0,
];

// LTP tables (RFC 6716 Section 4.2.7.5)
export const SILK_LTP_PER_INDEX_ICDF: readonly number[] = [203, 150, 0];
export const SILK_LTP_SCALE_ICDF: readonly number[] = [128, 64, 0];

export const SILK_LTP_GAIN_ICDF_0: readonly number[] = [
  224, 192, 160, 128, 96, 64, 32, 0,
];
export const SILK_LTP_GAIN_ICDF_1: readonly number[] = [
  240, 224, 208, 192, 176, 160, 144, 128, 112, 96, 80, 64, 48, 32, 16, 0,
];
export const SILK_LTP_GAIN_ICDF_2: readonly number[] = [
  248, 240, 232, 224, 216, 208, 200, 192, 184, 176, 168, 160, 152, 144, 136,
  128, 120, 112, 104, 96, 88, 80, 72, 64, 56, 48, 40, 32, 24, 16, 8, 0,
];

// Seed ICDF (0..3)
export const SILK_SEED_ICDF: readonly number[] = [64, 32, 16, 0];

// Pulse / Shell ICDFs (RFC 6716 Section 4.2.7.8)
export const SILK_RATE_LEVELS_ICDF: readonly number[][] = [
  // Unvoiced / Inactive
  [241, 190, 178, 132, 87, 74, 41, 14, 0],
  // Voiced
  [223, 193, 157, 140, 106, 57, 39, 18, 0],
];

// Pulses per block [rateLevel 0..9][0..17] (17 = escape)
export const SILK_PULSES_PER_BLOCK_ICDF: readonly number[][] = [
  [125, 51, 26, 18, 15, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0],
  [198, 105, 45, 22, 15, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0],
  [213, 162, 116, 83, 59, 43, 32, 24, 18, 15, 12, 9, 7, 6, 5, 3, 2, 0],
  [239, 187, 116, 59, 28, 16, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0],
  [250, 229, 188, 135, 86, 51, 30, 19, 13, 10, 8, 6, 5, 4, 3, 2, 1, 0],
  [249, 235, 213, 185, 156, 128, 103, 83, 66, 53, 42, 33, 26, 21, 17, 13, 10, 0],
  [254, 249, 235, 206, 164, 118, 77, 46, 27, 16, 10, 7, 5, 4, 3, 2, 1, 0],
  [255, 253, 249, 239, 220, 191, 156, 119, 85, 57, 37, 23, 15, 10, 6, 4, 2, 0],
  [255, 253, 251, 246, 237, 223, 203, 179, 152, 124, 98, 75, 55, 40, 29, 21, 15, 0],
  [255, 254, 253, 247, 220, 162, 106, 67, 42, 28, 18, 12, 9, 6, 4, 3, 2, 0],
];

export const SILK_LSB_ICDF: readonly number[] = [128, 0];

// Sign ICDF indexed by clamped offset (0..41), 2 symbols: [neg, pos] -> [p_neg, 0]
export const SILK_SIGNS_ICDF: readonly number[][] = [
  [128, 0],
  [140, 0],
  [148, 0],
  [156, 0],
  [164, 0],
  [172, 0],
  [180, 0],
];

export const SILK_SHELL_SPLIT_ICDF: readonly number[] = [
  240, 220, 196, 168, 138, 108, 80, 56, 36, 22, 12, 6, 3, 1, 0,
];

/**
 * Convert SILK NLSF vector (Q15, sorted in [0, 32768]) to LPC coefficients a[0..order]
 * using the standard LSF->Chebyshev P(z)/Q(z) conversion + 0.99^k bandwidth expansion.
 */
export function nlsfToLpc(nlsfQ15: number[]): number[] {
  const order = nlsfQ15.length;
  const half = order >> 1;
  const x = new Float64Array(order);
  for (let i = 0; i < order; i++) {
    const theta = (Math.max(1, Math.min(32767, nlsfQ15[i])) / 32768) * Math.PI;
    x[i] = Math.cos(theta);
  }

  // Build P(z) from even indices, Q(z) from odd indices
  const polyFromRoots = (indices: number[]): Float64Array => {
    let p = new Float64Array(half + 1);
    p[0] = 1;
    let deg = 0;
    for (const idx of indices) {
      const c = -2 * x[idx];
      const next = new Float64Array(half + 1);
      for (let k = 0; k <= deg; k++) {
        next[k] += p[k];
        if (k + 1 <= half) next[k + 1] += c * p[k];
        if (k + 2 <= half) next[k + 2] += p[k];
      }
      // Actually quadratic (1 + c z^-1 + z^-2) can double degree up to order
      p = next;
      deg += 2;
    }
    return p;
  };
  void polyFromRoots;

  // Full degree convolution
  const buildCheb = (parity: 0 | 1): Float64Array => {
    let p = new Float64Array(order + 2);
    p[0] = 1;
    let deg = 0;
    for (let i = parity; i < order; i += 2) {
      const c = -2 * x[i];
      const next = new Float64Array(order + 2);
      for (let k = 0; k <= deg; k++) {
        next[k] += p[k];
        next[k + 1] += c * p[k];
        next[k + 2] += p[k];
      }
      p = next;
      deg += 2;
    }
    // Multiply P by (1 - z^-1) or Q by (1 + z^-1)
    const out = new Float64Array(order + 2);
    const sign = parity === 0 ? -1 : 1;
    for (let k = 0; k <= deg; k++) {
      out[k] += p[k];
      out[k + 1] += sign * p[k];
    }
    return out;
  };

  const pPoly = buildCheb(0);
  const qPoly = buildCheb(1);
  const a = new Array<number>(order + 1);
  a[0] = 1;
  let bwExp = 1;
  for (let k = 1; k <= order; k++) {
    bwExp *= 0.99;
    a[k] = 0.5 * (pPoly[k] + qPoly[k]) * bwExp;
  }
  return a;
}

/**
 * Evaluate SILK LPC spectral magnitude 1/|A(e^jw)| in dB (peak-normalized to 0 dB)
 * across target frequencies `freqsHz`, returning DB_FLOOR (-72) above Nyquist.
 */
export function computeSilkLpcEnvelopeDb(
  nlsfQ15: number[],
  fsKhz: 8 | 12 | 16,
  freqsHz: Float32Array,
  gainDb: number,
  floorDb = -72.0
): number[] {
  const a = nlsfToLpc(nlsfQ15);
  const fsHz = fsKhz * 1000;
  const nyquist = fsHz * 0.5;
  const out = new Array<number>(freqsHz.length);
  let maxDb = -200;

  for (let i = 0; i < freqsHz.length; i++) {
    const f = freqsHz[i];
    if (f > nyquist) {
      out[i] = floorDb;
      continue;
    }
    const w = (2 * Math.PI * f) / fsHz;
    let re = 1;
    let im = 0;
    for (let k = 1; k < a.length; k++) {
      re += a[k] * Math.cos(k * w);
      im -= a[k] * Math.sin(k * w);
    }
    const magSq = Math.max(re * re + im * im, 1e-8);
    const db = -10 * Math.log10(magSq);
    out[i] = db;
    if (db > maxDb) maxDb = db;
  }

  // Peak-normalize LPC shape and bias by frame quantization gain (mapped to realistic dBFS)
  const levelOffset = Math.max(-55, Math.min(-6, gainDb - 70));
  for (let i = 0; i < out.length; i++) {
    if (freqsHz[i] > nyquist) {
      out[i] = floorDb;
    } else {
      out[i] = Math.max(floorDb, Math.min(0, out[i] - maxDb + levelOffset));
    }
  }
  return out;
}
