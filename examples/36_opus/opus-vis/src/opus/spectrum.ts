export const STFT_NFFT = 1024;
export const STFT_HOP = 960; // 20 ms at 48 kHz
export const WATERFALL_BINS = 256;
export const FREQ_MIN_HZ = 50;
export const FREQ_MAX_HZ = 20000;
export const DB_FLOOR = -72.0;

export function buildLogFreqAxis(bins = WATERFALL_BINS): Float32Array {
  const freqs = new Float32Array(bins);
  const ratio = FREQ_MAX_HZ / FREQ_MIN_HZ;
  for (let i = 0; i < bins; i++) {
    const t = i / (bins - 1);
    freqs[i] = FREQ_MIN_HZ * Math.pow(ratio, t);
  }
  return freqs;
}

export function hzToYFrac(hz: number): number {
  const clamped = Math.max(FREQ_MIN_HZ, Math.min(FREQ_MAX_HZ, hz));
  return Math.log(clamped / FREQ_MIN_HZ) / Math.log(FREQ_MAX_HZ / FREQ_MIN_HZ);
}

// Precompute bit-reversed indices and twiddle factors for 1024-pt Radix-2 FFT
const BITREV = new Uint16Array(STFT_NFFT);
const HANN = new Float32Array(STFT_NFFT);
const COS_TAB = new Float32Array(STFT_NFFT >> 1);
const SIN_TAB = new Float32Array(STFT_NFFT >> 1);

(() => {
  const logN = 10;
  let hannSum = 0;
  for (let i = 0; i < STFT_NFFT; i++) {
    let x = i;
    let r = 0;
    for (let b = 0; b < logN; b++) {
      r = (r << 1) | (x & 1);
      x >>>= 1;
    }
    BITREV[i] = r;
    const w = 0.5 * (1 - Math.cos((2 * Math.PI * i) / STFT_NFFT));
    HANN[i] = w;
    hannSum += w;
  }
  // Normalize Hann window coherent gain so 0 dBFS sine reads ~0 dBFS
  const norm = 2.0 / hannSum;
  for (let i = 0; i < STFT_NFFT; i++) {
    HANN[i] *= norm;
  }
  for (let k = 0; k < STFT_NFFT >> 1; k++) {
    const ang = (-2 * Math.PI * k) / STFT_NFFT;
    COS_TAB[k] = Math.cos(ang);
    SIN_TAB[k] = Math.sin(ang);
  }
})();

function fft1024(re: Float32Array, im: Float32Array): void {
  // Bit-reversal permutation
  for (let i = 0; i < STFT_NFFT; i++) {
    const j = BITREV[i];
    if (i < j) {
      const tr = re[i];
      re[i] = re[j];
      re[j] = tr;
      const ti = im[i];
      im[i] = im[j];
      im[j] = ti;
    }
  }

  for (let size = 2; size <= STFT_NFFT; size <<= 1) {
    const half = size >> 1;
    const step = STFT_NFFT / size;
    for (let start = 0; start < STFT_NFFT; start += size) {
      for (let k = 0; k < half; k++) {
        const twIdx = k * step;
        const wr = COS_TAB[twIdx];
        const wi = SIN_TAB[twIdx];
        const even = start + k;
        const odd = even + half;
        const or = re[odd];
        const oi = im[odd];
        const tr = wr * or - wi * oi;
        const ti = wr * oi + wi * or;
        re[odd] = re[even] - tr;
        im[odd] = im[even] - ti;
        re[even] += tr;
        im[even] += ti;
      }
    }
  }
}

export interface StftResult {
  hops: number;
  bins: number;
  freqsHz: Float32Array;
  spectrumDb: Float32Array;
  centroidsHz: Float32Array;
}

export function computeStft(mono48k: Float32Array, targetHops?: number): StftResult {
  const freqsHz = buildLogFreqAxis(WATERFALL_BINS);
  const minHops = Math.max(1, Math.ceil(mono48k.length / STFT_HOP));
  const hops = targetHops ? Math.max(1, targetHops) : minHops;

  const spectrumDb = new Float32Array(hops * WATERFALL_BINS);
  const centroidsHz = new Float32Array(hops);

  const re = new Float32Array(STFT_NFFT);
  const im = new Float32Array(STFT_NFFT);
  const magLin = new Float32Array((STFT_NFFT >> 1) + 1);

  // Map each log waterfall bin [0..255] to fractional FFT bin k = hz * NFFT / 48000
  const binIdxLo = new Uint16Array(WATERFALL_BINS);
  const binIdxHi = new Uint16Array(WATERFALL_BINS);
  const binFrac = new Float32Array(WATERFALL_BINS);
  const nyqBin = STFT_NFFT >> 1;
  for (let b = 0; b < WATERFALL_BINS; b++) {
    const fk = (freqsHz[b] * STFT_NFFT) / 48000;
    const lo = Math.max(0, Math.min(nyqBin - 1, Math.floor(fk)));
    const hi = Math.min(nyqBin, lo + 1);
    binIdxLo[b] = lo;
    binIdxHi[b] = hi;
    binFrac[b] = Math.max(0, Math.min(1, fk - lo));
  }

  for (let h = 0; h < hops; h++) {
    const center = h * STFT_HOP;
    const start = center - (STFT_NFFT >> 1);
    for (let i = 0; i < STFT_NFFT; i++) {
      const idx = start + i;
      const s = idx >= 0 && idx < mono48k.length ? mono48k[idx] : 0;
      re[i] = s * HANN[i];
      im[i] = 0;
    }

    fft1024(re, im);

    let sumWeightedFreq = 0;
    let sumEnergy = 0;
    for (let k = 0; k <= nyqBin; k++) {
      const m2 = re[k] * re[k] + im[k] * im[k];
      magLin[k] = m2;
      if (k >= 1) {
        const hz = (k * 48000) / STFT_NFFT;
        sumWeightedFreq += hz * m2;
        sumEnergy += m2;
      }
    }

    centroidsHz[h] = sumEnergy > 1e-12 ? sumWeightedFreq / sumEnergy : 0;

    const rowOffset = h * WATERFALL_BINS;
    for (let b = 0; b < WATERFALL_BINS; b++) {
      const lo = binIdxLo[b];
      const hi = binIdxHi[b];
      const f = binFrac[b];
      const m2 = magLin[lo] * (1 - f) + magLin[hi] * f;
      const db = m2 > 1e-15 ? 10 * Math.log10(m2) : DB_FLOOR;
      spectrumDb[rowOffset + b] = Math.max(DB_FLOOR, Math.min(6.0, db));
    }
  }

  return {
    hops,
    bins: WATERFALL_BINS,
    freqsHz,
    spectrumDb,
    centroidsHz,
  };
}
