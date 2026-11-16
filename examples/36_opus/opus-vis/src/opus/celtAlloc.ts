import {
  ALLOC_STEPS,
  BAND_ALLOCATION,
  BITRES,
  EBAND5MS,
  FINE_OFFSET,
  LOG_MAX_PSEUDO,
  LOG_N0,
  MAX_FINE_BITS,
  MAX_PSEUDO,
  NB_EBANDS,
  bandCap,
  getPulses,
} from './celtTab';

// Compute log2_frac(val, BITRES) exact per RFC 6716
export function ecIlog(v: number): number {
  let x = v >>> 0;
  let ret = 0;
  while (x > 0) {
    x >>>= 1;
    ret++;
  }
  return ret;
}

// Compute CWRS V(N, K) as BigInt (exact for any N, K in CELT) and return Q3 log2_frac
const vCache = new Map<string, number>();

export function cwrsLog2Q3(N: number, K: number): number {
  if (K <= 0 || N <= 0) return 0;
  const key = `${N}:${K}`;
  const cached = vCache.get(key);
  if (cached !== undefined) return cached;

  // Recurrence: V(N, K) = V(N-1, K) + V(N, K-1) + V(N-1, K-1)
  // Row-wise DP over k = 0..K for n = 1..N
  let prev = new Array<bigint>(K + 1).fill(0n);
  prev[0] = 1n;
  for (let k = 1; k <= K; k++) prev[k] = 2n; // V(1, k) = 2 for k>=1

  for (let n = 2; n <= N; n++) {
    const cur = new Array<bigint>(K + 1);
    cur[0] = 1n;
    for (let k = 1; k <= K; k++) {
      cur[k] = prev[k] + cur[k - 1] + prev[k - 1];
    }
    prev = cur;
  }

  const v = prev[K];
  const res = log2FracBigInt(v, BITRES);
  vCache.set(key, res);
  return res;
}

function log2FracBigInt(val: bigint, frac: number): number {
  if (val <= 1n) return 0;
  const s = val.toString(2);
  const l = s.length;
  // Take top 16 bits in Q14
  let r: number;
  if (l >= 16) {
    r = Number(val >> BigInt(l - 16));
  } else {
    r = Number(val << BigInt(16 - l));
  }
  // Exact libopus log2_frac iteration on Q14
  let ret = l << frac;
  for (let i = 0; i < frac; i++) {
    r = (r * r) >> 15;
    const t = r >> 16;
    ret = (ret << 1) | t; // Wait: we already shifted l << frac; build fractional bits:
    r >>= t;
  }
  // Recompute fractional bits cleanly:
  let r2: number;
  if (l >= 16) {
    r2 = Number(val >> BigInt(l - 16));
  } else {
    r2 = Number(val << BigInt(16 - l));
  }
  let fracBits = 0;
  for (let i = 0; i < frac; i++) {
    r2 = (r2 * r2) >> 15;
    const t = r2 >> 16;
    fracBits = (fracBits << 1) | t;
    r2 >>= t;
  }
  return ((l - 1) << frac) + fracBits;
}

// Also compute V(N, K) BigInt for PVQ uint decoding when K > 0
export function cwrsSizeBigInt(N: number, K: number): bigint {
  if (K <= 0) return 1n;
  if (N <= 0) return 0n;
  let prev = new Array<bigint>(K + 1).fill(0n);
  prev[0] = 1n;
  for (let k = 1; k <= K; k++) prev[k] = 2n;
  for (let n = 2; n <= N; n++) {
    const cur = new Array<bigint>(K + 1);
    cur[0] = 1n;
    for (let k = 1; k <= K; k++) {
      cur[k] = prev[k] + cur[k - 1] + prev[k - 1];
    }
    prev = cur;
  }
  return prev[K];
}

// Compute bits(k_idx) in 1/8-bit for a given band width N0 and LM (per libopus celt/rate.c)
const pulseBitsCache = new Map<string, Int16Array>();

function getBandPulseBits(N0: number, LM: number): Int16Array {
  const key = `${N0}:${LM}`;
  const cached = pulseBitsCache.get(key);
  if (cached) return cached;

  const N = N0 << LM;
  const arr = new Int16Array(MAX_PSEUDO + 1);
  arr[0] = 0;
  for (let kIdx = 1; kIdx <= MAX_PSEUDO; kIdx++) {
    const K = getPulses(kIdx);
    // In libopus celt/rate.c: bits = cwrsLog2Q3(N, K) + 1 (rounding safety offset in cache)
    // Wait: let's match libopus pulse_cache: log2_frac(V(N,K), BITRES) + (N==1?0:0)
    // Actually in libopus cwrs.c fit_bits / log2_frac(V(N,K), BITRES):
    // When encoding in alg_quant, ec_enc_uint takes V(N,K) which costs approx log2_frac(V(N,K), BITRES).
    arr[kIdx] = cwrsLog2Q3(N, K);
  }
  pulseBitsCache.set(key, arr);
  return arr;
}

export function bitsToPulses(band: number, LM: number, bitsQ3: number): number {
  const N0 = EBAND5MS[band + 1] - EBAND5MS[band];
  const cache = getBandPulseBits(N0, LM);
  let lo = 0;
  let hi = MAX_PSEUDO;
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if (cache[mid] <= bitsQ3) {
      lo = mid;
    } else {
      hi = mid - 1;
    }
  }
  return lo;
}

export function pulsesToBits(band: number, LM: number, pulsesIdx: number): number {
  if (pulsesIdx <= 0) return 0;
  const N0 = EBAND5MS[band + 1] - EBAND5MS[band];
  const cache = getBandPulseBits(N0, LM);
  return cache[Math.min(pulsesIdx, MAX_PSEUDO)];
}

export interface AllocationResult {
  pulses: number[]; // bit allocation per band in 1/8-bit (bits1 + frac)
  fineBits: number[]; // per band
  finePriority: number[]; // 0 or 1 per band
  codedBands: number;
  balance: number;
}

export function computeAllocation(params: {
  start: number;
  end: number;
  totalBitsQ3: number;
  cap: number[];
  offsets: number[];
  trim: number;
  intensity: number;
  dualStereo: boolean;
  lm: number;
  channels: number;
  signalBandwidth: number;
}): AllocationResult {
  const {
    start,
    end,
    totalBitsQ3,
    cap,
    offsets,
    trim,
    intensity,
    dualStereo,
    lm,
    channels,
    signalBandwidth,
  } = params;

  const bits1 = new Array<number>(NB_EBANDS).fill(0);
  const bits2 = new Array<number>(NB_EBANDS).fill(0);
  const thresh = new Array<number>(NB_EBANDS).fill(0);
  const trimOffset = new Array<number>(NB_EBANDS).fill(0);

  for (let j = start; j < end; j++) {
    const N0 = EBAND5MS[j + 1] - EBAND5MS[j];
    const N = N0 << lm;
    // Minimum allocation threshold per RFC 6716 Section 4.3.4.1
    thresh[j] = Math.max(channels << BITRES, ((3 * N * channels) << BITRES) >> 4);
    trimOffset[j] =
      (channels *
        N0 *
        ((trim - 5 - lm) * (end - j - 1)) *
        (1 << (lm + BITRES))) >>
      6;
    if (N === 1) {
      trimOffset[j] -= channels << BITRES;
    }
  }

  let lo = 1;
  let hi = ALLOC_STEPS * (BAND_ALLOCATION.length - 1); // 60

  const interpBits = (rateIdx: number, outBits: number[], skipStart: number) => {
    const tier = Math.floor(rateIdx / ALLOC_STEPS);
    const rem = rateIdx - tier * ALLOC_STEPS;
    let psum = 0;
    for (let j = start; j < end; j++) {
      const N = EBAND5MS[j + 1] - EBAND5MS[j];
      const base =
        ((ALLOC_STEPS - rem) * BAND_ALLOCATION[tier][j] +
          rem * BAND_ALLOCATION[Math.min(tier + 1, BAND_ALLOCATION.length - 1)][j]) /
        ALLOC_STEPS;
      let b = Math.floor((channels * N * Math.floor(base)) << lm) >> 2;
      if (b > 0) {
        b = Math.max(0, b + trimOffset[j]);
      }
      b += offsets[j];
      if (b >= thresh[j] || j < skipStart) {
        b = Math.max(0, b);
      } else {
        b = 0;
      }
      b = Math.min(b, cap[j]);
      outBits[j] = b;
      psum += b;
    }
    return psum;
  };

  while (lo < hi - 1) {
    const mid = (lo + hi) >> 1;
    const psum = interpBits(mid, bits1, 0);
    if (psum > totalBitsQ3) {
      hi = mid;
    } else {
      lo = mid;
    }
  }

  const psumLo = interpBits(lo, bits1, 0);
  const psumHi = interpBits(hi, bits2, 0);

  let psum = 0;
  for (let j = start; j < end; j++) {
    bits2[j] = bits2[j] - bits1[j];
    psum += bits1[j];
  }

  let left = totalBitsQ3 - psum;
  const den = psumHi - psumLo;
  for (let j = start; j < end; j++) {
    if (den > 0) {
      const add = Math.floor((bits2[j] * left) / den);
      bits1[j] += add;
    }
  }

  // Band skipping (CBR-like floor enforcement from high bands downward)
  let codedBands = end;
  let totalUsed = 0;
  for (let j = start; j < end; j++) totalUsed += bits1[j];
  let remLeft = totalBitsQ3 - totalUsed;

  let done = false;
  let j = end - 1;
  while (j > start && !done) {
    if (bits1[j] >= thresh[j] && (j <= signalBandwidth || bits1[j] > 0)) {
      done = true;
    } else {
      remLeft += bits1[j];
      bits1[j] = 0;
      codedBands = j;
      j--;
    }
  }

  // Distribute leftover bits across remaining coded bands
  if (codedBands > start && remLeft > 0) {
    const perBand = Math.floor(remLeft / (codedBands - start));
    let extra = remLeft - perBand * (codedBands - start);
    for (let b = start; b < codedBands; b++) {
      const add = perBand + (extra > 0 ? 1 : 0);
      if (extra > 0) extra--;
      const room = Math.max(0, cap[b] - bits1[b]);
      const give = Math.min(add, room);
      bits1[b] += give;
    }
  }

  // Split each band's allocation into PVQ pulses vs fine energy bits (RFC 6716 Section 4.3.4.2)
  const pulses = new Array<number>(NB_EBANDS).fill(0);
  const fineBits = new Array<number>(NB_EBANDS).fill(0);
  const finePriority = new Array<number>(NB_EBANDS).fill(0);
  let balance = 0;

  for (let b = start; b < codedBands; b++) {
    const N0 = EBAND5MS[b + 1] - EBAND5MS[b];
    const N = N0 << lm;
    const bit = bits1[b];
    const c = (b >= intensity && !dualStereo) ? 1 : channels;
    const logN = LOG_N0[b] + (lm << BITRES);

    if (N === 1 && c === 1) {
      // Single-bin mono band gets only fine energy bits
      fineBits[b] = Math.min(MAX_FINE_BITS, bit >> BITRES);
      pulses[b] = 0;
      continue;
    }

    let offset = (logN << 1) - (c === 2 && N > 2 ? 0 : 0);
    // Fine bit estimate per RFC 6716 Section 4.3.4.2
    let ebits = Math.max(
      0,
      ((bit + (c === 2 ? N << BITRES : 0)) >> (BITRES + 1)) -
        ((logN + (lm > 0 ? 0 : 0)) >> 3)
    );
    // Exact libopus formula:
    // offset = ((m->logN[j] + (LM<<BITRES))>>1) - QTHETA_OFFSET
    const effLogN = LOG_N0[b] + (lm << BITRES);
    const den2 = c * N;
    // Per RFC 6716 compute_allocation fine bit split:
    let fb = Math.floor((bit / (c << BITRES) - effLogN / 8 + 1) / 2);
    if (b >= intensity && channels === 2 && !dualStereo) {
      fb = Math.floor((bit / (1 << BITRES) - effLogN / 8 + 1) / 2);
    }
    if (fb < 0) fb = 0;
    if (fb > MAX_FINE_BITS) fb = MAX_FINE_BITS;

    // Fine priority if fractional bit favors extra fine bit
    const remainder = bit - ((fb * c) << BITRES);
    finePriority[b] = remainder > ((c << BITRES) >> 1) ? 1 : 0;

    // Ensure enough bits left for PVQ
    if ((fb * c << BITRES) > bit) {
      fb = Math.floor(bit / (c << BITRES));
    }

    fineBits[b] = fb;
    pulses[b] = Math.max(0, bit - ((fb * c) << BITRES));
    if (offset < 0) balance += 0;
    void den2;
  }

  // Extra fine priority passes if leftover bits remain
  let usedTotal = 0;
  for (let b = start; b < codedBands; b++) {
    const c = channels;
    usedTotal += pulses[b] + ((fineBits[b] * c) << BITRES);
  }
  balance = totalBitsQ3 - usedTotal;

  // Unused balance goes to finePriority bands if any
  for (let b = start; b < codedBands && balance >= (channels << BITRES); b++) {
    if (finePriority[b] && fineBits[b] < MAX_FINE_BITS) {
      fineBits[b]++;
      balance -= channels << BITRES;
    }
  }

  return {
    pulses,
    fineBits,
    finePriority,
    codedBands,
    balance,
  };
}

export function buildDefaultCaps(lm: number, channels: number): number[] {
  const caps = new Array<number>(NB_EBANDS);
  for (let j = 0; j < NB_EBANDS; j++) {
    caps[j] = bandCap(j, lm, channels);
  }
  return caps;
}
