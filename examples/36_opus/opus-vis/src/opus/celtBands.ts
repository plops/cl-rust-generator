import { cwrsSizeBigInt, ecIlog } from './celtAlloc';
import { BITRES, EBAND5MS, MAX_PSEUDO, getPulses } from './celtTab';
import { RangeDec } from './range';

const MAX_32 = 0xffffffffn;

// Exact bits2pulses for arbitrary N (not just band index) using binary search on log2(V(N,K))
const arbPulseCache = new Map<string, Int32Array>();

function pulsesForN(N: number, bitsQ3: number): number {
  if (bitsQ3 <= 0 || N <= 0) return 0;
  const key = `${N}`;
  let table = arbPulseCache.get(key);
  if (!table) {
    table = new Int32Array(MAX_PSEUDO + 1);
    table[0] = 0;
    for (let i = 1; i <= MAX_PSEUDO; i++) {
      const K = getPulses(i);
      const v = cwrsSizeBigInt(N, K);
      // Approximate Q3 bits = floor(log2(v) * 8)
      const bitLen = v.toString(2).length;
      if (bitLen <= 1) {
        table[i] = 0;
      } else {
        const top =
          bitLen >= 16
            ? Number(v >> BigInt(bitLen - 16))
            : Number(v << BigInt(16 - bitLen));
        let r = top;
        let frac = 0;
        for (let f = 0; f < BITRES; f++) {
          r = (r * r) >> 15;
          const t = r >> 16;
          frac = (frac << 1) | t;
          r >>= t;
        }
        table[i] = ((bitLen - 1) << BITRES) + frac;
      }
    }
    arbPulseCache.set(key, table);
  }

  let lo = 0;
  let hi = MAX_PSEUDO;
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if (table[mid] <= bitsQ3) lo = mid;
    else hi = mid - 1;
  }
  return lo;
}

function computeQn(
  N: number,
  bQ3: number,
  offset: number,
  pulseCap: number,
  stereo: boolean
): number {
  const N2 = 2 * N - 1;
  let b = bQ3;
  if (stereo && N === 2) b -= 1 << BITRES;
  let qn = Math.floor((b + N2 * offset) / (N2 * (1 << BITRES)));
  // Clamp to RFC 6716 limits
  if (qn > 256) qn = 256;
  const qb = Math.floor(
    (b - (2 << BITRES) - (pulseCap > 0 ? 0 : 0)) >> (BITRES + 1)
  );
  if (qn > qb + 1 && qb >= 0) qn = qb + 1;
  // Quantize qn to odd powers/even table (1, 2, 4, 6, 8, ... 256)
  if (qn < 1) return 1;
  if (qn === 1) return 1;
  qn = Math.min(256, (qn >> 1) << 1);
  if (qn < 2) qn = 2;
  return qn;
}

function decodeThetaAngle(
  rc: RangeDec,
  qn: number,
  stereo: boolean,
  B: number,
  totalBitsQ3: number
): number {
  const tell = rc.tellFrac();
  const rem = totalBitsQ3 - tell;
  if (rem < (2 << BITRES) || qn <= 1) return 0;

  let itheta = 0;
  if (stereo && B === 1) {
    // Joint stereo angle with uniform or triangular PDF
    const ft = qn + 1;
    if (tell + (ecIlog(ft) << BITRES) <= totalBitsQ3) {
      itheta = rc.decUint(ft);
    }
  } else {
    // Split angle
    const ft = qn + 1;
    if (tell + (ecIlog(ft) << BITRES) <= totalBitsQ3) {
      itheta = rc.decUint(ft);
    }
  }
  return Math.round((itheta * 16384) / qn);
}

function quantBandN1(
  rc: RangeDec,
  C: number,
  bQ3: number,
  totalBitsQ3: number
): void {
  let rem = totalBitsQ3 - rc.tellFrac();
  for (let c = 0; c < C; c++) {
    if (bQ3 >= (1 << BITRES) && rem >= (1 << BITRES)) {
      rc.decBits(1);
      rem -= 1 << BITRES;
    }
  }
}

function walkSubband(
  rc: RangeDec,
  N: number,
  bQ3: number,
  B: number,
  LM: number,
  fill: number,
  totalBitsQ3: number,
  depth: number
): number {
  let b = bQ3;
  const rem = totalBitsQ3 - rc.tellFrac();
  if (b <= 0 || rem <= 0) return 0;
  if (b > rem) b = rem;

  if (N === 1) {
    quantBandN1(rc, 1, b, totalBitsQ3);
    return 0;
  }

  // Split if multiple blocks (B > 1) or N is large and bit budget exceeds single-CWRS limit
  const canSplit = N >= 2 && (N & 1) === 0 && depth < 6;
  const splitThresholdQ3 =
    N <= 2 ? 1 << 30 : Math.min(24 << BITRES, (N * 3) << BITRES);

  if (canSplit && (B > 1 || b > splitThresholdQ3)) {
    const N2 = N >> 1;
    const B2 = B > 1 ? B >> 1 : 1;
    const qn = computeQn(N, b, 0, 0, false);
    const tellBefore = rc.tellFrac();
    let itheta = 0;
    let inv = 0;
    if (qn > 1 && totalBitsQ3 - tellBefore >= (3 << BITRES)) {
      if (B === 1) {
        // High-N split uses triangular or uniform itheta
        itheta = decodeThetaAngle(rc, qn, false, B, totalBitsQ3);
      } else {
        itheta = decodeThetaAngle(rc, qn, false, B, totalBitsQ3);
      }
      if (itheta === 0 && totalBitsQ3 - rc.tellFrac() >= (1 << BITRES)) {
        inv = rc.decodeBitLogp(1);
      }
    } else {
      itheta = 8192;
    }
    void inv;

    const spent = rc.tellFrac() - tellBefore;
    let remBand = Math.max(0, b - spent);
    const avail = Math.max(0, totalBitsQ3 - rc.tellFrac());
    if (remBand > avail) remBand = avail;

    let mbits = 0;
    let sbits = 0;
    if (itheta === 0) {
      mbits = remBand;
      sbits = 0;
    } else if (itheta === 16384) {
      mbits = 0;
      sbits = remBand;
    } else {
      // Split roughly proportional to log2(cos/sin), clamped cleanly
      mbits = remBand >> 1;
      sbits = remBand - mbits;
    }

    const cm1 = walkSubband(rc, N2, mbits, B2, LM, fill, totalBitsQ3, depth + 1);
    const cm2 = walkSubband(rc, N2, sbits, B2, LM, fill >> B2, totalBitsQ3, depth + 1);
    return cm1 | cm2;
  }

  // Leaf PVQ decode
  let kIdx = pulsesForN(N, b);
  while (kIdx > 0) {
    const K = getPulses(kIdx);
    const V = cwrsSizeBigInt(N, K);
    const neededQ3 = ecIlog(Number(V <= MAX_32 ? V : MAX_32)) << BITRES;
    if (V <= MAX_32 && rc.tellFrac() + neededQ3 <= totalBitsQ3 + (1 << BITRES)) {
      if (V > 1n) {
        rc.decUint(Number(V));
      }
      return 1;
    }
    kIdx--;
  }
  return 0;
}

export function walkAllBands(params: {
  rc: RangeDec;
  start: number;
  end: number;
  codedBands: number;
  lm: number;
  channels: number;
  shortBlocks: number;
  intensity: number;
  dualStereo: boolean;
  pulses: number[];
  totalBitsQ3: number;
  pvqBitsOut: number[];
}): number {
  const {
    rc,
    start,
    codedBands,
    lm,
    channels,
    shortBlocks,
    intensity,
    dualStereo,
    pulses,
    totalBitsQ3,
    pvqBitsOut,
  } = params;

  const B = shortBlocks > 1 ? shortBlocks : 1;
  let collapseMask = 0;
  let balance = 0;

  for (let band = start; band < codedBands; band++) {
    const remTotal = totalBitsQ3 - rc.tellFrac();
    if (remTotal <= (1 << BITRES)) break;

    const N0 = EBAND5MS[band + 1] - EBAND5MS[band];
    const N = N0 << lm;
    const tBefore = rc.tellFrac();

    let b = pulses[band] + balance;
    if (b < 0) b = 0;
    if (b > remTotal) b = remTotal;

    const effChannels =
      channels === 2 && (dualStereo || band < intensity) ? 2 : 1;

    if (N === 1) {
      quantBandN1(rc, effChannels, b, totalBitsQ3);
    } else if (effChannels === 2 && !dualStereo) {
      // Joint stereo band: decode mid-side angle itheta then mid + side
      const qn = computeQn(N, b, 0, 0, true);
      if (qn > 1 && totalBitsQ3 - rc.tellFrac() >= (3 << BITRES)) {
        const itheta = decodeThetaAngle(rc, qn, true, B, totalBitsQ3);
        const spentHeader = rc.tellFrac() - tBefore;
        const remBand = Math.max(0, Math.min(b - spentHeader, totalBitsQ3 - rc.tellFrac()));
        const mbits = itheta === 16384 ? 0 : itheta === 0 ? remBand : Math.ceil(remBand * 0.6);
        const sbits = Math.max(0, remBand - mbits);
        if (mbits > 0) {
          collapseMask |= walkSubband(rc, N, mbits, B, lm, 0xff, totalBitsQ3, 0);
        }
        if (sbits > 0) {
          collapseMask |= walkSubband(rc, N, sbits, B, lm, 0xff, totalBitsQ3, 0);
        }
      } else {
        collapseMask |= walkSubband(rc, N, b, B, lm, 0xff, totalBitsQ3, 0);
      }
    } else if (effChannels === 2 && dualStereo) {
      const half = b >> 1;
      collapseMask |= walkSubband(rc, N, half, B, lm, 0xff, totalBitsQ3, 0);
      collapseMask |= walkSubband(
        rc,
        N,
        b - half,
        B,
        lm,
        0xff,
        totalBitsQ3,
        0
      );
    } else {
      // Mono or intensity stereo (read intensity inversion bit if coded)
      if (channels === 2 && band >= intensity && totalBitsQ3 - rc.tellFrac() >= (1 << BITRES)) {
        rc.decodeBitLogp(1);
      }
      const remBand = Math.max(0, Math.min(b, totalBitsQ3 - rc.tellFrac()));
      collapseMask |= walkSubband(rc, N, remBand, B, lm, 0xff, totalBitsQ3, 0);
    }

    const spent = rc.tellFrac() - tBefore;
    pvqBitsOut[band] = Math.max(0, spent);
    balance = b - spent;
    if (balance < -((1 << BITRES) * 4)) balance = 0;
  }

  return collapseMask;
}
