import { computeAllocation, buildDefaultCaps } from './celtAlloc';
import { walkAllBands } from './celtBands';
import {
  BETA_COEF,
  BETA_INTRA,
  BITRES,
  CELT_DEEMPH_DB,
  E_MEANS,
  E_PROB_MODEL,
  MAX_FINE_BITS,
  NB_EBANDS,
  SPREAD_ICDF,
  TAPSET_ICDF,
  TRIM_ICDF,
} from './celtTab';
import { RangeDec } from './range';
import { Budget, CeltFrameWalk } from './types';

export const DB_FLOOR = -72.0;
const CAL_DB = -81.5;

export class CeltState {
  public oldBandE: Float64Array[]; // [2][21]

  constructor() {
    this.oldBandE = [
      new Float64Array(NB_EBANDS).fill(0),
      new Float64Array(NB_EBANDS).fill(0),
    ];
  }

  public reset(): void {
    this.oldBandE[0].fill(0);
    this.oldBandE[1].fill(0);
  }
}

export function walkCeltFrame(params: {
  rc: RangeDec;
  state: CeltState;
  channels: number;
  lm: number; // 0=2.5ms, 1=5ms, 2=10ms, 3=20ms
  startBand: number;
  endBand: number;
  effectiveBytes: number;
  budget: Budget;
  isRedundancy?: boolean;
}): CeltFrameWalk {
  const {
    rc,
    state,
    channels,
    lm,
    startBand,
    endBand,
    effectiveBytes,
    budget,
    isRedundancy = false,
  } = params;

  const totalBits = effectiveBytes * 8;
  const totalBitsQ3 = totalBits << BITRES;

  const energyDb = new Array<number>(NB_EBANDS).fill(DB_FLOOR);
  const energyCoded = new Array<boolean>(NB_EBANDS).fill(false);
  const coarseBits = new Array<number>(NB_EBANDS).fill(0);
  const fineBitsOut = new Array<number>(NB_EBANDS).fill(0);
  const pvqBitsOut = new Array<number>(NB_EBANDS).fill(0);
  const bandTotalBits = new Array<number>(NB_EBANDS).fill(0);

  const chargeHeader = (delta: number) => {
    if (isRedundancy) budget.redundancy += delta;
    else budget.celtHeader += delta;
  };
  const chargeEnergy = (delta: number) => {
    if (isRedundancy) budget.redundancy += delta;
    else budget.celtEnergy += delta;
  };
  const chargePvq = (delta: number) => {
    if (isRedundancy) budget.redundancy += delta;
    else budget.celtPvq += delta;
  };

  let t0 = rc.tellFrac();
  let silence = 0;
  let postfilterGain = 0;
  let postfilterPeriod = 0;
  let transient = false;
  let shortBlocks = 1;
  let intra = false;

  // 1. Header flags (silence, postfilter, transient, intra) per RFC 6716 §4.3.1
  let tell = rc.tell();
  if (tell >= totalBits) {
    silence = 1;
  } else if (effectiveBytes === 1) {
    silence = 1;
  } else {
    if (startBand === 0 && tell + 16 <= totalBits) {
      silence = rc.decodeBitLogp(15);
    }
    if (silence === 0 && startBand === 0 && rc.tell() + 16 <= totalBits) {
      const pf = rc.decodeBitLogp(1);
      if (pf) {
        let octave = rc.decUint(6);
        postfilterPeriod = (16 << octave) + rc.decBits(4 + octave) - 1;
        const pfGainQ4 = rc.decBits(3) + 1;
        postfilterGain = pfGainQ4 / 8;
        if (rc.tell() + 2 <= totalBits) {
          rc.decodeIcdf(TAPSET_ICDF, 2);
        }
      }
    }
    if (lm > 0 && rc.tell() + 3 <= totalBits) {
      transient = rc.decodeBitLogp(3) !== 0;
      if (transient) {
        shortBlocks = 1 << lm;
      }
    }
    if (rc.tell() + 3 <= totalBits) {
      intra = rc.decodeBitLogp(3) !== 0;
    }
  }
  chargeHeader(Math.max(0, rc.tellFrac() - t0));

  if (silence) {
    for (let c = 0; c < channels; c++) {
      state.oldBandE[c].fill(-28.0);
    }
    return {
      endBand,
      startBand,
      transient: false,
      shortBlocks: 1,
      intra: false,
      spread: 0,
      dynallocCount: 0,
      intensityBand: endBand,
      dualStereo: false,
      postfilterGain: 0,
      postfilterPeriod: 0,
      energyDb,
      energyCoded,
      coarseBits,
      fineBits: fineBitsOut,
      pvqBits: pvqBitsOut,
      bandTotalBits,
      antiCollapse: false,
    };
  }

  // 2. Coarse energy quantization (RFC 6716 §4.3.2)
  t0 = rc.tellFrac();
  const beta = intra ? BETA_INTRA : BETA_COEF[lm];
  const betaF = beta / 32768;
  const coefF = intra ? 0 : betaF;
  const bandE: number[][] = [
    new Array<number>(NB_EBANDS).fill(-28),
    new Array<number>(NB_EBANDS).fill(-28),
  ];

  // Clear unused low bands in hybrid mode
  if (startBand > 0) {
    for (let c = 0; c < channels; c++) {
      for (let b = 0; b < startBand; b++) {
        bandE[c][b] = 0;
      }
    }
  }

  const prev = [0, 0];
  const probModel = E_PROB_MODEL[lm][intra ? 1 : 0];
  let stopCoarse = false;

  for (let b = startBand; b < endBand; b++) {
    const bStartTell = rc.tellFrac();
    for (let c = 0; c < channels; c++) {
      const tellNow = rc.tell();
      let qi = 0;
      if (!stopCoarse) {
        if (tellNow + 15 <= totalBits) {
          const fs0 = probModel[2 * b] << 7;
          const decay = probModel[2 * b + 1] << 6;
          qi = rc.laplaceDecode(fs0, decay);
        } else if (tellNow + 3 <= totalBits) {
          // Low-rate fallback: single sign bit or clamp
          stopCoarse = true;
          qi = 0;
        } else {
          stopCoarse = true;
          qi = 0;
        }
      }
      const pred = prev[c] + coefF * state.oldBandE[c][b];
      const q = qi;
      const val = pred + q;
      bandE[c][b] = val;
      prev[c] = prev[c] + q - betaF * q;
      energyCoded[b] = true;
    }
    coarseBits[b] = Math.max(0, rc.tellFrac() - bStartTell);
  }
  chargeEnergy(Math.max(0, rc.tellFrac() - t0));

  // 3. TF flags, spread, dynalloc, trim, intensity, dualStereo
  t0 = rc.tellFrac();
  const tfRes = new Array<number>(NB_EBANDS).fill(0);
  if (rc.tell() + (endBand - startBand) <= totalBits && endBand > startBand) {
    // tf_select bit + per-band change bits per RFC 6716 §4.3.3
    let budgetLeft = totalBits - rc.tell();
    let tfChanged = 0;
    let tfLogp = lm > 0 && transient ? 2 : 4;
    for (let b = startBand; b < endBand; b++) {
      if (rc.tell() + tfLogp + 1 <= totalBits) {
        const chg = rc.decodeBitLogp(tfLogp);
        if (chg) {
          tfChanged ^= 1;
          tfLogp = 5;
        }
      }
      tfRes[b] = tfChanged;
    }
    if (tfChanged && rc.tell() + 1 <= totalBits && lm > 0) {
      rc.decodeBitLogp(1);
    }
    void budgetLeft;
  }

  let spread = 2;
  if (rc.tell() + 5 <= totalBits) {
    spread = rc.decodeIcdf(SPREAD_ICDF, 5);
  }

  // Dynalloc
  const offsets = new Array<number>(NB_EBANDS).fill(0);
  let dynallocCount = 0;
  let dynLogp = 6;
  const maxDepth = totalBits - rc.tell();
  if (maxDepth > dynLogp) {
    let totalBoost = 0;
    for (let b = startBand; b < endBand; b++) {
      if (rc.tell() + dynLogp + 1 > totalBits) break;
      const flag = rc.decodeBitLogp(dynLogp);
      if (flag) {
        let boost = 1;
        while (rc.tell() + 1 <= totalBits && boost < 6 && totalBoost < 16) {
          if (!rc.decodeBitLogp(1)) break;
          boost++;
          totalBoost++;
        }
        offsets[b] = boost << BITRES;
        dynallocCount++;
        dynLogp = 1;
      } else {
        dynLogp = Math.min(6, dynLogp + 1);
      }
    }
  }

  let trim = 5;
  if (rc.tell() + 7 <= totalBits) {
    trim = rc.decodeIcdf(TRIM_ICDF, 7);
  }

  let intensity = endBand;
  let dualStereo = false;
  if (channels === 2 && rc.tell() + 2 <= totalBits) {
    const dual = rc.decodeBitLogp(1) !== 0;
    dualStereo = dual;
    if (!dual && endBand > startBand) {
      intensity = startBand + rc.decUint(endBand - startBand + 1);
    }
  }
  chargeHeader(Math.max(0, rc.tellFrac() - t0));

  // 4. Bit allocation
  const usedQ3 = rc.tellFrac();
  const availQ3 = Math.max(0, totalBitsQ3 - usedQ3);
  const caps = buildDefaultCaps(lm, channels);
  const alloc = computeAllocation({
    start: startBand,
    end: endBand,
    totalBitsQ3: availQ3,
    cap: caps,
    offsets,
    trim,
    intensity,
    dualStereo,
    lm,
    channels,
    signalBandwidth: endBand - 1,
  });

  // 5. Fine energy bits (raw bits read backwards from end of packet)
  t0 = rc.tellFrac();
  const finePow = new Array<number>(NB_EBANDS).fill(0);
  for (let b = startBand; b < alloc.codedBands; b++) {
    const fb = alloc.fineBits[b];
    if (fb <= 0) continue;
    const scale = 1 << (fb + 1);
    for (let c = 0; c < channels; c++) {
      if (rc.tellFrac() + fb * 8 <= totalBitsQ3) {
        const q = rc.decBits(fb);
        const offset = (q + 0.5) / (1 << fb) - 0.5;
        bandE[c][b] += offset;
        fineBitsOut[b] += fb << BITRES;
      }
    }
    finePow[b] = scale;
  }
  void finePow;
  chargeEnergy(Math.max(0, rc.tellFrac() - t0));

  // 6. Anti-collapse bit check (1 bit if transient && lm >= 2 && start == 0)
  let antiCollapseRsv = 0;
  if (transient && lm >= 2 && startBand === 0 && availQ3 >= ((alloc.codedBands * 2) << BITRES)) {
    antiCollapseRsv = 1 << BITRES;
  }

  // 7. PVQ walk across coded bands
  t0 = rc.tellFrac();
  walkAllBands({
    rc,
    start: startBand,
    end: endBand,
    codedBands: alloc.codedBands,
    lm,
    channels,
    shortBlocks,
    intensity,
    dualStereo,
    pulses: alloc.pulses,
    totalBitsQ3: Math.max(0, totalBitsQ3 - antiCollapseRsv),
    pvqBitsOut,
  });
  chargePvq(Math.max(0, rc.tellFrac() - t0));

  // 8. Anti-collapse & energy finalise bits
  let antiCollapse = false;
  if (antiCollapseRsv > 0 && rc.tellFrac() + (1 << BITRES) <= totalBitsQ3) {
    const tAc = rc.tellFrac();
    antiCollapse = rc.decBits(1) !== 0;
    chargeHeader(Math.max(0, rc.tellFrac() - tAc));
  }

  const tFin = rc.tellFrac();
  for (let prio = 0; prio < 2; prio++) {
    for (let b = startBand; b < alloc.codedBands; b++) {
      if (alloc.fineBits[b] >= MAX_FINE_BITS) continue;
      if (alloc.finePriority[b] !== prio) continue;
      if (rc.tellFrac() + ((1 * channels) << BITRES) > totalBitsQ3) break;
      const fb = alloc.fineBits[b];
      for (let c = 0; c < channels; c++) {
        const bit = rc.decBits(1);
        const delta = (bit - 0.5) / (1 << (fb + 1));
        bandE[c][b] += delta;
        fineBitsOut[b] += 1 << BITRES;
      }
    }
  }
  chargeEnergy(Math.max(0, rc.tellFrac() - tFin));

  // Update oldBandE state & compute dBFS per band
  for (let c = 0; c < channels; c++) {
    for (let b = startBand; b < endBand; b++) {
      state.oldBandE[c][b] = Math.max(-28, Math.min(28, bandE[c][b]));
    }
  }

  for (let b = startBand; b < endBand; b++) {
    const coded = alloc.pulses[b] > 0 || alloc.fineBits[b] > 0 || energyCoded[b];
    if (!coded) continue;
    let avgLogE = bandE[0][b];
    if (channels === 2) {
      avgLogE = 0.5 * (bandE[0][b] + bandE[1][b]);
    }
    const db = 6.0206 * (avgLogE + E_MEANS[b]) + CAL_DB + CELT_DEEMPH_DB[b];
    energyDb[b] = Math.max(DB_FLOOR, Math.min(6.0, db));
    bandTotalBits[b] = coarseBits[b] + fineBitsOut[b] + pvqBitsOut[b];
  }

  return {
    endBand,
    startBand,
    transient,
    shortBlocks,
    intra,
    spread,
    dynallocCount,
    intensityBand: intensity,
    dualStereo,
    postfilterGain,
    postfilterPeriod,
    energyDb,
    energyCoded,
    coarseBits,
    fineBits: fineBitsOut,
    pvqBits: pvqBitsOut,
    bandTotalBits,
    antiCollapse,
  };
}
