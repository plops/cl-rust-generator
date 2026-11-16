import { RangeDec } from './range';
import {
  SILK_LSB_ICDF,
  SILK_PULSES_PER_BLOCK_ICDF,
  SILK_RATE_LEVELS_ICDF,
  SILK_SHELL_SPLIT_ICDF,
  SILK_SIGNS_ICDF,
} from './silkTab';
import { SignalType } from './types';

function decodeShellSplit(
  rc: RangeDec,
  totalPulses: number,
  maxBits: number
): [number, number] {
  if (totalPulses <= 0) return [0, 0];
  if (rc.tell() + 8 > maxBits) return [totalPulses >> 1, totalPulses - (totalPulses >> 1)];
  // Binomial-like split using truncated shell ICDF
  const len = Math.min(totalPulses + 1, SILK_SHELL_SPLIT_ICDF.length);
  const icdf = new Array<number>(len);
  const top = len > 1 ? SILK_SHELL_SPLIT_ICDF[len - 2] : 256;
  for (let i = 0; i < len - 1; i++) {
    const scaled = Math.round(
      ((SILK_SHELL_SPLIT_ICDF[i] - top) / Math.max(1, 256 - top)) * 255
    );
    icdf[i] = Math.max(1, Math.min(255, scaled));
  }
  icdf[len - 1] = 0;
  const p1 = Math.min(totalPulses, rc.decodeIcdf(icdf, 8));
  return [p1, totalPulses - p1];
}

export function walkSilkPulses(params: {
  rc: RangeDec;
  signalType: SignalType;
  quantOffsetType: number;
  frameLengthSamples: number; // at SILK fs (80/160 for NB, 120/240 for MB, 160/320 for WB)
  maxFrameBytes: number;
}): { rateLevel: number; pulsesTotal: number } {
  const { rc, signalType, quantOffsetType, frameLengthSamples, maxFrameBytes } =
    params;
  const maxBits = maxFrameBytes * 8;
  if (rc.tell() + 4 > maxBits) {
    return { rateLevel: 0, pulsesTotal: 0 };
  }

  const rateIdxType = signalType === 'Voiced' ? 1 : 0;
  const rateLevel = Math.min(
    8,
    rc.decodeIcdf(SILK_RATE_LEVELS_ICDF[rateIdxType], 8)
  );

  // Number of 16-sample shell blocks (rounded up to multiple of 16)
  const nBlocks = Math.ceil(frameLengthSamples / 16);
  const sumPulses = new Array<number>(nBlocks).fill(0);
  const nLsb = new Array<number>(nBlocks).fill(0);
  let pulsesTotal = 0;

  for (let b = 0; b < nBlocks; b++) {
    if (rc.tell() + 4 > maxBits) break;
    let sp = rc.decodeIcdf(SILK_PULSES_PER_BLOCK_ICDF[rateLevel], 8);
    if (sp >= 17) {
      let lsb = 0;
      while (lsb < 10 && rc.tell() + 4 <= maxBits) {
        lsb++;
        const ext = rc.decodeIcdf(SILK_PULSES_PER_BLOCK_ICDF[9], 8);
        if (ext < 17) {
          sp = ext;
          break;
        }
      }
      nLsb[b] = lsb;
      sumPulses[b] = sp << lsb;
    } else {
      sumPulses[b] = sp;
    }
    pulsesTotal += sumPulses[b];
  }

  // Decode shell coding binary splits (16 -> 8 -> 4 -> 2 -> 1)
  const pulses16 = new Array<number>(16);
  const nonzeroBlocks: number[][] = [];
  for (let b = 0; b < nBlocks; b++) {
    const sp = sumPulses[b] >> nLsb[b];
    if (sp <= 0) {
      nonzeroBlocks.push([]);
      continue;
    }
    const [p0_7, p8_15] = decodeShellSplit(rc, sp, maxBits);
    const [p0_3, p4_7] = decodeShellSplit(rc, p0_7, maxBits);
    const [p8_11, p12_15] = decodeShellSplit(rc, p8_15, maxBits);
    const quads = [p0_3, p4_7, p8_11, p12_15];
    let idx = 0;
    for (let q = 0; q < 4; q++) {
      const [pa, pb] = decodeShellSplit(rc, quads[q], maxBits);
      const [s0, s1] = decodeShellSplit(rc, pa, maxBits);
      const [s2, s3] = decodeShellSplit(rc, pb, maxBits);
      pulses16[idx++] = s0;
      pulses16[idx++] = s1;
      pulses16[idx++] = s2;
      pulses16[idx++] = s3;
    }
    nonzeroBlocks.push([...pulses16]);
  }

  // Decode LSBs
  for (let b = 0; b < nBlocks; b++) {
    const lsbCount = nLsb[b];
    if (lsbCount <= 0) continue;
    const arr = nonzeroBlocks[b];
    for (let k = 0; k < arr.length; k++) {
      if (arr[k] > 0 || lsbCount > 0) {
        for (let bit = 0; bit < lsbCount; bit++) {
          if (rc.tell() + 1 > maxBits) break;
          const bVal = rc.decodeIcdf(SILK_LSB_ICDF, 8);
          arr[k] = (arr[k] << 1) | bVal;
        }
      }
    }
  }

  // Decode signs for each non-zero pulse
  const sigOffset =
    (signalType === 'Voiced' ? 4 : signalType === 'Unvoiced' ? 2 : 0) +
    quantOffsetType;
  const signIcdf = SILK_SIGNS_ICDF[Math.min(sigOffset, SILK_SIGNS_ICDF.length - 1)];
  for (let b = 0; b < nBlocks; b++) {
    const arr = nonzeroBlocks[b];
    for (let k = 0; k < arr.length; k++) {
      if (arr[k] > 0) {
        if (rc.tell() + 1 > maxBits) break;
        rc.decodeIcdf(signIcdf, 8);
      }
    }
  }

  return { rateLevel, pulsesTotal };
}
