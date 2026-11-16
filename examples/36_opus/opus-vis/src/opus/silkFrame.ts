import { RangeDec } from './range';
import { walkSilkPulses } from './silkPulse';
import {
  SILK_DELTA_GAIN_ICDF,
  SILK_GAIN_ICDF,
  SILK_LTP_GAIN_ICDF_0,
  SILK_LTP_GAIN_ICDF_1,
  SILK_LTP_GAIN_ICDF_2,
  SILK_LTP_PER_INDEX_ICDF,
  SILK_LTP_SCALE_ICDF,
  SILK_NLSF_CB1_ICDF_NB_MB,
  SILK_NLSF_CB1_ICDF_WB,
  SILK_NLSF_EXT_ICDF,
  SILK_NLSF_INTERP_ICDF,
  SILK_NLSF_STAGE2_ICDF,
  SILK_PITCH_CONTOUR_10_ICDF,
  SILK_PITCH_CONTOUR_20_ICDF,
  SILK_PITCH_CONTOUR_NB_10_ICDF,
  SILK_PITCH_CONTOUR_NB_20_ICDF,
  SILK_PITCH_DELTA_ICDF,
  SILK_PITCH_LAG_ICDF,
  SILK_SEED_ICDF,
  SILK_TYPE_OFFSET_NO_VAD_ICDF,
  SILK_TYPE_OFFSET_VAD_ICDF,
  SILK_UNIFORM4_ICDF,
  SILK_UNIFORM6_ICDF,
  SILK_UNIFORM8_ICDF,
  computeSilkLpcEnvelopeDb,
  getNlsfStage1BaseQ15,
} from './silkTab';
import { Budget, SignalType, SilkChannelFrame } from './types';

export interface SilkChannelState {
  lastGainIndex: number;
  ecPrevSignalType: SignalType;
  ecPrevLagIndex: number;
}

export function createSilkChannelState(): SilkChannelState {
  return {
    lastGainIndex: 10,
    ecPrevSignalType: 'Inactive',
    ecPrevLagIndex: 0,
  };
}

export function walkSilkChannelFrame(params: {
  rc: RangeDec;
  state: SilkChannelState;
  vad: boolean;
  fsKhz: 8 | 12 | 16;
  nbSubframes: 2 | 4;
  condCoding: boolean; // true if conditional on previous 20ms frame in packet
  maxFrameBytes: number;
  freqsHz: Float32Array;
  budget: Budget;
  isLbrr?: boolean;
}): SilkChannelFrame {
  const {
    rc,
    state,
    vad,
    fsKhz,
    nbSubframes,
    condCoding,
    maxFrameBytes,
    freqsHz,
    budget,
    isLbrr = false,
  } = params;

  const maxBits = maxFrameBytes * 8;
  const chargeSide = (delta: number) => {
    if (isLbrr) budget.silkLbrr += delta;
    else budget.silkSide += delta;
  };
  const chargePulses = (delta: number) => {
    if (isLbrr) budget.silkLbrr += delta;
    else budget.silkPulses += delta;
  };

  const tSideStart = rc.tellFrac();

  // 1. Frame type + quantization offset (RFC 6716 §4.2.7.1)
  let signalType: SignalType = 'Inactive';
  let quantOffsetType = 0;
  if (rc.tell() + 2 <= maxBits) {
    if (!vad) {
      signalType = 'Inactive';
      quantOffsetType = rc.decodeIcdf(SILK_TYPE_OFFSET_NO_VAD_ICDF, 8);
    } else {
      const idx = rc.decodeIcdf(SILK_TYPE_OFFSET_VAD_ICDF, 8);
      signalType = idx < 2 ? 'Unvoiced' : 'Voiced';
      quantOffsetType = idx & 1;
    }
  }

  // 2. Gains (RFC 6716 §4.2.7.2)
  const stIdx = signalType === 'Inactive' ? 0 : signalType === 'Unvoiced' ? 1 : 2;
  let lastGainIndex = state.lastGainIndex;
  const gainsIdx = new Array<number>(nbSubframes).fill(lastGainIndex);

  for (let i = 0; i < nbSubframes; i++) {
    if (rc.tell() + 4 > maxBits) break;
    if (i === 0 && !condCoding) {
      const msb = rc.decodeIcdf(SILK_GAIN_ICDF[stIdx], 8);
      const lsb = rc.tell() + 3 <= maxBits ? rc.decodeIcdf(SILK_UNIFORM8_ICDF, 8) : 0;
      lastGainIndex = (msb << 3) + lsb;
    } else {
      const delta = rc.decodeIcdf(SILK_DELTA_GAIN_ICDF, 8);
      lastGainIndex = Math.max(
        lastGainIndex + delta - 4,
        lastGainIndex + delta - 16 < 0 ? 0 : lastGainIndex
      );
      // RFC 6716 exact rule: double-step clamp for delta=0..4 vs >4
      if (delta === 0) {
        lastGainIndex = Math.max(0, state.lastGainIndex - 4);
      } else {
        lastGainIndex = Math.max(0, Math.min(63, state.lastGainIndex + delta - 4));
      }
    }
    gainsIdx[i] = lastGainIndex;
  }
  state.lastGainIndex = lastGainIndex;

  // Convert SILK gain index (0..63) to approximate dB (log gain range ~2 dB..88 dB)
  const avgGainIdx =
    gainsIdx.reduce((a, b) => a + b, 0) / Math.max(1, gainsIdx.length);
  const gainDb = (avgGainIdx * 88) / 63;

  // 3. NLSF Stage 1 & Stage 2 + Interpolation (RFC 6716 §4.2.7.3)
  const lpcOrder = fsKhz === 16 ? 16 : 10;
  const cb1Icdf =
    fsKhz === 16 ? SILK_NLSF_CB1_ICDF_WB : SILK_NLSF_CB1_ICDF_NB_MB;
  const nlsfType = signalType === 'Voiced' ? 1 : 0;
  let cb1Idx = 0;
  if (rc.tell() + 5 <= maxBits) {
    cb1Idx = rc.decodeIcdf(cb1Icdf[nlsfType], 8);
  }

  const nlsfQ15 = getNlsfStage1BaseQ15(lpcOrder, cb1Idx);
  for (let i = 0; i < lpcOrder; i++) {
    if (rc.tell() + 3 > maxBits) break;
    let res = rc.decodeIcdf(SILK_NLSF_STAGE2_ICDF, 8) - 4;
    if (res === -4 && rc.tell() + 2 <= maxBits) {
      res -= rc.decodeIcdf(SILK_NLSF_EXT_ICDF, 8);
    } else if (res === 4 && rc.tell() + 2 <= maxBits) {
      res += rc.decodeIcdf(SILK_NLSF_EXT_ICDF, 8);
    }
    nlsfQ15[i] = Math.max(200, Math.min(32500, nlsfQ15[i] + res * 220));
  }
  // Ensure strictly monotonic NLSFs for stable LPC
  for (let i = 1; i < lpcOrder; i++) {
    if (nlsfQ15[i] <= nlsfQ15[i - 1] + 64) {
      nlsfQ15[i] = Math.min(32600, nlsfQ15[i - 1] + 64);
    }
  }

  if (nbSubframes === 4 && rc.tell() + 2 <= maxBits) {
    rc.decodeIcdf(SILK_NLSF_INTERP_ICDF, 8);
  }

  // 4. Pitch lag & LTP if Voiced (RFC 6716 §4.2.7.4 - 4.2.7.5)
  let pitchLagSamples: number | null = null;
  let pitchHz: number | null = null;

  if (signalType === 'Voiced') {
    const minLag = 2 * fsKhz; // 16, 24, 32
    const maxLag = 18 * fsKhz; // 144, 216, 288
    const step = fsKhz >> 1; // 4, 6, 8

    let lagIndex = state.ecPrevLagIndex;
    if (!condCoding || state.ecPrevSignalType !== 'Voiced') {
      if (rc.tell() + 5 <= maxBits) {
        const high = rc.decodeIcdf(SILK_PITCH_LAG_ICDF, 8);
        let low = 0;
        if (fsKhz === 8 && rc.tell() + 2 <= maxBits) {
          low = rc.decodeIcdf(SILK_UNIFORM4_ICDF, 8);
        } else if (fsKhz === 12 && rc.tell() + 3 <= maxBits) {
          low = rc.decodeIcdf(SILK_UNIFORM6_ICDF, 8);
        } else if (fsKhz === 16 && rc.tell() + 3 <= maxBits) {
          low = rc.decodeIcdf(SILK_UNIFORM8_ICDF, 8);
        }
        lagIndex = high * step + low;
      }
    } else if (rc.tell() + 3 <= maxBits) {
      const delta = rc.decodeIcdf(SILK_PITCH_DELTA_ICDF, 8) - 9;
      lagIndex = Math.max(0, Math.min(maxLag - minLag, lagIndex + delta));
    }

    state.ecPrevLagIndex = lagIndex;
    pitchLagSamples = minLag + lagIndex;
    pitchHz = (fsKhz * 1000) / Math.max(1, pitchLagSamples);

    // Pitch contour index
    if (rc.tell() + 3 <= maxBits) {
      const contourIcdf =
        fsKhz === 8
          ? nbSubframes === 2
            ? SILK_PITCH_CONTOUR_NB_10_ICDF
            : SILK_PITCH_CONTOUR_NB_20_ICDF
          : nbSubframes === 2
            ? SILK_PITCH_CONTOUR_10_ICDF
            : SILK_PITCH_CONTOUR_20_ICDF;
      rc.decodeIcdf(contourIcdf, 8);
    }

    // LTP periodicity + filter taps
    let perIdx = 1;
    if (rc.tell() + 2 <= maxBits) {
      perIdx = rc.decodeIcdf(SILK_LTP_PER_INDEX_ICDF, 8);
    }
    const ltpIcdf =
      perIdx === 0
        ? SILK_LTP_GAIN_ICDF_0
        : perIdx === 1
          ? SILK_LTP_GAIN_ICDF_1
          : SILK_LTP_GAIN_ICDF_2;
    for (let k = 0; k < nbSubframes; k++) {
      if (rc.tell() + 3 > maxBits) break;
      rc.decodeIcdf(ltpIcdf, 8);
    }

    if (!condCoding && rc.tell() + 2 <= maxBits) {
      rc.decodeIcdf(SILK_LTP_SCALE_ICDF, 8);
    }
  }

  state.ecPrevSignalType = signalType;

  // 5. Seed
  if (rc.tell() + 2 <= maxBits) {
    rc.decodeIcdf(SILK_SEED_ICDF, 8);
  }

  chargeSide(Math.max(0, rc.tellFrac() - tSideStart));

  // 6. Excitation Pulses
  const tPulseStart = rc.tellFrac();
  const subframeSamples = 5 * fsKhz; // 40, 60, 80
  const frameLengthSamples = subframeSamples * nbSubframes;
  const { rateLevel, pulsesTotal } = walkSilkPulses({
    rc,
    signalType,
    quantOffsetType,
    frameLengthSamples,
    maxFrameBytes,
  });
  chargePulses(Math.max(0, rc.tellFrac() - tPulseStart));

  const lpcEnvDb = computeSilkLpcEnvelopeDb(nlsfQ15, fsKhz, freqsHz, gainDb);

  return {
    signalType,
    pitchHz,
    pitchLagSamples,
    gainDb,
    vad,
    rateLevel,
    pulsesTotal,
    nlsfQ15,
    lpcOrder,
    lpcEnvDb,
  };
}
