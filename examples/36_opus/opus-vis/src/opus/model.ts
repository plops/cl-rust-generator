import { EBAND5MS, NB_EBANDS } from './celtTab';
import { DB_FLOOR, WATERFALL_BINS } from './spectrum';
import {
  Bandwidth,
  FrameWalk,
  HopInfo,
  Mode,
  PacketWalk,
  Sources,
  Summary,
  budgetSilkBits,
  emptyBudget,
} from './types';

// Colormap LUT helpers matching opus-vis palette
const HEAT_STOPS: Array<[number, [number, number, number]]> = [
  [0.0, [10, 10, 16]],
  [0.2, [62, 22, 86]],
  [0.4, [182, 42, 92]],
  [0.6, [242, 122, 42]],
  [0.8, [250, 222, 120]],
  [1.0, [255, 255, 240]],
];

export function heatRgb(norm01: number): [number, number, number] {
  const t = Math.max(0, Math.min(1, norm01));
  for (let i = 0; i < HEAT_STOPS.length - 1; i++) {
    const [t0, c0] = HEAT_STOPS[i];
    const [t1, c1] = HEAT_STOPS[i + 1];
    if (t <= t1) {
      const f = (t - t0) / (t1 - t0);
      return [
        Math.round(c0[0] + f * (c1[0] - c0[0])),
        Math.round(c0[1] + f * (c1[1] - c0[1])),
        Math.round(c0[2] + f * (c1[2] - c0[2])),
      ];
    }
  }
  return [255, 255, 240];
}

export function divergingRgb(db: number): [number, number, number] {
  if (Number.isNaN(db)) {
    return [74, 76, 84];
  }
  const t = Math.max(-1, Math.min(1, db / 12.0));
  if (t < 0) {
    const a = -t; // 0..1 towards -12 dB blue
    return [
      Math.round(10 + a * (40 - 10)),
      Math.round(10 + a * (110 - 10)),
      Math.round(16 + a * (220 - 16)),
    ];
  } else {
    const a = t; // 0..1 towards +12 dB orange/gold
    return [
      Math.round(10 + a * (255 - 10)),
      Math.round(10 + a * (170 - 10)),
      Math.round(16 + a * (60 - 16)),
    ];
  }
}

export const MODE_COLORS: Record<Mode, string> = {
  SILK: '#46AAFF',
  Hybrid: '#B978FF',
  CELT: '#FFB246',
};

export function buildSummary(packets: PacketWalk[]): Summary {
  const packetCount = packets.length;
  let totalBytes = 0;
  let durationMs = 0;
  const bitrates: number[] = [];

  const modeCounts: Record<Mode, number> = { SILK: 0, Hybrid: 0, CELT: 0 };
  const modeDurationsMs: Record<Mode, number> = { SILK: 0, Hybrid: 0, CELT: 0 };
  const bwCounts: Record<Bandwidth, number> = {
    NB: 0,
    MB: 0,
    WB: 0,
    SWB: 0,
    FB: 0,
  };
  const bwDurationsMs: Record<Bandwidth, number> = {
    NB: 0,
    MB: 0,
    WB: 0,
    SWB: 0,
    FB: 0,
  };

  let modeSwitches = 0;
  let bandwidthSwitches = 0;
  const frameSizeSet = new Set<number>();
  let stereoFrames = 0;
  let plcFrames = 0;
  let fecFrames = 0;
  let redundancyFrames = 0;
  let transientFrames = 0;

  for (const p of packets) {
    const m = p.meta;
    totalBytes += m.bytes;
    durationMs = Math.max(durationMs, m.tEndMs);
    bitrates.push(m.bitrateKbps);
    modeCounts[m.mode]++;
    modeDurationsMs[m.mode] += m.durationMs;
    bwCounts[m.bandwidth]++;
    bwDurationsMs[m.bandwidth] += m.durationMs;
    frameSizeSet.add(Number((m.samples / m.frameCount / 48).toFixed(2)));
    if (m.stereo) stereoFrames++;
    if (p.modeChanged) modeSwitches++;
    if (p.bandwidthChanged) bandwidthSwitches++;
    if (p.hasFec) fecFrames++;
    if (p.hasRedundancy) redundancyFrames++;
    if (p.hasTransient) transientFrames++;
    for (const f of p.frames) {
      if (f.plc) plcFrames++;
    }
  }

  bitrates.sort((a, b) => a - b);
  const minBitrateKbps = bitrates[0] ?? 0;
  const maxBitrateKbps = bitrates[bitrates.length - 1] ?? 0;
  const p5BitrateKbps = bitrates[Math.floor(bitrates.length * 0.05)] ?? 0;
  const p95BitrateKbps = bitrates[Math.floor(bitrates.length * 0.95)] ?? 0;
  const durationSec = durationMs / 1000;
  const avgBitrateKbps = durationMs > 0 ? (totalBytes * 8) / durationMs : 0;
  const vbrSpreadRatio =
    p5BitrateKbps > 0.1 ? p95BitrateKbps / p5BitrateKbps : 1;

  const modes: Summary['modes'] = (['SILK', 'Hybrid', 'CELT'] as Mode[])
    .filter((m) => modeCounts[m] > 0)
    .map((m) => ({
      mode: m,
      pct: durationMs > 0 ? (modeDurationsMs[m] / durationMs) * 100 : 0,
      seconds: modeDurationsMs[m] / 1000,
    }));

  const bandwidths: Summary['bandwidths'] = (
    ['NB', 'MB', 'WB', 'SWB', 'FB'] as Bandwidth[]
  )
    .filter((b) => bwCounts[b] > 0)
    .map((b) => ({
      band: b,
      pct: durationMs > 0 ? (bwDurationsMs[b] / durationMs) * 100 : 0,
      seconds: bwDurationsMs[b] / 1000,
    }));

  return {
    packetCount,
    totalBytes,
    durationMs,
    durationSec,
    avgBitrateKbps,
    minBitrateKbps,
    maxBitrateKbps,
    p5BitrateKbps,
    p95BitrateKbps,
    vbrSpreadRatio,
    modeCounts,
    modeDurationsMs,
    modes,
    modeSwitches,
    bandwidthCounts: bwCounts,
    bandwidthDurationsMs: bwDurationsMs,
    bandwidths,
    bandwidthSwitches,
    frameSizesMs: Array.from(frameSizeSet).sort((a, b) => a - b),
    stereoFrames,
    plcFrames,
    fecFrames,
    redundancyFrames,
    transientFrames,
  };
}

// Map each waterfall bin [0..255] to CELT band [0..20] (or -1 if above 20kHz)
function buildFreqToCeltBand(freqsHz: Float32Array): Int8Array {
  const map = new Int8Array(freqsHz.length);
  for (let i = 0; i < freqsHz.length; i++) {
    const hz = freqsHz[i];
    let found = -1;
    for (let b = 0; b < NB_EBANDS; b++) {
      const fLo = EBAND5MS[b] * 200;
      const fHi = EBAND5MS[b + 1] * 200;
      if (hz >= fLo && hz <= fHi) {
        found = b;
        break;
      }
    }
    if (found < 0 && hz < 200) found = 0;
    if (found < 0 && hz > 20000) found = 20;
    map[i] = found;
  }
  return map;
}

export function buildSources(params: {
  packets: PacketWalk[];
  freqsHz: Float32Array;
  spectrumDb: Float32Array;
  centroidsHz: Float32Array;
  hops: number;
}): Sources {
  const { packets, freqsHz, spectrumDb, centroidsHz, hops } = params;
  const hopMs = 20.0;
  const bins = WATERFALL_BINS;

  const bitstreamDb = new Float32Array(hops * bins).fill(DB_FLOOR);
  const divergenceDb = new Float32Array(hops * bins).fill(Number.NaN);
  const densityBits = new Float32Array(hops * bins).fill(0);
  const hopInfos: HopInfo[] = new Array(hops);

  const freqToCeltBand = buildFreqToCeltBand(freqsHz);
  // Count how many waterfall bins fall into each CELT band so per-band bits distribute cleanly
  const binsPerBand = new Int16Array(NB_EBANDS);
  for (let i = 0; i < bins; i++) {
    const b = freqToCeltBand[i];
    if (b >= 0) binsPerBand[b]++;
  }

  // Index packets by time for O(1) advancing cursor
  let pktCursor = 0;
  const densitySamples: number[] = [];

  for (let h = 0; h < hops; h++) {
    const tMs = h * hopMs;
    while (
      pktCursor + 1 < packets.length &&
      packets[pktCursor].meta.tEndMs <= tMs
    ) {
      pktCursor++;
    }
    const pkt = packets[pktCursor];

    if (!pkt) {
      hopInfos[h] = {
        tMs,
        packetIdx: 0,
        frameIdxInPacket: 0,
        mode: 'CELT',
        bandwidth: 'FB',
        bitrateKbps: 0,
        signalType: 'Inactive',
        pitchHz: null,
        gainDb: null,
        transient: false,
        redundancy: null,
        midOnly: false,
        lbrr: false,
        centroidHz: centroidsHz[h] ?? 0,
        budget: emptyBudget(),
        silkBandShare: 0,
      };
      continue;
    }

    const frameDurMs = pkt.meta.durationMs / Math.max(1, pkt.frames.length);
    const fIdx = Math.min(
      pkt.frames.length - 1,
      Math.max(0, Math.floor((tMs - pkt.meta.tStartMs) / Math.max(0.1, frameDurMs)))
    );
    const frame: FrameWalk | undefined = pkt.frames[fIdx];

    const mode = pkt.meta.mode;
    const ch0 = frame?.silk?.channels[0];
    const signalType = ch0?.signalType ?? pkt.dominantSignal;
    const pitchHz = ch0?.pitchHz ?? null;
    const gainDb = ch0?.gainDb ?? null;
    const transient =
      frame?.celt?.transient === true ||
      frame?.redundancy?.celt?.transient === true;
    const redundancy = frame?.redundancy ?? null;
    const midOnly = frame?.silk?.midOnly ?? false;
    const lbrr = frame?.silk?.lbrr ?? false;
    const budget = frame?.budget ?? pkt.budget;

    const totalB = Math.max(
      1,
      budget.silkFlags +
        budget.silkLbrr +
        budget.silkStereo +
        budget.silkSide +
        budget.silkPulses +
        budget.celtHeader +
        budget.celtEnergy +
        budget.celtPvq
    );
    const silkBandShare =
      (budgetSilkBits(budget) * 8) / totalB;

    hopInfos[h] = {
      tMs,
      packetIdx: pkt.meta.index,
      frameIdxInPacket: fIdx,
      mode,
      bandwidth: pkt.meta.bandwidth,
      bitrateKbps: pkt.meta.bitrateKbps,
      signalType,
      pitchHz,
      gainDb,
      transient,
      redundancy,
      midOnly,
      lbrr,
      centroidHz: centroidsHz[h] ?? 0,
      budget,
      silkBandShare,
    };

    const row = h * bins;
    const silkEnv = ch0?.lpcEnvDb ?? null;
    const celt = frame?.celt ?? null;

    // Precompute SILK spectral density weights (proportional to 10^(env/40))
    let silkWeightSum = 0;
    const silkWeights = silkEnv ? new Float32Array(bins) : null;
    if (silkEnv && silkWeights) {
      const silkNyq =
        pkt.meta.bandwidth === 'NB'
          ? 4000
          : pkt.meta.bandwidth === 'MB'
            ? 6000
            : 8000;
      for (let i = 0; i < bins; i++) {
        if (freqsHz[i] <= silkNyq) {
          const w = Math.pow(10, silkEnv[i] / 40);
          silkWeights[i] = w;
          silkWeightSum += w;
        }
      }
    }
    const silkTotalBitsForHop = budgetSilkBits(budget);

    for (let i = 0; i < bins; i++) {
      const spec = spectrumDb[row + i];
      const hz = freqsHz[i];
      const cBand = freqToCeltBand[i];

      let modelVal = DB_FLOOR;
      let hasModel = false;
      let dens = 0;

      if (mode === 'CELT' && celt && cBand >= 0 && cBand < celt.endBand) {
        if (celt.energyCoded[cBand]) {
          modelVal = celt.energyDb[cBand];
          hasModel = true;
        }
        const nBinsInBand = Math.max(1, binsPerBand[cBand]);
        dens = celt.bandTotalBits[cBand] / 8 / nBinsInBand;
      } else if (mode === 'SILK' && silkEnv) {
        const silkNyq =
          pkt.meta.bandwidth === 'NB'
            ? 4000
            : pkt.meta.bandwidth === 'MB'
              ? 6000
              : 8000;
        if (hz <= silkNyq) {
          modelVal = silkEnv[i];
          hasModel = true;
          if (silkWeights && silkWeightSum > 0) {
            dens = (silkTotalBitsForHop * silkWeights[i]) / silkWeightSum;
          }
        }
      } else if (mode === 'Hybrid') {
        if (hz <= 8000 && silkEnv) {
          modelVal = silkEnv[i];
          hasModel = true;
          if (silkWeights && silkWeightSum > 0) {
            dens = (silkTotalBitsForHop * silkWeights[i]) / silkWeightSum;
          }
        } else if (celt && cBand >= celt.startBand && cBand < celt.endBand) {
          if (celt.energyCoded[cBand]) {
            modelVal = celt.energyDb[cBand];
            hasModel = true;
          }
          const nBinsInBand = Math.max(1, binsPerBand[cBand]);
          dens = celt.bandTotalBits[cBand] / 8 / nBinsInBand;
        }
      }

      bitstreamDb[row + i] = modelVal;
      divergenceDb[row + i] = hasModel ? spec - modelVal : Number.NaN;
      densityBits[row + i] = dens;
      if (dens > 0) densitySamples.push(dens);
    }
  }

  // 99th percentile for Density heatmap scale
  let densityMaxP99 = 1.0;
  if (densitySamples.length > 0) {
    densitySamples.sort((a, b) => a - b);
    const idx = Math.min(
      densitySamples.length - 1,
      Math.floor(densitySamples.length * 0.99)
    );
    densityMaxP99 = Math.max(0.25, densitySamples[idx]);
  }

  return {
    bins,
    hops,
    hopMs,
    freqsHz,
    spectrumDb,
    bitstreamDb,
    divergenceDb,
    densityBits,
    densityMaxP99,
    hopInfos,
  };
}
