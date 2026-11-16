import { WATERFALL_BINS } from './spectrum';
import {
  ChannelStats,
  Fakten,
  OpusHead,
  OpusTags,
  PacketWalk,
  Sources,
  Summary,
  budgetSum,
  emptyBudget,
} from './types';

function median(vals: number[]): number {
  if (vals.length === 0) return 0;
  const sorted = [...vals].sort((a, b) => a - b);
  const mid = sorted.length >> 1;
  return (sorted.length & 1) !== 0
    ? sorted[mid]
    : 0.5 * (sorted[mid - 1] + sorted[mid]);
}

export function buildFakten(params: {
  fileSize: number;
  head: OpusHead;
  tags: OpusTags;
  packets: PacketWalk[];
  summary: Summary;
  sources: Sources;
  pcmStats: ChannelStats;
}): Fakten {
  const { fileSize, head, tags, packets, summary, sources, pcmStats } = params;

  const vendor = tags.vendor.trim();
  const optionsEntry = tags.comments.find(
    ([k]) => k.toUpperCase() === 'ENCODER_OPTIONS'
  );
  const optionsRaw = optionsEntry ? optionsEntry[1] : null;
  const libopusMatch = vendor.match(/libopus\s+([0-9]+\.[0-9]+[^\s,]*)/i);
  const libopusVersion = libopusMatch ? libopusMatch[1] : null;
  const encoderShort = vendor
    ? libopusVersion
      ? `libopus ${libopusVersion}`
      : vendor
    : 'unbekannt';
  const encoderFull = optionsRaw ? `${vendor} (${optionsRaw})` : vendor || 'unbekannt';

  // PCM reference bitrate: 48 kHz * 16-bit * channels (1536 kbps stereo / 768 kbps mono)
  const pcmBitrateKbps = 48 * 16 * head.channels;
  const avgKbps = summary.avgBitrateKbps;
  const compressionRatio = avgKbps > 0.01 ? pcmBitrateKbps / avgKbps : 0;

  // Silence ratio from STFT spectrum (hops where max bin <= -65 dBFS)
  let silentHops = 0;
  const hops = sources.hops;
  for (let h = 0; h < hops; h++) {
    let maxDb = -120;
    const row = h * WATERFALL_BINS;
    for (let b = 0; b < WATERFALL_BINS; b++) {
      const v = sources.spectrumDb[row + b];
      if (v > maxDb) maxDb = v;
    }
    if (maxDb <= -65.0) silentHops++;
  }
  const silentPct = hops > 0 ? (silentHops / hops) * 100 : 0;

  // Voice / Signal breakdown & pitch percentiles
  let voiced = 0;
  let unvoiced = 0;
  let inactive = 0;
  const pitches: number[] = [];
  const centroids: number[] = [];

  for (const hInfo of sources.hopInfos) {
    if (hInfo.centroidHz > 0) centroids.push(hInfo.centroidHz);
    if (hInfo.mode === 'SILK' || hInfo.mode === 'Hybrid') {
      if (hInfo.signalType === 'Voiced') {
        voiced++;
        if (hInfo.pitchHz !== null && hInfo.pitchHz >= 45 && hInfo.pitchHz <= 600) {
          pitches.push(hInfo.pitchHz);
        }
      } else if (hInfo.signalType === 'Unvoiced') {
        unvoiced++;
      } else {
        inactive++;
      }
    }
  }

  const silkHops = voiced + unvoiced + inactive;
  const voicedPct = silkHops > 0 ? (voiced / silkHops) * 100 : 0;
  const unvoicedPct = silkHops > 0 ? (unvoiced / silkHops) * 100 : 0;
  const inactivePct = silkHops > 0 ? (inactive / silkHops) * 100 : 0;

  let pitchMinHz: number | null = null;
  let pitchMedianHz: number | null = null;
  let pitchMaxHz: number | null = null;
  if (pitches.length > 0) {
    pitches.sort((a, b) => a - b);
    pitchMinHz = pitches[0];
    pitchMedianHz = median(pitches);
    pitchMaxHz = pitches[pitches.length - 1];
  }
  const centroidMedianHz = median(centroids);

  // Bit budget aggregation
  const tot = emptyBudget();
  let walkErrors = 0;
  let allBudgetsExact = true;
  let redSilkToCelt = 0;
  let redCeltToSilk = 0;
  let midOnlyCount = 0;
  let stereoSilkCount = 0;
  let lbrrCount = 0;

  for (const p of packets) {
    const b = p.budget;
    tot.init += b.init;
    tot.silkFlags += b.silkFlags;
    tot.silkLbrr += b.silkLbrr;
    tot.silkStereo += b.silkStereo;
    tot.silkSide += b.silkSide;
    tot.silkPulses += b.silkPulses;
    tot.redHeader += b.redHeader;
    tot.celtHeader += b.celtHeader;
    tot.celtEnergy += b.celtEnergy;
    tot.celtPvq += b.celtPvq;
    tot.redundancy += b.redundancy;
    tot.unused += b.unused;

    for (const f of p.frames) {
      if (f.error) walkErrors++;
      if (!f.plc && budgetSum(f.budget) !== f.bytes * 64) {
        allBudgetsExact = false;
      }
      if (f.redundancy) {
        if (f.redundancy.celtToSilk) redCeltToSilk++;
        else redSilkToCelt++;
      }
      if (f.silk) {
        if (f.silk.lbrr) lbrrCount++;
        if (p.meta.stereo) {
          stereoSilkCount++;
          if (f.silk.midOnly) midOnlyCount++;
        }
      }
    }
  }

  const grandTotal = Math.max(1, budgetSum(tot));
  const pct = (val: number) => (val / grandTotal) * 100;

  const totalFrames = packets.reduce((acc, p) => acc + p.frames.length, 0);
  const midOnlyPct = stereoSilkCount > 0 ? (midOnlyCount / stereoSilkCount) * 100 : 0;
  const lbrrPct = totalFrames > 0 ? (lbrrCount / totalFrames) * 100 : 0;
  const transientRatePerSec =
    summary.durationSec > 0 ? summary.transientFrames / summary.durationSec : 0;

  const silkHeadInit = packets
    .filter((p) => p.meta.mode !== 'CELT')
    .reduce((acc, p) => acc + p.budget.init, 0);
  const celtHeadInit = tot.init - silkHeadInit;

  return {
    fileSize,
    durationSec: summary.durationSec,
    avgKbps,
    compressionRatio,
    pcmBitrateKbps,
    encoderShort,
    encoderFull,
    libopusVersion,
    optionsRaw,
    pcmStats,
    silentPct,
    silentHops,
    voicedPct,
    unvoicedPct,
    inactivePct,
    pitchMinHz,
    pitchMedianHz,
    pitchMaxHz,
    centroidMedianHz,
    budgetTotals: tot,
    budgetPcts: {
      silkHead: pct(silkHeadInit + tot.silkFlags + tot.silkStereo + tot.silkSide),
      silkPulses: pct(tot.silkPulses),
      fec: pct(tot.silkLbrr),
      celtHead: pct(celtHeadInit + tot.redHeader + tot.celtHeader),
      energy: pct(tot.celtEnergy),
      pvq: pct(tot.celtPvq),
      redundancy: pct(tot.redundancy),
      unused: pct(tot.unused),
    },
    redundancySilkToCelt: redSilkToCelt,
    redundancyCeltToSilk: redCeltToSilk,
    midOnlyPct,
    lbrrPct,
    transientRatePerSec,
    walkErrors,
    allBudgetsExact,
  };
}
