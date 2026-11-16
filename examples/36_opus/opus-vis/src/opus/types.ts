export type Mode = 'SILK' | 'Hybrid' | 'CELT';

export type Bandwidth = 'NB' | 'MB' | 'WB' | 'SWB' | 'FB';

export type SignalType = 'Inactive' | 'Unvoiced' | 'Voiced';

export type ViewKind = 'spectrum' | 'bitstream' | 'divergence' | 'density';

export interface Budget {
  init: number;
  silkFlags: number;
  silkLbrr: number;
  silkStereo: number;
  silkSide: number;
  silkPulses: number;
  redHeader: number;
  celtHeader: number;
  celtEnergy: number;
  celtPvq: number;
  redundancy: number;
  unused: number;
}

export function emptyBudget(): Budget {
  return {
    init: 0,
    silkFlags: 0,
    silkLbrr: 0,
    silkStereo: 0,
    silkSide: 0,
    silkPulses: 0,
    redHeader: 0,
    celtHeader: 0,
    celtEnergy: 0,
    celtPvq: 0,
    redundancy: 0,
    unused: 0,
  };
}

export function budgetSum(b: Budget): number {
  return (
    b.init +
    b.silkFlags +
    b.silkLbrr +
    b.silkStereo +
    b.silkSide +
    b.silkPulses +
    b.redHeader +
    b.celtHeader +
    b.celtEnergy +
    b.celtPvq +
    b.redundancy +
    b.unused
  );
}

export function budgetSilkBits(b: Budget): number {
  return (b.silkFlags + b.silkLbrr + b.silkStereo + b.silkSide + b.silkPulses) / 8;
}

export function budgetCeltBits(b: Budget): number {
  return (b.celtHeader + b.celtEnergy + b.celtPvq) / 8;
}

export function budgetFecBits(b: Budget): number {
  return b.silkLbrr / 8;
}

export function budgetRedBits(b: Budget): number {
  return (b.redHeader + b.redundancy) / 8;
}

export interface SilkChannelFrame {
  signalType: SignalType;
  pitchHz: number | null;
  pitchLagSamples: number | null;
  gainDb: number;
  vad: boolean;
  rateLevel: number;
  pulsesTotal: number;
  nlsfQ15: number[];
  lpcOrder: number;
  lpcEnvDb: number[]; // length 256 (matching waterfall frequency bins) or fsKhz bins
}

export interface SilkOpusFrame {
  lbrr: boolean;
  lbrrChannels: number;
  midOnly: boolean;
  stereoPred: [number, number] | null;
  subframesSilkCount: number;
  channels: SilkChannelFrame[];
}

export interface CeltFrameWalk {
  endBand: number;
  startBand: number;
  transient: boolean;
  shortBlocks: number;
  intra: boolean;
  spread: number;
  dynallocCount: number;
  intensityBand: number;
  dualStereo: boolean;
  postfilterGain: number;
  postfilterPeriod: number;
  energyDb: number[]; // per band [0..20] in dBFS
  energyCoded: boolean[]; // per band
  coarseBits: number[]; // 1/8-bit per band
  fineBits: number[]; // 1/8-bit per band
  pvqBits: number[]; // 1/8-bit per band
  bandTotalBits: number[]; // 1/8-bit per band
  antiCollapse: boolean;
}

export interface RedundancyWalk {
  celtToSilk: boolean;
  bytes: number;
  celt: CeltFrameWalk | null;
}

export interface FrameWalk {
  frameIndex: number;
  bytes: number;
  plc: boolean;
  finalRange: number;
  silk: SilkOpusFrame | null;
  celt: CeltFrameWalk | null;
  redundancy: RedundancyWalk | null;
  budget: Budget;
  error?: string;
}

export interface OpusHead {
  version: number;
  channels: number;
  preSkip: number;
  inputSampleRate: number;
  outputGainDb: number;
  mappingFamily: number;
}

export interface OpusTags {
  vendor: string;
  comments: Array<[string, string]>;
}

export interface OggPacket {
  data: Uint8Array;
  granulePos: number;
  pageSeq: number;
  packetIndex: number;
}

export interface FrameMeta {
  index: number;
  tStartMs: number;
  tEndMs: number;
  durationMs: number;
  samples: number;
  bytes: number;
  bitrateKbps: number;
  tocByte: number;
  config: number;
  mode: Mode;
  bandwidth: Bandwidth;
  stereo: boolean;
  code: number;
  frameCount: number;
  vbr: boolean;
  paddingBytes: number;
}

export interface PacketWalk {
  meta: FrameMeta;
  frames: FrameWalk[];
  budget: Budget;
  modeChanged: boolean;
  bandwidthChanged: boolean;
  hasRedundancy: boolean;
  hasFec: boolean;
  hasTransient: boolean;
  dominantSignal: SignalType;
  pitchHz: number | null;
  gainDb: number | null;
  silkSharePct: number;
  celtSharePct: number;
  fecSharePct: number;
  redSharePct: number;
  unusedSharePct: number;
}

export interface ChannelStats {
  channels: number;
  samplesPerChannel: number;
  durationSec: number;
  peakDbfs: number;
  rmsDbfs: number;
  crestDb: number;
  clippedSamples: number;
  stereoCorr: number | null;
  midDb: number | null;
  sideDb: number | null;
}

export interface BandSummary {
  band: Bandwidth;
  pct: number;
  seconds: number;
}

export interface ModeSummary {
  mode: Mode;
  pct: number;
  seconds: number;
}

export interface Summary {
  packetCount: number;
  totalBytes: number;
  durationMs: number;
  durationSec: number;
  avgBitrateKbps: number;
  minBitrateKbps: number;
  maxBitrateKbps: number;
  p5BitrateKbps: number;
  p95BitrateKbps: number;
  vbrSpreadRatio: number;
  modeCounts: Record<Mode, number>;
  modeDurationsMs: Record<Mode, number>;
  modes: ModeSummary[];
  modeSwitches: number;
  bandwidthCounts: Record<Bandwidth, number>;
  bandwidthDurationsMs: Record<Bandwidth, number>;
  bandwidths: BandSummary[];
  bandwidthSwitches: number;
  frameSizesMs: number[];
  stereoFrames: number;
  plcFrames: number;
  fecFrames: number;
  redundancyFrames: number;
  transientFrames: number;
}

export interface HopInfo {
  tMs: number;
  packetIdx: number;
  frameIdxInPacket: number;
  mode: Mode;
  bandwidth: Bandwidth;
  bitrateKbps: number;
  signalType: SignalType;
  pitchHz: number | null;
  gainDb: number | null;
  transient: boolean;
  redundancy: RedundancyWalk | null;
  midOnly: boolean;
  lbrr: boolean;
  centroidHz: number;
  budget: Budget;
  silkBandShare: number;
}

export interface Sources {
  bins: number; // 256
  hops: number;
  hopMs: number; // 20.0
  freqsHz: Float32Array;
  spectrumDb: Float32Array; // [hops * 256]
  bitstreamDb: Float32Array; // [hops * 256]
  divergenceDb: Float32Array; // [hops * 256], NaN where uncoded/no model
  densityBits: Float32Array; // [hops * 256]
  densityMaxP99: number;
  hopInfos: HopInfo[];
}

export interface Fakten {
  fileSize: number;
  durationSec: number;
  avgKbps: number;
  compressionRatio: number;
  pcmBitrateKbps: number;
  encoderShort: string;
  encoderFull: string;
  libopusVersion: string | null;
  optionsRaw: string | null;
  pcmStats: ChannelStats;
  silentPct: number;
  silentHops: number;
  voicedPct: number;
  unvoicedPct: number;
  inactivePct: number;
  pitchMinHz: number | null;
  pitchMedianHz: number | null;
  pitchMaxHz: number | null;
  centroidMedianHz: number;
  budgetTotals: Budget;
  budgetPcts: {
    silkHead: number;
    silkPulses: number;
    fec: number;
    celtHead: number;
    energy: number;
    pvq: number;
    redundancy: number;
    unused: number;
  };
  redundancySilkToCelt: number;
  redundancyCeltToSilk: number;
  midOnlyPct: number;
  lbrrPct: number;
  transientRatePerSec: number;
  walkErrors: number;
  allBudgetsExact: boolean;
}

export interface OpusDocument {
  fileName: string;
  fileSize: number;
  head: OpusHead;
  tags: OpusTags;
  packets: PacketWalk[];
  summary: Summary;
  sources: Sources;
  fakten: Fakten;
  pcmChannels: Float32Array[];
  sampleRate: 48000;
  reportText: string;
  decodeErrors: string[];
}
