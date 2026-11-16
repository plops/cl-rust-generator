import { Bandwidth, Mode } from './types';

export interface TocInfo {
  tocByte: number;
  config: number;
  mode: Mode;
  bandwidth: Bandwidth;
  samplesPerFrame: number; // at 48 kHz
  durationMs: number;
  stereo: boolean;
  code: number; // 0..3
}

const FRAME_SAMPLES_48K: number[] = [
  // 0..3: SILK NB
  480, 960, 1920, 2880,
  // 4..7: SILK MB
  480, 960, 1920, 2880,
  // 8..11: SILK WB
  480, 960, 1920, 2880,
  // 12..13: Hybrid SWB
  480, 960,
  // 14..15: Hybrid FB
  480, 960,
  // 16..19: CELT NB
  120, 240, 480, 960,
  // 20..23: CELT WB
  120, 240, 480, 960,
  // 24..27: CELT SWB
  120, 240, 480, 960,
  // 28..31: CELT FB
  120, 240, 480, 960,
];

const BANDWIDTH_BY_CONFIG: Bandwidth[] = [
  'NB', 'NB', 'NB', 'NB',
  'MB', 'MB', 'MB', 'MB',
  'WB', 'WB', 'WB', 'WB',
  'SWB', 'SWB',
  'FB', 'FB',
  'NB', 'NB', 'NB', 'NB',
  'WB', 'WB', 'WB', 'WB',
  'SWB', 'SWB', 'SWB', 'SWB',
  'FB', 'FB', 'FB', 'FB',
];

export function parseToc(tocByte: number): TocInfo {
  const config = (tocByte >> 3) & 0x1f;
  const stereo = (tocByte & 0x04) !== 0;
  const code = tocByte & 0x03;

  let mode: Mode;
  if (config < 12) mode = 'SILK';
  else if (config < 16) mode = 'Hybrid';
  else mode = 'CELT';

  const bandwidth = BANDWIDTH_BY_CONFIG[config];
  const samplesPerFrame = FRAME_SAMPLES_48K[config];
  const durationMs = samplesPerFrame / 48;

  return {
    tocByte,
    config,
    mode,
    bandwidth,
    samplesPerFrame,
    durationMs,
    stereo,
    code,
  };
}

export function bandwidthMaxHz(bw: Bandwidth): number {
  switch (bw) {
    case 'NB':
      return 4000;
    case 'MB':
      return 6000;
    case 'WB':
      return 8000;
    case 'SWB':
      return 12000;
    case 'FB':
      return 20000;
  }
}

export function bandwidthEndBand(bw: Bandwidth): number {
  switch (bw) {
    case 'NB':
      return 13;
    case 'MB':
      return 17;
    case 'WB':
      return 17;
    case 'SWB':
      return 19;
    case 'FB':
      return 21;
  }
}

export function silkBandwidthKhz(bw: Bandwidth): 8 | 12 | 16 {
  switch (bw) {
    case 'NB':
      return 8;
    case 'MB':
      return 12;
    case 'WB':
    case 'SWB':
    case 'FB':
      return 16;
  }
}
