import { OggOpusDecoder } from 'ogg-opus-decoder';
import { ChannelStats } from './types';

export interface DecodedAudioResult {
  channels: Float32Array[];
  mono: Float32Array;
  samplesPerChannel: number;
  sampleRate: 48000;
  stats: ChannelStats;
  errors: string[];
}

export async function decodeOpusAudio(
  fileBytes: Uint8Array,
  expectedDurationSamples?: number
): Promise<DecodedAudioResult> {
  const decoder = new OggOpusDecoder();
  try {
    await decoder.ready;
    const decoded = await decoder.decodeFile(fileBytes);
    const rawChannels = decoded.channelData;
    const errors = (decoded.errors ?? []).map((e) => e.message || String(e));

    if (!rawChannels || rawChannels.length === 0) {
      throw new Error('WASM-Opus-Decoder lieferte keine Audiokanäle.');
    }

    let nSamples = rawChannels[0].length;
    // Align length cleanly if decoder returned trailing zero-pad or slight mismatch
    if (expectedDurationSamples && expectedDurationSamples > 0) {
      const maxAllow = Math.max(nSamples, expectedDurationSamples);
      void maxAllow;
    }

    const chCount = Math.min(2, rawChannels.length);
    const channels: Float32Array[] = [];
    for (let c = 0; c < chCount; c++) {
      channels.push(rawChannels[c].slice(0, nSamples));
    }

    const mono = new Float32Array(nSamples);
    if (chCount === 1) {
      mono.set(channels[0]);
    } else {
      const l = channels[0];
      const r = channels[1];
      for (let i = 0; i < nSamples; i++) {
        mono[i] = 0.5 * (l[i] + r[i]);
      }
    }

    const stats = computeChannelStats(channels);

    return {
      channels,
      mono,
      samplesPerChannel: nSamples,
      sampleRate: 48000,
      stats,
      errors,
    };
  } finally {
    decoder.free();
  }
}

export function computeChannelStats(channels: Float32Array[]): ChannelStats {
  const chCount = channels.length;
  const nSamples = channels[0]?.length ?? 0;
  const durationSec = nSamples / 48000;

  let peak = 0;
  let sumSq = 0;
  let clippedSamples = 0;
  const totalCount = chCount * nSamples;

  for (let c = 0; c < chCount; c++) {
    const ch = channels[c];
    for (let i = 0; i < nSamples; i++) {
      const v = ch[i];
      const av = v < 0 ? -v : v;
      if (av > peak) peak = av;
      if (av >= 0.999) clippedSamples++;
      sumSq += v * v;
    }
  }

  const rms = totalCount > 0 ? Math.sqrt(sumSq / totalCount) : 0;
  const peakDbfs = peak > 1e-10 ? 20 * Math.log10(peak) : -120;
  const rmsDbfs = rms > 1e-10 ? 20 * Math.log10(rms) : -120;
  const crestDb = peakDbfs - rmsDbfs;

  let stereoCorr: number | null = null;
  let midDb: number | null = null;
  let sideDb: number | null = null;

  if (chCount === 2 && nSamples > 0) {
    const l = channels[0];
    const r = channels[1];
    let ll = 0;
    let rr = 0;
    let lr = 0;
    let mm = 0;
    let ss = 0;
    for (let i = 0; i < nSamples; i++) {
      const a = l[i];
      const b = r[i];
      ll += a * a;
      rr += b * b;
      lr += a * b;
      const m = 0.5 * (a + b);
      const s = 0.5 * (a - b);
      mm += m * m;
      ss += s * s;
    }
    const denom = Math.sqrt(ll * rr);
    stereoCorr = denom > 1e-12 ? lr / denom : 0;
    midDb = mm / nSamples > 1e-12 ? 10 * Math.log10(mm / nSamples) : -120;
    sideDb = ss / nSamples > 1e-12 ? 10 * Math.log10(ss / nSamples) : -120;
  }

  return {
    channels: chCount,
    samplesPerChannel: nSamples,
    durationSec,
    peakDbfs,
    rmsDbfs,
    crestDb,
    clippedSamples,
    stereoCorr,
    midDb,
    sideDb,
  };
}
