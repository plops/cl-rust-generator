import { decodeOpusAudio } from './decoder';
import { buildFakten } from './fakten';
import { buildSources, buildSummary } from './model';
import { demuxOggOpus } from './ogg';
import { buildGermanReport } from './report';
import { WATERFALL_BINS, buildLogFreqAxis, computeStft } from './spectrum';
import { OpusDocument, PacketWalk } from './types';
import { OpusWalker } from './walker';

export type AnalysisStage =
  | 'demux'
  | 'decode'
  | 'walk'
  | 'stft'
  | 'model'
  | 'done';

export interface AnalysisProgress {
  stage: AnalysisStage;
  label: string;
  pct: number;
}

export async function analyzeOpusBytes(
  fileName: string,
  bytes: Uint8Array,
  onProgress?: (p: AnalysisProgress) => void
): Promise<OpusDocument> {
  onProgress?.({ stage: 'demux', label: 'Ogg-Container wird zerlegt …', pct: 10 });
  const { head, tags, packets } = demuxOggOpus(bytes);

  onProgress?.({ stage: 'walk', label: `Bitstrom-Walk (${packets.length} Pakete) …`, pct: 32 });
  const freqsHz = buildLogFreqAxis(WATERFALL_BINS);
  const walker = new OpusWalker();
  const walkedPackets: PacketWalk[] = [];
  let cursorMs = 0;

  for (let i = 0; i < packets.length; i++) {
    const { packetWalk, nextMs } = walker.walkPacket(packets[i], cursorMs, freqsHz);
    walkedPackets.push(packetWalk);
    cursorMs = nextMs;
    if (i % 120 === 0 && i > 0) {
      // Yield to main thread briefly on very long files so UI stays responsive
      await new Promise((r) => setTimeout(r, 0));
    }
  }

  onProgress?.({ stage: 'decode', label: 'Opus-Audio (48 kHz WASM) wird dekodiert …', pct: 58 });
  const decoded = await decodeOpusAudio(bytes);

  onProgress?.({ stage: 'stft', label: '1024-Punkt STFT-Spektrogramm …', pct: 80 });
  const targetHops = Math.max(1, Math.ceil(cursorMs / 20.0));
  const stft = computeStft(decoded.mono, targetHops);

  onProgress?.({ stage: 'model', label: 'Wasserfall & Diagnose-Report …', pct: 92 });
  const summary = buildSummary(walkedPackets);
  const sources = buildSources({
    packets: walkedPackets,
    freqsHz: stft.freqsHz,
    spectrumDb: stft.spectrumDb,
    centroidsHz: stft.centroidsHz,
    hops: stft.hops,
  });

  const fakten = buildFakten({
    fileSize: bytes.length,
    head,
    tags,
    packets: walkedPackets,
    summary,
    sources,
    pcmStats: decoded.stats,
  });

  const reportText = buildGermanReport({
    fileName,
    head,
    tags,
    summary,
    fakten,
  });

  onProgress?.({ stage: 'done', label: 'Analyse abgeschlossen', pct: 100 });

  return {
    fileName,
    fileSize: bytes.length,
    head,
    tags,
    packets: walkedPackets,
    summary,
    sources,
    fakten,
    pcmChannels: decoded.channels,
    sampleRate: 48000,
    reportText,
    decodeErrors: decoded.errors,
  };
}
