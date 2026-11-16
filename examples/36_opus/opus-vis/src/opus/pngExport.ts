import { MODE_COLORS, divergingRgb, heatRgb } from './model';
import { DB_FLOOR, WATERFALL_BINS, hzToYFrac } from './spectrum';
import { Mode, OpusDocument, ViewKind, budgetCeltBits, budgetFecBits, budgetRedBits, budgetSilkBits } from './types';

export interface PngExportOptions {
  doc: OpusDocument;
  view: ViewKind;
  t0Ms: number;
  t1Ms: number;
}

const VIEW_LABELS: Record<ViewKind, string> = {
  spectrum: 'Spektrum (dBFS)',
  bitstream: 'Bitstrom-Modell (dBFS)',
  divergence: 'Divergenz (STFT - Bitstrom, ±12 dB)',
  density: 'Bit-Dichte (Bits / Bin)',
};

export async function renderWaterfallPngBlob(
  opts: PngExportOptions
): Promise<Blob> {
  const { doc, view, t0Ms, t1Ms } = opts;
  const { sources, summary } = doc;

  const width = 1600;
  const height = 980;
  const padL = 68;
  const padR = 24;
  const padT = 56;
  const plotW = width - padL - padR;

  const wfH = 540;
  const axisH = 26;
  const modeH = 22;
  const bitrateH = 74;
  const f0H = 74;
  const budgetH = 84;
  const gap = 12;

  const yWf = padT;
  const yAxis = yWf + wfH;
  const yMode = yAxis + axisH + 4;
  const yBitrate = yMode + modeH + gap;
  const yF0 = yBitrate + bitrateH + gap;
  const yBudget = yF0 + f0H + gap;

  const canvas = document.createElement('canvas');
  canvas.width = width;
  canvas.height = height;
  const ctx = canvas.getContext('2d');
  if (!ctx) {
    throw new Error('2D-Canvas-Kontext für PNG-Export nicht verfügbar.');
  }

  // Obsidian background
  ctx.fillStyle = '#0B0E17';
  ctx.fillRect(0, 0, width, height);

  // Header strip
  ctx.fillStyle = '#F1F5F9';
  ctx.font = '600 18px "Plus Jakarta Sans", sans-serif';
  ctx.fillText(`Opus-Vis · ${doc.fileName}`, padL, 30);

  ctx.fillStyle = '#94A3B8';
  ctx.font = '400 13px "JetBrains Mono", monospace';
  const metaText = `${VIEW_LABELS[view]}  |  ${summary.avgBitrateKbps.toFixed(1)} kbps avg  |  ${(t0Ms / 1000).toFixed(2)}s – ${(t1Ms / 1000).toFixed(2)}s`;
  ctx.fillText(metaText, padL, 48);

  // Render Waterfall pixels via ImageData
  const img = ctx.createImageData(plotW, wfH);
  const px = img.data;
  const spanMs = Math.max(1, t1Ms - t0Ms);
  const hopStart = Math.max(0, Math.floor(t0Ms / sources.hopMs));
  const hopEnd = Math.min(sources.hops - 1, Math.ceil(t1Ms / sources.hopMs));

  for (let x = 0; x < plotW; x++) {
    const t = t0Ms + ((x + 0.5) / plotW) * spanMs;
    const h = Math.max(
      hopStart,
      Math.min(hopEnd, Math.floor(t / sources.hopMs))
    );
    const rowOff = h * WATERFALL_BINS;

    for (let y = 0; y < wfH; y++) {
      const binFrac = (1 - y / (wfH - 1)) * (WATERFALL_BINS - 1);
      const b = Math.max(0, Math.min(WATERFALL_BINS - 1, Math.round(binFrac)));
      let rgb: [number, number, number];

      if (view === 'spectrum') {
        const db = sources.spectrumDb[rowOff + b];
        rgb = heatRgb((db - DB_FLOOR) / (0 - DB_FLOOR));
      } else if (view === 'bitstream') {
        const db = sources.bitstreamDb[rowOff + b];
        rgb = heatRgb((db - DB_FLOOR) / (0 - DB_FLOOR));
      } else if (view === 'divergence') {
        rgb = divergingRgb(sources.divergenceDb[rowOff + b]);
      } else {
        const d = sources.densityBits[rowOff + b];
        rgb = heatRgb(d / sources.densityMaxP99);
      }

      const p = (y * plotW + x) * 4;
      px[p] = rgb[0];
      px[p + 1] = rgb[1];
      px[p + 2] = rgb[2];
      px[p + 3] = 255;
    }
  }
  ctx.putImageData(img, padL, yWf);

  // Plot border & Frequency grid
  ctx.strokeStyle = 'rgba(148, 163, 184, 0.2)';
  ctx.lineWidth = 1;
  ctx.strokeRect(padL + 0.5, yWf + 0.5, plotW - 1, wfH - 1);

  const freqTicks = [50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000];
  ctx.font = '400 11px "JetBrains Mono", monospace';
  ctx.textAlign = 'right';
  ctx.textBaseline = 'middle';

  for (const hz of freqTicks) {
    const yf = hzToYFrac(hz);
    const py = Math.round(yWf + (1 - yf) * (wfH - 1)) + 0.5;
    ctx.strokeStyle = 'rgba(255, 255, 255, 0.08)';
    ctx.beginPath();
    ctx.moveTo(padL, py);
    ctx.lineTo(padL + plotW, py);
    ctx.stroke();

    ctx.fillStyle = '#94A3B8';
    const lbl = hz >= 1000 ? `${hz / 1000}k` : `${hz}`;
    ctx.fillText(lbl, padL - 8, py);
  }

  // Time axis
  ctx.textAlign = 'center';
  ctx.textBaseline = 'top';
  const tickCount = 8;
  for (let i = 0; i <= tickCount; i++) {
    const frac = i / tickCount;
    const tx = padL + frac * plotW;
    const tVal = (t0Ms + frac * spanMs) / 1000;
    ctx.strokeStyle = '#334155';
    ctx.beginPath();
    ctx.moveTo(tx, yAxis);
    ctx.lineTo(tx, yAxis + 5);
    ctx.stroke();
    ctx.fillStyle = '#94A3B8';
    ctx.fillText(`${tVal.toFixed(2)}s`, tx, yAxis + 8);
  }

  // Helper to map tMs -> pixel X
  const tToX = (t: number) => padL + ((t - t0Ms) / spanMs) * plotW;

  // Lane Labels on left
  const drawLaneLabel = (label: string, y: number, h: number) => {
    ctx.save();
    ctx.fillStyle = '#64748B';
    ctx.font = '500 11px "JetBrains Mono", monospace';
    ctx.textAlign = 'right';
    ctx.textBaseline = 'middle';
    ctx.fillText(label, padL - 8, y + h / 2);
    ctx.restore();
  };

  // Lane 1: Modus strip
  drawLaneLabel('MODUS', yMode, modeH);
  ctx.fillStyle = '#131824';
  ctx.fillRect(padL, yMode, plotW, modeH);
  for (let x = 0; x < plotW; x++) {
    const t = t0Ms + ((x + 0.5) / plotW) * spanMs;
    const h = Math.max(0, Math.min(sources.hops - 1, Math.floor(t / sources.hopMs)));
    const info = sources.hopInfos[h];
    if (!info) continue;
    ctx.fillStyle = MODE_COLORS[info.mode as Mode];
    ctx.fillRect(padL + x, yMode, 1, modeH);
  }

  // Lane 2: Bitrate
  drawLaneLabel('KBPS', yBitrate, bitrateH);
  ctx.fillStyle = '#131824';
  ctx.fillRect(padL, yBitrate, plotW, bitrateH);
  const maxBr = Math.max(32, summary.maxBitrateKbps * 1.1);
  for (const pkt of doc.packets) {
    if (pkt.meta.tEndMs < t0Ms || pkt.meta.tStartMs > t1Ms) continue;
    const x0 = Math.max(padL, tToX(pkt.meta.tStartMs));
    const x1 = Math.min(padL + plotW, tToX(pkt.meta.tEndMs));
    const barH = (pkt.meta.bitrateKbps / maxBr) * (bitrateH - 4);
    ctx.fillStyle = MODE_COLORS[pkt.meta.mode];
    ctx.fillRect(x0, yBitrate + bitrateH - barH, Math.max(1, x1 - x0 - 0.5), barH);
  }
  const avgY = yBitrate + bitrateH - (summary.avgBitrateKbps / maxBr) * (bitrateH - 4);
  ctx.save();
  ctx.setLineDash([5, 4]);
  ctx.strokeStyle = '#E2E8F0';
  ctx.beginPath();
  ctx.moveTo(padL, avgY);
  ctx.lineTo(padL + plotW, avgY);
  ctx.stroke();
  ctx.restore();

  // Lane 3: F0 Pitch
  drawLaneLabel('F0 HZ', yF0, f0H);
  ctx.fillStyle = '#131824';
  ctx.fillRect(padL, yF0, plotW, f0H);
  for (let x = 0; x < plotW; x++) {
    const t = t0Ms + ((x + 0.5) / plotW) * spanMs;
    const h = Math.max(0, Math.min(sources.hops - 1, Math.floor(t / sources.hopMs)));
    const info = sources.hopInfos[h];
    if (!info || info.mode === 'CELT') continue;
    if (info.signalType === 'Voiced' && info.pitchHz) {
      const clamped = Math.max(50, Math.min(500, info.pitchHz));
      const frac = Math.log(clamped / 50) / Math.log(500 / 50);
      const py = yF0 + (1 - frac) * (f0H - 8) + 4;
      ctx.fillStyle = '#F59E0B';
      ctx.fillRect(padL + x, py - 2, 2, 4);
    } else if (info.signalType === 'Unvoiced') {
      ctx.fillStyle = '#38BDF8';
      ctx.fillRect(padL + x, yF0 + f0H - 5, 1, 3);
    } else {
      ctx.fillStyle = '#475569';
      ctx.fillRect(padL + x, yF0 + f0H - 3, 1, 2);
    }
  }

  // Lane 4: Bit-Budget stacked bar
  drawLaneLabel('BUDGET', yBudget, budgetH);
  ctx.fillStyle = '#131824';
  ctx.fillRect(padL, yBudget, plotW, budgetH);
  for (const pkt of doc.packets) {
    if (pkt.meta.tEndMs < t0Ms || pkt.meta.tStartMs > t1Ms) continue;
    const x0 = Math.max(padL, tToX(pkt.meta.tStartMs));
    const x1 = Math.min(padL + plotW, tToX(pkt.meta.tEndMs));
    const w = Math.max(1, x1 - x0 - 0.5);
    const b = pkt.budget;
    const tot = Math.max(1, pkt.meta.bytes * 64);

    const silkHeadInit = pkt.meta.mode !== 'CELT' ? b.init : 0;
    const celtHeadInit = pkt.meta.mode === 'CELT' ? b.init : 0;

    const slices: Array<[number, string]> = [
      [(silkHeadInit + b.silkFlags + b.silkStereo + b.silkSide) / tot, '#38BDF8'],
      [b.silkPulses / tot, '#2563EB'],
      [b.silkLbrr / tot, '#3CC88C'],
      [(celtHeadInit + b.redHeader + b.celtHeader) / tot, '#FBBF24'],
      [b.celtEnergy / tot, '#F97316'],
      [b.celtPvq / tot, '#EA580C'],
      [b.redundancy / tot, '#DC46A0'],
    ];

    let curY = yBudget + budgetH;
    for (const [share, color] of slices) {
      const sh = share * budgetH;
      if (sh < 0.3) continue;
      curY -= sh;
      ctx.fillStyle = color;
      ctx.fillRect(x0, curY, w, sh);
    }
    void budgetSilkBits;
    void budgetCeltBits;
    void budgetFecBits;
    void budgetRedBits;
  }

  return await new Promise<Blob>((resolve, reject) => {
    canvas.toBlob((blob) => {
      if (blob) resolve(blob);
      else reject(new Error('PNG-Blob konnte nicht erzeugt werden.'));
    }, 'image/png');
  });
}
