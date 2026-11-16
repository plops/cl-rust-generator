import React, { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { LoopState } from '../audio/player';
import { MODE_COLORS, divergingRgb, heatRgb } from '../opus/model';
import { DB_FLOOR, FREQ_MAX_HZ, FREQ_MIN_HZ, WATERFALL_BINS, hzToYFrac } from '../opus/spectrum';
import { Mode, OpusDocument, ViewKind } from '../opus/types';

export interface InspectPoint {
  tMs: number;
  freqHz: number;
}

interface WaterfallCanvasProps {
  doc: OpusDocument;
  view: ViewKind;
  t0Ms: number;
  t1Ms: number;
  playheadMs: number;
  loop: LoopState;
  inspectPoint: InspectPoint | null;
  interactionMode: 'pan' | 'scrub';
  onRangeChange: (nextT0: number, nextT1: number) => void;
  onSeek: (tMs: number) => void;
  onInspect: (pt: InspectPoint) => void;
  onResetZoom: () => void;
}

interface LayoutGeometry {
  width: number;
  height: number;
  padL: number;
  padR: number;
  padT: number;
  plotW: number;
  yWf: number;
  wfH: number;
  yAxis: number;
  axisH: number;
  yMode: number;
  modeH: number;
  yBitrate: number;
  bitrateH: number;
  yF0: number;
  f0H: number;
  yBudget: number;
  budgetH: number;
}

function computeGeometry(width: number, height: number): LayoutGeometry {
  const isCompact = width < 640;
  const padL = isCompact ? 42 : 54;
  const padR = isCompact ? 10 : 16;
  const padT = 8;
  const padB = 8;
  const plotW = Math.max(40, width - padL - padR);

  const axisH = 20;
  const modeH = isCompact ? 14 : 18;
  const bitrateH = isCompact ? 38 : 50;
  const f0H = isCompact ? 38 : 48;
  const budgetH = isCompact ? 40 : 52;
  const gap = isCompact ? 6 : 8;
  const fixedH = padT + padB + axisH + modeH + bitrateH + f0H + budgetH + gap * 4 + 4;
  const wfH = Math.max(130, height - fixedH);

  const yWf = padT;
  const yAxis = yWf + wfH;
  const yMode = yAxis + axisH + 2;
  const yBitrate = yMode + modeH + gap;
  const yF0 = yBitrate + bitrateH + gap;
  const yBudget = yF0 + f0H + gap;

  return {
    width,
    height,
    padL,
    padR,
    padT,
    plotW,
    yWf,
    wfH,
    yAxis,
    axisH,
    yMode,
    modeH,
    yBitrate,
    bitrateH,
    yF0,
    f0H,
    yBudget,
    budgetH,
  };
}

function yFracToHz(yFrac: number): number {
  const f = Math.max(0, Math.min(1, yFrac));
  return FREQ_MIN_HZ * Math.pow(FREQ_MAX_HZ / FREQ_MIN_HZ, f);
}

const VIEW_SCALE_INFO: Record<ViewKind, string> = {
  spectrum: '-72 … 0 dBFS',
  bitstream: '-72 … 0 dBFS',
  divergence: '-12 … +12 dB',
  density: '0 … P99 Bits/Bin',
};

export const WaterfallCanvas: React.FC<WaterfallCanvasProps> = ({
  doc,
  view,
  t0Ms,
  t1Ms,
  playheadMs,
  loop,
  inspectPoint,
  interactionMode,
  onRangeChange,
  onSeek,
  onInspect,
  onResetZoom,
}) => {
  const containerRef = useRef<HTMLDivElement | null>(null);
  const baseCanvasRef = useRef<HTMLCanvasElement | null>(null);
  const overlayCanvasRef = useRef<HTMLCanvasElement | null>(null);
  const [size, setSize] = useState<{ width: number; height: number }>({
    width: 800,
    height: 460,
  });

  // Track active pointers for tap, pan, scrub, and 2-finger pinch-to-zoom
  const pointersRef = useRef<Map<number, { x: number; y: number }>>(new Map());
  const gestureRef = useRef<{
    type: 'none' | 'single' | 'pinch';
    startX: number;
    startY: number;
    startT0: number;
    startT1: number;
    moved: boolean;
    pinchAnchorTMs: number;
    pinchStartDist: number;
    pinchStartSpan: number;
    pinchAnchorFrac: number;
  }>({
    type: 'none',
    startX: 0,
    startY: 0,
    startT0: 0,
    startT1: 1,
    moved: false,
    pinchAnchorTMs: 0,
    pinchStartDist: 1,
    pinchStartSpan: 1,
    pinchAnchorFrac: 0.5,
  });

  // ResizeObserver for crisp HiDPI sizing
  useEffect(() => {
    const el = containerRef.current;
    if (!el) return;

    const update = () => {
      const rect = el.getBoundingClientRect();
      const w = Math.max(240, Math.floor(rect.width));
      const h = Math.max(300, Math.floor(rect.height));
      setSize((prev) => (prev.width === w && prev.height === h ? prev : { width: w, height: h }));
    };

    update();
    const ro = new ResizeObserver(update);
    ro.observe(el);
    return () => ro.disconnect();
  }, []);

  const geom = useMemo(() => computeGeometry(size.width, size.height), [size]);
  const totalDurMs = Math.max(20, doc.summary.durationMs);

  // Base Canvas Draw (Waterfall + Axes + 4 Lanes)
  useEffect(() => {
    const canvas = baseCanvasRef.current;
    if (!canvas) return;
    const dpr = Math.min(2, window.devicePixelRatio || 1);
    canvas.width = Math.floor(geom.width * dpr);
    canvas.height = Math.floor(geom.height * dpr);

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.fillStyle = '#0B0E17';
    ctx.fillRect(0, 0, geom.width, geom.height);

    const { sources, summary } = doc;
    const spanMs = Math.max(20, t1Ms - t0Ms);
    const plotWInt = Math.max(1, Math.floor(geom.plotW));
    const wfHInt = Math.max(1, Math.floor(geom.wfH));

    // Render waterfall spectrogram pixels in physical-like resolution (1x logical px for speed)
    const offscreen = document.createElement('canvas');
    offscreen.width = plotWInt;
    offscreen.height = wfHInt;
    const offCtx = offscreen.getContext('2d');
    if (offCtx) {
      const img = offCtx.createImageData(plotWInt, wfHInt);
      const px = img.data;
      const hopStart = Math.max(0, Math.floor(t0Ms / sources.hopMs));
      const hopEnd = Math.min(sources.hops - 1, Math.ceil(t1Ms / sources.hopMs));

      for (let x = 0; x < plotWInt; x++) {
        const t = t0Ms + ((x + 0.5) / plotWInt) * spanMs;
        const h = Math.max(hopStart, Math.min(hopEnd, Math.floor(t / sources.hopMs)));
        const rowOff = h * WATERFALL_BINS;

        for (let y = 0; y < wfHInt; y++) {
          const binFrac = (1 - y / Math.max(1, wfHInt - 1)) * (WATERFALL_BINS - 1);
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

          const p = (y * plotWInt + x) * 4;
          px[p] = rgb[0];
          px[p + 1] = rgb[1];
          px[p + 2] = rgb[2];
          px[p + 3] = 255;
        }
      }
      offCtx.putImageData(img, 0, 0);
      ctx.drawImage(offscreen, geom.padL, geom.yWf, geom.plotW, geom.wfH);
    }

    // Waterfall hairline border
    ctx.strokeStyle = 'rgba(148, 163, 184, 0.22)';
    ctx.lineWidth = 1;
    ctx.strokeRect(geom.padL + 0.5, geom.yWf + 0.5, geom.plotW - 1, geom.wfH - 1);

    // Frequency Grid & Labels
    const freqTicks =
      geom.wfH < 200
        ? [100, 500, 1000, 4000, 8000, 20000]
        : [50, 100, 200, 500, 1000, 2000, 4000, 8000, 16000];
    ctx.font = '400 10px "JetBrains Mono", monospace';
    ctx.textAlign = 'right';
    ctx.textBaseline = 'middle';

    for (const hz of freqTicks) {
      const yf = hzToYFrac(hz);
      const py = Math.round(geom.yWf + (1 - yf) * (geom.wfH - 1)) + 0.5;
      ctx.strokeStyle = hz === 8000 ? 'rgba(185, 120, 255, 0.22)' : 'rgba(255, 255, 255, 0.08)';
      ctx.beginPath();
      ctx.moveTo(geom.padL, py);
      ctx.lineTo(geom.padL + geom.plotW, py);
      ctx.stroke();

      ctx.fillStyle = '#94A3B8';
      const lbl = hz >= 1000 ? `${hz / 1000}k` : `${hz}`;
      ctx.fillText(lbl, geom.padL - 6, py);
    }

    // Scale badge inside top-right of waterfall
    const scaleText = VIEW_SCALE_INFO[view];
    ctx.font = '500 10px "JetBrains Mono", monospace';
    const scaleW = ctx.measureText(scaleText).width + 12;
    ctx.fillStyle = 'rgba(11, 14, 23, 0.72)';
    ctx.fillRect(geom.padL + geom.plotW - scaleW - 6, geom.yWf + 6, scaleW, 18);
    ctx.strokeStyle = 'rgba(30, 41, 59, 0.9)';
    ctx.strokeRect(geom.padL + geom.plotW - scaleW - 6, geom.yWf + 6, scaleW, 18);
    ctx.fillStyle = '#CBD5E1';
    ctx.textAlign = 'right';
    ctx.textBaseline = 'middle';
    ctx.fillText(scaleText, geom.padL + geom.plotW - 12, geom.yWf + 15);

    // Time Axis
    const tickCount = geom.width < 520 ? 5 : 8;
    ctx.textAlign = 'center';
    ctx.textBaseline = 'top';
    ctx.font = '400 10px "JetBrains Mono", monospace';
    for (let i = 0; i <= tickCount; i++) {
      const frac = i / tickCount;
      const tx = geom.padL + frac * geom.plotW;
      const tSec = (t0Ms + frac * spanMs) / 1000;
      ctx.strokeStyle = '#334155';
      ctx.beginPath();
      ctx.moveTo(tx, geom.yAxis);
      ctx.lineTo(tx, geom.yAxis + 4);
      ctx.stroke();

      ctx.fillStyle = '#94A3B8';
      const clampedX = Math.max(geom.padL + 16, Math.min(geom.padL + geom.plotW - 16, tx));
      const decimals = spanMs < 2000 ? 2 : 1;
      ctx.fillText(`${tSec.toFixed(decimals)}s`, clampedX, geom.yAxis + 6);
    }

    const tToX = (t: number) => geom.padL + ((t - t0Ms) / spanMs) * geom.plotW;

    const drawLaneLabel = (label: string, y: number, h: number) => {
      ctx.fillStyle = '#64748B';
      ctx.font = '500 9px "JetBrains Mono", monospace';
      ctx.textAlign = 'right';
      ctx.textBaseline = 'middle';
      ctx.fillText(label, geom.padL - 6, y + h / 2);
    };

    // Lane 1: Modus strip
    drawLaneLabel('MODUS', geom.yMode, geom.modeH);
    ctx.fillStyle = '#131824';
    ctx.fillRect(geom.padL, geom.yMode, geom.plotW, geom.modeH);
    for (let x = 0; x < plotWInt; x++) {
      const t = t0Ms + ((x + 0.5) / plotWInt) * spanMs;
      const h = Math.max(0, Math.min(sources.hops - 1, Math.floor(t / sources.hopMs)));
      const info = sources.hopInfos[h];
      if (!info) continue;
      ctx.fillStyle = MODE_COLORS[info.mode as Mode];
      ctx.fillRect(geom.padL + x, geom.yMode, 1.5, geom.modeH);
    }

    // Lane 2: Bitrate + AVG line
    drawLaneLabel('KBPS', geom.yBitrate, geom.bitrateH);
    ctx.fillStyle = '#131824';
    ctx.fillRect(geom.padL, geom.yBitrate, geom.plotW, geom.bitrateH);
    const maxBr = Math.max(24, summary.maxBitrateKbps * 1.12);
    for (const pkt of doc.packets) {
      if (pkt.meta.tEndMs < t0Ms || pkt.meta.tStartMs > t1Ms) continue;
      const x0 = Math.max(geom.padL, tToX(pkt.meta.tStartMs));
      const x1 = Math.min(geom.padL + geom.plotW, tToX(pkt.meta.tEndMs));
      const barH = Math.max(1, (pkt.meta.bitrateKbps / maxBr) * (geom.bitrateH - 4));
      ctx.fillStyle = MODE_COLORS[pkt.meta.mode];
      ctx.fillRect(x0, geom.yBitrate + geom.bitrateH - barH, Math.max(1, x1 - x0 - 0.4), barH);
    }
    const avgY =
      geom.yBitrate + geom.bitrateH - (summary.avgBitrateKbps / maxBr) * (geom.bitrateH - 4);
    ctx.save();
    ctx.setLineDash([4, 3]);
    ctx.strokeStyle = 'rgba(226, 232, 240, 0.78)';
    ctx.beginPath();
    ctx.moveTo(geom.padL, avgY);
    ctx.lineTo(geom.padL + geom.plotW, avgY);
    ctx.stroke();
    ctx.restore();

    ctx.fillStyle = '#E2E8F0';
    ctx.font = '500 9px "JetBrains Mono", monospace';
    ctx.textAlign = 'left';
    ctx.textBaseline = 'top';
    ctx.fillText(`AVG ${summary.avgBitrateKbps.toFixed(0)}k`, geom.padL + 4, geom.yBitrate + 2);

    // Lane 3: F0 Pitch
    drawLaneLabel('F0 HZ', geom.yF0, geom.f0H);
    ctx.fillStyle = '#131824';
    ctx.fillRect(geom.padL, geom.yF0, geom.plotW, geom.f0H);
    for (let x = 0; x < plotWInt; x++) {
      const t = t0Ms + ((x + 0.5) / plotWInt) * spanMs;
      const h = Math.max(0, Math.min(sources.hops - 1, Math.floor(t / sources.hopMs)));
      const info = sources.hopInfos[h];
      if (!info || info.mode === 'CELT') continue;
      if (info.signalType === 'Voiced' && info.pitchHz) {
        const clamped = Math.max(50, Math.min(500, info.pitchHz));
        const frac = Math.log(clamped / 50) / Math.log(500 / 50);
        const py = geom.yF0 + (1 - frac) * (geom.f0H - 8) + 4;
        ctx.fillStyle = '#F59E0B';
        ctx.fillRect(geom.padL + x, py - 1.5, 2, 3);
      } else if (info.signalType === 'Unvoiced') {
        ctx.fillStyle = '#38BDF8';
        ctx.fillRect(geom.padL + x, geom.yF0 + geom.f0H - 4, 1.5, 2.5);
      } else {
        ctx.fillStyle = '#475569';
        ctx.fillRect(geom.padL + x, geom.yF0 + geom.f0H - 2, 1.5, 1.5);
      }
    }

    // Lane 4: Bit-Budget 7-slice stack
    drawLaneLabel('BUDGET', geom.yBudget, geom.budgetH);
    ctx.fillStyle = '#131824';
    ctx.fillRect(geom.padL, geom.yBudget, geom.plotW, geom.budgetH);
    for (const pkt of doc.packets) {
      if (pkt.meta.tEndMs < t0Ms || pkt.meta.tStartMs > t1Ms) continue;
      const x0 = Math.max(geom.padL, tToX(pkt.meta.tStartMs));
      const x1 = Math.min(geom.padL + geom.plotW, tToX(pkt.meta.tEndMs));
      const w = Math.max(1, x1 - x0 - 0.4);
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

      let curY = geom.yBudget + geom.budgetH;
      for (const [share, color] of slices) {
        const sh = share * geom.budgetH;
        if (sh < 0.35) continue;
        curY -= sh;
        ctx.fillStyle = color;
        ctx.fillRect(x0, curY, w, sh);
      }
    }

    // Hairline borders around telemetry lanes
    ctx.strokeStyle = 'rgba(30, 41, 59, 0.9)';
    ctx.lineWidth = 1;
    for (const [y, h] of [
      [geom.yMode, geom.modeH],
      [geom.yBitrate, geom.bitrateH],
      [geom.yF0, geom.f0H],
      [geom.yBudget, geom.budgetH],
    ]) {
      ctx.strokeRect(geom.padL + 0.5, y + 0.5, geom.plotW - 1, h - 1);
    }
  }, [doc, geom, t0Ms, t1Ms, view]);

  // Overlay Canvas Draw (60 FPS Playhead, A-B Loop Region, Crosshair)
  useEffect(() => {
    const canvas = overlayCanvasRef.current;
    if (!canvas) return;
    const dpr = Math.min(2, window.devicePixelRatio || 1);
    canvas.width = Math.floor(geom.width * dpr);
    canvas.height = Math.floor(geom.height * dpr);

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, geom.width, geom.height);

    const spanMs = Math.max(20, t1Ms - t0Ms);
    const tToX = (t: number) => geom.padL + ((t - t0Ms) / spanMs) * geom.plotW;
    const topY = geom.yWf;
    const botY = geom.yBudget + geom.budgetH;

    // A-B Loop Region & Markers
    if (loop.aMs !== null || loop.bMs !== null) {
      const xA = loop.aMs !== null ? tToX(loop.aMs) : null;
      const xB = loop.bMs !== null ? tToX(loop.bMs) : null;

      if (xA !== null && xB !== null) {
        const left = Math.max(geom.padL, Math.min(xA, xB));
        const right = Math.min(geom.padL + geom.plotW, Math.max(xA, xB));
        if (right > left) {
          ctx.fillStyle = loop.active
            ? 'rgba(6, 182, 212, 0.14)'
            : 'rgba(148, 163, 184, 0.09)';
          ctx.fillRect(left, geom.yWf, right - left, geom.wfH);
        }
      }

      const drawMarker = (x: number, label: 'A' | 'B') => {
        if (x < geom.padL || x > geom.padL + geom.plotW) return;
        ctx.save();
        ctx.setLineDash([4, 3]);
        ctx.strokeStyle = loop.active ? '#22D3EE' : '#94A3B8';
        ctx.lineWidth = 1.5;
        ctx.beginPath();
        ctx.moveTo(x, topY);
        ctx.lineTo(x, botY);
        ctx.stroke();
        ctx.restore();

        // Crisp flag badge at top
        ctx.fillStyle = loop.active ? '#06B6D4' : '#475569';
        ctx.fillRect(x - 9, topY, 18, 16);
        ctx.fillStyle = loop.active ? '#0B0E17' : '#F8FAFC';
        ctx.font = '700 10px "JetBrains Mono", monospace';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(label, x, topY + 8.5);
      };

      if (xA !== null) drawMarker(xA, 'A');
      if (xB !== null) drawMarker(xB, 'B');
    }

    // Touch-Inspector Crosshair (if inside visible range)
    if (inspectPoint && inspectPoint.tMs >= t0Ms && inspectPoint.tMs <= t1Ms) {
      const ix = tToX(inspectPoint.tMs);
      const yf = hzToYFrac(inspectPoint.freqHz);
      const iy = geom.yWf + (1 - yf) * (geom.wfH - 1);

      ctx.save();
      ctx.strokeStyle = 'rgba(248, 250, 252, 0.42)';
      ctx.lineWidth = 1;
      ctx.beginPath();
      ctx.moveTo(ix, geom.yWf);
      ctx.lineTo(ix, botY);
      ctx.moveTo(geom.padL, iy);
      ctx.lineTo(geom.padL + geom.plotW, iy);
      ctx.stroke();

      ctx.fillStyle = '#F8FAFC';
      ctx.beginPath();
      ctx.arc(ix, iy, 3.5, 0, Math.PI * 2);
      ctx.fill();
      ctx.restore();
    }

    // Playhead (60 FPS Cyan hairline)
    if (playheadMs >= t0Ms && playheadMs <= t1Ms) {
      const px = tToX(playheadMs);
      ctx.save();
      ctx.strokeStyle = '#06B6D4';
      ctx.lineWidth = 2;
      ctx.beginPath();
      ctx.moveTo(px, topY);
      ctx.lineTo(px, botY);
      ctx.stroke();

      // Top playhead triangle
      ctx.fillStyle = '#06B6D4';
      ctx.beginPath();
      ctx.moveTo(px - 5, topY);
      ctx.lineTo(px + 5, topY);
      ctx.lineTo(px, topY + 7);
      ctx.closePath();
      ctx.fill();
      ctx.restore();
    }
  }, [geom, t0Ms, t1Ms, playheadMs, loop, inspectPoint]);

  const clientToLocal = useCallback((clientX: number, clientY: number) => {
    const rect = overlayCanvasRef.current?.getBoundingClientRect();
    if (!rect) return { x: 0, y: 0 };
    return { x: clientX - rect.left, y: clientY - rect.top };
  }, []);

  const localToPoint = useCallback(
    (x: number, y: number): InspectPoint => {
      const spanMs = Math.max(20, t1Ms - t0Ms);
      const fracX = Math.max(0, Math.min(1, (x - geom.padL) / geom.plotW));
      const tMs = Math.max(0, Math.min(totalDurMs, t0Ms + fracX * spanMs));

      const fracY = Math.max(0, Math.min(1, 1 - (y - geom.yWf) / Math.max(1, geom.wfH - 1)));
      const freqHz = yFracToHz(fracY);
      return { tMs, freqHz };
    },
    [geom, t0Ms, t1Ms, totalDurMs]
  );

  const clampWindow = useCallback(
    (nextT0: number, nextT1: number): [number, number] => {
      const minSpan = Math.min(totalDurMs, 160);
      let span = Math.max(minSpan, Math.min(totalDurMs, nextT1 - nextT0));
      let a = nextT0;
      let b = a + span;
      if (a < 0) {
        a = 0;
        b = span;
      }
      if (b > totalDurMs) {
        b = totalDurMs;
        a = Math.max(0, b - span);
      }
      return [a, b];
    },
    [totalDurMs]
  );

  const handlePointerDown = (e: React.PointerEvent<HTMLCanvasElement>) => {
    e.currentTarget.setPointerCapture(e.pointerId);
    const pt = clientToLocal(e.clientX, e.clientY);
    pointersRef.current.set(e.pointerId, pt);

    if (pointersRef.current.size === 1) {
      gestureRef.current = {
        type: 'single',
        startX: pt.x,
        startY: pt.y,
        startT0: t0Ms,
        startT1: t1Ms,
        moved: false,
        pinchAnchorTMs: 0,
        pinchStartDist: 1,
        pinchStartSpan: t1Ms - t0Ms,
        pinchAnchorFrac: 0.5,
      };
      if (interactionMode === 'scrub') {
        const ip = localToPoint(pt.x, pt.y);
        onInspect(ip);
        onSeek(ip.tMs);
      }
    } else if (pointersRef.current.size === 2) {
      const pts = Array.from(pointersRef.current.values());
      const dist = Math.max(12, Math.hypot(pts[1].x - pts[0].x, pts[1].y - pts[0].y));
      const midX = (pts[0].x + pts[1].x) / 2;
      const anchorFrac = Math.max(0, Math.min(1, (midX - geom.padL) / geom.plotW));
      const curSpan = Math.max(20, t1Ms - t0Ms);
      gestureRef.current = {
        type: 'pinch',
        startX: midX,
        startY: (pts[0].y + pts[1].y) / 2,
        startT0: t0Ms,
        startT1: t1Ms,
        moved: true,
        pinchAnchorTMs: t0Ms + anchorFrac * curSpan,
        pinchStartDist: dist,
        pinchStartSpan: curSpan,
        pinchAnchorFrac: anchorFrac,
      };
    }
  };

  const handlePointerMove = (e: React.PointerEvent<HTMLCanvasElement>) => {
    if (!pointersRef.current.has(e.pointerId)) return;
    const pt = clientToLocal(e.clientX, e.clientY);
    pointersRef.current.set(e.pointerId, pt);

    const g = gestureRef.current;

    if (pointersRef.current.size === 2 && g.type === 'pinch') {
      const pts = Array.from(pointersRef.current.values());
      const dist = Math.max(12, Math.hypot(pts[1].x - pts[0].x, pts[1].y - pts[0].y));
      const midX = (pts[0].x + pts[1].x) / 2;
      const curFrac = Math.max(0, Math.min(1, (midX - geom.padL) / geom.plotW));
      const scale = g.pinchStartDist / dist;
      const nextSpan = Math.max(Math.min(totalDurMs, 160), Math.min(totalDurMs, g.pinchStartSpan * scale));
      const nextT0 = g.pinchAnchorTMs - curFrac * nextSpan;
      const [c0, c1] = clampWindow(nextT0, nextT0 + nextSpan);
      onRangeChange(c0, c1);
      return;
    }

    if (pointersRef.current.size === 1 && g.type === 'single') {
      const dx = pt.x - g.startX;
      const dy = pt.y - g.startY;
      if (Math.hypot(dx, dy) > 6) {
        g.moved = true;
      }

      if (interactionMode === 'scrub') {
        const ip = localToPoint(pt.x, pt.y);
        onInspect(ip);
        onSeek(ip.tMs);
      } else if (g.moved) {
        const span = Math.max(20, g.startT1 - g.startT0);
        const dt = (-dx / Math.max(1, geom.plotW)) * span;
        const [c0, c1] = clampWindow(g.startT0 + dt, g.startT1 + dt);
        onRangeChange(c0, c1);
      }
    }
  };

  const handlePointerUp = (e: React.PointerEvent<HTMLCanvasElement>) => {
    const pt = clientToLocal(e.clientX, e.clientY);
    const g = gestureRef.current;

    if (pointersRef.current.size === 1 && g.type === 'single' && !g.moved) {
      const ip = localToPoint(pt.x, pt.y);
      onInspect(ip);
      onSeek(ip.tMs);
    }

    pointersRef.current.delete(e.pointerId);
    if (pointersRef.current.size === 0) {
      gestureRef.current.type = 'none';
    }
    if (e.currentTarget.hasPointerCapture(e.pointerId)) {
      e.currentTarget.releasePointerCapture(e.pointerId);
    }
  };

  const handleWheel = (e: React.WheelEvent<HTMLCanvasElement>) => {
    if (Math.abs(e.deltaY) < 0.5 && Math.abs(e.deltaX) < 0.5) return;
    e.preventDefault();
    const pt = clientToLocal(e.clientX, e.clientY);
    const span = Math.max(20, t1Ms - t0Ms);

    if (Math.abs(e.deltaX) > Math.abs(e.deltaY) && !e.ctrlKey) {
      const dt = (e.deltaX / Math.max(1, geom.plotW)) * span;
      const [c0, c1] = clampWindow(t0Ms + dt, t1Ms + dt);
      onRangeChange(c0, c1);
      return;
    }

    const anchorFrac = Math.max(0, Math.min(1, (pt.x - geom.padL) / geom.plotW));
    const anchorMs = t0Ms + anchorFrac * span;
    const factor = e.deltaY > 0 ? 1.22 : 1 / 1.22;
    const nextSpan = Math.max(Math.min(totalDurMs, 160), Math.min(totalDurMs, span * factor));
    const nextT0 = anchorMs - anchorFrac * nextSpan;
    const [c0, c1] = clampWindow(nextT0, nextT0 + nextSpan);
    onRangeChange(c0, c1);
  };

  const isZoomed = t0Ms > 5 || t1Ms < totalDurMs - 5;

  return (
    <div
      ref={containerRef}
      className="relative h-full w-full select-none overflow-hidden rounded-lg border border-slate-800 bg-[#0B0E17]"
    >
      <canvas
        ref={baseCanvasRef}
        className="pointer-events-none absolute inset-0 h-full w-full"
      />
      <canvas
        ref={overlayCanvasRef}
        onPointerDown={handlePointerDown}
        onPointerMove={handlePointerMove}
        onPointerUp={handlePointerUp}
        onPointerCancel={handlePointerUp}
        onWheel={handleWheel}
        onDoubleClick={onResetZoom}
        className="absolute inset-0 h-full w-full cursor-crosshair touch-none"
      />
      {isZoomed && (
        <button
          type="button"
          onClick={onResetZoom}
          className="absolute left-12 top-2 z-10 min-h-[32px] rounded border border-slate-700 bg-[#131824]/90 px-2.5 py-1 text-[11px] font-medium text-slate-200 hover:border-cyan-500/60"
        >
          Zoom 100%
        </button>
      )}
    </div>
  );
};
