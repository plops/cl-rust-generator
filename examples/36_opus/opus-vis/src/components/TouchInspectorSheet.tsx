import React, { useMemo, useState } from 'react';
import { ChevronDown, ChevronUp } from 'lucide-react';
import { EBAND5MS, NB_EBANDS } from '../opus/celtTab';
import { MODE_COLORS } from '../opus/model';
import { WATERFALL_BINS, hzToYFrac } from '../opus/spectrum';
import {
  OpusDocument,
  ViewKind,
  budgetCeltBits,
  budgetFecBits,
  budgetRedBits,
  budgetSilkBits,
  budgetSum,
} from '../opus/types';
import { InspectPoint } from './WaterfallCanvas';

interface TouchInspectorSheetProps {
  doc: OpusDocument;
  view: ViewKind;
  inspectPoint: InspectPoint | null;
  playheadMs: number;
}

const SIGNAL_DE: Record<string, string> = {
  Voiced: 'STIMMHAFT',
  Unvoiced: 'STIMMLOS',
  Inactive: 'INAKTIV',
};

export const TouchInspectorSheet: React.FC<TouchInspectorSheetProps> = ({
  doc,
  view,
  inspectPoint,
  playheadMs,
}) => {
  const [expanded, setExpanded] = useState(false);

  const telemetry = useMemo(() => {
    const tMs = inspectPoint?.tMs ?? playheadMs;
    const freqHz = inspectPoint?.freqHz ?? 1000;
    const { sources, packets } = doc;

    const hopIdx = Math.max(
      0,
      Math.min(sources.hops - 1, Math.floor(tMs / sources.hopMs))
    );
    const binIdx = Math.max(
      0,
      Math.min(
        WATERFALL_BINS - 1,
        Math.round(hzToYFrac(freqHz) * (WATERFALL_BINS - 1))
      )
    );
    const cellIdx = hopIdx * WATERFALL_BINS + binIdx;
    const info = sources.hopInfos[hopIdx];
    const pkt = packets[info?.packetIdx ?? 0] ?? packets[0];
    const frame = pkt?.frames[info?.frameIdxInPacket ?? 0] ?? pkt?.frames[0];

    const specDb = sources.spectrumDb[cellIdx] ?? -72;
    const modelDb = sources.bitstreamDb[cellIdx] ?? -72;
    const divDb = sources.divergenceDb[cellIdx];
    const densBits = sources.densityBits[cellIdx] ?? 0;

    let valueStr = `PEGEL=${specDb.toFixed(1)}DB`;
    if (view === 'bitstream') {
      valueStr = `MODELL=${modelDb.toFixed(1)}DB`;
    } else if (view === 'divergence') {
      valueStr = Number.isNaN(divDb)
        ? 'DIV=---'
        : `DIV=${divDb >= 0 ? '+' : ''}${divDb.toFixed(1)}DB`;
    } else if (view === 'density') {
      valueStr = `DICHTE=${densBits.toFixed(2)}B`;
    }

    const line1 = `T=${(tMs / 1000).toFixed(3)}S  F=${Math.round(freqHz)}HZ  ${valueStr}`;

    const pktDurMs = pkt ? Math.round(pkt.meta.durationMs / Math.max(1, pkt.frames.length)) : 20;
    const bytes = frame?.bytes ?? pkt?.meta.bytes ?? 0;
    const kbps = pkt?.meta.bitrateKbps ?? 0;
    const chTag = pkt?.meta.stereo ? 'ST' : 'C0';
    const line2 = `${(pkt?.meta.mode ?? 'CELT').toUpperCase()} ${pkt?.meta.bandwidth ?? 'FB'} ${pktDurMs}MS ${bytes}B ${kbps.toFixed(1)}KBPS ${chTag} P${pkt?.meta.index ?? 0}`;

    const silkCh0 = frame?.silk?.channels[0];
    const celt = frame?.celt ?? null;
    const transCount = pkt?.frames.filter(
      (f) => f.celt?.transient || f.redundancy?.celt?.transient
    ).length ?? 0;

    let line3 = '';
    if (pkt?.meta.mode === 'SILK' || pkt?.meta.mode === 'Hybrid') {
      const sigLabel = SIGNAL_DE[info?.signalType ?? 'Inactive'] ?? 'INAKTIV';
      const pitchPart =
        info?.signalType === 'Voiced' && info.pitchHz
          ? ` F0=${Math.round(info.pitchHz)}HZ`
          : '';
      const gainPart =
        silkCh0 !== undefined ? ` GAIN=${silkCh0.gainDb.toFixed(0)}DB` : '';
      const hybridPart =
        pkt.meta.mode === 'Hybrid'
          ? ` SILK=${Math.round((info?.silkBandShare ?? 0) * 100)}%`
          : '';
      const celtPart =
        celt && pkt.meta.mode === 'Hybrid' ? ` TRANS=${transCount}` : '';
      line3 = `${sigLabel}${pitchPart}${gainPart}${hybridPart}${celtPart}`;
    } else {
      const bandRange = celt ? `${celt.startBand}..${celt.endBand - 1}` : '0..20';
      const centHz = Math.round(info?.centroidHz ?? 0);
      line3 = `TRANS=${transCount}  BÄNDER=${bandRange}  SPREAD=${celt?.spread ?? 0}  CENT=${centHz}HZ`;
    }

    const b = frame?.budget ?? pkt?.budget;
    const tot = Math.max(1, b ? budgetSum(b) : 1);
    const silkPct = b ? Math.round(((budgetSilkBits(b) * 8) / tot) * 100) : 0;
    const celtPct = b ? Math.round(((budgetCeltBits(b) * 8) / tot) * 100) : 0;
    const fecPct = b ? Math.round(((budgetFecBits(b) * 8) / tot) * 100) : 0;
    const redPct = b ? Math.round(((budgetRedBits(b) * 8) / tot) * 100) : 0;
    const restPct = b ? Math.round((b.unused / tot) * 100) : 0;

    const extras: string[] = [];
    if (fecPct > 0) extras.push(`FEC ${fecPct}%`);
    if (redPct > 0 && frame?.redundancy) {
      extras.push(
        frame.redundancy.celtToSilk ? 'RED:CELT-SILK' : 'RED:SILK-CELT'
      );
    }
    if (frame?.silk?.midOnly) extras.push('MID-ONLY');

    const line4 = `BITS SILK ${silkPct}% CELT ${celtPct}% REST ${restPct}%${
      extras.length ? '  ' + extras.join('  ') : ''
    }`;

    return {
      line1,
      line2,
      line3,
      line4,
      pkt,
      frame,
      silkCh0,
      celt,
      budget: b,
    };
  }, [doc, inspectPoint, playheadMs, view]);

  const modeColor = telemetry.pkt
    ? MODE_COLORS[telemetry.pkt.meta.mode]
    : '#06B6D4';

  return (
    <div className="rounded-lg border border-slate-800 bg-[#131824]">
      <div className="flex items-start justify-between gap-2 px-3 py-2">
        <div className="min-w-0 flex-1 space-y-0.5 font-mono-tabular text-[11px] leading-snug sm:text-xs">
          <div className="flex items-center gap-2 truncate text-cyan-300">
            <span
              className="inline-block h-2 w-2 shrink-0 rounded-full"
              style={{ backgroundColor: modeColor }}
            />
            <span className="truncate font-semibold">{telemetry.line1}</span>
          </div>
          <div className="truncate text-slate-200">{telemetry.line2}</div>
          <div className="truncate text-slate-300">{telemetry.line3}</div>
          <div className="truncate text-slate-400">{telemetry.line4}</div>
        </div>

        <button
          type="button"
          onClick={() => setExpanded((v) => !v)}
          className="inline-flex min-h-[44px] shrink-0 items-center gap-1 rounded-lg border border-slate-700 bg-[#0B0E17] px-2.5 py-2 text-xs font-medium text-slate-200 transition-colors hover:border-cyan-500/60"
          aria-expanded={expanded}
        >
          <span>Details</span>
          {expanded ? (
            <ChevronDown className="h-4 w-4 text-cyan-400" />
          ) : (
            <ChevronUp className="h-4 w-4 text-cyan-400" />
          )}
        </button>
      </div>

      {expanded && telemetry.frame && (
        <div className="max-h-72 overflow-y-auto border-t border-slate-800 bg-[#0B0E17]/90 px-3 py-3 text-xs">
          <div className="grid gap-4 md:grid-cols-2">
            {/* Frame Bit-Budget 12-field breakdown */}
            <div>
              <h4 className="mb-2 text-[11px] font-semibold uppercase tracking-wider text-slate-400">
                Frame Bit-Budget ({telemetry.frame.bytes * 8} Bits)
              </h4>
              {telemetry.budget && (
                <div className="grid grid-cols-2 gap-x-4 gap-y-1 font-mono-tabular text-[11px]">
                  {(
                    [
                      ['Init / TOC', telemetry.budget.init],
                      ['SILK Flags', telemetry.budget.silkFlags],
                      ['SILK LBRR/FEC', telemetry.budget.silkLbrr],
                      ['SILK Stereo', telemetry.budget.silkStereo],
                      ['SILK Side/LPC', telemetry.budget.silkSide],
                      ['SILK Pulse', telemetry.budget.silkPulses],
                      ['Redundanz-Hdr', telemetry.budget.redHeader],
                      ['CELT Header', telemetry.budget.celtHeader],
                      ['CELT Energie', telemetry.budget.celtEnergy],
                      ['CELT PVQ', telemetry.budget.celtPvq],
                      ['Redundanz-Daten', telemetry.budget.redundancy],
                      ['Ungenutzt', telemetry.budget.unused],
                    ] as Array<[string, number]>
                  )
                    .filter(([, q3]) => q3 > 0)
                    .map(([label, q3]) => (
                      <div
                        key={label}
                        className="flex items-center justify-between border-b border-slate-800/70 py-0.5"
                      >
                        <span className="text-slate-400">{label}</span>
                        <span className="text-slate-100">
                          {(q3 / 8).toFixed(1)} b
                        </span>
                      </div>
                    ))}
                </div>
              )}

              {telemetry.frame.silk && (
                <div className="mt-3">
                  <h4 className="mb-1.5 text-[11px] font-semibold uppercase tracking-wider text-slate-400">
                    SILK Interna
                  </h4>
                  <div className="grid grid-cols-2 gap-x-4 gap-y-1 font-mono-tabular text-[11px] text-slate-300">
                    <div>Subframes: {telemetry.frame.silk.subframesSilkCount}</div>
                    <div>LBRR/FEC: {telemetry.frame.silk.lbrr ? 'Ja' : 'Nein'}</div>
                    <div>Mid-Only: {telemetry.frame.silk.midOnly ? 'Ja' : 'Nein'}</div>
                    <div>
                      LPC-Ordnung: {telemetry.silkCh0?.lpcOrder ?? '–'}
                    </div>
                    <div>
                      Pulse Ch0: {telemetry.silkCh0?.pulsesTotal ?? 0}
                    </div>
                    <div>
                      Rate-Level: {telemetry.silkCh0?.rateLevel ?? 0}
                    </div>
                  </div>
                </div>
              )}
            </div>

            {/* CELT 21-Band Breakdown */}
            <div>
              <h4 className="mb-2 text-[11px] font-semibold uppercase tracking-wider text-slate-400">
                {telemetry.celt
                  ? `CELT Skalenfaktor-Bänder (${telemetry.celt.startBand}..${telemetry.celt.endBand - 1})`
                  : 'CELT Bänder (in diesem Frame inaktiv)'}
              </h4>
              {telemetry.celt ? (
                <div className="space-y-1 font-mono-tabular text-[11px]">
                  {Array.from({ length: NB_EBANDS }, (_, b) => {
                    const c = telemetry.celt!;
                    if (b < c.startBand || b >= c.endBand) return null;
                    const fLo = EBAND5MS[b] * 200;
                    const fHi = EBAND5MS[b + 1] * 200;
                    const bits = c.bandTotalBits[b] / 8;
                    const maxBandBits = Math.max(
                      12,
                      ...c.bandTotalBits.map((v) => v / 8)
                    );
                    const pct = Math.min(100, (bits / maxBandBits) * 100);
                    return (
                      <div
                        key={b}
                        className="grid grid-cols-[26px_74px_1fr_54px_48px] items-center gap-2"
                      >
                        <span className="text-slate-500">B{b}</span>
                        <span className="text-slate-400">
                          {fLo >= 1000 ? `${(fLo / 1000).toFixed(1)}k` : fLo}–
                          {fHi >= 1000 ? `${(fHi / 1000).toFixed(1)}k` : fHi}
                        </span>
                        <div className="h-1.5 overflow-hidden rounded-full bg-slate-800">
                          <div
                            className="h-full bg-amber-400"
                            style={{ width: `${pct}%` }}
                          />
                        </div>
                        <span className="text-right text-slate-200">
                          {c.energyCoded[b]
                            ? `${c.energyDb[b].toFixed(0)}dB`
                            : '---'}
                        </span>
                        <span className="text-right text-cyan-300">
                          {bits.toFixed(1)}b
                        </span>
                      </div>
                    );
                  })}
                </div>
              ) : (
                <p className="text-slate-500">
                  Aktiver Frame arbeitet im reinen SILK-Sprachmodus (0–8 kHz).
                </p>
              )}
            </div>
          </div>
        </div>
      )}
    </div>
  );
};
