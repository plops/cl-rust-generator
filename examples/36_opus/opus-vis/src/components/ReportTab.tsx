import React, { useState } from 'react';
import { Check, Copy, Download, FileJson, FileText, Image as ImageIcon } from 'lucide-react';
import { buildJsonSchema3 } from '../opus/jsonExport';
import { renderWaterfallPngBlob } from '../opus/pngExport';
import { OpusDocument, ViewKind } from '../opus/types';

interface ReportTabProps {
  doc: OpusDocument;
  view: ViewKind;
  t0Ms: number;
  t1Ms: number;
}

function triggerBlobDownload(blob: Blob, fileName: string): void {
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = fileName;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}

export const ReportTab: React.FC<ReportTabProps> = ({
  doc,
  view,
  t0Ms,
  t1Ms,
}) => {
  const [copied, setCopied] = useState(false);
  const [exportingPng, setExportingPng] = useState(false);
  const [exportError, setExportError] = useState<string | null>(null);

  const { summary, fakten } = doc;
  const baseName = doc.fileName.replace(/\.[^.]+$/, '') || 'opus-analyse';

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(doc.reportText);
      setCopied(true);
      setTimeout(() => setCopied(false), 1800);
    } catch {
      setExportError('Zwischenablage nicht verfügbar.');
    }
  };

  const handleDownloadTxt = () => {
    const blob = new Blob([doc.reportText], {
      type: 'text/plain;charset=utf-8',
    });
    triggerBlobDownload(blob, `${baseName}_report.txt`);
  };

  const handleDownloadJson = () => {
    const json = buildJsonSchema3(doc);
    const blob = new Blob([json], { type: 'application/json;charset=utf-8' });
    triggerBlobDownload(blob, `${baseName}_schema3.json`);
  };

  const handleDownloadPng = async () => {
    setExportingPng(true);
    setExportError(null);
    try {
      const blob = await renderWaterfallPngBlob({ doc, view, t0Ms, t1Ms });
      triggerBlobDownload(blob, `${baseName}_${view}.png`);
    } catch (err) {
      setExportError(
        err instanceof Error ? err.message : 'PNG-Export fehlgeschlagen.'
      );
    } finally {
      setExportingPng(false);
    }
  };

  const pct = fakten.budgetPcts;
  const budgetSegments = [
    { label: 'SILK Header', pct: pct.silkHead, color: '#38BDF8' },
    { label: 'SILK Pulse', pct: pct.silkPulses, color: '#2563EB' },
    { label: 'LBRR / FEC', pct: pct.fec, color: '#3CC88C' },
    { label: 'CELT Header', pct: pct.celtHead, color: '#FBBF24' },
    { label: 'CELT Energie', pct: pct.energy, color: '#F97316' },
    { label: 'CELT PVQ', pct: pct.pvq, color: '#EA580C' },
    { label: 'Redundanz', pct: pct.redundancy, color: '#DC46A0' },
    { label: 'Ungenutzt', pct: pct.unused, color: '#475569' },
  ].filter((s) => s.pct > 0.05);

  return (
    <div className="flex h-full flex-col gap-3 overflow-y-auto pb-4">
      {/* KPI Overview */}
      <div className="grid grid-cols-2 gap-2 sm:grid-cols-4">
        <div className="rounded-lg border border-slate-800 bg-[#131824] p-3">
          <div className="text-[11px] font-medium uppercase tracking-wider text-slate-400">
            Ø Bitrate
          </div>
          <div className="mt-1 font-mono-tabular text-lg font-semibold text-cyan-300">
            {summary.avgBitrateKbps.toFixed(1)} kbps
          </div>
          <div className="mt-0.5 font-mono-tabular text-[11px] text-slate-400">
            {summary.minBitrateKbps.toFixed(0)}–{summary.maxBitrateKbps.toFixed(0)} kbps
          </div>
        </div>

        <div className="rounded-lg border border-slate-800 bg-[#131824] p-3">
          <div className="text-[11px] font-medium uppercase tracking-wider text-slate-400">
            Dauer &amp; Pakete
          </div>
          <div className="mt-1 font-mono-tabular text-lg font-semibold text-slate-100">
            {summary.durationSec.toFixed(2)} s
          </div>
          <div className="mt-0.5 font-mono-tabular text-[11px] text-slate-400">
            {summary.packetCount} Pakete · {fakten.compressionRatio.toFixed(1)}× Komp.
          </div>
        </div>

        <div className="rounded-lg border border-slate-800 bg-[#131824] p-3">
          <div className="text-[11px] font-medium uppercase tracking-wider text-slate-400">
            Pegel (RMS / Peak)
          </div>
          <div className="mt-1 font-mono-tabular text-lg font-semibold text-slate-100">
            {fakten.pcmStats.rmsDbfs.toFixed(1)} dBFS
          </div>
          <div className="mt-0.5 font-mono-tabular text-[11px] text-slate-400">
            Peak {fakten.pcmStats.peakDbfs.toFixed(1)} dB · Crest {fakten.pcmStats.crestDb.toFixed(1)} dB
          </div>
        </div>

        <div className="rounded-lg border border-slate-800 bg-[#131824] p-3">
          <div className="text-[11px] font-medium uppercase tracking-wider text-slate-400">
            Bitstrom-Walk
          </div>
          <div className="mt-1 font-mono-tabular text-lg font-semibold text-emerald-400">
            {fakten.allBudgetsExact ? '100% Exakt' : `${fakten.walkErrors} Abweich.`}
          </div>
          <div className="mt-0.5 font-mono-tabular text-[11px] text-slate-400">
            Encoder: {fakten.encoderShort}
          </div>
        </div>
      </div>

      {/* Stacked Bit-Budget Breakdown */}
      <div className="rounded-lg border border-slate-800 bg-[#131824] p-3">
        <div className="mb-2 flex items-center justify-between">
          <h3 className="text-xs font-semibold uppercase tracking-wider text-slate-300">
            Gesamt-Bit-Budget
          </h3>
          <span className="font-mono-tabular text-xs text-slate-400">
            {summary.totalBytes.toLocaleString('de-DE')} Bytes
          </span>
        </div>

        <div className="flex h-4 w-full overflow-hidden rounded bg-slate-900">
          {budgetSegments.map((s) => (
            <div
              key={s.label}
              title={`${s.label}: ${s.pct.toFixed(1)}%`}
              style={{ width: `${s.pct}%`, backgroundColor: s.color }}
            />
          ))}
        </div>

        <div className="mt-2.5 flex flex-wrap gap-x-4 gap-y-1.5 font-mono-tabular text-[11px] text-slate-300">
          {budgetSegments.map((s) => (
            <div key={s.label} className="flex items-center gap-1.5">
              <span
                className="inline-block h-2.5 w-2.5 rounded-sm"
                style={{ backgroundColor: s.color }}
              />
              <span>
                {s.label} {s.pct.toFixed(1)}%
              </span>
            </div>
          ))}
        </div>
      </div>

      {/* Export Buttons */}
      <div className="flex flex-wrap items-center gap-2">
        <button
          type="button"
          onClick={() => void handleCopy()}
          className="inline-flex min-h-[44px] items-center gap-2 rounded-lg border border-slate-700 bg-[#131824] px-3.5 py-2 text-xs font-medium text-slate-100 hover:border-cyan-500/60"
        >
          {copied ? (
            <Check className="h-4 w-4 text-emerald-400" />
          ) : (
            <Copy className="h-4 w-4 text-cyan-400" />
          )}
          <span>{copied ? 'Kopiert' : 'Report kopieren'}</span>
        </button>

        <button
          type="button"
          onClick={handleDownloadTxt}
          className="inline-flex min-h-[44px] items-center gap-2 rounded-lg border border-slate-700 bg-[#131824] px-3.5 py-2 text-xs font-medium text-slate-100 hover:border-cyan-500/60"
        >
          <FileText className="h-4 w-4 text-cyan-400" />
          <span>Report (.txt)</span>
        </button>

        <button
          type="button"
          onClick={handleDownloadJson}
          className="inline-flex min-h-[44px] items-center gap-2 rounded-lg border border-slate-700 bg-[#131824] px-3.5 py-2 text-xs font-medium text-slate-100 hover:border-cyan-500/60"
        >
          <FileJson className="h-4 w-4 text-cyan-400" />
          <span>JSON Schema 3</span>
        </button>

        <button
          type="button"
          onClick={() => void handleDownloadPng()}
          disabled={exportingPng}
          className="inline-flex min-h-[44px] items-center gap-2 rounded-lg bg-cyan-500 px-3.5 py-2 text-xs font-semibold text-slate-950 hover:bg-cyan-400 disabled:opacity-50"
        >
          {exportingPng ? (
            <Download className="h-4 w-4 animate-bounce" />
          ) : (
            <ImageIcon className="h-4 w-4" />
          )}
          <span>
            {exportingPng ? 'PNG wird gerendert …' : 'Wasserfall (.png)'}
          </span>
        </button>
      </div>

      {exportError && (
        <div className="rounded-lg border border-rose-500/40 bg-rose-950/40 px-3 py-2 text-xs text-rose-200">
          {exportError}
        </div>
      )}

      {/* German Text Report */}
      <div className="rounded-lg border border-slate-800 bg-[#131824] p-3">
        <pre className="overflow-x-auto whitespace-pre font-mono-tabular text-[11px] leading-relaxed text-slate-200 sm:text-xs">
          {doc.reportText}
        </pre>
      </div>
    </div>
  );
};
