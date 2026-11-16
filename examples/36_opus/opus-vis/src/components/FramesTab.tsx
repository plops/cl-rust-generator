import React, { useMemo, useState } from 'react';
import { Search } from 'lucide-react';
import { MODE_COLORS } from '../opus/model';
import { OpusDocument, PacketWalk, budgetSum } from '../opus/types';

type FrameFilter = 'all' | 'modeSwitch' | 'redundancy' | 'fec' | 'transient';

interface FramesTabProps {
  doc: OpusDocument;
  playheadMs: number;
  onSelectPacket: (pkt: PacketWalk) => void;
}

const FILTER_LABELS: Array<{ id: FrameFilter; label: string }> = [
  { id: 'all', label: 'Alle' },
  { id: 'modeSwitch', label: 'Moduswechsel' },
  { id: 'redundancy', label: 'Redundanz' },
  { id: 'fec', label: 'FEC' },
  { id: 'transient', label: 'Transienten' },
];

const SIGNAL_SHORT: Record<string, string> = {
  Voiced: 'Stimmhaft',
  Unvoiced: 'Stimmlos',
  Inactive: 'Inaktiv',
};

export const FramesTab: React.FC<FramesTabProps> = ({
  doc,
  playheadMs,
  onSelectPacket,
}) => {
  const [filter, setFilter] = useState<FrameFilter>('all');
  const [query, setQuery] = useState('');
  const [limit, setLimit] = useState(200);

  const filtered = useMemo(() => {
    const q = query.trim().toLowerCase();
    const numMatch = q.startsWith('#') ? q.slice(1) : q;

    return doc.packets.filter((p) => {
      if (filter === 'modeSwitch' && !p.modeChanged) return false;
      if (filter === 'redundancy' && !p.hasRedundancy) return false;
      if (filter === 'fec' && !p.hasFec) return false;
      if (filter === 'transient' && !p.hasTransient) return false;

      if (!q) return true;
      if (/^\d+$/.test(numMatch) && String(p.meta.index).includes(numMatch)) {
        return true;
      }
      if (p.meta.mode.toLowerCase().includes(q)) return true;
      if (p.meta.bandwidth.toLowerCase().includes(q)) return true;
      if (`0x${p.meta.tocByte.toString(16).padStart(2, '0')}`.includes(q)) {
        return true;
      }
      return false;
    });
  }, [doc.packets, filter, query]);

  const visible = useMemo(() => filtered.slice(0, limit), [filtered, limit]);

  return (
    <div className="flex h-full flex-col gap-3 overflow-hidden">
      {/* Filter + Search Bar */}
      <div className="flex flex-col gap-2 rounded-lg border border-slate-800 bg-[#131824] p-3 sm:flex-row sm:items-center sm:justify-between">
        <div className="flex flex-wrap gap-1.5">
          {FILTER_LABELS.map((f) => (
            <button
              key={f.id}
              type="button"
              onClick={() => {
                setFilter(f.id);
                setLimit(200);
              }}
              className={`min-h-[40px] rounded-lg border px-3 text-xs font-medium transition-colors ${
                filter === f.id
                  ? 'border-cyan-500/60 bg-cyan-500/15 text-cyan-200'
                  : 'border-slate-700 bg-[#0B0E17] text-slate-300 hover:border-slate-500'
              }`}
            >
              {f.label}
            </button>
          ))}
        </div>

        <div className="relative w-full sm:w-56">
          <Search className="pointer-events-none absolute left-3 top-1/2 h-4 w-4 -translate-y-1/2 text-slate-500" />
          <input
            type="search"
            value={query}
            onChange={(e) => {
              setQuery(e.target.value);
              setLimit(200);
            }}
            placeholder="Suche #Nr, SILK, CELT, FB …"
            className="min-h-[40px] w-full rounded-lg border border-slate-700 bg-[#0B0E17] pl-9 pr-3 text-xs text-slate-100 placeholder:text-slate-500 focus:border-cyan-500 focus:outline-none"
          />
        </div>
      </div>

      {/* Count Strip */}
      <div className="flex items-center justify-between px-1 font-mono-tabular text-xs text-slate-400">
        <span>
          Treffer: {filtered.length} von {doc.packets.length} Paketen
        </span>
        <span>Tippe ein Paket an zum Springen</span>
      </div>

      {/* Packet List */}
      <div className="flex-1 overflow-y-auto rounded-lg border border-slate-800 bg-[#131824]">
        {visible.length === 0 ? (
          <div className="p-8 text-center text-sm text-slate-400">
            Keine Pakete für diesen Filter gefunden.
          </div>
        ) : (
          <div className="divide-y divide-slate-800/80">
            {visible.map((pkt) => {
              const m = pkt.meta;
              const isActive =
                playheadMs >= m.tStartMs && playheadMs < m.tEndMs;
              const totQ3 = Math.max(1, budgetSum(pkt.budget));
              const silkHeadInit = m.mode !== 'CELT' ? pkt.budget.init : 0;
              const celtHeadInit = m.mode === 'CELT' ? pkt.budget.init : 0;

              const segs = [
                {
                  w:
                    ((silkHeadInit +
                      pkt.budget.silkFlags +
                      pkt.budget.silkStereo +
                      pkt.budget.silkSide) /
                      totQ3) *
                    100,
                  c: '#38BDF8',
                },
                { w: (pkt.budget.silkPulses / totQ3) * 100, c: '#2563EB' },
                { w: (pkt.budget.silkLbrr / totQ3) * 100, c: '#3CC88C' },
                {
                  w:
                    ((celtHeadInit +
                      pkt.budget.redHeader +
                      pkt.budget.celtHeader) /
                      totQ3) *
                    100,
                  c: '#FBBF24',
                },
                { w: (pkt.budget.celtEnergy / totQ3) * 100, c: '#F97316' },
                { w: (pkt.budget.celtPvq / totQ3) * 100, c: '#EA580C' },
                { w: (pkt.budget.redundancy / totQ3) * 100, c: '#DC46A0' },
              ];

              const flags: string[] = [];
              if (pkt.modeChanged) flags.push('MODUSWECHSEL');
              if (pkt.hasRedundancy) flags.push('RED');
              if (pkt.hasFec) flags.push('FEC');
              if (pkt.hasTransient) flags.push('TRANS');

              return (
                <button
                  key={m.index}
                  type="button"
                  onClick={() => onSelectPacket(pkt)}
                  className={`flex w-full flex-col gap-1.5 px-3 py-2.5 text-left transition-colors hover:bg-slate-800/50 ${
                    isActive ? 'bg-cyan-500/10' : ''
                  }`}
                >
                  <div className="flex flex-wrap items-center justify-between gap-2 font-mono-tabular text-xs">
                    <div className="flex items-center gap-2">
                      <span
                        className="inline-block h-2.5 w-2.5 shrink-0 rounded-full"
                        style={{ backgroundColor: MODE_COLORS[m.mode] }}
                      />
                      <span className="font-semibold text-slate-100">
                        #{m.index}
                      </span>
                      <span className="text-slate-400">
                        {(m.tStartMs / 1000).toFixed(3)}s
                      </span>
                      <span className="text-slate-500">
                        TOC 0x{m.tocByte.toString(16).padStart(2, '0')}
                      </span>
                    </div>

                    <div className="flex items-center gap-2 text-slate-200">
                      <span style={{ color: MODE_COLORS[m.mode] }}>
                        {m.mode} {m.bandwidth}
                      </span>
                      <span className="text-slate-500">·</span>
                      <span>{m.durationMs.toFixed(0)}ms</span>
                      <span className="text-slate-500">·</span>
                      <span>{m.bytes}B</span>
                      <span className="text-slate-500">·</span>
                      <span className="text-cyan-300">
                        {m.bitrateKbps.toFixed(1)} kbps
                      </span>
                    </div>
                  </div>

                  <div className="flex flex-wrap items-center justify-between gap-2 text-[11px]">
                    <div className="font-mono-tabular text-slate-400">
                      {m.mode !== 'CELT' ? (
                        <span>
                          {SIGNAL_SHORT[pkt.dominantSignal] ?? pkt.dominantSignal}
                          {pkt.pitchHz ? ` · F0 ${Math.round(pkt.pitchHz)} Hz` : ''}
                          {pkt.gainDb !== null ? ` · ${pkt.gainDb.toFixed(0)} dB` : ''}
                        </span>
                      ) : (
                        <span>
                          {m.frameCount} Frame{m.frameCount > 1 ? 's' : ''} · CELT PVQ{' '}
                          {pkt.celtSharePct.toFixed(0)}%
                        </span>
                      )}
                      {flags.length > 0 && (
                        <span className="ml-2 text-amber-300">
                          {flags.join(' · ')}
                        </span>
                      )}
                    </div>

                    {/* Mini stacked budget bar */}
                    <div className="flex h-2 w-36 overflow-hidden rounded bg-slate-900">
                      {segs.map((s, idx) =>
                        s.w > 0.4 ? (
                          <div
                            key={idx}
                            style={{ width: `${s.w}%`, backgroundColor: s.c }}
                          />
                        ) : null
                      )}
                    </div>
                  </div>
                </button>
              );
            })}
          </div>
        )}

        {visible.length < filtered.length && (
          <div className="border-t border-slate-800 p-3 text-center">
            <button
              type="button"
              onClick={() => setLimit((n) => n + 200)}
              className="min-h-[44px] rounded-lg border border-slate-700 bg-[#0B0E17] px-4 py-2 text-xs font-medium text-slate-200 hover:border-cyan-500/60"
            >
              Weitere 200 Pakete laden ({filtered.length - visible.length} übrig)
            </button>
          </div>
        )}
      </div>
    </div>
  );
};
