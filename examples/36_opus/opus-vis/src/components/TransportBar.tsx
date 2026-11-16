import React from 'react';
import {
  Crosshair,
  Hand,
  Maximize2,
  Pause,
  Play,
  Repeat,
  RotateCcw,
  RotateCw,
  X,
} from 'lucide-react';
import { LoopState } from '../audio/player';

interface TransportBarProps {
  isPlaying: boolean;
  currentTimeMs: number;
  durationMs: number;
  loop: LoopState;
  interactionMode: 'pan' | 'scrub';
  isZoomed: boolean;
  onTogglePlay: () => void;
  onSeek: (ms: number) => void;
  onSetA: () => void;
  onSetB: () => void;
  onToggleLoop: () => void;
  onClearLoop: () => void;
  onChangeInteractionMode: (mode: 'pan' | 'scrub') => void;
  onResetZoom: () => void;
}

function fmtTime(ms: number): string {
  const sec = Math.max(0, ms) / 1000;
  const m = Math.floor(sec / 60);
  const s = sec - m * 60;
  return `${m}:${s < 10 ? '0' : ''}${s.toFixed(2)}`;
}

export const TransportBar: React.FC<TransportBarProps> = ({
  isPlaying,
  currentTimeMs,
  durationMs,
  loop,
  interactionMode,
  isZoomed,
  onTogglePlay,
  onSeek,
  onSetA,
  onSetB,
  onToggleLoop,
  onClearLoop,
  onChangeInteractionMode,
  onResetZoom,
}) => {
  const safeDur = Math.max(1, durationMs);
  const hasBothBounds =
    loop.aMs !== null && loop.bMs !== null && Math.abs(loop.bMs - loop.aMs) >= 40;
  const hasAnyBound = loop.aMs !== null || loop.bMs !== null;

  const loopLeftPct =
    loop.aMs !== null && loop.bMs !== null
      ? (Math.min(loop.aMs, loop.bMs) / safeDur) * 100
      : 0;
  const loopWidthPct =
    loop.aMs !== null && loop.bMs !== null
      ? (Math.abs(loop.bMs - loop.aMs) / safeDur) * 100
      : 0;

  return (
    <div className="rounded-lg border border-slate-800 bg-[#131824] px-3 py-2">
      {/* Timeline Scrubber */}
      <div className="flex items-center gap-3">
        <span className="w-16 shrink-0 font-mono-tabular text-[11px] text-cyan-300">
          {fmtTime(currentTimeMs)}
        </span>

        <div className="relative flex flex-1 items-center py-2">
          <div className="pointer-events-none relative h-1.5 w-full overflow-hidden rounded-full bg-slate-800">
            {hasBothBounds && (
              <div
                className={`absolute top-0 bottom-0 ${
                  loop.active ? 'bg-cyan-500/40' : 'bg-slate-500/35'
                }`}
                style={{ left: `${loopLeftPct}%`, width: `${loopWidthPct}%` }}
              />
            )}
            <div
              className="h-full bg-cyan-500"
              style={{
                width: `${Math.min(100, (currentTimeMs / safeDur) * 100)}%`,
              }}
            />
          </div>
          <input
            type="range"
            min={0}
            max={safeDur}
            step={10}
            value={Math.min(safeDur, currentTimeMs)}
            onChange={(e) => onSeek(Number(e.target.value))}
            aria-label="Wiedergabeposition"
            className="absolute inset-0 h-full w-full cursor-pointer opacity-0"
          />
        </div>

        <span className="w-16 shrink-0 text-right font-mono-tabular text-[11px] text-slate-400">
          {fmtTime(durationMs)}
        </span>
      </div>

      {/* Controls Row */}
      <div className="mt-1 flex flex-wrap items-center justify-between gap-1.5">
        {/* Left: Play / Step */}
        <div className="flex items-center gap-1.5">
          <button
            type="button"
            onClick={onTogglePlay}
            className="inline-flex h-11 min-w-[44px] items-center justify-center gap-1.5 rounded-lg bg-cyan-500 px-3.5 text-xs font-semibold text-slate-950 transition-colors hover:bg-cyan-400 focus-visible:outline-2 focus-visible:outline-cyan-300"
            aria-label={isPlaying ? 'Pause' : 'Abspielen'}
          >
            {isPlaying ? (
              <Pause className="h-4 w-4 fill-current" />
            ) : (
              <Play className="h-4 w-4 fill-current" />
            )}
            <span className="hidden sm:inline">
              {isPlaying ? 'Pause' : 'Play'}
            </span>
          </button>

          <button
            type="button"
            onClick={() => onSeek(Math.max(0, currentTimeMs - 2000))}
            className="inline-flex h-11 min-w-[44px] items-center justify-center rounded-lg border border-slate-700 bg-[#0B0E17] px-2 text-xs font-mono-tabular text-slate-200 hover:border-slate-500"
            title="2 Sekunden zurück"
            aria-label="2 Sekunden zurück"
          >
            <RotateCcw className="h-3.5 w-3.5 sm:mr-1" />
            <span className="hidden sm:inline">-2s</span>
          </button>

          <button
            type="button"
            onClick={() => onSeek(Math.min(durationMs, currentTimeMs + 2000))}
            className="inline-flex h-11 min-w-[44px] items-center justify-center rounded-lg border border-slate-700 bg-[#0B0E17] px-2 text-xs font-mono-tabular text-slate-200 hover:border-slate-500"
            title="2 Sekunden vor"
            aria-label="2 Sekunden vor"
          >
            <RotateCw className="h-3.5 w-3.5 sm:mr-1" />
            <span className="hidden sm:inline">+2s</span>
          </button>
        </div>

        {/* Center/Right: Gesture Mode + A-B Loop */}
        <div className="flex flex-wrap items-center gap-1.5">
          {/* Pan vs Scrub */}
          <div className="inline-flex rounded-lg border border-slate-700 bg-[#0B0E17] p-0.5">
            <button
              type="button"
              onClick={() => onChangeInteractionMode('pan')}
              className={`inline-flex min-h-[38px] items-center gap-1 rounded-md px-2.5 text-xs font-medium transition-colors ${
                interactionMode === 'pan'
                  ? 'bg-slate-800 text-cyan-300'
                  : 'text-slate-400 hover:text-slate-200'
              }`}
              title="1-Finger-Ziehen verschiebt Zeitachse"
            >
              <Hand className="h-3.5 w-3.5" />
              <span>Pan</span>
            </button>
            <button
              type="button"
              onClick={() => onChangeInteractionMode('scrub')}
              className={`inline-flex min-h-[38px] items-center gap-1 rounded-md px-2.5 text-xs font-medium transition-colors ${
                interactionMode === 'scrub'
                  ? 'bg-slate-800 text-cyan-300'
                  : 'text-slate-400 hover:text-slate-200'
              }`}
              title="1-Finger-Ziehen scrubbt Playhead"
            >
              <Crosshair className="h-3.5 w-3.5" />
              <span>Scrub</span>
            </button>
          </div>

          {/* A-B Loop Controls */}
          <div className="inline-flex items-center gap-1 rounded-lg border border-slate-700 bg-[#0B0E17] p-0.5">
            <button
              type="button"
              onClick={onSetA}
              className={`min-h-[38px] min-w-[38px] rounded-md px-2 font-mono-tabular text-xs font-semibold transition-colors ${
                loop.aMs !== null
                  ? 'bg-cyan-500/20 text-cyan-300'
                  : 'text-slate-300 hover:bg-slate-800'
              }`}
              title="Loop-Start A an aktueller Position setzen"
            >
              A
            </button>
            <button
              type="button"
              onClick={onSetB}
              className={`min-h-[38px] min-w-[38px] rounded-md px-2 font-mono-tabular text-xs font-semibold transition-colors ${
                loop.bMs !== null
                  ? 'bg-cyan-500/20 text-cyan-300'
                  : 'text-slate-300 hover:bg-slate-800'
              }`}
              title="Loop-Ende B an aktueller Position setzen"
            >
              B
            </button>
            <button
              type="button"
              onClick={onToggleLoop}
              disabled={!hasBothBounds}
              className={`inline-flex min-h-[38px] items-center gap-1 rounded-md px-2.5 text-xs font-medium transition-colors disabled:opacity-40 ${
                loop.active && hasBothBounds
                  ? 'bg-cyan-500 text-slate-950 font-semibold'
                  : 'text-slate-300 hover:bg-slate-800'
              }`}
              title="A-B-Loop ein/aus"
            >
              <Repeat className="h-3.5 w-3.5" />
              <span className="hidden sm:inline">Loop</span>
            </button>
            {hasAnyBound && (
              <button
                type="button"
                onClick={onClearLoop}
                className="inline-flex min-h-[38px] min-w-[34px] items-center justify-center rounded-md text-slate-400 hover:bg-slate-800 hover:text-slate-100"
                title="A-B-Marken löschen"
                aria-label="A-B-Marken löschen"
              >
                <X className="h-3.5 w-3.5" />
              </button>
            )}
          </div>

          {isZoomed && (
            <button
              type="button"
              onClick={onResetZoom}
              className="inline-flex h-11 items-center gap-1 rounded-lg border border-slate-700 bg-[#0B0E17] px-2.5 text-xs font-medium text-slate-200 hover:border-cyan-500/60"
              title="Zoom zurücksetzen (R)"
            >
              <Maximize2 className="h-3.5 w-3.5 text-cyan-400" />
              <span className="hidden md:inline">100%</span>
            </button>
          )}
        </div>
      </div>
    </div>
  );
};
