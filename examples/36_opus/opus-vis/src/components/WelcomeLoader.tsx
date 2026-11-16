import React, { useRef, useState } from 'react';
import { AudioWaveform, FileAudio, FolderOpen, Sparkles, X } from 'lucide-react';
import { AnalysisProgress } from '../opus/analyzer';

export interface DemoItem {
  id: string;
  title: string;
  subtitle: string;
  meta: string;
  url: string;
  fileName: string;
  badgeColor: string;
}

export const BUILTIN_DEMOS: DemoItem[] = [
  {
    id: 'sprache',
    title: 'Sprache (SILK WB)',
    subtitle: 'Sprachkodierung mit F0-Pitch, LPC-Hüllkurve und LBRR/FEC-Anteilen',
    meta: '20.9 KB · 6.0 s · ~28 kbps · Mono',
    url: '/demos/sprache_silk.opus',
    fileName: 'sprache_silk.opus',
    badgeColor: '#46AAFF',
  },
  {
    id: 'musik',
    title: 'Musik (CELT FB)',
    subtitle: 'Fullband-MDCT-Spektrum bis 20 kHz, 21 Skalenfaktorbänder und PVQ-Dichte',
    meta: '108.9 KB · 6.0 s · ~145 kbps · Stereo',
    url: '/demos/musik_celt.opus',
    fileName: 'musik_celt.opus',
    badgeColor: '#FFB246',
  },
  {
    id: 'wechsel',
    title: 'Moduswechsel (SILK ↔ Hybrid ↔ CELT)',
    subtitle: 'Dynamische Umschaltung zwischen Sprache, Hybrid-Übergang und Musik',
    meta: '47.1 KB · 9.0 s · ~42 kbps · 5 Moduswechsel',
    url: '/demos/wechsel_hybrid.opus',
    fileName: 'wechsel_hybrid.opus',
    badgeColor: '#B978FF',
  },
];

interface WelcomeLoaderProps {
  isModal?: boolean;
  progress: AnalysisProgress | null;
  error: string | null;
  onSelectFile: (file: File) => void;
  onSelectDemo: (demo: DemoItem) => void;
  onClose?: () => void;
}

export const WelcomeLoader: React.FC<WelcomeLoaderProps> = ({
  isModal = false,
  progress,
  error,
  onSelectFile,
  onSelectDemo,
  onClose,
}) => {
  const inputRef = useRef<HTMLInputElement | null>(null);
  const [dragOver, setDragOver] = useState(false);

  const isLoading = progress !== null && progress.stage !== 'done';

  const handleFiles = (files: FileList | null) => {
    const file = files?.[0];
    if (file) onSelectFile(file);
  };

  const content = (
    <div className="mx-auto w-full max-w-2xl rounded-xl border border-slate-800 bg-[#131824] p-5 sm:p-6">
      <div className="flex items-start justify-between gap-4">
        <div className="flex items-center gap-3">
          <div className="flex h-11 w-11 shrink-0 items-center justify-center rounded-lg border border-cyan-500/30 bg-cyan-500/10">
            <AudioWaveform className="h-6 w-6 text-cyan-400" />
          </div>
          <div>
            <h2 className="text-lg font-semibold text-slate-100">
              Opus-Datei öffnen
            </h2>
            <p className="text-xs text-slate-400">
              Lokaler Ogg/Opus-Bitstrom-Inspektor · Keine Cloud-Übertragung
            </p>
          </div>
        </div>

        {isModal && onClose && !isLoading && (
          <button
            type="button"
            onClick={onClose}
            className="flex h-11 w-11 items-center justify-center rounded-lg text-slate-400 hover:bg-slate-800 hover:text-slate-100"
            aria-label="Schließen"
          >
            <X className="h-5 w-5" />
          </button>
        )}
      </div>

      {/* Drop Zone / Picker */}
      <div
        onDragOver={(e) => {
          e.preventDefault();
          setDragOver(true);
        }}
        onDragLeave={() => setDragOver(false)}
        onDrop={(e) => {
          e.preventDefault();
          setDragOver(false);
          handleFiles(e.dataTransfer.files);
        }}
        className={`mt-5 flex flex-col items-center justify-center rounded-xl border border-dashed px-4 py-7 text-center transition-colors ${
          dragOver
            ? 'border-cyan-400 bg-cyan-500/10'
            : 'border-slate-700 bg-[#0B0E17]/70'
        }`}
      >
        <FileAudio className="h-8 w-8 text-cyan-400" />
        <p className="mt-2.5 text-sm font-medium text-slate-200">
          .opus- oder .ogg-Datei hier ablegen oder vom Gerät wählen
        </p>
        <p className="mt-1 text-xs text-slate-400">
          Analysiert Ogg-Container, Range-Coder-Bits (1/8-Bit), STFT &amp; 48-kHz-Audio direkt lokal
        </p>

        <input
          ref={inputRef}
          type="file"
          accept=".opus,.ogg,audio/opus,audio/ogg"
          className="hidden"
          onChange={(e) => {
            handleFiles(e.target.files);
            e.target.value = '';
          }}
        />

        <button
          type="button"
          disabled={isLoading}
          onClick={() => inputRef.current?.click()}
          className="mt-4 inline-flex min-h-[44px] items-center gap-2 rounded-lg bg-cyan-500 px-5 py-2.5 text-sm font-semibold text-slate-950 transition-colors hover:bg-cyan-400 disabled:opacity-50"
        >
          <FolderOpen className="h-4 w-4" />
          <span>Opus-Datei auswählen</span>
        </button>
      </div>

      {/* Progress Bar */}
      {isLoading && progress && (
        <div className="mt-4 rounded-lg border border-cyan-500/30 bg-[#0B0E17] p-3">
          <div className="flex items-center justify-between text-xs">
            <span className="font-medium text-cyan-200">{progress.label}</span>
            <span className="font-mono-tabular text-cyan-300">
              {progress.pct}%
            </span>
          </div>
          <div className="mt-2 h-1.5 w-full overflow-hidden rounded-full bg-slate-800">
            <div
              className="h-full bg-cyan-400 transition-all duration-200"
              style={{ width: `${progress.pct}%` }}
            />
          </div>
        </div>
      )}

      {/* Error Message */}
      {error && (
        <div className="mt-4 rounded-lg border border-rose-500/40 bg-rose-950/40 px-3.5 py-2.5 text-xs text-rose-200">
          {error}
        </div>
      )}

      {/* Offline Demo Presets */}
      <div className="mt-5">
        <div className="mb-2.5 flex items-center gap-2 text-xs font-semibold uppercase tracking-wider text-slate-400">
          <Sparkles className="h-3.5 w-3.5 text-cyan-400" />
          <span>Sofort starten · Offline-Demo-Dateien</span>
        </div>

        <div className="grid gap-2 sm:grid-cols-3">
          {BUILTIN_DEMOS.map((demo) => (
            <button
              key={demo.id}
              type="button"
              disabled={isLoading}
              onClick={() => onSelectDemo(demo)}
              className="flex min-h-[44px] flex-col justify-between rounded-lg border border-slate-800 bg-[#0B0E17] p-3 text-left transition-colors hover:border-cyan-500/60 disabled:opacity-50"
            >
              <div>
                <div className="flex items-center gap-2">
                  <span
                    className="inline-block h-2.5 w-2.5 shrink-0 rounded-full"
                    style={{ backgroundColor: demo.badgeColor }}
                  />
                  <span className="text-xs font-semibold text-slate-100">
                    {demo.title}
                  </span>
                </div>
                <p className="mt-1.5 text-[11px] leading-snug text-slate-400">
                  {demo.subtitle}
                </p>
              </div>
              <div className="mt-2.5 font-mono-tabular text-[10px] text-cyan-300">
                {demo.meta}
              </div>
            </button>
          ))}
        </div>
      </div>
    </div>
  );

  if (isModal) {
    return (
      <div className="fixed inset-0 z-50 flex items-center justify-center overflow-y-auto bg-black/75 p-4 backdrop-blur-xs">
        {content}
      </div>
    );
  }

  return (
    <div className="flex flex-1 items-center justify-center overflow-y-auto p-4">
      {content}
    </div>
  );
};
