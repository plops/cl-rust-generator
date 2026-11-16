import React, { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import {
  Activity,
  AudioWaveform,
  FileText,
  FolderOpen,
  ListTree,
} from 'lucide-react';
import { OpusAudioPlayer, PlayerSnapshot } from './audio/player';
import { FramesTab } from './components/FramesTab';
import { OfflineIndicator } from './components/OfflineIndicator';
import { PWAInstallButton } from './components/PWAInstallButton';
import { ReportTab } from './components/ReportTab';
import { TouchInspectorSheet } from './components/TouchInspectorSheet';
import { TransportBar } from './components/TransportBar';
import { InspectPoint, WaterfallCanvas } from './components/WaterfallCanvas';
import { DemoItem, WelcomeLoader } from './components/WelcomeLoader';
import { AnalysisProgress, analyzeOpusBytes } from './opus/analyzer';
import { MODE_COLORS } from './opus/model';
import { OpusDocument, PacketWalk, ViewKind } from './opus/types';

type MainTab = 'waterfall' | 'frames' | 'report';

const VIEW_TABS: Array<{ id: ViewKind; label: string; keyHint: string }> = [
  { id: 'spectrum', label: 'Spektrum', keyHint: '1' },
  { id: 'bitstream', label: 'Bitstrom', keyHint: '2' },
  { id: 'divergence', label: 'Divergenz', keyHint: '3' },
  { id: 'density', label: 'Dichte', keyHint: '4' },
];

export default function App() {
  const playerRef = useRef<OpusAudioPlayer | null>(null);
  if (!playerRef.current) {
    playerRef.current = new OpusAudioPlayer();
  }

  const [doc, setDoc] = useState<OpusDocument | null>(null);
  const [progress, setProgress] = useState<AnalysisProgress | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [showFileModal, setShowFileModal] = useState(false);

  const [activeTab, setActiveTab] = useState<MainTab>('waterfall');
  const [view, setView] = useState<ViewKind>('spectrum');
  const [t0Ms, setT0Ms] = useState(0);
  const [t1Ms, setT1Ms] = useState(1000);
  const [interactionMode, setInteractionMode] = useState<'pan' | 'scrub'>('pan');
  const [inspectPoint, setInspectPoint] = useState<InspectPoint | null>(null);

  const [playerSnap, setPlayerSnap] = useState<PlayerSnapshot>(() =>
    playerRef.current!.getSnapshot()
  );

  // Subscribe to Web Audio Player (60 FPS during playback)
  useEffect(() => {
    const player = playerRef.current!;
    const unsub = player.subscribe(setPlayerSnap);
    return () => {
      unsub();
      player.dispose();
    };
  }, []);

  const loadBytes = useCallback(async (fileName: string, bytes: Uint8Array) => {
    setLoadError(null);
    setProgress({ stage: 'demux', label: 'Datei wird vorbereitet …', pct: 4 });
    try {
      const nextDoc = await analyzeOpusBytes(fileName, bytes, setProgress);
      playerRef.current!.loadPcm(nextDoc.pcmChannels, nextDoc.sampleRate);
      setDoc(nextDoc);
      setT0Ms(0);
      setT1Ms(Math.max(20, nextDoc.summary.durationMs));
      setInspectPoint({ tMs: 0, freqHz: 1000 });
      setActiveTab('waterfall');
      setShowFileModal(false);
    } catch (err) {
      setLoadError(
        err instanceof Error
          ? err.message
          : 'Opus-Datei konnte nicht analysiert werden.'
      );
      setProgress(null);
    }
  }, []);

  const handleSelectFile = useCallback(
    async (file: File) => {
      try {
        const buf = await file.arrayBuffer();
        await loadBytes(file.name, new Uint8Array(buf));
      } catch (err) {
        setLoadError(
          err instanceof Error
            ? err.message
            : 'Datei konnte nicht gelesen werden.'
        );
      }
    },
    [loadBytes]
  );

  const handleSelectDemo = useCallback(
    async (demo: DemoItem) => {
      setLoadError(null);
      setProgress({
        stage: 'demux',
        label: `Demo „${demo.title}“ wird geladen …`,
        pct: 5,
      });
      try {
        const res = await fetch(demo.url);
        if (!res.ok) {
          throw new Error(`Demo konnte nicht geladen werden (${res.status}).`);
        }
        const buf = await res.arrayBuffer();
        await loadBytes(demo.fileName, new Uint8Array(buf));
      } catch (err) {
        setLoadError(
          err instanceof Error
            ? err.message
            : 'Demo-Datei konnte nicht geladen werden.'
        );
        setProgress(null);
      }
    },
    [loadBytes]
  );

  const resetZoom = useCallback(() => {
    if (!doc) return;
    setT0Ms(0);
    setT1Ms(Math.max(20, doc.summary.durationMs));
  }, [doc]);

  // Global keyboard shortcuts (1-4 views, R reset zoom, Space play/pause)
  useEffect(() => {
    if (!doc) return;
    const onKeyDown = (e: KeyboardEvent) => {
      const tag = (e.target as HTMLElement | null)?.tagName;
      if (tag === 'INPUT' || tag === 'TEXTAREA') return;

      if (e.key === '1') setView('spectrum');
      else if (e.key === '2') setView('bitstream');
      else if (e.key === '3') setView('divergence');
      else if (e.key === '4') setView('density');
      else if (e.key === 'r' || e.key === 'R') resetZoom();
      else if (e.code === 'Space') {
        e.preventDefault();
        void playerRef.current!.toggle();
      }
    };
    window.addEventListener('keydown', onKeyDown);
    return () => window.removeEventListener('keydown', onKeyDown);
  }, [doc, resetZoom]);

  const handleSeek = useCallback((ms: number) => {
    playerRef.current!.seek(ms);
  }, []);

  const handleSetA = useCallback(() => {
    const cur = playerRef.current!.getSnapshot();
    const nextA = cur.currentTimeMs;
    const nextB = cur.loop.bMs;
    const shouldActivate =
      nextB !== null && Math.abs(nextB - nextA) >= 40 ? true : cur.loop.active;
    playerRef.current!.setLoop(nextA, nextB, shouldActivate);
  }, []);

  const handleSetB = useCallback(() => {
    const cur = playerRef.current!.getSnapshot();
    const nextB = cur.currentTimeMs;
    const nextA = cur.loop.aMs;
    const shouldActivate =
      nextA !== null && Math.abs(nextB - nextA) >= 40 ? true : cur.loop.active;
    playerRef.current!.setLoop(nextA, nextB, shouldActivate);
  }, []);

  const handleToggleLoop = useCallback(() => {
    const cur = playerRef.current!.getSnapshot();
    playerRef.current!.setLoop(cur.loop.aMs, cur.loop.bMs, !cur.loop.active);
  }, []);

  const handleClearLoop = useCallback(() => {
    playerRef.current!.setLoop(null, null, false);
  }, []);

  const handleSelectPacket = useCallback(
    (pkt: PacketWalk) => {
      if (!doc) return;
      const t = pkt.meta.tStartMs;
      playerRef.current!.seek(t);
      setInspectPoint((prev) => ({
        tMs: t,
        freqHz: prev?.freqHz ?? 1000,
      }));
      // Center zoomed window if packet lies outside current viewport
      if (t < t0Ms || t > t1Ms) {
        const span = Math.max(200, t1Ms - t0Ms);
        const total = Math.max(20, doc.summary.durationMs);
        let a = Math.max(0, t - span * 0.35);
        let b = a + span;
        if (b > total) {
          b = total;
          a = Math.max(0, b - span);
        }
        setT0Ms(a);
        setT1Ms(b);
      }
      setActiveTab('waterfall');
    },
    [doc, t0Ms, t1Ms]
  );

  const isZoomed = useMemo(() => {
    if (!doc) return false;
    return t0Ms > 5 || t1Ms < doc.summary.durationMs - 5;
  }, [doc, t0Ms, t1Ms]);

  const dominantModeLabel = useMemo(() => {
    if (!doc) return '';
    const m = doc.summary.modes[0];
    return m ? `${m.mode} ${Math.round(m.pct)}%` : 'Opus';
  }, [doc]);

  const dominantModeColor = useMemo(() => {
    if (!doc || doc.summary.modes.length === 0) return '#06B6D4';
    return MODE_COLORS[doc.summary.modes[0].mode];
  }, [doc]);

  return (
    <div className="pt-safe pb-safe flex h-dvh w-full flex-col overflow-hidden bg-[#0B0E17] text-slate-100">
      {/* Compact Top App Bar */}
      <header className="flex shrink-0 items-center justify-between gap-2 border-b border-slate-800 bg-[#131824] px-3 py-1.5 sm:px-4">
        <div className="flex min-w-0 items-center gap-2.5">
          <div className="flex h-8 w-8 shrink-0 items-center justify-center rounded-lg border border-cyan-500/30 bg-cyan-500/10">
            <AudioWaveform className="h-4 w-4 text-cyan-400" />
          </div>
          <div className="min-w-0">
            <div className="flex items-center gap-2">
              <h1 className="truncate text-sm font-semibold tracking-tight text-slate-100">
                {doc ? doc.fileName : 'Opus-Vis'}
              </h1>
              {doc && (
                <span
                  className="hidden h-2 w-2 shrink-0 rounded-full sm:inline-block"
                  style={{ backgroundColor: dominantModeColor }}
                />
              )}
            </div>
            {doc ? (
              <p className="truncate font-mono-tabular text-[11px] text-slate-400">
                {doc.summary.durationSec.toFixed(1)}s ·{' '}
                {doc.summary.avgBitrateKbps.toFixed(1)} kbps · {dominantModeLabel}
                {doc.head.channels > 1 ? ' · Stereo' : ' · Mono'}
              </p>
            ) : (
              <p className="truncate text-[11px] text-slate-400">
                Opus Bitstrom- &amp; Audio-Inspektor
              </p>
            )}
          </div>
        </div>

        <div className="flex shrink-0 items-center gap-2">
          <PWAInstallButton />
          <button
            type="button"
            onClick={() => {
              setLoadError(null);
              setShowFileModal(true);
            }}
            className="inline-flex min-h-[44px] items-center gap-1.5 rounded-lg border border-slate-700 bg-[#0B0E17] px-3 py-2 text-xs font-medium text-slate-100 transition-colors hover:border-cyan-500/60"
          >
            <FolderOpen className="h-4 w-4 text-cyan-400" />
            <span className="hidden sm:inline">Datei öffnen</span>
          </button>
        </div>
      </header>

      {/* Main Content */}
      {!doc ? (
        <WelcomeLoader
          progress={progress}
          error={loadError}
          onSelectFile={(f) => void handleSelectFile(f)}
          onSelectDemo={(d) => void handleSelectDemo(d)}
        />
      ) : (
        <main className="flex min-h-0 flex-1 flex-col gap-2 px-2 py-2 sm:px-3">
          {/* Desktop / Tablet Top Tab Switcher (hidden on mobile where bottom nav lives) */}
          <div className="hidden shrink-0 items-center justify-between gap-2 sm:flex">
            <div className="inline-flex rounded-lg border border-slate-800 bg-[#131824] p-0.5">
              <button
                type="button"
                onClick={() => setActiveTab('waterfall')}
                className={`inline-flex min-h-[38px] items-center gap-1.5 rounded-md px-3 text-xs font-medium transition-colors ${
                  activeTab === 'waterfall'
                    ? 'bg-cyan-500/15 text-cyan-300'
                    : 'text-slate-400 hover:text-slate-100'
                }`}
              >
                <Activity className="h-3.5 w-3.5" />
                <span>Spektrogramm</span>
              </button>
              <button
                type="button"
                onClick={() => setActiveTab('frames')}
                className={`inline-flex min-h-[38px] items-center gap-1.5 rounded-md px-3 text-xs font-medium transition-colors ${
                  activeTab === 'frames'
                    ? 'bg-cyan-500/15 text-cyan-300'
                    : 'text-slate-400 hover:text-slate-100'
                }`}
              >
                <ListTree className="h-3.5 w-3.5" />
                <span>Frames ({doc.packets.length})</span>
              </button>
              <button
                type="button"
                onClick={() => setActiveTab('report')}
                className={`inline-flex min-h-[38px] items-center gap-1.5 rounded-md px-3 text-xs font-medium transition-colors ${
                  activeTab === 'report'
                    ? 'bg-cyan-500/15 text-cyan-300'
                    : 'text-slate-400 hover:text-slate-100'
                }`}
              >
                <FileText className="h-3.5 w-3.5" />
                <span>Report &amp; Export</span>
              </button>
            </div>

            {activeTab === 'waterfall' && (
              <div className="inline-flex rounded-lg border border-slate-800 bg-[#131824] p-0.5">
                {VIEW_TABS.map((vt) => (
                  <button
                    key={vt.id}
                    type="button"
                    onClick={() => setView(vt.id)}
                    className={`min-h-[38px] rounded-md px-3 text-xs font-medium transition-colors ${
                      view === vt.id
                        ? 'bg-cyan-500 text-slate-950 font-semibold'
                        : 'text-slate-300 hover:text-white'
                    }`}
                  >
                    {vt.label}
                  </button>
                ))}
              </div>
            )}
          </div>

          {/* Mobile 4-View Segmented Bar (when in Spektrogramm tab) */}
          {activeTab === 'waterfall' && (
            <div className="grid shrink-0 grid-cols-4 gap-1 rounded-lg border border-slate-800 bg-[#131824] p-0.5 sm:hidden">
              {VIEW_TABS.map((vt) => (
                <button
                  key={vt.id}
                  type="button"
                  onClick={() => setView(vt.id)}
                  className={`min-h-[36px] rounded-md px-1 text-[11px] font-medium transition-colors ${
                    view === vt.id
                      ? 'bg-cyan-500 text-slate-950 font-semibold'
                      : 'text-slate-300'
                  }`}
                >
                  {vt.label}
                </button>
              ))}
            </div>
          )}

          {/* Active Workspace */}
          {activeTab === 'waterfall' && (
            <div className="flex min-h-0 flex-1 flex-col gap-2">
              <div className="min-h-[260px] flex-1">
                <WaterfallCanvas
                  doc={doc}
                  view={view}
                  t0Ms={t0Ms}
                  t1Ms={t1Ms}
                  playheadMs={playerSnap.currentTimeMs}
                  loop={playerSnap.loop}
                  inspectPoint={inspectPoint}
                  interactionMode={interactionMode}
                  onRangeChange={(a, b) => {
                    setT0Ms(a);
                    setT1Ms(b);
                  }}
                  onSeek={handleSeek}
                  onInspect={setInspectPoint}
                  onResetZoom={resetZoom}
                />
              </div>

              <TouchInspectorSheet
                doc={doc}
                view={view}
                inspectPoint={inspectPoint}
                playheadMs={playerSnap.currentTimeMs}
              />

              <TransportBar
                isPlaying={playerSnap.isPlaying}
                currentTimeMs={playerSnap.currentTimeMs}
                durationMs={playerSnap.durationMs}
                loop={playerSnap.loop}
                interactionMode={interactionMode}
                isZoomed={isZoomed}
                onTogglePlay={() => void playerRef.current!.toggle()}
                onSeek={handleSeek}
                onSetA={handleSetA}
                onSetB={handleSetB}
                onToggleLoop={handleToggleLoop}
                onClearLoop={handleClearLoop}
                onChangeInteractionMode={setInteractionMode}
                onResetZoom={resetZoom}
              />
            </div>
          )}

          {activeTab === 'frames' && (
            <div className="min-h-0 flex-1">
              <FramesTab
                doc={doc}
                playheadMs={playerSnap.currentTimeMs}
                onSelectPacket={handleSelectPacket}
              />
            </div>
          )}

          {activeTab === 'report' && (
            <div className="min-h-0 flex-1">
              <ReportTab doc={doc} view={view} t0Ms={t0Ms} t1Ms={t1Ms} />
            </div>
          )}
        </main>
      )}

      {/* Mobile Bottom Thumb Navigation (shown when file loaded) */}
      {doc && (
        <nav
          aria-label="Hauptnavigation"
          className="grid shrink-0 grid-cols-3 border-t border-slate-800 bg-[#131824] sm:hidden"
        >
          <button
            type="button"
            onClick={() => setActiveTab('waterfall')}
            className={`flex min-h-[48px] flex-col items-center justify-center gap-0.5 text-[11px] font-medium ${
              activeTab === 'waterfall' ? 'text-cyan-400' : 'text-slate-400'
            }`}
          >
            <Activity className="h-4 w-4" />
            <span>Spektrogramm</span>
          </button>
          <button
            type="button"
            onClick={() => setActiveTab('frames')}
            className={`flex min-h-[48px] flex-col items-center justify-center gap-0.5 text-[11px] font-medium ${
              activeTab === 'frames' ? 'text-cyan-400' : 'text-slate-400'
            }`}
          >
            <ListTree className="h-4 w-4" />
            <span>Frames</span>
          </button>
          <button
            type="button"
            onClick={() => setActiveTab('report')}
            className={`flex min-h-[48px] flex-col items-center justify-center gap-0.5 text-[11px] font-medium ${
              activeTab === 'report' ? 'text-cyan-400' : 'text-slate-400'
            }`}
          >
            <FileText className="h-4 w-4" />
            <span>Report</span>
          </button>
        </nav>
      )}

      {/* File Open Modal */}
      {showFileModal && (
        <WelcomeLoader
          isModal
          progress={progress}
          error={loadError}
          onSelectFile={(f) => void handleSelectFile(f)}
          onSelectDemo={(d) => void handleSelectDemo(d)}
          onClose={() => setShowFileModal(false)}
        />
      )}

      <OfflineIndicator />
    </div>
  );
}
