import React, { useState } from 'react';
import { Download, Share, X } from 'lucide-react';
import { usePWAInstall } from '../hooks/usePWAInstall';

export const PWAInstallButton: React.FC = () => {
  const { isInstallable, isInstalled, isIOS, install } = usePWAInstall();
  const [showIOSGuide, setShowIOSGuide] = useState(false);

  if (isInstalled) {
    return null;
  }

  if (isInstallable) {
    return (
      <button
        type="button"
        onClick={() => void install()}
        className="inline-flex min-h-[44px] items-center gap-2 rounded-lg border border-cyan-500/40 bg-cyan-500/15 px-3.5 py-2 text-xs font-semibold text-cyan-200 transition-colors hover:bg-cyan-500/25 focus-visible:outline-2 focus-visible:outline-cyan-400 whitespace-nowrap shrink-0"
        title="Als Android-App installieren"
      >
        <Download className="h-4 w-4 shrink-0 text-cyan-400" />
        <span>App installieren</span>
      </button>
    );
  }

  if (isIOS) {
    return (
      <>
        <button
          type="button"
          onClick={() => setShowIOSGuide(true)}
          className="inline-flex min-h-[44px] items-center gap-2 rounded-lg border border-slate-700 bg-[#131824] px-3 py-2 text-xs font-medium text-slate-200 transition-colors hover:border-slate-500 focus-visible:outline-2 focus-visible:outline-cyan-400 whitespace-nowrap shrink-0"
        >
          <Share className="h-4 w-4 shrink-0 text-cyan-400" />
          <span>Zum Home-Bildschirm</span>
        </button>

        {showIOSGuide && (
          <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/70 p-4">
            <div className="w-full max-w-sm rounded-xl border border-slate-800 bg-[#131824] p-5 text-slate-100">
              <div className="flex items-center justify-between gap-4">
                <h3 className="text-base font-semibold">Opus-Vis installieren</h3>
                <button
                  type="button"
                  onClick={() => setShowIOSGuide(false)}
                  className="flex h-11 w-11 items-center justify-center rounded-lg text-slate-400 hover:bg-slate-800 hover:text-slate-100"
                  aria-label="Schließen"
                >
                  <X className="h-5 w-5" />
                </button>
              </div>
              <ol className="mt-3 space-y-2 text-sm text-slate-300">
                <li>1. Tippe in Safari auf das Teilen-Symbol.</li>
                <li>2. Wähle „Zum Home-Bildschirm“.</li>
                <li>3. Tippe oben rechts auf „Hinzufügen“.</li>
              </ol>
              <button
                type="button"
                onClick={() => setShowIOSGuide(false)}
                className="mt-5 w-full min-h-[44px] rounded-lg bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 transition-colors hover:bg-cyan-400"
              >
                Verstanden
              </button>
            </div>
          </div>
        )}
      </>
    );
  }

  return null;
};
