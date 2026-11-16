import { Fakten, OpusHead, OpusTags, Summary } from './types';

function f1(n: number): string {
  return n.toFixed(1);
}
function f2(n: number): string {
  return n.toFixed(2);
}

export function buildGermanReport(params: {
  fileName: string;
  head: OpusHead;
  tags: OpusTags;
  summary: Summary;
  fakten: Fakten;
}): string {
  const { fileName, head, summary, fakten } = params;
  const lines: string[] = [];

  lines.push('opus-report');
  lines.push(
    `datei:       ${fileName} (${(fakten.fileSize / 1024).toFixed(1)} KiB, ${f2(summary.durationSec)} s, ${summary.packetCount} Pakete)`
  );
  lines.push(
    `kopf:        ${head.channels === 2 ? 'Stereo (2 Kanäle)' : 'Mono (1 Kanal)'} · Pre-Skip ${head.preSkip} Samples · Input ${head.inputSampleRate || 48000} Hz · Gain ${f2(head.outputGainDb)} dB`
  );
  lines.push(`encoder:     ${fakten.encoderFull}`);
  lines.push(
    `bitrate:     avg ${f1(summary.avgBitrateKbps)} kbps · min ${f1(summary.minBitrateKbps)} · p5 ${f1(summary.p5BitrateKbps)} · p95 ${f1(summary.p95BitrateKbps)} · max ${f1(summary.maxBitrateKbps)} kbps (VBR-Spreizung ${f2(summary.vbrSpreadRatio)}x)`
  );
  lines.push(
    `kompression: ${f1(fakten.compressionRatio)}x gegenüber PCM (${fakten.pcmBitrateKbps} kbps @ 48 kHz 16-bit)`
  );

  const modiStr = summary.modes
    .map((m) => `${m.mode} ${f1(m.pct)}% (${f2(m.seconds)}s)`)
    .join(' · ');
  lines.push(`modi:        ${modiStr || 'keine'}`);
  lines.push(
    `wechsel:     ${summary.modeSwitches} Moduswechsel · ${summary.bandwidthSwitches} Bandbreitenwechsel · Framegrößen ${summary.frameSizesMs.map((s) => `${s} ms`).join(', ')}`
  );

  const bwStr = summary.bandwidths
    .map((b) => `${b.band} ${f1(b.pct)}%`)
    .join(' · ');
  lines.push(`bandbreite:  ${bwStr || 'keine'}`);

  if (summary.modeCounts.SILK + summary.modeCounts.Hybrid > 0) {
    const pitchPart =
      fakten.pitchMedianHz !== null
        ? `F0 Median ${Math.round(fakten.pitchMedianHz)} Hz (${Math.round(fakten.pitchMinHz ?? 0)}–${Math.round(fakten.pitchMaxHz ?? 0)} Hz)`
        : 'keine stimmhaften F0-Frames';
    lines.push(
      `stimme:      stimmhaft ${f1(fakten.voicedPct)}% · stimmlos ${f1(fakten.unvoicedPct)}% · inaktiv ${f1(fakten.inactivePct)}% · ${pitchPart}`
    );
  }

  const st = fakten.pcmStats;
  const stereoInfo =
    st.stereoCorr !== null
      ? ` · Korrelation ${f2(st.stereoCorr)} (Mid ${f1(st.midDb ?? -120)} dBFS / Side ${f1(st.sideDb ?? -120)} dBFS)`
      : '';
  lines.push(
    `signal:      Peak ${f1(st.peakDbfs)} dBFS · RMS ${f1(st.rmsDbfs)} dBFS · Crest ${f1(st.crestDb)} dB · Clipped ${st.clippedSamples}${stereoInfo}`
  );
  lines.push(
    `stille:      ${f1(fakten.silentPct)}% (${fakten.silentHops} Hops <= -65 dBFS) · Spektraler Schwerpunkt Median ${Math.round(fakten.centroidMedianHz)} Hz`
  );
  lines.push(
    `transienten: ${summary.transientFrames} CELT-Transienten (${f2(fakten.transientRatePerSec)}/s)`
  );

  const bp = fakten.budgetPcts;
  lines.push(
    `bits:        SILK-Kopf ${f1(bp.silkHead)}% · SILK-Puls ${f1(bp.silkPulses)}% · FEC ${f1(bp.fec)}% · CELT-Kopf ${f1(bp.celtHead)}% · Energie ${f1(bp.energy)}% · PVQ ${f1(bp.pvq)}% · Redundanz ${f1(bp.redundancy)}% · Frei ${f1(bp.unused)}%`
  );
  lines.push(
    `schutz:      LBRR/FEC ${summary.fecFrames} Pakete (${f1(fakten.lbrrPct)}%) · Redundanz ${summary.redundancyFrames} (SILK->CELT ${fakten.redundancySilkToCelt}, CELT->SILK ${fakten.redundancyCeltToSilk}) · Mid-Only ${f1(fakten.midOnlyPct)}% · PLC ${summary.plcFrames}`
  );
  lines.push(
    `pruefung:    ${fakten.walkErrors === 0 ? '0 Decoder-Fehler' : `${fakten.walkErrors} Decoder-Fehler`} · Budget-Bilanz ${fakten.allBudgetsExact ? 'exakt (alle Frames 64*Bytes)' : 'gerundet'}`
  );

  // Diagnostic interpretation rules (max 5 + 2)
  const rules: string[] = [];

  if (summary.modes.length === 1 && summary.modes[0].mode === 'SILK') {
    rules.push(
      `Reine SILK-Codierung (${summary.bandwidths[0]?.band ?? 'WB'}): optimiert für Sprache über LPC-Formanten und Pitch-Prädiktion.`
    );
  } else if (summary.modes.length === 1 && summary.modes[0].mode === 'CELT') {
    rules.push(
      `Durchgehender CELT-Modus (${summary.bandwidths[0]?.band ?? 'FB'}): MDCT-Bänder mit feiner Energie- und PVQ-Pulsverteilung für Musik/Audio.`
    );
  } else if (summary.modeSwitches > 0 || summary.modeCounts.Hybrid > 0) {
    rules.push(
      `Dynamische Modus-Steuerung mit ${summary.modeSwitches} Wechseln zwischen SILK/Hybrid (Sprache bis 8 kHz) und CELT (Breitband/Musik).`
    );
  }

  if (summary.vbrSpreadRatio < 1.15) {
    rules.push(
      `Nahezu konstante Bitrate (CBR/Constrained-VBR, Spreizung ${f2(summary.vbrSpreadRatio)}x) — gleichmäßige Paketgröße für Streaming.`
    );
  } else if (summary.vbrSpreadRatio >= 1.8) {
    rules.push(
      `Ausgeprägtes VBR-Profil (Spreizung ${f2(summary.vbrSpreadRatio)}x, ${f1(summary.p5BitrateKbps)}–${f1(summary.p95BitrateKbps)} kbps): spart Bits in Pausen und investiert in komplexe Transienten.`
    );
  }

  if (summary.fecFrames > 0 || summary.redundancyFrames > 0) {
    rules.push(
      `Aktiver Paketverlust-Schutz (${summary.fecFrames} LBRR-FEC, ${summary.redundancyFrames} Redundanz-Frames) erhöht Robustheit bei Netzverlust.`
    );
  }

  if (st.clippedSamples > 0) {
    rules.push(
      `Achtung: ${st.clippedSamples} Samples erreichen >= 0.999 FS (mögliches Clipping, Peak ${f1(st.peakDbfs)} dBFS).`
    );
  }

  if (fakten.midOnlyPct > 30 && head.channels === 2) {
    rules.push(
      `SILK-Stereo nutzt überwiegend Mid-Only-Codierung (${f1(fakten.midOnlyPct)}%): effizient für zentrierte Sprachsignale.`
    );
  }

  if (summary.transientFrames > 0 && fakten.transientRatePerSec > 2) {
    rules.push(
      `Hohe Transientendichte (${f2(fakten.transientRatePerSec)}/s): CELT verkürzt häufig auf Kurzblöcke zur Vermeidung von Pre-Echo.`
    );
  }

  if (fakten.silentPct >= 15) {
    rules.push(
      `Signifikanter Stille-/Pausenanteil (${f1(fakten.silentPct)}% Hops unter -65 dBFS).`
    );
  }

  lines.push('');
  lines.push('einordnung:');
  for (const r of rules.slice(0, 7)) {
    lines.push(`- ${r}`);
  }

  return lines.join('\n');
}
