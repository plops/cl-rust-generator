import { OpusDocument } from './types';

export function buildJsonSchema3(doc: OpusDocument): string {
  const payload = {
    schema: 3,
    file: doc.fileName,
    sizeBytes: doc.fileSize,
    head: doc.head,
    tags: doc.tags,
    summary: doc.summary,
    fakten: doc.fakten,
    packets: doc.packets.map((p) => ({
      index: p.meta.index,
      tStartMs: Number(p.meta.tStartMs.toFixed(3)),
      tEndMs: Number(p.meta.tEndMs.toFixed(3)),
      durationMs: Number(p.meta.durationMs.toFixed(3)),
      bytes: p.meta.bytes,
      bitrateKbps: Number(p.meta.bitrateKbps.toFixed(2)),
      tocByte: p.meta.tocByte,
      config: p.meta.config,
      mode: p.meta.mode,
      bandwidth: p.meta.bandwidth,
      stereo: p.meta.stereo,
      code: p.meta.code,
      frameCount: p.meta.frameCount,
      vbr: p.meta.vbr,
      paddingBytes: p.meta.paddingBytes,
      modeChanged: p.modeChanged,
      bandwidthChanged: p.bandwidthChanged,
      hasRedundancy: p.hasRedundancy,
      hasFec: p.hasFec,
      hasTransient: p.hasTransient,
      dominantSignal: p.dominantSignal,
      pitchHz: p.pitchHz !== null ? Number(p.pitchHz.toFixed(2)) : null,
      gainDb: p.gainDb !== null ? Number(p.gainDb.toFixed(2)) : null,
      budgetEighthBits: p.budget,
      frames: p.frames.map((f) => ({
        frameIndex: f.frameIndex,
        bytes: f.bytes,
        plc: f.plc,
        finalRange: `0x${f.finalRange.toString(16).padStart(8, '0')}`,
        silk: f.silk
          ? {
              lbrr: f.silk.lbrr,
              lbrrChannels: f.silk.lbrrChannels,
              midOnly: f.silk.midOnly,
              stereoPred: f.silk.stereoPred,
              subframes: f.silk.subframesSilkCount,
              channels: f.silk.channels.map((ch) => ({
                signalType: ch.signalType,
                pitchHz: ch.pitchHz !== null ? Number(ch.pitchHz.toFixed(2)) : null,
                pitchLagSamples: ch.pitchLagSamples,
                gainDb: Number(ch.gainDb.toFixed(2)),
                vad: ch.vad,
                rateLevel: ch.rateLevel,
                pulsesTotal: ch.pulsesTotal,
              })),
            }
          : null,
        celt: f.celt
          ? {
              startBand: f.celt.startBand,
              endBand: f.celt.endBand,
              transient: f.celt.transient,
              shortBlocks: f.celt.shortBlocks,
              intra: f.celt.intra,
              spread: f.celt.spread,
              dynallocCount: f.celt.dynallocCount,
              intensityBand: f.celt.intensityBand,
              dualStereo: f.celt.dualStereo,
              energyDb: f.celt.energyDb.map((v) => Number(v.toFixed(2))),
              bandTotalBits: f.celt.bandTotalBits.map((v) => Number((v / 8).toFixed(2))),
            }
          : null,
        redundancy: f.redundancy
          ? {
              celtToSilk: f.redundancy.celtToSilk,
              bytes: f.redundancy.bytes,
            }
          : null,
      })),
    })),
  };

  return JSON.stringify(payload, null, 2);
}
