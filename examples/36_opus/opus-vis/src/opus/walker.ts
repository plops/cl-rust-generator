import { CeltState, walkCeltFrame } from './celtFrame';
import { RangeDec } from './range';
import { SilkDec } from './silkDec';
import { splitPacket } from './split';
import { bandwidthEndBand, silkBandwidthKhz } from './toc';
import {
  Bandwidth,
  Budget,
  FrameMeta,
  FrameWalk,
  Mode,
  OggPacket,
  PacketWalk,
  RedundancyWalk,
  SignalType,
  SilkOpusFrame,
  CeltFrameWalk,
  budgetCeltBits,
  budgetFecBits,
  budgetRedBits,
  budgetSilkBits,
  budgetSum,
  emptyBudget,
} from './types';

const HYBRID_START_BAND = 17;

function samplesToLm(samples: number): number {
  if (samples <= 120) return 0;
  if (samples <= 240) return 1;
  if (samples <= 480) return 2;
  return 3;
}

function addBudget(dst: Budget, src: Budget): void {
  dst.init += src.init;
  dst.silkFlags += src.silkFlags;
  dst.silkLbrr += src.silkLbrr;
  dst.silkStereo += src.silkStereo;
  dst.silkSide += src.silkSide;
  dst.silkPulses += src.silkPulses;
  dst.redHeader += src.redHeader;
  dst.celtHeader += src.celtHeader;
  dst.celtEnergy += src.celtEnergy;
  dst.celtPvq += src.celtPvq;
  dst.redundancy += src.redundancy;
  dst.unused += src.unused;
}

export class OpusWalker {
  private silkDec = new SilkDec();
  private celtState = new CeltState();
  private prevMode: Mode | null = null;
  private prevBandwidth: Bandwidth | null = null;

  public walkPacket(
    pkt: OggPacket,
    tStartMs: number,
    freqsHz: Float32Array
  ): { packetWalk: PacketWalk; nextMs: number } {
    const split = splitPacket(pkt.data);
    const { toc, frames, vbr, paddingBytes } = split;
    const totalSamples = frames.length * toc.samplesPerFrame;
    const durationMs = totalSamples / 48;
    const tEndMs = tStartMs + durationMs;
    const bitrateKbps =
      durationMs > 0 ? (pkt.data.length * 8) / durationMs : 0;

    const meta: FrameMeta = {
      index: pkt.packetIndex,
      tStartMs,
      tEndMs,
      durationMs,
      samples: totalSamples,
      bytes: pkt.data.length,
      bitrateKbps,
      tocByte: toc.tocByte,
      config: toc.config,
      mode: toc.mode,
      bandwidth: toc.bandwidth,
      stereo: toc.stereo,
      code: toc.code,
      frameCount: frames.length,
      vbr,
      paddingBytes,
    };

    const modeChanged =
      this.prevMode !== null && this.prevMode !== toc.mode;
    const bandwidthChanged =
      this.prevBandwidth !== null && this.prevBandwidth !== toc.bandwidth;

    const walkedFrames: FrameWalk[] = [];
    const pktBudget = emptyBudget();

    for (let fIdx = 0; fIdx < frames.length; fIdx++) {
      const fBytes = frames[fIdx];
      const fw = this.walkSingleFrame(
        fIdx,
        fBytes,
        toc.mode,
        toc.bandwidth,
        toc.samplesPerFrame,
        toc.stereo,
        freqsHz
      );
      walkedFrames.push(fw);
      addBudget(pktBudget, fw.budget);
    }

    // Aggregate packet summary telemetry
    const hasRedundancy = walkedFrames.some((f) => f.redundancy !== null);
    const hasFec = walkedFrames.some((f) => f.silk?.lbrr === true);
    const hasTransient = walkedFrames.some(
      (f) => f.celt?.transient === true || f.redundancy?.celt?.transient === true
    );

    let dominantSignal: SignalType = 'Inactive';
    let pitchHz: number | null = null;
    let gainDb: number | null = null;
    for (const f of walkedFrames) {
      const ch0 = f.silk?.channels[0];
      if (ch0) {
        if (ch0.signalType === 'Voiced') {
          dominantSignal = 'Voiced';
          if (ch0.pitchHz !== null) pitchHz = ch0.pitchHz;
        } else if (
          ch0.signalType === 'Unvoiced' &&
          dominantSignal !== 'Voiced'
        ) {
          dominantSignal = 'Unvoiced';
        }
        gainDb = ch0.gainDb;
      }
    }

    const totalEighths = Math.max(1, budgetSum(pktBudget));
    const totalBits = totalEighths / 8;
    const silkSharePct = (budgetSilkBits(pktBudget) / totalBits) * 100;
    const celtSharePct = (budgetCeltBits(pktBudget) / totalBits) * 100;
    const fecSharePct = (budgetFecBits(pktBudget) / totalBits) * 100;
    const redSharePct = (budgetRedBits(pktBudget) / totalBits) * 100;
    const unusedSharePct = (pktBudget.unused / totalEighths) * 100;

    return {
      packetWalk: {
        meta,
        frames: walkedFrames,
        budget: pktBudget,
        modeChanged,
        bandwidthChanged,
        hasRedundancy,
        hasFec,
        hasTransient,
        dominantSignal,
        pitchHz,
        gainDb,
        silkSharePct,
        celtSharePct,
        fecSharePct,
        redSharePct,
        unusedSharePct,
      },
      nextMs: tEndMs,
    };
  }

  private walkSingleFrame(
    frameIndex: number,
    frameBytes: Uint8Array,
    mode: Mode,
    bandwidth: Bandwidth,
    samplesPerFrame: number,
    stereo: boolean,
    freqsHz: Float32Array
  ): FrameWalk {
    const budget = emptyBudget();
    const len = frameBytes.length;

    // PLC / DTX empty frame (<= 1 byte with empty payload)
    if (len === 0) {
      return {
        frameIndex,
        bytes: 0,
        plc: true,
        finalRange: 0,
        silk: null,
        celt: null,
        redundancy: null,
        budget,
      };
    }

    const rc = new RangeDec(frameBytes);
    budget.init = rc.tellFrac();

    let silk: SilkOpusFrame | null = null;
    let celt: CeltFrameWalk | null = null;
    let redundancy: RedundancyWalk | null = null;
    let redRng = 0;

    const channels = stereo ? 2 : 1;

    // Reset state on mode transitions per RFC 6716 §5.2
    if (mode !== 'CELT' && this.prevMode === 'CELT') {
      this.silkDec.reset();
    }
    if (mode !== 'SILK' && this.prevMode === 'SILK') {
      this.celtState.reset();
    }

    let effectiveBytes = len;

    if (mode === 'SILK' || mode === 'Hybrid') {
      const fsKhz = silkBandwidthKhz(bandwidth);
      silk = this.silkDec.walkOpusFrame({
        rc,
        fsKhz,
        samplesPerFrame48k: samplesPerFrame,
        codedStereo: stereo,
        maxFrameBytes: len,
        freqsHz,
        budget,
      });

      // Check redundancy flag in Hybrid (tell + 37 <= len*8) or implicit CELT->SILK in SILK
      if (mode === 'Hybrid' && rc.tell() + 37 <= len * 8) {
        const tRed = rc.tellFrac();
        const hasRed = rc.decodeBitLogp(12) !== 0;
        if (hasRed) {
          const c2s = rc.decodeBitLogp(1) !== 0;
          const rb = Math.min(len - 1, rc.decUint(256) + 2);
          budget.redHeader += Math.max(0, rc.tellFrac() - tRed);

          if (rb >= 2 && rc.tell() + rb * 8 <= len * 8) {
            const redSlice = frameBytes.subarray(len - rb, len);
            effectiveBytes = len - rb;
            rc.storage = effectiveBytes;

            const redRc = new RangeDec(redSlice);
            const tRedFrame = redRc.tellFrac();
            const redCelt = walkCeltFrame({
              rc: redRc,
              state: this.celtState,
              channels,
              lm: 1, // 5ms redundancy frame
              startBand: 0,
              endBand: bandwidthEndBand(bandwidth),
              effectiveBytes: rb,
              budget,
              isRedundancy: true,
            });
            redRng = redRc.rng >>> 0;
            // Charge remaining bits of redundancy frame into budget.redundancy
            const redTotalEighths = rb * 64;
            const redSpent = Math.max(0, redRc.tellFrac() - tRedFrame);
            budget.redundancy += Math.max(0, redTotalEighths - redSpent);

            redundancy = {
              celtToSilk: c2s,
              bytes: rb,
              celt: redCelt,
            };
          }
        } else {
          budget.redHeader += Math.max(0, rc.tellFrac() - tRed);
        }
      }
    }

    if (mode === 'CELT' || mode === 'Hybrid') {
      const startBand = mode === 'Hybrid' ? HYBRID_START_BAND : 0;
      const endBand = bandwidthEndBand(bandwidth);
      const lm = samplesToLm(samplesPerFrame);

      celt = walkCeltFrame({
        rc,
        state: this.celtState,
        channels,
        lm,
        startBand,
        endBand,
        effectiveBytes,
        budget,
        isRedundancy: false,
      });
    }

    const finalRange = ((rc.rng >>> 0) ^ (redRng >>> 0)) >>> 0;

    // Exact 1/8-bit budget closure: budgetSum(budget) === len * 64
    const targetEighths = len * 64;
    const usedEighths =
      budget.init +
      budget.silkFlags +
      budget.silkLbrr +
      budget.silkStereo +
      budget.silkSide +
      budget.silkPulses +
      budget.redHeader +
      budget.celtHeader +
      budget.celtEnergy +
      budget.celtPvq +
      budget.redundancy;

    if (usedEighths <= targetEighths) {
      budget.unused = targetEighths - usedEighths;
    } else {
      let excess = usedEighths - targetEighths;
      const trimField = (k: keyof Budget) => {
        if (excess <= 0) return;
        const take = Math.min(excess, budget[k]);
        budget[k] -= take;
        excess -= take;
      };
      trimField('celtPvq');
      trimField('silkPulses');
      trimField('celtEnergy');
      trimField('silkSide');
      trimField('celtHeader');
      trimField('silkFlags');
      trimField('init');
      budget.unused = 0;
    }

    this.prevMode = mode;
    this.prevBandwidth = bandwidth;

    return {
      frameIndex,
      bytes: len,
      plc: false,
      finalRange,
      silk,
      celt,
      redundancy,
      budget,
    };
  }
}
