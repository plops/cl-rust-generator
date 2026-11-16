import { RangeDec } from './range';
import {
  SilkChannelState,
  createSilkChannelState,
  walkSilkChannelFrame,
} from './silkFrame';
import {
  SILK_STEREO_ONLY_CODE_MID_ICDF,
  SILK_STEREO_PRED_JOINT_ICDF,
  SILK_UNIFORM8_ICDF,
} from './silkTab';
import { Budget, SilkChannelFrame, SilkOpusFrame } from './types';

export class SilkDec {
  private channels: [SilkChannelState, SilkChannelState];

  constructor() {
    this.channels = [createSilkChannelState(), createSilkChannelState()];
  }

  public reset(): void {
    this.channels = [createSilkChannelState(), createSilkChannelState()];
  }

  public walkOpusFrame(params: {
    rc: RangeDec;
    fsKhz: 8 | 12 | 16;
    samplesPerFrame48k: number; // 480, 960, 1920, 2880
    codedStereo: boolean;
    maxFrameBytes: number;
    freqsHz: Float32Array;
    budget: Budget;
  }): SilkOpusFrame {
    const {
      rc,
      fsKhz,
      samplesPerFrame48k,
      codedStereo,
      maxFrameBytes,
      freqsHz,
      budget,
    } = params;

    const maxBits = maxFrameBytes * 8;
    const nFramesInPacket =
      samplesPerFrame48k <= 960 ? 1 : samplesPerFrame48k === 1920 ? 2 : 3;
    const nbSubframes: 2 | 4 = samplesPerFrame48k === 480 ? 2 : 4;
    const nSilkChannels = codedStereo ? 2 : 1;

    // 1. Read VAD flags & LBRR flags at start of Opus SILK frame (RFC 6716 §4.2.3)
    const tFlagsStart = rc.tellFrac();
    const vadFlags: boolean[][] = [[], []];
    const lbrrFlags: boolean[] = [false, false];

    for (let c = 0; c < nSilkChannels; c++) {
      for (let i = 0; i < nFramesInPacket; i++) {
        vadFlags[c].push(rc.tell() + 1 <= maxBits ? rc.decodeBitLogp(1) !== 0 : false);
      }
      lbrrFlags[c] = rc.tell() + 1 <= maxBits ? rc.decodeBitLogp(1) !== 0 : false;
    }

    // Per-subframe LBRR presence flags
    const lbrrSubframe: boolean[][] = [
      new Array<boolean>(nFramesInPacket).fill(false),
      new Array<boolean>(nFramesInPacket).fill(false),
    ];
    let anyLbrr = false;
    for (let c = 0; c < nSilkChannels; c++) {
      if (!lbrrFlags[c]) continue;
      anyLbrr = true;
      if (nFramesInPacket === 1) {
        lbrrSubframe[c][0] = true;
      } else {
        let sum = 0;
        for (let i = 0; i < nFramesInPacket; i++) {
          const f = rc.tell() + 1 <= maxBits ? rc.decodeBitLogp(1) !== 0 : false;
          lbrrSubframe[c][i] = f;
          if (f) sum++;
        }
        if (sum === 0) lbrrSubframe[c][0] = true;
      }
    }
    budget.silkFlags += Math.max(0, rc.tellFrac() - tFlagsStart);

    // 2. Decode LBRR frames if present
    let lbrrChannels = 0;
    if (anyLbrr) {
      for (let i = 0; i < nFramesInPacket; i++) {
        for (let c = 0; c < nSilkChannels; c++) {
          if (!lbrrSubframe[c][i]) continue;
          lbrrChannels++;
          walkSilkChannelFrame({
            rc,
            state: createSilkChannelState(),
            vad: true,
            fsKhz,
            nbSubframes,
            condCoding: i > 0 && lbrrSubframe[c][i - 1],
            maxFrameBytes,
            freqsHz,
            budget,
            isLbrr: true,
          });
        }
      }
    }

    // 3. Walk main 1..3 SILK subframes (10/20ms each)
    let midOnly = false;
    let stereoPred: [number, number] | null = null;
    const outChannels: SilkChannelFrame[] = [];

    for (let i = 0; i < nFramesInPacket; i++) {
      if (codedStereo) {
        const tStereo = rc.tellFrac();
        if (rc.tell() + 11 <= maxBits) {
          const n = rc.decodeIcdf(SILK_STEREO_PRED_JOINT_ICDF, 8);
          const ix0 = Math.floor(n / 5);
          const ix1 = n % 5;
          const fine0 = rc.tell() + 3 <= maxBits ? rc.decodeIcdf(SILK_UNIFORM8_ICDF, 8) : 0;
          const fine1 = rc.tell() + 3 <= maxBits ? rc.decodeIcdf(SILK_UNIFORM8_ICDF, 8) : 0;
          const p0 = (ix0 * 8 + fine0 - 20) / 16;
          const p1 = (ix1 * 8 + fine1 - 20) / 16;
          stereoPred = [p0, p1];
        }
        if (!vadFlags[1][i] && rc.tell() + 2 <= maxBits) {
          midOnly = rc.decodeIcdf(SILK_STEREO_ONLY_CODE_MID_ICDF, 8) !== 0;
        } else {
          midOnly = false;
        }
        budget.silkStereo += Math.max(0, rc.tellFrac() - tStereo);
      }

      const midFrame = walkSilkChannelFrame({
        rc,
        state: this.channels[0],
        vad: vadFlags[0][i] ?? false,
        fsKhz,
        nbSubframes,
        condCoding: i > 0,
        maxFrameBytes,
        freqsHz,
        budget,
      });

      if (i === 0) outChannels.push(midFrame);
      else outChannels[0] = midFrame;

      if (codedStereo && !midOnly) {
        const sideFrame = walkSilkChannelFrame({
          rc,
          state: this.channels[1],
          vad: vadFlags[1][i] ?? false,
          fsKhz,
          nbSubframes,
          condCoding: i > 0,
          maxFrameBytes,
          freqsHz,
          budget,
        });
        if (outChannels.length < 2) outChannels.push(sideFrame);
        else outChannels[1] = sideFrame;
      }
    }

    return {
      lbrr: anyLbrr,
      lbrrChannels,
      midOnly,
      stereoPred,
      subframesSilkCount: nFramesInPacket,
      channels: outChannels,
    };
  }
}
