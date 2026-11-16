export interface LoopState {
  aMs: number | null;
  bMs: number | null;
  active: boolean;
}

export interface PlayerSnapshot {
  isPlaying: boolean;
  currentTimeMs: number;
  durationMs: number;
  loop: LoopState;
}

export class OpusAudioPlayer {
  private ctx: AudioContext | null = null;
  private buffer: AudioBuffer | null = null;
  private source: AudioBufferSourceNode | null = null;
  private rafId: number | null = null;

  private isPlaying = false;
  private currentTimeMs = 0;
  private durationMs = 0;
  private playStartCtxSec = 0;
  private playOffsetSec = 0;

  private loop: LoopState = { aMs: null, bMs: null, active: false };
  private listeners = new Set<(snap: PlayerSnapshot) => void>();

  public subscribe(fn: (snap: PlayerSnapshot) => void): () => void {
    this.listeners.add(fn);
    fn(this.getSnapshot());
    return () => {
      this.listeners.delete(fn);
    };
  }

  private emit(): void {
    const snap = this.getSnapshot();
    for (const fn of this.listeners) fn(snap);
  }

  public getSnapshot(): PlayerSnapshot {
    return {
      isPlaying: this.isPlaying,
      currentTimeMs: this.currentTimeMs,
      durationMs: this.durationMs,
      loop: { ...this.loop },
    };
  }

  public loadPcm(channels: Float32Array[], sampleRate = 48000): void {
    this.stop();
    const len = channels[0]?.length ?? 0;
    this.durationMs = (len / sampleRate) * 1000;
    this.currentTimeMs = 0;
    this.playOffsetSec = 0;
    this.loop = { aMs: null, bMs: null, active: false };

    if (len === 0) {
      this.buffer = null;
      this.emit();
      return;
    }

    const AudioCtx =
      window.AudioContext ||
      (window as unknown as { webkitAudioContext: typeof AudioContext })
        .webkitAudioContext;
    if (!this.ctx) {
      this.ctx = new AudioCtx({ sampleRate });
    }
    const buf = this.ctx.createBuffer(channels.length, len, sampleRate);
    for (let c = 0; c < channels.length; c++) {
      buf.getChannelData(c).set(channels[c]);
    }
    this.buffer = buf;
    this.emit();
  }

  private stopSourceOnly(): void {
    if (this.source) {
      try {
        this.source.onended = null;
        this.source.stop();
        this.source.disconnect();
      } catch {
        // Ignore already stopped
      }
      this.source = null;
    }
    if (this.rafId !== null) {
      cancelAnimationFrame(this.rafId);
      this.rafId = null;
    }
  }

  private startFrom(offsetSec: number): void {
    if (!this.ctx || !this.buffer) return;
    this.stopSourceOnly();

    if (this.ctx.state === 'suspended') {
      void this.ctx.resume();
    }

    const durSec = this.buffer.duration;
    let clampedOffset = Math.max(0, Math.min(durSec - 0.001, offsetSec));

    const hasValidLoop =
      this.loop.active &&
      this.loop.aMs !== null &&
      this.loop.bMs !== null &&
      this.loop.bMs - this.loop.aMs >= 40;

    const src = this.ctx.createBufferSource();
    src.buffer = this.buffer;

    if (hasValidLoop) {
      const aSec = Math.max(0, (this.loop.aMs as number) / 1000);
      const bSec = Math.min(durSec, (this.loop.bMs as number) / 1000);
      src.loop = true;
      src.loopStart = aSec;
      src.loopEnd = bSec;
      if (clampedOffset < aSec || clampedOffset >= bSec) {
        clampedOffset = aSec;
      }
    } else {
      src.loop = false;
    }

    src.connect(this.ctx.destination);
    this.playStartCtxSec = this.ctx.currentTime;
    this.playOffsetSec = clampedOffset;
    this.currentTimeMs = clampedOffset * 1000;
    this.isPlaying = true;

    src.onended = () => {
      if (this.source === src && !src.loop) {
        this.isPlaying = false;
        this.currentTimeMs = this.durationMs;
        this.playOffsetSec = 0;
        this.stopSourceOnly();
        this.emit();
      }
    };

    src.start(0, clampedOffset);
    this.source = src;
    this.tick();
    this.emit();
  }

  private tick = (): void => {
    if (!this.isPlaying || !this.ctx || !this.buffer) return;
    const elapsed = this.ctx.currentTime - this.playStartCtxSec;
    let posSec = this.playOffsetSec + elapsed;

    const hasValidLoop =
      this.loop.active &&
      this.loop.aMs !== null &&
      this.loop.bMs !== null &&
      this.loop.bMs - this.loop.aMs >= 40;

    if (hasValidLoop) {
      const aSec = (this.loop.aMs as number) / 1000;
      const bSec = (this.loop.bMs as number) / 1000;
      const span = bSec - aSec;
      if (posSec >= bSec && span > 0) {
        posSec = aSec + ((posSec - aSec) % span);
      }
    } else if (posSec >= this.buffer.duration) {
      posSec = this.buffer.duration;
    }

    this.currentTimeMs = posSec * 1000;
    this.emit();
    this.rafId = requestAnimationFrame(this.tick);
  };

  public async play(): Promise<void> {
    if (!this.buffer || !this.ctx) return;
    if (this.ctx.state === 'suspended') {
      await this.ctx.resume();
    }
    const startSec =
      this.currentTimeMs >= this.durationMs - 10 ? 0 : this.currentTimeMs / 1000;
    this.startFrom(startSec);
  }

  public pause(): void {
    if (!this.isPlaying) return;
    if (this.ctx) {
      const elapsed = this.ctx.currentTime - this.playStartCtxSec;
      const posSec = Math.min(this.durationMs / 1000, this.playOffsetSec + elapsed);
      this.currentTimeMs = posSec * 1000;
    }
    this.isPlaying = false;
    this.stopSourceOnly();
    this.emit();
  }

  public async toggle(): Promise<void> {
    if (this.isPlaying) this.pause();
    else await this.play();
  }

  public seek(ms: number): void {
    const clamped = Math.max(0, Math.min(this.durationMs, ms));
    this.currentTimeMs = clamped;
    if (this.isPlaying) {
      this.startFrom(clamped / 1000);
    } else {
      this.emit();
    }
  }

  public setLoop(aMs: number | null, bMs: number | null, active: boolean): void {
    let normA = aMs;
    let normB = bMs;
    if (normA !== null && normB !== null && normA > normB) {
      const tmp = normA;
      normA = normB;
      normB = tmp;
    }
    this.loop = { aMs: normA, bMs: normB, active };
    if (this.isPlaying) {
      this.startFrom(this.currentTimeMs / 1000);
    } else {
      this.emit();
    }
  }

  public stop(): void {
    this.isPlaying = false;
    this.stopSourceOnly();
    this.currentTimeMs = 0;
    this.emit();
  }

  public dispose(): void {
    this.stop();
    if (this.ctx) {
      void this.ctx.close();
      this.ctx = null;
    }
  }
}
