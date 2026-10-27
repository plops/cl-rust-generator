// 02_opus.js — Sparmodus: WebCodecs Opus-Encoder (Mikro→Opus) und
// -Decoder (Opus→Lautsprecher). SILK erzeugt der Server (24-kHz-Encoder);
// der Browser dekodiert mit sampleRate 24000.
"use strict";

// Pruefen, ob WebCodecs-Audio vorhanden ist (async: isConfigSupported).
async function opusSupported() {
  if (typeof AudioEncoder === "undefined" || typeof AudioDecoder === "undefined") return false;
  try {
    const enc = await AudioEncoder.isConfigSupported({
      codec: "opus", sampleRate: 16000, numberOfChannels: 1, bitrate: 12000,
    });
    const dec = await AudioDecoder.isConfigSupported({
      codec: "opus", sampleRate: 24000, numberOfChannels: 1,
    });
    return !!(enc && enc.supported && dec && dec.supported);
  } catch {
    return false;
  }
}

// Mikrofon-Uplink: Int16-16-kHz-Blöcke → Opus-Pakete → ws.send(Binary).
class OpusUplink {
  constructor(send, onBytes) {
    this.send = send;     // (ArrayBuffer) => void
    this.onBytes = onBytes; // (n) => void  (fuer kbit/s-Statistik)
    this.ts = 0;          // Mikrosekunden-Zeitstempel
    this.enc = new AudioEncoder({
      output: (chunk) => {
        const buf = new ArrayBuffer(chunk.byteLength);
        chunk.copyTo(buf);
        this.onBytes(buf.byteLength);
        this.send(buf);
      },
      error: (e) => console.error("AudioEncoder:", e.message),
    });
    this.enc.configure({
      codec: "opus", sampleRate: 16000, numberOfChannels: 1, bitrate: 12000,
      opus: { application: "voip", signal: "voice", frameDuration: 60000 },
    });
  }
  // Int16-Samples (16 kHz mono) kodieren.
  pushInt16(int16) {
    if (this.enc.state !== "configured" || !int16.length) return;
    const data = new AudioData({
      format: "s16", sampleRate: 16000, numberOfFrames: int16.length,
      numberOfChannels: 1, timestamp: this.ts,
      data: new Uint8Array(int16.buffer, int16.byteOffset, int16.byteLength),
    });
    this.ts += Math.round((int16.length / 16000) * 1e6);
    this.enc.encode(data);
    data.close();
  }
  close() { try { this.enc.close(); } catch {} }
}

// Downlink: empfangene Opus-Pakete (SILK, 24 kHz) → gapless Wiedergabe.
class OpusDownlink {
  constructor(queue) {
    this.queue = queue;   // GaplessQueue
    this.ts = 0;
    this.dec = new AudioDecoder({
      output: (audioData) => {
        const n = audioData.numberOfFrames;
        const f32 = new Float32Array(n);
        audioData.copyTo(f32, { planeIndex: 0, format: "f32-planar" });
        this.queue.pushFloat(f32, audioData.sampleRate);
        audioData.close();
      },
      error: (e) => console.error("AudioDecoder:", e.message),
    });
    this.dec.configure({ codec: "opus", sampleRate: 24000, numberOfChannels: 1 });
  }
  // Ein Opus-Paket (ArrayBuffer) dekodieren.
  pushPacket(buf) {
    if (this.dec.state !== "configured") return;
    const chunk = new EncodedAudioChunk({
      type: "key", timestamp: this.ts, data: buf,
    });
    this.ts += 60000; // 60 ms je Paket
    this.dec.decode(chunk);
  }
  close() { try { this.dec.close(); } catch {} }
}
