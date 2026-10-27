// 01_audio.js — Audio-Hilfen: Resampling, Base64, gapless Wiedergabe.
// Wird von app.js (PCM-Pfad) und 02_opus.js (Opus-Pfad) genutzt.
"use strict";

// Eingang Gemini: 16 kHz; Ausgang (Uebersetzung): 24 kHz.
const SR_IN = 16000;
const SR_OUT = 24000;

// Float32 [-1,1] auf Int16-LE herunterrechnen (16 kHz) — fuer Gemini-PCM.
// `from` ist die tatsaechliche AudioContext-Rate (oft 48 kHz).
function downsampleTo16k(buf, from) {
  if (from === SR_IN) return floatToInt16(buf, buf.length);
  const ratio = from / SR_IN;
  const n = Math.round(buf.length / ratio);
  const out = new Int16Array(n);
  for (let i = 0; i < n; i++) {
    const o = i * ratio, l = Math.floor(o), r = Math.min(l + 1, buf.length - 1), w = o - l;
    const s = Math.max(-1, Math.min(1, buf[l] * (1 - w) + buf[r] * w));
    out[i] = s < 0 ? s * 0x8000 : s * 0x7fff;
  }
  return out;
}

// Float32 → Int16 (ohne Ratenwechsel).
function floatToInt16(buf, n) {
  const o = new Int16Array(n);
  for (let i = 0; i < n; i++) {
    const s = Math.max(-1, Math.min(1, buf[i]));
    o[i] = s < 0 ? s * 0x8000 : s * 0x7fff;
  }
  return o;
}

// ArrayBuffer → Base64 (fuer Gemini-JSON).
function bufToB64(buffer) {
  let s = "";
  const b = new Uint8Array(buffer);
  for (let i = 0; i < b.length; i++) s += String.fromCharCode(b[i]);
  return btoa(s);
}

// Base64 → Uint8Array (Gemini-Antwort).
function b64ToBuf(b64) {
  const bin = atob(b64), out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}

// Gapless-Wiedergabe: reiht AudioBuffer lueckenlos aneinander.
// Nutzbar fuer PCM24 (Int16-Bytes) und fuer dekodierte Opus-Frames (Float32).
class GaplessQueue {
  constructor(ctx) {
    this.ctx = ctx;
    this.next = 0; // naechster Startzeitpunkt
  }
  // Int16-LE-PCM mit fester Rate abspielen (PCM-Pfad: 24 kHz).
  pushPcm16(bytes, rate) {
    const view = new Int16Array(bytes.buffer, bytes.byteOffset, bytes.byteLength >> 1);
    const f32 = new Float32Array(view.length);
    for (let i = 0; i < view.length; i++) f32[i] = view[i] / 32768;
    this.pushFloat(f32, rate);
  }
  // Float32-Samples (ein Kanal) mit gegebener Rate anhaengen.
  pushFloat(f32, rate) {
    if (!this.ctx || !f32.length) return;
    const ab = this.ctx.createBuffer(1, f32.length, rate);
    ab.getChannelData(0).set(f32);
    const node = this.ctx.createBufferSource();
    node.buffer = ab;
    node.connect(this.ctx.destination);
    const now = this.ctx.currentTime;
    if (this.next < now) this.next = now + 0.02;
    node.start(this.next);
    this.next += ab.duration;
  }
}
