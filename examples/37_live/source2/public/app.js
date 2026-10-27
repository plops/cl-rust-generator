// app.js — Steuerung: Sprachen laden, UI, PCM- und Opus-Pfad, Statistik.
// Setzt 01_audio.js (Resampling, Queue) und 02_opus.js (WebCodecs) voraus;
// index.html laedt die drei Dateien in dieser Reihenfolge.
"use strict";

const $ = (id) => document.getElementById(id);
const el = {
  key: $("apiKey"), room: $("roomName"), speak: $("langSpeak"), hear: $("langHear"),
  swap: $("swap"), connect: $("connectBtn"), disconnect: $("disconnectBtn"),
  status: $("status"), peer: $("peerInfo"), kbUp: $("kbUp"), kbDown: $("kbDown"),
  subs: $("subs"), trPcm: $("trPcm"), trOpus: $("trOpus"), trOpusLabel: $("trOpusLabel"),
  opusHint: $("opusHint"),
};

// Gemini-Basis-URL; per ?gemini=ws://… nur fuer Tests ueberschreibbar.
const GEMINI_BASE = new URLSearchParams(location.search).get("gemini")
  || "wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1beta.GenerativeService.BidiGenerateContent";

// Gesamt-Statistik fuer die automatischen Tests.
window.__stats = { up: 0, down: 0, opusUp: 0, opusDown: 0, pcmDown: 0, texts: 0 };

// Gleitendes 1-s-Fenster fuer die kbit/s-Anzeige.
const win = { up: [], down: [] };
let ctx = null, mic = null, proc = null, geminiWs = null, roomWs = null;
let queue = null, upOpus = null, downOpus = null, live = false;

// ---- Sprachen laden ----------------------------------------------------
async function loadLanguages() {
  const base = location.pathname.replace(/\/[^/]*$/, "");
  const r = await fetch(`${base}/api/languages`);
  const j = await r.json();
  for (const sel of [el.speak, el.hear]) {
    sel.innerHTML = "";
    for (const l of j.languages) {
      const o = document.createElement("option");
      o.value = l.code; o.textContent = `${l.name} (${l.code})`;
      sel.appendChild(o);
    }
  }
  el.speak.value = j.default_lang;
  el.hear.value = j.default_target;
}

// ---- WebCodecs-Verfuegbarkeit pruefen ----------------------------------
async function checkOpus() {
  const ok = await opusSupported();
  if (!ok) {
    el.trOpus.disabled = true;
    el.trOpusLabel.classList.add("disabled");
    el.opusHint.textContent = "In diesem Browser nicht verfuegbar (WebCodecs fehlt).";
  }
}

// ---- Statistik ----------------------------------------------------------
function countUp(n) { window.__stats.up += n; win.up.push([performance.now(), n]); }
function countDown(n) { window.__stats.down += n; win.down.push([performance.now(), n]); }
function kbits(arr) {
  const now = performance.now();
  while (arr.length && now - arr[0][0] > 1000) arr.shift();
  const bytes = arr.reduce((s, [, n]) => s + n, 0);
  return Math.round((bytes * 8) / 1000);
}
setInterval(() => {
  el.kbUp.textContent = kbits(win.up);
  el.kbDown.textContent = kbits(win.down);
}, 500);

// ---- Untertitel ---------------------------------------------------------
function addSub(who, dir, text) {
  if (!text) return;
  window.__stats.texts++;
  const li = document.createElement("li");
  li.className = who;
  li.innerHTML = `<span class="who">${who === "me" ? "Ich" : "Partner"} · ${dir}</span>`;
  li.appendChild(document.createTextNode(text));
  el.subs.appendChild(li);
  el.subs.scrollTop = el.subs.scrollHeight;
}

function setStatus(t, isLive) {
  el.status.textContent = t;
  el.status.classList.toggle("live", !!isLive);
  live = !!isLive;
}
function setUI(active) {
  el.connect.disabled = active;
  el.disconnect.disabled = !active;
  for (const e of [el.key, el.room, el.speak, el.hear, el.swap, el.trPcm, el.trOpus]) e.disabled = active;
}

// ---- WS-URL zum Relay (subpfad-faehig) ----------------------------------
function relayUrl(transport) {
  const proto = location.protocol === "https:" ? "wss:" : "ws:";
  const base = location.pathname.replace(/\/[^/]*$/, "");
  const p = new URLSearchParams({
    room: el.room.value.trim(), transport,
    lang: el.speak.value, target: el.hear.value,
  });
  return `${proto}//${location.host}${base}/ws?${p}`;
}

// ---- Start / Stop -------------------------------------------------------
el.connect.addEventListener("click", start);
el.disconnect.addEventListener("click", cleanup);
el.swap.addEventListener("click", () => {
  const a = el.speak.value; el.speak.value = el.hear.value; el.hear.value = a;
});

async function start() {
  const room = el.room.value.trim();
  if (!room) return alert("Bitte Raumnamen angeben!");
  localStorage.setItem("live_silk_room", room);
  const transport = el.trOpus.checked ? "opus" : "pcm";
  setUI(true);
  setStatus("Initialisiere Audio…");
  try {
    ctx = new (window.AudioContext || window.webkitAudioContext)({ latencyHint: "interactive" });
    await ctx.resume();
    queue = new GaplessQueue(ctx);
    if (transport === "opus") await startOpus();
    else await startPcm();
  } catch (e) {
    setStatus("Fehler: " + e.message);
    cleanup();
  }
}

// ---- PCM-Pfad: Browser spricht selbst mit Gemini -----------------------
async function startPcm() {
  const key = el.key.value.trim();
  if (!key) { setStatus("Im Normalmodus wird ein API-Key benoetigt."); return cleanup(); }
  localStorage.setItem("live_silk_key", key);
  connectRelay("pcm");
  setStatus("Verbinde zu Gemini…");
  geminiWs = new WebSocket(`${GEMINI_BASE}?key=${key}`);
  geminiWs.onopen = () => {
    geminiWs.send(JSON.stringify({
      setup: {
        model: "models/gemini-3.5-live-translate-preview",
        generationConfig: {
          responseModalities: ["AUDIO"],
          translationConfig: { targetLanguageCode: el.hear.value },
        },
        inputAudioTranscription: {},
        outputAudioTranscription: {},
      },
    }));
    startMic(false);
  };
  geminiWs.onmessage = async (ev) => {
    const txt = ev.data instanceof Blob ? await ev.data.text() : ev.data;
    let data; try { data = JSON.parse(txt); } catch { return; }
    if (data.setupComplete) { setStatus("Live! Einfach sprechen.", true); return; }
    const sc = data.serverContent;
    if (!sc) return;
    for (const p of (sc.modelTurn?.parts || [])) {
      if (p.inlineData?.data) {
        const buf = b64ToBuf(p.inlineData.data);
        // Eigene Uebersetzung an den Relay (Partner hoert sie).
        if (roomWs?.readyState === WebSocket.OPEN) {
          roomWs.send(buf.buffer); countUp(buf.byteLength);
        }
      }
    }
    if (sc.inputTranscription?.text) subAndSend("in", sc.inputTranscription.text);
    if (sc.outputTranscription?.text) subAndSend("out", sc.outputTranscription.text);
  };
  geminiWs.onerror = () => setStatus("Gemini-Fehler — API-Key pruefen.");
  geminiWs.onclose = () => { if (live) setStatus("Gemini getrennt."); };
}

// Transkript lokal zeigen und als Text an den Relay (fuer den Partner).
function subAndSend(dir, text) {
  addSub("me", dir, text);
  if (roomWs?.readyState === WebSocket.OPEN) {
    roomWs.send(JSON.stringify({ type: "text", dir, text }));
  }
}

// ---- Opus-Pfad: Sparmodus ----------------------------------------------
async function startOpus() {
  connectRelay("opus");
  setStatus("Sparmodus — verbinde…");
}

// Mikro starten; `opus` entscheidet Encoder-Pfad vs. Gemini-PCM.
async function startMic(opus) {
  mic = await navigator.mediaDevices.getUserMedia({
    audio: { channelCount: 1, echoCancellation: true, noiseSuppression: true, autoGainControl: true },
  });
  const src = ctx.createMediaStreamSource(mic);
  proc = ctx.createScriptProcessor(2048, 1, 1);
  src.connect(proc); proc.connect(ctx.destination);
  proc.onaudioprocess = (e) => {
    const f32 = e.inputBuffer.getChannelData(0);
    const int16 = downsampleTo16k(f32, ctx.sampleRate);
    if (opus) {
      if (upOpus) upOpus.pushInt16(int16);
    } else if (geminiWs?.readyState === WebSocket.OPEN) {
      const b64 = bufToB64(int16.buffer);
      const msg = JSON.stringify({ realtimeInput: { audio: { mimeType: "audio/pcm;rate=16000", data: b64 } } });
      geminiWs.send(msg); countUp(msg.length);
    }
  };
}

// ---- Relay-Verbindung ---------------------------------------------------
function connectRelay(transport) {
  roomWs = new WebSocket(relayUrl(transport));
  roomWs.binaryType = "arraybuffer";
  roomWs.onopen = () => {
    if (transport === "opus") {
      // hello mit optionalem Key (leer = Server-Key nutzen).
      roomWs.send(JSON.stringify({ type: "hello", key: el.key.value.trim() }));
      upOpus = new OpusUplink((buf) => {
        roomWs.send(buf); countUp(buf.byteLength); window.__stats.opusUp += buf.byteLength;
      }, () => {});
      downOpus = new OpusDownlink(queue);
    }
  };
  roomWs.onmessage = (ev) => {
    if (ev.data instanceof ArrayBuffer) {
      countDown(ev.data.byteLength);
      if (transport === "opus") {
        window.__stats.opusDown += ev.data.byteLength;
        if (downOpus) downOpus.pushPacket(ev.data);
      } else {
        window.__stats.pcmDown += ev.data.byteLength;
        queue.pushPcm16(new Uint8Array(ev.data), 24000);
      }
      return;
    }
    let m; try { m = JSON.parse(ev.data); } catch { return; }
    handleRelayText(m, transport);
  };
  roomWs.onerror = () => setStatus("Relay-Verbindung fehlgeschlagen.");
  roomWs.onclose = () => { if (live) setStatus("Relay getrennt."); };
}

function handleRelayText(m, transport) {
  switch (m.type) {
    case "ready":
      // Opus-Pfad: jetzt erst Mikro starten und „Live!" melden.
      if (transport === "opus") { startMic(true).catch((e) => setStatus("Mikro: " + e.message)); }
      setStatus("Live!", true);
      break;
    case "peers": {
      const others = (m.peers || []).filter((p) => p.transport !== undefined);
      el.peer.textContent = others.length > 1
        ? `${others.length} im Raum` : (others.length === 1 ? "wartet…" : "—");
      // genauer: Partner-Sprache zeigen, wenn genau 2.
      if ((m.peers || []).length >= 2) el.peer.textContent = "verbunden";
      break;
    }
    case "text":
      addSub(m.who || "partner", m.dir || "", m.text || "");
      break;
    case "error":
      setStatus("Fehler: " + (m.msg || "unbekannt"));
      break;
  }
}

// ---- Aufraeumen ---------------------------------------------------------
function cleanup() {
  setUI(false);
  setStatus("Getrennt.");
  live = false;
  if (proc) { proc.disconnect(); proc.onaudioprocess = null; proc = null; }
  if (mic) { mic.getTracks().forEach((t) => t.stop()); mic = null; }
  if (upOpus) { upOpus.close(); upOpus = null; }
  if (downOpus) { downOpus.close(); downOpus = null; }
  if (geminiWs) { geminiWs.onclose = null; geminiWs.close(); geminiWs = null; }
  if (roomWs) { roomWs.onclose = null; roomWs.close(); roomWs = null; }
  if (ctx && ctx.state !== "closed") { ctx.close(); ctx = null; }
}

// ---- Init ---------------------------------------------------------------
window.addEventListener("DOMContentLoaded", async () => {
  const k = localStorage.getItem("live_silk_key"); if (k) el.key.value = k;
  const r = localStorage.getItem("live_silk_room"); if (r) el.room.value = r;
  try { await loadLanguages(); } catch (e) { setStatus("Sprachen laden fehlgeschlagen: " + e.message); }
  await checkOpus();
});
