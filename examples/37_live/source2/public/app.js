// app.js — Steuerung: Sprachen laden, UI, PCM- und Opus-Pfad, Statistik.
// Setzt 01_audio.js (Resampling, Queue) und 02_opus.js (WebCodecs) voraus;
// index.html laedt die drei Dateien in dieser Reihenfolge.
//
// Neu (Nutzer-Feedback): Jeder waehlt NUR die eigene Sprache. Die Zielsprache
// der eigenen Uebersetzung ist automatisch die Sprache des Partners im Raum.
// Der Server schickt dazu persoenliche `partner`-Events. Im PCM-Modus baut der
// Browser seine Gemini-Verbindung erst auf, wenn ein Partner bekannt ist, und
// neu bei jedem Partnerwechsel.
"use strict";

const $ = (id) => document.getElementById(id);
const el = {
  key: $("apiKey"), room: $("roomName"), speak: $("langSpeak"), voice: $("voiceSelect"),
  voiceHint: $("voiceHint"),
  connect: $("connectBtn"), disconnect: $("disconnectBtn"),
  status: $("status"), peer: $("peerInfo"), kbUp: $("kbUp"), kbDown: $("kbDown"),
  subs: $("subs"), trPcm: $("trPcm"), trOpus: $("trOpus"), trOpusLabel: $("trOpusLabel"),
  opusHint: $("opusHint"), keyField: $("keyField"),
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
let transportMode = "pcm";        // aktueller Transport dieser Sitzung
let partner = null;               // {lang, transport, voice} oder null
let partnerSetup = null;          // fertiges Gemini-Setup aus dem partner-Event (PCM)
let partnerModel = null;          // "translate" | "live" (fuer Hinweistext)
let partnerSegment = null;        // {min_ms,max_ms,quiet_rms} nur bei Live, sonst null
let segmenter = null;             // laufende Segmentierung (nur Live)
let langNames = {};               // code → Anzeigename (aus /api/languages)
let defaultVoiceMap = {};         // code → Stimmenname (feste Default-Stimmen)
let autoMarker = "auto";          // Marker fuer „Automatisch"
let voiceTouched = false;         // hat der Nutzer die Stimme selbst gewaehlt?
const langAvail = {};             // code → "both" | "live" | "translate"
const AVAIL_MARK = { both: "", live: " *", translate: " **" };

// In welchen Modellen gibt es die Sprache? (Flags aus /api/languages)
function avail(l) {
  if (l.translate && l.live) return "both";
  return l.live ? "live" : "translate";
}

// Geschlossenes Select einfaerben (Klasse am <select>, nicht nur an <option>).
function markSpeak() {
  const a = langAvail[el.speak.value] || "both";
  el.speak.classList.toggle("only-live", a === "live");
  el.speak.classList.toggle("only-translate", a === "translate");
}

// ---- Sprachen + Stimmen laden -----------------------------------------
async function loadLanguages() {
  const base = location.pathname.replace(/\/[^/]*$/, "");
  const r = await fetch(`${base}/api/languages`);
  const j = await r.json();
  el.speak.innerHTML = "";
  langNames = {};
  for (const l of j.languages) {
    langNames[l.code] = l.name;
    langAvail[l.code] = avail(l);
    const o = document.createElement("option");
    o.value = l.code;
    // Markierung: „*" nur Live-Modell (feste Stimme), „**" nur
    // Uebersetzungsmodell (Automatisch). Farbe zusaetzlich per Klasse
    // (iOS-Safari faerbt <option> nicht, daher immer auch das Zeichen).
    o.textContent = `${l.name} (${l.code})${AVAIL_MARK[langAvail[l.code]]}`;
    if (langAvail[l.code] !== "both") o.className = `only-${langAvail[l.code]}`;
    el.speak.appendChild(o);
  }
  el.speak.value = j.default_lang;
  markSpeak();

  // Stimmen-Select: „Automatisch" + Optgroups Weiblich/Maennlich.
  autoMarker = j.auto || "auto";
  defaultVoiceMap = j.default_voice || {};
  el.voice.innerHTML = "";
  const auto = document.createElement("option");
  auto.value = autoMarker;
  auto.textContent = "Automatisch – eigene Stimme nachbilden (empfohlen)";
  el.voice.appendChild(auto);
  const groups = { female: document.createElement("optgroup"), male: document.createElement("optgroup") };
  groups.female.label = "Feste Stimme · weiblich (experimentell)";
  groups.male.label = "Feste Stimme · männlich (experimentell)";
  for (const v of (j.voices || [])) {
    const o = document.createElement("option");
    o.value = v.name; o.textContent = v.name;
    (groups[v.gender] || groups.female).appendChild(o);
  }
  el.voice.appendChild(groups.female);
  el.voice.appendChild(groups.male);

  // Gespeicherte Stimme bevorzugen, sonst Default der Sprache.
  const saved = localStorage.getItem("live_silk_voice");
  if (saved && [...el.voice.options].some((o) => o.value === saved)) {
    el.voice.value = saved; voiceTouched = true;
  } else {
    applyDefaultVoice();
  }
  renderVoiceHint();
}

// Default-Stimme der aktuellen Sprache setzen (zh → Kore, de → Charon, sonst
// Automatisch) — nur, solange der Nutzer nichts eigenes gewaehlt hat.
function applyDefaultVoice() {
  const dv = defaultVoiceMap[el.speak.value] || autoMarker;
  el.voice.value = [...el.voice.options].some((o) => o.value === dv) ? dv : autoMarker;
}

// Hinweistext zum Modell: ohne Partner generisch, sonst aus dem partner-Event.
function renderVoiceHint() {
  const a = langAvail[el.speak.value] || "both";
  const auto = el.voice.value === autoMarker;
  if (a === "live" && auto) {
    el.voiceHint.textContent =
      "Deine Sprache (*) gibt es nur im Live-Modell: es wird automatisch eine feste Stimme genutzt (experimentell, spricht nach Pausen).";
  } else if (a === "translate" && !auto) {
    el.voiceHint.textContent =
      "Deine Sprache (**) gibt es nur im Übersetzungsmodell: feste Stimme nicht möglich, deine eigene Stimme wird nachgebildet.";
  } else if (partnerModel === "translate") {
    el.voiceHint.textContent = "Übersetzung fortlaufend, eigene Stimme.";
  } else if (partnerModel === "live") {
    el.voiceHint.textContent = "Gewählte Stimme, spricht nach kurzer Pause.";
  } else {
    el.voiceHint.textContent =
      "„Automatisch“ bildet deine eigene Stimme nach (fortlaufend). Eine feste Stimme spricht nach kurzer Pause.";
  }
}

// Sprachwechsel: Default-Stimme nachziehen (wenn Nutzer nichts gewaehlt hat).
el.speak.addEventListener("change", () => {
  if (!voiceTouched) applyDefaultVoice();
  markSpeak();
  renderVoiceHint();
});
// Nutzer waehlt Stimme selbst: merken + speichern.
el.voice.addEventListener("change", () => {
  voiceTouched = true;
  localStorage.setItem("live_silk_voice", el.voice.value);
  renderVoiceHint();
});

// ---- WebCodecs-Verfuegbarkeit pruefen ----------------------------------
async function checkOpus() {
  // Sparmodus braucht WebCodecs im Browser UND einen Key auf dem Server.
  let serverKey = false;
  try {
    const base = location.pathname.replace(/\/[^/]*$/, "");
    serverKey = (await (await fetch(`${base}/api/config`)).json()).server_key === true;
  } catch { /* Server alt/unerreichbar: wie ohne Key behandeln */ }
  const webcodecs = await opusSupported();
  if (!webcodecs || !serverKey) {
    el.trOpus.disabled = true;
    el.trOpusLabel.classList.add("disabled");
    el.trPcm.checked = true;
    el.opusHint.textContent = !webcodecs
      ? "In diesem Browser nicht verfuegbar (WebCodecs fehlt)."
      : "Nicht verfuegbar: der Server hat keinen API-Key.";
  }
  syncKeyField();
}

// API-Key-Feld nur im Normalmodus zeigen (im Sparmodus nutzt der Server seinen Key).
function syncKeyField() { el.keyField.hidden = !el.trPcm.checked; }
el.trPcm.addEventListener("change", syncKeyField);
el.trOpus.addEventListener("change", syncKeyField);

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
  for (const e of [el.key, el.room, el.speak, el.voice, el.trPcm, el.trOpus]) e.disabled = active;
}

// Partner-Anzeige: „Partner spricht: <Name> · <Normal|Sparmodus>" bzw. Warten.
function renderPartner() {
  if (!partner) { el.peer.textContent = "Warte auf Partner…"; return; }
  const name = langNames[partner.lang] || partner.lang;
  const modus = partner.transport === "opus" ? "Sparmodus" : "Normal";
  el.peer.textContent = `Partner spricht: ${name} · ${modus}`;
}

// ---- WS-URL zum Relay (subpfad-faehig) — ohne target! -------------------
function relayUrl(transport) {
  const proto = location.protocol === "https:" ? "wss:" : "ws:";
  const base = location.pathname.replace(/\/[^/]*$/, "");
  const p = new URLSearchParams({
    room: el.room.value.trim(), transport, lang: el.speak.value, voice: el.voice.value,
  });
  return `${proto}//${location.host}${base}/ws?${p}`;
}

// ---- Eigene Segmentierung fuer das Live-Modell -------------------------
// Dieselbe Regel wie der Server (05b_segment.rs): Segment beginnt mit
// activityStart; endet am ersten leisen 100-ms-Block (RMS unter Schwelle)
// nach min_ms, spaetestens nach max_ms; dann sofort activityEnd+activityStart.
// Ob ueberhaupt segmentiert wird, entscheidet NUR der Server (segment-Feld im
// partner-Event) — keine eigene Modellwahl im Browser. send(kind) schickt eine
// Steuernachricht ("activityStart"/"activityEnd") an Gemini.
class PcmSegmenter {
  constructor(params, send) {
    this.minMs = params.min_ms;
    this.maxMs = params.max_ms;
    this.quiet = params.quiet_rms; // normiert [-1,1]
    this.send = send;
    this.open = false;
    this.msInSegment = 0;
    this.rest = [];                // Int16-Rest (< ein 100-ms-Block)
    this.blockSamples = 1600;      // 100 ms @ 16 kHz
  }
  // Int16-16-kHz-Block verarbeiten; sendet Steuernachrichten nach Bedarf.
  // Reihenfolge am Netz: activityStart → Audio → activityEnd. Ein Schnitt
  // schliesst das aktuelle Segment (activityEnd nach diesem Chunk); das
  // Folgesegment oeffnet der naechste push (activityStart davor).
  push(int16) {
    if (!this.open) { this.send("activityStart"); this.open = true; }
    const buf = this.rest.concat(Array.from(int16));
    let i = 0;
    while (i + this.blockSamples <= buf.length) {
      const block = buf.slice(i, i + this.blockSamples);
      i += this.blockSamples;
      this.msInSegment += 100;
      const leise = rmsNorm(block) < this.quiet;
      const langGenug = this.msInSegment >= this.minMs;
      const zuLang = this.msInSegment >= this.maxMs;
      if ((leise && langGenug) || zuLang) {
        this.send("activityEnd");
        this.open = false;
        this.msInSegment = 0;
        this.rest = buf.slice(i); // Rest gehoert zum Folgesegment
        return;
      }
    }
    this.rest = buf.slice(i);
  }
  // Beim Schliessen: offenes Segment mit activityEnd beenden.
  finish() {
    if (this.open) { this.send("activityEnd"); this.open = false; }
    this.msInSegment = 0; this.rest = [];
  }
}

// Normierter RMS (Int16 → [-1,1]) eines Blocks.
function rmsNorm(block) {
  if (!block.length) return 0;
  let sum = 0;
  for (const s of block) { const v = s / 32768; sum += v * v; }
  return Math.sqrt(sum / block.length);
}

// ---- Start / Stop -------------------------------------------------------
el.connect.addEventListener("click", start);
el.disconnect.addEventListener("click", cleanup);

async function start() {
  const room = el.room.value.trim();
  if (!room) return alert("Bitte Raumnamen angeben!");
  localStorage.setItem("live_silk_room", room);
  transportMode = el.trOpus.checked ? "opus" : "pcm";
  partner = null;
  partnerSetup = null;
  partnerModel = null;
  partnerSegment = null;
  renderPartner();
  setUI(true);
  setStatus("Initialisiere Audio…");
  try {
    ctx = new (window.AudioContext || window.webkitAudioContext)({ latencyHint: "interactive" });
    await ctx.resume();
    queue = new GaplessQueue(ctx);
    if (transportMode === "opus") await startOpus();
    else await startPcm();
  } catch (e) {
    setStatus("Fehler: " + e.message);
    cleanup();
  }
}

// ---- PCM-Pfad: Browser spricht selbst mit Gemini -----------------------
// Verbindung zu Gemini entsteht erst, wenn ein Partner bekannt ist; sie wird
// bei Partnerwechsel neu aufgebaut und geschlossen, wenn der Partner geht.
async function startPcm() {
  const key = el.key.value.trim();
  if (!key) { setStatus("Im Normalmodus wird ein API-Key benoetigt."); return cleanup(); }
  localStorage.setItem("live_silk_key", key);
  connectRelay("pcm");
  // Mikro laeuft dauerhaft; sendet aber nur, wenn eine Gemini-Sitzung offen ist.
  await startMic(false);
  // Nur „warten" anzeigen, wenn inzwischen noch kein Partner da ist (sonst
  // hat onPartner bereits openGemini angestossen).
  if (!partner) setStatus("Warte auf Partner…");
}

// Gemini-Sitzung (PCM) fuer den aktuellen Partner oeffnen. Das Setup kommt
// fertig aus dem partner-Event (EINE Quelle der Wahrheit im Server) — der
// Browser schickt es nur noch unveraendert an Gemini.
function openGemini() {
  if (!partner || !partnerSetup) return;
  const key = el.key.value.trim();
  closeGemini();
  setStatus("Verbinde zu Gemini…");
  geminiWs = new WebSocket(`${GEMINI_BASE}?key=${key}`);
  geminiWs.onopen = () => {
    geminiWs.send(JSON.stringify({ setup: partnerSetup }));
    // Nur das Live-Modell segmentiert selbst (Server sagt es via segment-Feld).
    segmenter = partnerSegment
      ? new PcmSegmenter(partnerSegment, (kind) => {
          if (geminiWs?.readyState === WebSocket.OPEN) {
            geminiWs.send(JSON.stringify({ realtimeInput: { [kind]: {} } }));
          }
        })
      : null;
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
        if (roomWs?.readyState === WebSocket.OPEN) {
          roomWs.send(buf.buffer); countUp(buf.byteLength);
        }
      }
    }
    if (sc.inputTranscription?.text) subAndSend("in", sc.inputTranscription.text);
    if (sc.outputTranscription?.text) subAndSend("out", sc.outputTranscription.text);
  };
  geminiWs.onerror = () => setStatus("Gemini-Fehler — API-Key pruefen.");
  geminiWs.onclose = () => { if (live && partner) setStatus("Gemini getrennt."); };
}

function closeGemini() {
  // Offenes Segment sauber mit activityEnd beenden, bevor die Verbindung schliesst.
  if (segmenter) { try { segmenter.finish(); } catch {} segmenter = null; }
  if (geminiWs) { geminiWs.onclose = null; try { geminiWs.close(); } catch {} geminiWs = null; }
}

// Untertitel-Artefakte filtern (gleiche Regel wie der Server,
// 05b_segment.rs): `<...>`/`{...}`-Tokens und „translation pending" raus,
// Mehrfach-Leerzeichen zusammen, getrimmt. Leer → "" (nicht senden).
function cleanSubtitle(text) {
  let out = "", eckig = 0, geschweift = 0;
  for (const c of text) {
    if (c === "<") eckig++;
    else if (c === ">") eckig = Math.max(0, eckig - 1);
    else if (c === "{") geschweift++;
    else if (c === "}") geschweift = Math.max(0, geschweift - 1);
    else if (eckig === 0 && geschweift === 0) out += c;
  }
  return out.replace(/translation pending/g, "").split(/\s+/).filter(Boolean).join(" ");
}

// Transkript lokal zeigen und als Text an den Relay (fuer den Partner).
function subAndSend(dir, text) {
  const sauber = cleanSubtitle(text);
  if (!sauber) return;             // leere/artefaktige Untertitel nicht senden
  addSub("me", dir, sauber);
  if (roomWs?.readyState === WebSocket.OPEN) {
    roomWs.send(JSON.stringify({ type: "text", dir, text: sauber }));
  }
}

// ---- Opus-Pfad: Sparmodus ----------------------------------------------
async function startOpus() {
  connectRelay("opus");
  setStatus("Sparmodus — warte auf Partner…");
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
      // Live-Modell: erst segmentieren (activityStart/End), dann Audio senden.
      if (segmenter) segmenter.push(int16);
      // Nur senden, wenn eine Gemini-Sitzung offen ist (Partner bekannt).
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
      roomWs.send(JSON.stringify({ type: "hello" })); // Sparmodus: Server-Key, kein Key vom Browser
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

// Reaktion auf einen Partnerwechsel (Join/Leave/Sprachwechsel). `m` ist das
// ganze partner-Event (enthaelt `model` und — bei PCM — `setup`).
function onPartner(m, transport) {
  const before = partner ? JSON.stringify([partner.lang, partnerSetup]) : null;
  partner = m.partner || null;
  partnerModel = partner ? (m.model || null) : null;
  partnerSetup = partner ? (m.setup || null) : null;
  partnerSegment = partner ? (m.segment || null) : null;
  renderPartner();
  renderVoiceHint();
  if (transport === "pcm") {
    if (!partner) {
      // Partner weg: Gemini schliessen (Mikro laeuft weiter, sendet nichts).
      closeGemini();
      setStatus("Warte auf Partner…");
    } else {
      const now = JSON.stringify([partner.lang, partnerSetup]);
      // Neuer/wechselnder Partner oder geaendertes Setup: Gemini neu aufbauen.
      if (now !== before) openGemini();
    }
  }
  // Opus-Modus: Status folgt waiting/ready vom Server.
}

function handleRelayText(m, transport) {
  switch (m.type) {
    case "partner":
      onPartner(m, transport);
      break;
    case "waiting":
      // Opus-Modus: Server hat (noch) keinen Partner → keine Sitzung.
      setStatus("Warte auf Partner…");
      break;
    case "ready":
      if (transport === "opus") {
        // Mikro erst jetzt starten (Sitzung steht).
        if (!mic) startMic(true).catch((e) => setStatus("Mikro: " + e.message));
      }
      setStatus("Live!", true);
      break;
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
  partner = null;
  partnerSetup = null;
  partnerModel = null;
  partnerSegment = null;
  renderPartner();
  renderVoiceHint();
  if (proc) { proc.disconnect(); proc.onaudioprocess = null; proc = null; }
  if (mic) { mic.getTracks().forEach((t) => t.stop()); mic = null; }
  if (upOpus) { upOpus.close(); upOpus = null; }
  if (downOpus) { downOpus.close(); downOpus = null; }
  closeGemini();
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
