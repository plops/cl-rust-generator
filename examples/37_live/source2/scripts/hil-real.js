// hil-real.js — HIL gegen das ECHTE Gemini mit zwei SEPARATEN Chrome-Prozessen.
//
// Der Fake-Mikrofon-Stream (--use-file-for-fake-audio-capture) gilt pro
// Browser-Prozess, darum zwei eigene chromium.launch():
//   A = Sparmodus (Opus), spricht Chinesisch (mic-zh.wav), lang zh-Hans;
//       Ziel folgt automatisch dem Partner (= de). Key-Feld leer → Server-Key
//       spricht mit Gemini.
//   B = Normal (PCM), spricht Deutsch (mic-de.wav), lang de; Ziel folgt dem
//       Partner (= zh-Hans). Key im Feld → B verbindet direkt mit Gemini.
// Gleicher Raum. Laufzeit ~45-60 s.
//
// Misst Status, Partner-Anzeige, __stats (Bytes/kbit/s), Untertitel je
// Richtung, Zeit bis erstes Partner-Audio, /api/rooms waehrend des Laufs.
//
// PASS: beide Live; A opusDown>0 & B pcmDown>0; B sieht deutschen Partner-
// Untertitel (lateinische Woerter), A sieht chinesische Zeichen
// (/[\u4e00-\u9fff]/); A sieht „Partner spricht: Deutsch", B „Chinesisch";
// A kbit/s < 40; keine pageerror; Key in KEINEM Log.
//
// Der Key wird NIE ausgegeben — nur maskiert (AIza…abcd). Er kommt aus der
// Datei und wird nur ins Key-Feld von B getippt sowie als Server-Key genutzt.
"use strict";
const fs = require("fs");
const path = require("path");
const { chromium } = require("playwright-core");

const BASE = process.env.HIL_BASE || "http://127.0.0.1:3312";
const ROOM = process.env.HIL_ROOM || `real-room-${Date.now()}`;
const CHROME = process.env.HIL_CHROME || "/tmp/live-hil/chrome-linux64/chrome";
const WAV_ZH = process.env.HIL_WAV_ZH || "/tmp/live-hil/mic-zh.wav";
const WAV_DE = process.env.HIL_WAV_DE || "/tmp/live-hil/mic-de.wav";
const SECONDS = parseInt(process.env.HIL_SECONDS || "50", 10);
const OUT = process.env.HIL_OUT || "/tmp/live-hil";
const KEYFILE = process.env.HIL_KEY_FILE || "/workspace/src/cl-rust-generator/api_key.txt";

const KEY = fs.readFileSync(KEYFILE, "utf8").trim();
const mask = (s) => (s.length > 8 ? `${s.slice(0, 4)}…${s.slice(-4)}` : "****");

// kbit/s aus Gesamtbytes und Laufzeit (Mittelwert ueber die Messdauer).
const kbit = (bytes, secs) => Math.round((bytes * 8) / secs / 1000);

// Chinesische Schriftzeichen?
const hasCjk = (s) => /[\u4e00-\u9fff]/.test(s || "");
// Lateinische Buchstaben + mindestens ein Wort mit >=3 Buchstaben?
const hasLatinWord = (s) => /[A-Za-zÄÖÜäöüß]{3,}/.test(s || "");

// Grundfrequenz F0 schaetzen: Autokorrelation je 40-ms-Frame, Median ueber
// stimmhafte Frames (genug Energie + klares Maximum). `sr` = Abtastrate.
// Suchbereich 70-400 Hz deckt maennliche (85-155) und weibliche (165-255)
// Sprechstimmen ab. Rueckgabe in Hz oder null (keine stimmhaften Frames).
function estimateF0(samples, sr) {
  if (!samples || samples.length < sr * 0.1) return null;
  const frame = Math.round(sr * 0.04); // 40 ms
  const minLag = Math.floor(sr / 400), maxLag = Math.floor(sr / 70);
  const f0s = [];
  for (let start = 0; start + frame <= samples.length; start += frame) {
    const seg = samples.subarray ? samples.subarray(start, start + frame) : samples.slice(start, start + frame);
    let energy = 0;
    for (let i = 0; i < seg.length; i++) energy += seg[i] * seg[i];
    if (energy / seg.length < 1e-4) continue; // zu leise → nicht stimmhaft
    let bestLag = -1, best = 0;
    const r0 = energy;
    for (let lag = minLag; lag <= maxLag && lag < seg.length; lag++) {
      let acc = 0;
      for (let i = 0; i + lag < seg.length; i++) acc += seg[i] * seg[i + lag];
      if (acc > best) { best = acc; bestLag = lag; }
    }
    // Klares Maximum (> 0,3 * Energie) als Stimmhaftigkeits-Kriterium.
    if (bestLag > 0 && best > 0.3 * r0) f0s.push(sr / bestLag);
  }
  if (!f0s.length) return null;
  f0s.sort((a, b) => a - b);
  return f0s[Math.floor(f0s.length / 2)]; // Median
}

// Eine Chrome-Instanz mit genau EINEM Fake-Mikro starten.
function launch(wav) {
  return chromium.launch({
    executablePath: CHROME,
    headless: true,
    args: [
      "--use-fake-device-for-media-stream",
      `--use-file-for-fake-audio-capture=${wav}%noloop`,
      "--use-fake-ui-for-media-stream",
      "--autoplay-policy=no-user-gesture-required",
      "--no-sandbox",
    ],
  });
}

// Eine Seite vorbereiten (Felder fuellen), aber noch nicht verbinden.
async function setupPage(browser, { transport, speak, key }) {
  const page = await browser.newPage();
  const errs = [];
  const subsPartner = []; // Untertitel, die diese Seite als "Partner" sieht.
  const subsMe = [];
  page.on("pageerror", (e) => errs.push(e.message));
  page.on("console", (m) => {
    const t = m.text();
    if (m.type() === "error") errs.push("[console] " + t);
  });
  // VOR dem Laden: Partner-Audio (24 kHz) mitschneiden. Beide Pfade (PCM und
  // Opus) enden in GaplessQueue.pushFloat — dort greifen wir ab.
  await page.addInitScript(() => {
    window.__partnerPcm = [];
    window.__partnerRate = 24000;
    const hook = () => {
      if (typeof GaplessQueue === "undefined" || GaplessQueue.__hooked) return;
      const orig = GaplessQueue.prototype.pushFloat;
      GaplessQueue.prototype.pushFloat = function (f32, rate) {
        window.__partnerRate = rate;
        // Nur begrenzt sammeln (max ~6 s bei 24 kHz), um Speicher zu schonen.
        if (window.__partnerPcm.length < 24000 * 6) {
          for (let i = 0; i < f32.length; i++) window.__partnerPcm.push(f32[i]);
        }
        return orig.call(this, f32, rate);
      };
      GaplessQueue.__hooked = true;
    };
    const iv = setInterval(hook, 30);
    setTimeout(() => clearInterval(iv), 8000);
  });
  await page.goto(BASE, { waitUntil: "networkidle" });
  await page.fill("#roomName", ROOM);
  await page.selectOption("#langSpeak", speak);
  await page.check(transport === "opus" ? "#trOpus" : "#trPcm");
  // Key-Feld gibt es nur im Normalmodus.
  if (transport === "pcm") await page.fill("#apiKey", key);
  // Zeit bis erstes Partner-Audio (Downlink) ueber __stats beobachten.
  await page.evaluate(() => {
    window.__firstDown = 0;
    const iv = setInterval(() => {
      if (!window.__firstDown && window.__stats && window.__stats.down > 0) {
        window.__firstDown = performance.now();
        clearInterval(iv);
      }
    }, 50);
  });
  page.__errs = errs;
  page.__subsPartner = subsPartner;
  page.__subsMe = subsMe;
  return page;
}

const stats = (page) => page.evaluate(() => window.__stats);
const firstDown = (page) => page.evaluate(() => window.__firstDown || 0);
const statusText = (page) => page.textContent("#status");
const peerText = (page) => page.textContent("#peerInfo");

// Untertitel-Liste aus dem DOM lesen: [{who,dir,text}].
async function readSubs(page) {
  return page.$$eval("#subs li", (lis) =>
    lis.map((li) => {
      const who = li.classList.contains("me") ? "me" : "partner";
      const span = li.querySelector(".who");
      const whoTxt = span ? span.textContent : "";
      const dir = /·\s*(\w+)/.exec(whoTxt);
      const text = li.textContent.replace(whoTxt, "").trim();
      return { who, dir: dir ? dir[1] : "", text };
    }),
  );
}

async function pollRooms() {
  try {
    const r = await fetch(`${BASE}/api/rooms`);
    return JSON.stringify(await r.json());
  } catch (e) {
    return "(rooms n/a: " + e.message + ")";
  }
}

(async () => {
  console.log(`key: ${mask(KEY)}  room: ${ROOM}  dauer: ${SECONDS}s`);
  if (!fs.existsSync(WAV_DE)) throw new Error("mic-de.wav fehlt (hil-real.sh erzeugt sie per TTS)");

  const browserA = await launch(WAV_ZH);
  const browserB = await launch(WAV_DE);

  // A: Opus, spricht zh-Hans (Ziel folgt automatisch dem Partner), Key leer
  // → Server-Key spricht mit Gemini.
  const a = await setupPage(browserA, { transport: "opus", speak: "zh-Hans", key: "" });
  // B: PCM, spricht de (Ziel = Partner-Sprache), Key im Feld (direkt zu Gemini).
  const b = await setupPage(browserB, { transport: "pcm", speak: "de", key: KEY });

  const t0 = Date.now();
  await a.click("#connectBtn");
  await b.click("#connectBtn");

  let roomsSnap = "(nicht erfasst)";
  for (let t = 0; t < SECONDS; t += 5) {
    await a.waitForTimeout(5000);
    const sa = await stats(a), sb = await stats(b);
    if (t === Math.floor(SECONDS / 2 / 5) * 5) roomsSnap = await pollRooms();
    console.log(
      `t=${t + 5}s A{st=${JSON.stringify((await statusText(a)).slice(0, 18))} opusUp=${sa.opusUp} opusDown=${sa.opusDown} tx=${sa.texts}} ` +
        `B{st=${JSON.stringify((await statusText(b)).slice(0, 18))} pcmDown=${sb.pcmDown} tx=${sb.texts}}`,
    );
  }

  const sa = await stats(a), sb = await stats(b);
  const statusA = await statusText(a), statusB = await statusText(b);
  const peerA = await peerText(a), peerB = await peerText(b);
  const fdA = await firstDown(a), fdB = await firstDown(b);
  const subsA = await readSubs(a), subsB = await readSubs(b);

  // F0 des Partner-Audios: B empfaengt A (zh, Default-Stimme Kore, weiblich).
  // A empfaengt B (de, Default-Stimme Charon, maennlich).
  const pcmB = await b.evaluate(() => ({ s: window.__partnerPcm || [], r: window.__partnerRate || 24000 }));
  const pcmA = await a.evaluate(() => ({ s: window.__partnerPcm || [], r: window.__partnerRate || 24000 }));
  const f0B = estimateF0(Float32Array.from(pcmB.s), pcmB.r); // A→B: Kore (weiblich)
  const f0A = estimateF0(Float32Array.from(pcmA.s), pcmA.r); // B→A: Charon (maennlich)

  await a.screenshot({ path: path.join(OUT, "real-A-opus.png") });
  await b.screenshot({ path: path.join(OUT, "real-B-pcm.png") });

  const aKbit = kbit(sa.up + sa.down, SECONDS);
  const bKbit = kbit(sb.up + sb.down, SECONDS);

  // Partner-Untertitel je Seite (max 5, gekuerzt).
  const partnerA = subsA.filter((s) => s.who === "partner");
  const partnerB = subsB.filter((s) => s.who === "partner");
  const short = (arr) => arr.slice(0, 5).map((s) => `[${s.dir}] ${s.text.slice(0, 40)}`);

  console.log("--- MESSWERTE ---");
  console.log(`A (Opus): up=${sa.up}B down=${sa.down}B opusUp=${sa.opusUp}B opusDown=${sa.opusDown}B texts=${sa.texts} → ${aKbit} kbit/s`);
  console.log(`B (PCM) : up=${sb.up}B down=${sb.down}B pcmDown=${sb.pcmDown}B texts=${sb.texts} → ${bKbit} kbit/s`);
  console.log(`erstes Partner-Audio: A nach ${fdA ? Math.round(fdA) : "—"} ms, B nach ${fdB ? Math.round(fdB) : "—"} ms (ab Verbindung)`);
  console.log(`F0 Partner-Audio: B hört A = ${f0B ? Math.round(f0B) : "—"} Hz (erw. weiblich/Kore >165), A hört B = ${f0A ? Math.round(f0A) : "—"} Hz (erw. männlich/Charon)`);
  console.log(`/api/rooms (Mitte): ${roomsSnap}`);
  console.log(`A Partner-Untertitel (erw. Chinesisch): ${JSON.stringify(short(partnerA))}`);
  console.log(`B Partner-Untertitel (erw. Deutsch)   : ${JSON.stringify(short(partnerB))}`);
  console.log(`statusA=${JSON.stringify(statusA)} statusB=${JSON.stringify(statusB)}`);
  console.log(`peerA=${JSON.stringify(peerA)} peerB=${JSON.stringify(peerB)}`);
  const errs = [...a.__errs, ...b.__errs];
  console.log("seitenfehler:", errs.length ? errs.slice(0, 6) : "keine");

  const aSeesCjk = partnerA.some((s) => hasCjk(s.text));
  const bSeesLatin = partnerB.some((s) => hasLatinWord(s.text));

  const checks = {
    "A live": statusA.includes("Live"),
    "B live": statusB.includes("Live"),
    "A opusDown>0": sa.opusDown > 0,
    "B pcmDown>0": sb.pcmDown > 0,
    "A sieht chin. Partner-Untertitel": aSeesCjk,
    "B sieht dt. Partner-Untertitel": bSeesLatin,
    "A sieht Partner Deutsch": peerA.includes("Deutsch"),
    "B sieht Partner Chinesisch": peerB.includes("Chinesisch"),
    "B hört weibliche Stimme (Kore, F0>165)": f0B !== null && f0B > 165,
    "A kbit/s < 40": aKbit < 40,
    "keine pageerror": errs.length === 0,
  };
  console.log("--- CHECKS ---");
  let pass = true;
  for (const [k, v] of Object.entries(checks)) {
    console.log(`${v ? "ok  " : "FAIL"} ${k}`);
    if (!v) pass = false;
  }

  await browserA.close();
  await browserB.close();
  console.log(pass ? "HIL-REAL PASS" : "HIL-REAL FAIL");
  process.exit(pass ? 0 : 1);
})().catch((e) => {
  console.error("ABBRUCH:", e.stack || e.message);
  process.exit(2);
});
