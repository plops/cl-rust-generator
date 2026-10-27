// browser-fake.js — zwei Headless-Chrome-Tabs gegen Fake-Gemini.
// Tab A: Sparmodus (Opus), lang zh-Hans → target de, Key leer (Server-Key).
// Tab B: Normal (PCM) mit ?gemini=ws://FAKE, Dummy-Key.
// PASS: beide Live; A opusUp>0 & opusDown>0; B pcmDown>0; Untertitel>0 bei
// beiden; A kbit/s (up+down) < 40; keine pageerror. Messwerte → stdout.
"use strict";
const fs = require("fs");
const path = require("path");
const { chromium } = require("playwright-core");

const BASE = process.env.HIL_BASE || "http://127.0.0.1:3311";
const FAKE = process.env.FAKE_WS || "ws://127.0.0.1:8900";
const ROOM = process.env.HIL_ROOM || "fake-room-1";
const CHROME = process.env.HIL_CHROME || "/tmp/live-hil/chrome-linux64/chrome";
const WAV = process.env.HIL_WAV || "/tmp/live-hil/mic-zh.wav";
const SECONDS = parseInt(process.env.HIL_SECONDS || "20", 10);
const OUT = process.env.HIL_OUT || "/tmp/live-hil";

// kbit/s aus Gesamtbytes und Laufzeit abschaetzen (gleitendes Fenster zeigt
// die UI; fuer PASS reicht der Mittelwert ueber die Messdauer).
function kbit(bytes, secs) { return Math.round((bytes * 8) / secs / 1000); }

async function setupPage(browser, { transport, speak, hear, key, gemini }) {
  const url = gemini ? `${BASE}/?gemini=${encodeURIComponent(gemini)}` : BASE;
  const page = await browser.newPage();
  const errs = [];
  page.on("pageerror", (e) => errs.push(e.message));
  page.on("console", (m) => { if (m.type() === "error") errs.push("[console] " + m.text()); });
  await page.goto(url, { waitUntil: "networkidle" });
  await page.fill("#apiKey", key);
  await page.fill("#roomName", ROOM);
  await page.selectOption("#langSpeak", speak);
  await page.selectOption("#langHear", hear);
  await page.check(transport === "opus" ? "#trOpus" : "#trPcm");
  page.__errs = errs;
  return page;
}

async function stats(page) {
  return page.evaluate(() => window.__stats);
}
async function statusText(page) {
  return page.textContent("#status");
}

(async () => {
  const browser = await chromium.launch({
    executablePath: CHROME,
    headless: true,
    args: [
      "--use-fake-device-for-media-stream",
      `--use-file-for-fake-audio-capture=${WAV}%noloop`,
      "--use-fake-ui-for-media-stream",
      "--autoplay-policy=no-user-gesture-required",
      "--no-sandbox",
    ],
  });

  // Tab A zuerst, Tab B danach (B ist PCM-Partner, spricht selbst mit Fake).
  const a = await setupPage(browser, { transport: "opus", speak: "zh-Hans", hear: "de", key: "", gemini: null });
  const b = await setupPage(browser, { transport: "pcm", speak: "de", hear: "zh-Hans", key: "dummy-key", gemini: FAKE });

  await a.click("#connectBtn");
  await b.click("#connectBtn");

  for (let t = 0; t < SECONDS; t += 5) {
    await a.waitForTimeout(5000);
    const sa = await stats(a), sb = await stats(b);
    console.log(`t=${t + 5}s A{status=${JSON.stringify(await statusText(a))} opusUp=${sa.opusUp} opusDown=${sa.opusDown} texts=${sa.texts}} B{status=${JSON.stringify(await statusText(b))} pcmDown=${sb.pcmDown} texts=${sb.texts}}`);
  }

  const sa = await stats(a), sb = await stats(b);
  const statusA = await statusText(a), statusB = await statusText(b);
  await a.screenshot({ path: path.join(OUT, "fake-A-opus.png") });
  await b.screenshot({ path: path.join(OUT, "fake-B-pcm.png") });

  const aKbit = kbit(sa.up + sa.down, SECONDS);
  const bKbit = kbit(sb.up + sb.down, SECONDS);

  console.log("--- MESSWERTE ---");
  console.log(`A (Opus): up=${sa.up}B down=${sa.down}B opusUp=${sa.opusUp}B opusDown=${sa.opusDown}B texts=${sa.texts} → ${aKbit} kbit/s`);
  console.log(`B (PCM) : up=${sb.up}B down=${sb.down}B pcmDown=${sb.pcmDown}B texts=${sb.texts} → ${bKbit} kbit/s`);
  console.log(`statusA=${JSON.stringify(statusA)} statusB=${JSON.stringify(statusB)}`);
  const errs = [...a.__errs, ...b.__errs];
  console.log("seitenfehler:", errs.length ? errs.slice(0, 6) : "keine");

  const checks = {
    "A live": statusA.includes("Live"),
    "B live": statusB.includes("Live"),
    "A opusUp>0": sa.opusUp > 0,
    "A opusDown>0": sa.opusDown > 0,
    "B pcmDown>0": sb.pcmDown > 0,
    "A texts>0": sa.texts > 0,
    "B texts>0": sb.texts > 0,
    "A kbit<40": aKbit < 40,
    "keine pageerror": errs.length === 0,
  };
  console.log("--- CHECKS ---");
  let pass = true;
  for (const [k, v] of Object.entries(checks)) { console.log(`${v ? "ok  " : "FAIL"} ${k}`); if (!v) pass = false; }

  await browser.close();
  console.log(pass ? "BROWSER-FAKE PASS" : "BROWSER-FAKE FAIL");
  process.exit(pass ? 0 : 1);
})().catch((e) => { console.error("ABBRUCH:", e.stack || e.message); process.exit(2); });
