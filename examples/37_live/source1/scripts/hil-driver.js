// hil-driver.js — ein Browser-Tab spricht (Fake-Mikro), Peer B hoert zu.
// Steuerung per Umgebung (Defaults siehe hil-browser.sh):
// HIL_BASE, HIL_ROOM, HIL_MODE, HIL_KEY_FILE, HIL_CHROME, HIL_WAV,
// HIL_SECONDS, HIL_OUT (Shot-Verzeichnis). Key wird maskiert geloggt.
const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright-core');
const WebSocket = require('ws');

const BASE = process.env.HIL_BASE || 'http://127.0.0.1:3201';
const ROOM = process.env.HIL_ROOM || 'hil-zh-de-1';
const MODE = process.env.HIL_MODE || 'zh-to-de';
const KEY_FILE = process.env.HIL_KEY_FILE || '/workspace/src/cl-rust-generator/api_key.txt';
const CHROME = process.env.HIL_CHROME || '/tmp/live-hil/chrome-linux64/chrome';
const WAV = process.env.HIL_WAV || '/tmp/live-hil/mic-zh.wav';
const SECONDS = parseInt(process.env.HIL_SECONDS || '75', 10);
const OUT = process.env.HIL_OUT || '/tmp/live-hil';
const HOST = BASE.replace('http://', '');

const mask = (s) => (s.length < 8 ? '***' : s.slice(0, 4) + '...' + s.slice(-4));

(async () => {
  const key = fs.readFileSync(KEY_FILE, 'utf8').trim();
  console.log('key geladen:', mask(key), 'len=', key.length);

  const ear = { bytes: 0, frames: 0, firstAt: 0 };
  const wsB = new WebSocket(`ws://${HOST}/ws?room=${encodeURIComponent(ROOM)}`);
  await new Promise((res, rej) => { wsB.on('open', res); wsB.on('error', rej); });
  console.log('peerB: relay-verbunden');
  wsB.on('message', (data, isBinary) => {
    if (isBinary) {
      if (ear.frames === 0) ear.firstAt = Date.now();
      ear.frames++;
      ear.bytes += data.length;
    }
  });

  const browser = await chromium.launch({
    executablePath: CHROME,
    headless: true,
    args: [
      '--use-fake-device-for-media-stream',
      `--use-file-for-fake-audio-capture=${WAV}`,
      '--use-fake-ui-for-media-stream',
      '--autoplay-policy=no-user-gesture-required',
      '--no-sandbox',
    ],
  });
  const page = await browser.newPage();
  const logs = [];
  page.on('console', (m) => logs.push(`[${m.type()}] ${m.text()}`));
  page.on('pageerror', (e) => logs.push(`[pageerror] ${e.message}`));

  await page.goto(BASE, { waitUntil: 'networkidle' });
  await page.fill('#apiKey', key);
  await page.fill('#roomName', ROOM);
  await page.selectOption('#langPair', MODE);
  const t0 = Date.now();
  await page.click('#connectBtn');

  const step = 5000;
  for (let t = step; t <= SECONDS * 1000; t += step) {
    await page.waitForTimeout(step);
    const status = await page.textContent('#status');
    console.log(`t=${Math.round((Date.now() - t0) / 1000)}s status=${JSON.stringify(status)} peerB: frames=${ear.frames} bytes=${ear.bytes}`);
    if (ear.bytes > 50000 && status.includes('Live')) break;
  }
  await page.screenshot({ path: path.join(OUT, 'hil-after.png') });
  const statusFinal = await page.textContent('#status');

  console.log('--- ERGEBNIS ---');
  console.log('statusFinal:', JSON.stringify(statusFinal));
  console.log('peerB frames:', ear.frames, 'bytes:', ear.bytes);
  const leaked = logs.some((l) => l.includes(key));
  console.log('key-in-logs:', leaked ? 'LECK!' : 'nein');
  const errors = logs.filter((l) => l.startsWith('[error]') || l.startsWith('[pageerror]'));
  console.log('seitenfehler:', errors.length ? errors.map((l) => l.slice(0, 160)) : 'keine');

  await browser.close();
  wsB.close();
  const pass = ear.bytes > 50000 && statusFinal.includes('Live') && !leaked && errors.length === 0;
  console.log(pass ? 'HIL PASS' : 'HIL FAIL');
  process.exit(pass ? 0 : 1);
})().catch((e) => { console.error('HIL ABBRUCH:', e.message); process.exit(2); });
