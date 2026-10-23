// hil-content.js — bidirektionaler Inhalts-Nachweis ohne Browser:
// Gemini-TTS-Referenz -> Live-Uebersetzung -> Relay-Zuhörer + Capture
// -> Stille-Trimm -> Gemini-STT-Transkript -> Stichwort-Check.
// ENV: HIL_KEY_FILE, HIL_BASE, HIL_OUT. ffmpeg muss im PATH sein.
const fs = require('fs');
const path = require('path');
const { execFileSync } = require('child_process');
const WebSocket = require('ws');

const KEY = fs.readFileSync(process.env.HIL_KEY_FILE || '/workspace/src/cl-rust-generator/api_key.txt', 'utf8').trim();
const BASE = process.env.HIL_BASE || 'http://127.0.0.1:3201';
const HOST = BASE.replace('http://', '');
const OUT = process.env.HIL_OUT || '/tmp/live-hil';
const LIVE = 'wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1beta.GenerativeService.BidiGenerateContent?key=' + KEY;
const mask = (s) => s.slice(0, 4) + '...' + s.slice(-4);

const DIRS = [
  {
    name: 'de-to-zh', say: 'Sage auf natuerliche Weise: Guten Morgen! Heute ist ein schöner Tag.',
    ttsVoice: 'Charon', liveVoice: 'Aoede', repeat: 3,
    instruction: 'You are a simultaneous interpreter. The user speaks German. Translate every German sentence into Mandarin Chinese and speak ONLY the Chinese translation. Never ask questions, never add comments.',
    sttPrompt: 'Transcribe this Chinese audio using Chinese characters. Reply with the transcript only.',
    expect: ['早上好'],
  },
  {
    name: 'zh-to-de', say: '用自然的语气说：早上好！今天天气很好。',
    ttsVoice: 'Aoede', liveVoice: 'Puck', repeat: 3,
    instruction: 'You are a simultaneous interpreter. The user speaks Mandarin Chinese. Translate every Chinese sentence into German and speak ONLY the German translation. Never ask questions, never add comments.',
    sttPrompt: 'Transcribe this German audio verbatim. Reply with the transcript only.',
    expect: ['Morgen', 'Wetter'],
  },
];

function wavWrap(pcm24) {
  const hdr = Buffer.alloc(44);
  hdr.write('RIFF', 0); hdr.writeUInt32LE(36 + pcm24.length, 4); hdr.write('WAVEfmt ', 8);
  hdr.writeUInt32LE(16, 16); hdr.writeUInt16LE(1, 20); hdr.writeUInt16LE(1, 22);
  hdr.writeUInt32LE(24000, 24); hdr.writeUInt32LE(48000, 28); hdr.writeUInt16LE(2, 32);
  hdr.writeUInt16LE(16, 34); hdr.write('data', 36); hdr.writeUInt32LE(pcm24.length, 40);
  return Buffer.concat([hdr, pcm24]);
}

async function tts(text, voice) {
  const r = await fetch('https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-tts:generateContent?key=' + KEY, {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ contents: [{ parts: [{ text }] }], generationConfig: { responseModalities: ['AUDIO'], speechConfig: { voiceConfig: { prebuiltVoiceConfig: { voiceName: voice } } } } }),
  });
  const j = await r.json();
  const p = j.candidates?.[0]?.content?.parts?.[0]?.inlineData;
  if (!p) throw new Error('TTS-Fehler ' + JSON.stringify(j).slice(0, 150));
  return Buffer.from(p.data, 'base64'); // 24 kHz mono16
}

async function stt(wav, prompt) {
  const r = await fetch('https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=' + KEY, {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ contents: [{ parts: [{ text: prompt }, { inlineData: { mimeType: 'audio/wav', data: wav.toString('base64') } }] }] }),
  });
  const j = await r.json();
  return j.candidates?.[0]?.content?.parts?.map((x) => x.text || '').join(' ') || 'STT-LEER';
}

// Roh-PCM (24 kHz) -> 16-kHz-WAV-Datei (fuer Live-Eingabe)
function to16kWavFile(raw24, file) {
  execFileSync('ffmpeg', ['-y', '-v', 'error', '-f', 's16le', '-ar', '24000', '-ac', '1', '-i', 'pipe:0',
    '-ar', '16000', '-ac', '1', '-c:a', 'pcm_s16le', '-f', 'wav', file], { input: raw24 });
}

// Roh-PCM (24 kHz) -> getrimmter 8-s-WAV-Ausschnitt (fuer STT)
function toTrimmedWavFile(raw24, file) {
  const n = raw24.length / 2;
  let last = 0;
  for (let i = 0; i < n; i++) {
    if (Math.abs(raw24.readInt16LE(i * 2)) > 200) last = i;
  }
  const cut = raw24.subarray(0, Math.min(n, last + 7200) * 2);
  execFileSync('ffmpeg', ['-y', '-v', 'error', '-f', 's16le', '-ar', '24000', '-ac', '1', '-i', 'pipe:0',
    '-t', '8', '-c:a', 'pcm_s16le', '-f', 'wav', file], { input: cut });
}

async function run(dir) {
  console.log(`--- ${dir.name}: TTS-Referenz ...`);
  const ref24 = await tts(dir.say, dir.ttsVoice);
  fs.writeFileSync(path.join(OUT, `ref-${dir.name}.wav`), wavWrap(ref24));
  to16kWavFile(ref24, path.join(OUT, `ref-${dir.name}-16k.wav`));
  let pcm16 = fs.readFileSync(path.join(OUT, `ref-${dir.name}-16k.wav`)).subarray(44);
  if (dir.repeat > 1) pcm16 = Buffer.concat(Array(dir.repeat).fill(pcm16));

  const room = 'content-' + dir.name + '-' + Date.now();
  const ear = { bytes: 0, frames: 0 };
  const wsB = new WebSocket(`ws://${HOST}/ws?room=${room}`);
  await new Promise((res, rej) => { wsB.on('open', res); wsB.on('error', rej); });
  wsB.on('message', (d, bin) => { if (bin) { ear.frames++; ear.bytes += d.length; } });
  const wsA = new WebSocket(`ws://${HOST}/ws?room=${room}`);
  await new Promise((res, rej) => { wsA.on('open', res); wsA.on('error', rej); });

  const out = [];
  const live = new WebSocket(LIVE);
  await new Promise((resolve) => {
    live.on('open', () => {
      live.send(JSON.stringify({ setup: { model: 'models/gemini-3.5-live-translate-preview', generationConfig: { responseModalities: ['AUDIO'], speechConfig: { voiceConfig: { prebuiltVoiceConfig: { voiceName: dir.liveVoice } } } }, systemInstruction: { parts: [{ text: dir.instruction }] } } }));
    });
    live.on('message', (d) => {
      const m = JSON.parse(d.toString());
      if (m.setupComplete) {
        let off = 0;
        const iv = setInterval(() => {
          if (off >= pcm16.length) {
            clearInterval(iv);
            live.send(JSON.stringify({ realtimeInput: { audioStreamEnd: true } }));
            setTimeout(() => live.close(), 12000);
            return;
          }
          live.send(JSON.stringify({ realtimeInput: { mediaChunks: [{ mimeType: 'audio/pcm;rate=16000', data: pcm16.subarray(off, off + 16000).toString('base64') }] } }));
          off += 16000;
        }, 500);
      }
      const parts = (m.serverContent && m.serverContent.modelTurn && m.serverContent.modelTurn.parts) || [];
      for (const p of parts) {
        if (p.inlineData && p.inlineData.data) {
          const b = Buffer.from(p.inlineData.data, 'base64');
          out.push(b);
          try { wsA.send(b); } catch {}
        }
      }
    });
    live.on('close', () => resolve());
    live.on('error', () => resolve());
    setTimeout(resolve, 120000);
  });
  await new Promise((r) => setTimeout(r, 800));
  wsB.close(); wsA.close();
  const raw = Buffer.concat(out);
  toTrimmedWavFile(raw, path.join(OUT, `out-${dir.name}.wav`));
  const tr = await stt(fs.readFileSync(path.join(OUT, `out-${dir.name}.wav`)), dir.sttPrompt);
  console.log(`  relay: frames=${ear.frames} bytes=${ear.bytes} | STT: ${tr.slice(0, 220)}`);
  const ok = ear.bytes > 50000 && dir.expect.every((w) => tr.includes(w));
  console.log(`  erwartet ${JSON.stringify(dir.expect)} -> ${ok ? 'PASS' : 'FAIL'}`);
  return ok;
}

(async () => {
  console.log('key:', mask(KEY));
  let pass = true;
  for (const d of DIRS) {
    try { pass = (await run(d)) && pass; }
    catch (e) { console.log(`  ABBRUCH ${d.name}:`, e.message.slice(0, 160)); pass = false; }
  }
  console.log(pass ? 'CONTENT PASS' : 'CONTENT FAIL');
  process.exit(pass ? 0 : 1);
})();
