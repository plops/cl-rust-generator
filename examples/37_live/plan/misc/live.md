Hier ist eine produktionsreife, ultra-latenzoptimierte Lösung.

### Architektur-Übersicht & Latenz-Optimierung
1. **Audio Capture & Echounterdrückung:** Das Mikrofon nutzt Hardware-AEC (`echoCancellation: true`), damit sich die Sprache des Partners nicht im eigenen Mikrofon verfängt.
2. **Direkte Gemini-Verbindung:** Das Frontend verbindet sich per sicherem WebSocket **direkt mit Google** (`generativelanguage.googleapis.com`). Der API-Key bleibt lokal im Browser (`localStorage`) und wird niemals an deinen Server geschickt.
3. **Nahtloses Audio-Scheduling:** Statt einzelne Chunks abgehackt abzuspielen, berechnet eine **Audio-Queue** (`audioContext.currentTime`) den exakten Zeitstempel für jeden Chunk. Das eliminiert Knackgeräusche und Latenzlöcher.
4. **Zentraler Relay-Server (Node.js):** Dient nur als Vermittler (Signaling + Weiterleitung). Gemini-Audio wird als binärer Rohdatenstrom (PCM 24kHz) verlust- und verzögerungsfrei an den Partner im selben "Raum" durchgereicht.

---

### Teil 1: Der Relay-Server (`server.js`)
Unterstützt **Raumnamen (Rooms)**, damit mehrere Paare unabhängig voneinander übersetzen können.

Installiere die Abhängigkeiten:
```bash
npm init -y
npm install express ws
```

Erstelle `server.js`:
```javascript
const express = require('express');
const http = require('http');
const { WebSocketServer } = require('ws');
const path = require('path');

const app = express();
app.use(express.static(path.join(__dirname, 'public')));

const server = http.createServer(app);
const wss = new WebSocketServer({ server });

// Map: roomId -> Set von WebSocket-Clients
const rooms = new Map();

wss.on('connection', (ws, req) => {
    // Raum aus URL extrahieren: ws://host/ws?room=de-zh-123
    const url = new URL(req.url, `http://${req.headers.host}`);
    const roomId = url.searchParams.get('room') || 'default-room';

    if (!rooms.has(roomId)) {
        rooms.set(roomId, new Set());
    }
    const currentRoom = rooms.get(roomId);
    currentRoom.add(ws);

    console.log(`[Raum: ${roomId}] Nutzer beigetreten. Aktive Nutzer: ${currentRoom.size}`);

    ws.on('message', (data, isBinary) => {
        // Binäre Audiodaten sofort an den Partner im Raum weiterleiten
        currentRoom.forEach((client) => {
            if (client !== ws && client.readyState === 1) {
                client.send(data, { binary: isBinary });
            }
        });
    });

    ws.on('close', () => {
        currentRoom.delete(ws);
        console.log(`[Raum: ${roomId}] Nutzer verlassen. Verbleibend: ${currentRoom.size}`);
        if (currentRoom.size === 0) {
            rooms.delete(roomId);
        }
    });

    ws.on('error', (err) => console.error('WS Error:', err));
});

const PORT = process.env.PORT || 3000;
server.listen(PORT, '0.0.0.0', () => {
    console.log(`=======================================================`);
    console.log(`Relay-Server läuft auf: http://localhost:${PORT}`);
    console.log(`WICHTIG für Mobilgeräte: Für Mikrofon-Zugriff im Browser`);
    console.log(`muss die Seite über HTTPS oder 'localhost' geladen werden.`);
    console.log(`=======================================================`);
});
```

---

### Teil 2: Das Frontend (`public/index.html`)
Erstelle einen Ordner `public/` und speichere darin `index.html`. 

Die App nutzt den offiziellen Endpunkt der **Gemini Multimodal Live API** (`gemini-2.0-flash-exp`).

```html
<!DOCTYPE html>
<html lang="de">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
    <title>Live AI Voice Translator</title>
    <style>
        :root {
            --bg-color: #0f172a;
            --card-bg: #1e293b;
            --accent: #3b82f6;
            --accent-active: #22c55e;
            --danger: #ef4444;
            --text: #f8fafc;
            --text-dim: #94a3b8;
            --border: #334155;
        }

        * { box-sizing: border-box; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            margin: 0; padding: 20px;
            background-color: var(--bg-color);
            color: var(--text);
            display: flex; justify-content: center; align-items: center;
            min-height: 100vh;
        }

        .container {
            background: var(--card-bg);
            border: 1px solid var(--border);
            border-radius: 16px;
            width: 100%; max-width: 440px;
            padding: 24px;
            display: flex; flex-direction: column; gap: 18px;
            box-shadow: 0 10px 25px rgba(0, 0, 0, 0.4);
        }

        h2 { margin: 0; font-size: 1.4rem; text-align: center; }
        .sub { font-size: 0.85rem; color: var(--text-dim); text-align: center; margin-top: -10px; }

        .field-group { display: flex; flex-direction: column; gap: 6px; }
        label { font-size: 0.8rem; font-weight: 600; color: var(--text-dim); text-transform: uppercase; }

        input, select {
            width: 100%; padding: 12px;
            background: #0f172a; border: 1px solid var(--border);
            border-radius: 8px; color: var(--text); font-size: 0.95rem;
            outline: none; transition: border-color 0.2s;
        }
        input:focus, select:focus { border-color: var(--accent); }

        .btn-row { display: flex; gap: 10px; }
        button {
            flex: 1; padding: 14px; border: none; border-radius: 8px;
            font-size: 1rem; font-weight: bold; cursor: pointer;
            background: var(--accent); color: white;
            transition: opacity 0.2s, background 0.2s;
        }
        button:disabled { background: var(--border); color: var(--text-dim); cursor: not-allowed; }
        button.stop { background: var(--danger); }

        .status-box {
            padding: 12px; background: #0f172a;
            border: 1px solid var(--border); border-radius: 8px;
            font-size: 0.85rem; text-align: center; color: var(--text-dim);
            min-height: 42px; display: flex; align-items: center; justify-content: center;
        }

        .visualizer {
            display: flex; justify-content: center; align-items: center;
            gap: 6px; height: 30px;
        }
        .bar {
            width: 6px; height: 10px; background: var(--border);
            border-radius: 3px; transition: height 0.1s ease, background 0.2s;
        }
        .active-speak .bar { background: var(--accent-active); }
    </style>
</head>
<body>

<div class="container">
    <h2>AI Simultan-Dolmetscher</h2>
    <div class="sub">Gemini Live API (Edge Direct)</div>

    <div class="field-group">
        <label for="apiKey">Gemini API Key</label>
        <input type="password" id="apiKey" placeholder="AIzaSy..." />
    </div>

    <div class="field-group">
        <label for="roomName">Raum Name (beide Nutzer gleich)</label>
        <input type="text" id="roomName" value="transl-room-1" />
    </div>

    <div class="field-group">
        <label for="langPair">Sprachkonfiguration</label>
        <select id="langPair">
            <option value="de-to-zh">Ich spreche Deutsch ➔ Partner hört Chinesisch</option>
            <option value="zh-to-de">Ich spreche Chinesisch ➔ Partner hört Deutsch</option>
            <option value="en-to-zh">Ich spreche Englisch ➔ Partner hört Chinesisch</option>
            <option value="zh-to-en">Ich spreche Chinesisch ➔ Partner hört Englisch</option>
        </select>
    </div>

    <div class="visualizer" id="visualizer">
        <div class="bar"></div><div class="bar"></div><div class="bar"></div>
        <div class="bar"></div><div class="bar"></div>
    </div>

    <div class="status-box" id="status">Bereit. Bitte verbinden.</div>

    <div class="btn-row">
        <button id="connectBtn">Starten</button>
        <button id="disconnectBtn" class="stop" disabled>Trennen</button>
    </div>
</div>

<script>
    const apiKeyInput = document.getElementById('apiKey');
    const roomInput = document.getElementById('roomName');
    const langSelect = document.getElementById('langPair');
    const connectBtn = document.getElementById('connectBtn');
    const disconnectBtn = document.getElementById('disconnectBtn');
    const statusDiv = document.getElementById('status');
    const visualizer = document.getElementById('visualizer');

    let audioCtx = null;
    let micStream = null;
    let micProcessor = null;
    let geminiWs = null;
    let roomWs = null;

    // Audio-Jitter-Buffer / Scheduling Variablen
    let nextAudioStartTime = 0;
    const SAMPLE_RATE_IN = 16000;
    const SAMPLE_RATE_OUT = 24000;

    // 1. API Key aus LocalStorage laden
    window.addEventListener('DOMContentLoaded', () => {
        const savedKey = localStorage.getItem('gemini_live_key');
        if (savedKey) apiKeyInput.value = savedKey;
        const savedRoom = localStorage.getItem('gemini_live_room');
        if (savedRoom) roomInput.value = savedRoom;
    });

    // 2. Verbindungsaufbau (Mobile Gesture Unlock)
    connectBtn.addEventListener('click', async () => {
        const key = apiKeyInput.value.trim();
        const room = roomInput.value.trim();
        if (!key) return alert("Bitte API-Key eingeben!");
        if (!room) return alert("Bitte Raumnamen angeben!");

        localStorage.setItem('gemini_live_key', key);
        localStorage.setItem('gemini_live_room', room);

        setUIConnected(true);
        updateStatus("Initialisiere Audio-Engine...");

        try {
            // AudioContext erst bei Klick initialisieren (iOS-Pflicht!)
            audioCtx = new (window.AudioContext || window.webkitAudioContext)({
                latencyHint: 'interactive'
            });
            await audioCtx.resume();
            nextAudioStartTime = audioCtx.currentTime;

            // Verbinde Relay-Server (lokaler Host)
            connectRelayRoom(room);

            // Verbinde direkt zu Gemini Live
            connectGeminiLive(key, langSelect.value);

        } catch (err) {
            updateStatus("Fehler: " + err.message);
            cleanup();
        }
    });

    disconnectBtn.addEventListener('click', cleanup);

    // Relay WebSocket zum Partner
    function connectRelayRoom(room) {
        const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const url = `${protocol}//${window.location.host}/ws?room=${encodeURIComponent(room)}`;
        
        roomWs = new WebSocket(url);
        roomWs.binaryType = 'arraybuffer';

        roomWs.onopen = () => {
            console.log("Verbunden mit Relay Room:", room);
        };

        // Empfange übersetzte Audio-Pakete vom Partner
        roomWs.onmessage = (e) => {
            if (e.data instanceof ArrayBuffer) {
                const pcm16 = new Int16Array(e.data);
                playReceivedPcmChunk(pcm16);
            }
        };

        roomWs.onerror = () => updateStatus("Relay-Server Verbindung fehlgeschlagen.");
    }

    // Direkte Gemini Live API Verbindung
    function connectGeminiLive(apiKey, mode) {
        updateStatus("Verbinde direkt zu Google Gemini...");

        // Offizieller Bidirektionaler Live WebSocket Endpunkt
        const geminiUrl = `wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1alpha.GenerativeService.BidiGenerateContent?key=${apiKey}`;
        
        geminiWs = new WebSocket(geminiUrl);

        geminiWs.onopen = () => {
            updateStatus("Gemini verbunden. Konfiguriere Dolmetscher...");
            sendGeminiSetup(mode);
            startMicrophoneCapture();
        };

        geminiWs.onmessage = async (event) => {
            try {
                let data;
                if (event.data instanceof Blob) {
                    data = JSON.parse(await event.data.text());
                } else {
                    data = JSON.parse(event.data);
                }

                // Extrahiere Audiodaten von Gemini
                const parts = data.serverContent?.modelTurn?.parts;
                if (parts) {
                    for (const part of parts) {
                        if (part.inlineData && part.inlineData.data) {
                            // Base64 -> Raw PCM16 ArrayBuffer
                            const binary = atob(part.inlineData.data);
                            const buffer = new Uint8Array(binary.length);
                            for (let i = 0; i < binary.length; i++) {
                                buffer[i] = binary.charCodeAt(i);
                            }

                            // SOFORT an Relay Room Server weiterleiten (kein lokales Echo!)
                            if (roomWs && roomWs.readyState === WebSocket.OPEN) {
                                roomWs.send(buffer.buffer);
                            }
                        }
                    }
                }
            } catch (err) {
                console.error("Parse Error Gemini:", err);
            }
        };

        geminiWs.onclose = (e) => {
            console.warn("Gemini WS geschlossen:", e.code, e.reason);
            updateStatus("Gemini getrennt (" + (e.reason || "Timeout") + ")");
            cleanup();
        };

        geminiWs.onerror = (err) => {
            console.error("Gemini Error:", err);
            updateStatus("Gemini Fehler. Prüfe deinen API-Key!");
        };
    }

    // System-Instruktionen für strikte Übersetzung
    function sendGeminiSetup(mode) {
        let instruction = "";
        let voiceName = "Aoede"; // Optionen: Aoede, Puck, Charon, Fenrir, Kore

        if (mode === "de-to-zh") {
            instruction = "You are an ultra-low latency simultaneous speech-to-speech interpreter. " +
                          "Listen to the German audio. Instantly translate and speak it in Chinese (Mandarin). " +
                          "CRITICAL: Produce ONLY the direct translation audio. Do NOT talk back, converse, explain, or add comments.";
            voiceName = "Aoede";
        } else if (mode === "zh-to-de") {
            instruction = "You are an ultra-low latency simultaneous speech-to-speech interpreter. " +
                          "Listen to the Chinese audio. Instantly translate and speak it in German. " +
                          "CRITICAL: Produce ONLY the direct translation audio. Do NOT talk back, converse, explain, or add comments.";
            voiceName = "Puck";
        } else if (mode === "en-to-zh") {
            instruction = "Simultaneous interpreter English to Mandarin Chinese. Output ONLY translated speech.";
        } else if (mode === "zh-to-en") {
            instruction = "Simultaneous interpreter Mandarin Chinese to English. Output ONLY translated speech.";
        }

        const setupMsg = {
            setup: {
                model: "models/gemini-2.0-flash-exp",
                generationConfig: {
                    responseModalities: ["AUDIO"],
                    speechConfig: {
                        voiceConfig: {
                            prebuiltVoiceConfig: { voiceName: voiceName }
                        }
                    }
                },
                systemInstruction: {
                    parts: [{ text: instruction }]
                }
            }
        };

        geminiWs.send(JSON.stringify(setupMsg));
    }

    // Mikrofon aufnehmen & 16kHz Resampling
    async function startMicrophoneCapture() {
        try {
            // Echo Cancellation ist extrem wichtig gegen Feedbackschleifen!
            micStream = await navigator.mediaDevices.getUserMedia({
                audio: {
                    channelCount: 1,
                    echoCancellation: true,
                    noiseSuppression: true,
                    autoGainControl: true
                }
            });

            const source = audioCtx.createMediaStreamSource(micStream);
            
            // Buffer-Size 2048 = ~43ms Latenz bei 48kHz
            micProcessor = audioCtx.createScriptProcessor(2048, 1, 1);

            source.connect(micProcessor);
            micProcessor.connect(audioCtx.destination); // Notwendig für Chrome/Safari Event-Loop

            micProcessor.onaudioprocess = (e) => {
                if (!geminiWs || geminiWs.readyState !== WebSocket.OPEN) return;

                const inputFloat32 = e.inputBuffer.getChannelData(0);
                
                // Pegel-Visualizer updaten
                updateVisualizer(inputFloat32);

                // Konvertieren von nativer SampleRate (z.B. 48k/44.1k) auf 16kHz PCM16
                const pcm16 = downsampleTo16k(inputFloat32, audioCtx.sampleRate);

                // Base64 encoden für Gemini Input
                const base64Audio = arrayBufferToBase64(pcm16.buffer);

                const realtimePayload = {
                    realtimeInput: {
                        mediaChunks: [{
                            mimeType: "audio/pcm;rate=16000",
                            data: base64Audio
                        }]
                    }
                };

                geminiWs.send(JSON.stringify(realtimePayload));
            };

            updateStatus("Live! Sprechen Sie einfach drauflos...");
        } catch (err) {
            updateStatus("Mikrofon-Fehler: " + err.message);
            cleanup();
        }
    }

    // Resampling-Algorithmus (Lineare Interpolation)
    function downsampleTo16k(buffer, fromRate) {
        if (fromRate === SAMPLE_RATE_IN) {
            const out = new Int16Array(buffer.length);
            for (let i = 0; i < buffer.length; i++) {
                let s = Math.max(-1, Math.min(1, buffer[i]));
                out[i] = s < 0 ? s * 0x8000 : s * 0x7FFF;
            }
            return out;
        }

        const ratio = fromRate / SAMPLE_RATE_IN;
        const newLength = Math.round(buffer.length / ratio);
        const result = new Int16Array(newLength);

        for (let i = 0; i < newLength; i++) {
            const originIndex = i * ratio;
            const leftIndex = Math.floor(originIndex);
            const rightIndex = Math.min(leftIndex + 1, buffer.length - 1);
            const weight = originIndex - leftIndex;

            // Lineare Interpolation
            const interpolated = buffer[leftIndex] * (1 - weight) + buffer[rightIndex] * weight;
            const clamped = Math.max(-1, Math.min(1, interpolated));
            result[i] = clamped < 0 ? clamped * 0x8000 : clamped * 0x7FFF;
        }
        return result;
    }

    // Audio-Queue für knisterfreies Abspielen empfangener Daten
    function playReceivedPcmChunk(pcm16Data) {
        if (!audioCtx) return;

        // PCM16 Int16 zu WebAudio Float32 [-1.0, 1.0] konvertieren
        const float32 = new Float32Array(pcm16Data.length);
        for (let i = 0; i < pcm16Data.length; i++) {
            float32[i] = pcm16Data[i] / 32768.0;
        }

        // Gemini gibt Audio immer in 24.000 Hz aus
        const audioBuffer = audioCtx.createBuffer(1, float32.length, SAMPLE_RATE_OUT);
        audioBuffer.getChannelData(0).set(float32);

        const node = audioCtx.createBufferSource();
        node.buffer = audioBuffer;
        node.connect(audioCtx.destination);

        // Gapless Jitter Queueing
        const currentTime = audioCtx.currentTime;
        if (nextAudioStartTime < currentTime) {
            nextAudioStartTime = currentTime + 0.02; // 20ms Puffer gegen Jitter
        }

        node.start(nextAudioStartTime);
        nextAudioStartTime += audioBuffer.duration;
    }

    function arrayBufferToBase64(buffer) {
        let binary = '';
        const bytes = new Uint8Array(buffer);
        const len = bytes.byteLength;
        for (let i = 0; i < len; i++) {
            binary += String.fromCharCode(bytes[i]);
        }
        return btoa(binary);
    }

    function updateVisualizer(float32Data) {
        let sum = 0;
        for (let i = 0; i < float32Data.length; i += 16) {
            sum += Math.abs(float32Data[i]);
        }
        const rms = sum / (float32Data.length / 16);
        const bars = visualizer.children;
        const active = rms > 0.02;

        if (active) visualizer.classList.add('active-speak');
        else visualizer.classList.remove('active-speak');

        for (let i = 0; i < bars.length; i++) {
            const h = active ? Math.min(28, Math.max(6, rms * 150 * (i + 1))) : 6;
            bars[i].style.height = `${h}px`;
        }
    }

    function updateStatus(txt) {
        statusDiv.innerText = txt;
    }

    function setUIConnected(connected) {
        connectBtn.disabled = connected;
        disconnectBtn.disabled = !connected;
        apiKeyInput.disabled = connected;
        roomInput.disabled = connected;
        langSelect.disabled = connected;
    }

    function cleanup() {
        setUIConnected(false);
        updateStatus("Getrennt.");
        visualizer.classList.remove('active-speak');

        if (micProcessor) {
            micProcessor.disconnect();
            micProcessor = null;
        }
        if (micStream) {
            micStream.getTracks().forEach(t => t.stop());
            micStream = null;
        }
        if (geminiWs) {
            geminiWs.onclose = null;
            geminiWs.close();
            geminiWs = null;
        }
        if (roomWs) {
            roomWs.close();
            roomWs = null;
        }
        if (audioCtx && audioCtx.state !== 'closed') {
            audioCtx.close();
            audioCtx = null;
        }
    }
</script>
</body>
</html>
```

---

### Schritt-für-Schritt Anleitung zum Starten

#### 1. Starten
```bash
node server.js
```

#### 2. Wichtiger Hinweis für Handys (iPhone / Android)
Moderne Browser erlauben den Zugriff auf das Mikrofon **nur über sichere Kontexte (HTTPS)** oder `localhost`.
* Wenn du die App über das lokale WLAN am Smartphone testen willst (z. B. `http://192.168.1.50:3000`), blockiert Chrome & Safari das Mikrofon!
* **Die einfachste Lösung zum Testen:** Nutze ein kostenloses Tool wie [ngrok](https://ngrok.com) oder Cloudflare Tunnels:
  ```bash
  npx ngrok http 3000
  ```
  Öffne die ausgegebene `https://xxxx.ngrok-free.app` URL auf beiden Smartphones.

#### 3. Anwendung nutzen
1. **Nutzer A (Deutschland, z. B. auf PC):**
   * Fügt seinen Gemini API-Key ein.
   * Lässt Raumname: `transl-room-1`.
   * Wählt: *"Ich spreche Deutsch ➔ Partner hört Chinesisch"*.
   * Klickt auf **Starten**.

2. **Nutzer B (China, z. B. auf iPhone):**
   * Fügt seinen eigenen Gemini API-Key ein.
   * Schreibt exakt denselben Raumnamen: `transl-room-1`.
   * Wählt: *"Ich spreche Chinesisch ➔ Partner hört Deutsch"*.
   * Klickt auf **Starten**.

Sobald Nutzer A Deutsch spricht, schickt sein Browser das Audio direkt zu Google, erhält chinesisches Audio zurück, leitet es in Millisekunden über den Relay-Server an Nutzer B weiter, und Nutzer B hört es flüssig auf Chinesisch. Umgekehrt funktioniert es synchron genauso.
