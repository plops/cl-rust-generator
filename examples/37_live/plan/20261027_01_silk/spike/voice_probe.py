# voice_probe.py — akzeptiert gemini-3.5-live-translate-preview eine feste
# Stimme (speechConfig.prebuiltVoiceConfig)? Misst die Grundfrequenz (F0)
# der Ausgabe: Maenner ~85-155 Hz, Frauen ~165-255 Hz.
import asyncio, json, base64, time, urllib.request, array, sys
import numpy as np, websockets
KEY = open('/workspace/src/cl-rust-generator/api_key.txt').read().strip()
URL = "wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1beta.GenerativeService.BidiGenerateContent?key=" + KEY

def tts(text, voice):
    body = json.dumps({"contents": [{"parts": [{"text": text}]}], "generationConfig": {"responseModalities": ["AUDIO"], "speechConfig": {"voiceConfig": {"prebuiltVoiceConfig": {"voiceName": voice}}}}}).encode()
    r = urllib.request.urlopen(urllib.request.Request("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-tts:generateContent?key=" + KEY, body, {"Content-Type": "application/json"}), timeout=60)
    return base64.b64decode(json.load(r)["candidates"][0]["content"]["parts"][0]["inlineData"]["data"])

def f0(pcm, rate=24000):
    x = np.frombuffer(pcm, dtype=np.int16).astype(np.float32)
    out = []
    for i in range(0, len(x) - 1200, 1200):
        w = x[i:i + 1200]
        if np.sqrt(np.mean(w ** 2)) < 500: continue
        w = w - w.mean(); c = np.correlate(w, w, 'full')[len(w) - 1:]
        lo, hi = rate // 400, rate // 60
        lag = lo + np.argmax(c[lo:hi])
        if c[lag] > 0.3 * c[0]: out.append(rate / lag)
    return float(np.median(out)) if out else 0.0

def gender(pcm):
    # Zweitmeinung: Gemini hoert die Ausgabe und urteilt "male"/"female".
    import io, wave
    b = io.BytesIO(); w = wave.open(b, 'wb'); w.setnchannels(1); w.setsampwidth(2); w.setframerate(24000); w.writeframes(pcm[:24000 * 2 * 8]); w.close()
    body = json.dumps({"contents": [{"parts": [{"text": "Is the speaker in this audio male or female? Answer with exactly one word: male or female."}, {"inlineData": {"mimeType": "audio/wav", "data": base64.b64encode(b.getvalue()).decode()}}]}]}).encode()
    r = urllib.request.urlopen(urllib.request.Request("https://generativelanguage.googleapis.com/v1beta/models/gemini-flash-latest:generateContent?key=" + KEY, body, {"Content-Type": "application/json"}), timeout=60)
    return json.load(r)["candidates"][0]["content"]["parts"][0]["text"].strip().lower()

async def run(pcm16, target, voice):
    gc = {"responseModalities": ["AUDIO"], "translationConfig": {"targetLanguageCode": target}}
    if voice: gc["speechConfig"] = {"voiceConfig": {"prebuiltVoiceConfig": {"voiceName": voice}}}
    async with websockets.connect(URL, max_size=None) as ws:
        await ws.send(json.dumps({"setup": {"model": "models/gemini-3.5-live-translate-preview", "generationConfig": gc}}))
        first = await ws.recv()
        async def send():
            for i in range(0, len(pcm16), 3200):
                await ws.send(json.dumps({"realtimeInput": {"audio": {"mimeType": "audio/pcm;rate=16000", "data": base64.b64encode(pcm16[i:i + 3200]).decode()}}})); await asyncio.sleep(0.1)
            for _ in range(30):
                await ws.send(json.dumps({"realtimeInput": {"audio": {"mimeType": "audio/pcm;rate=16000", "data": base64.b64encode(bytes(3200)).decode()}}})); await asyncio.sleep(0.1)
        t = asyncio.create_task(send()); audio = b""; t0 = time.time()
        try:
            while time.time() - t0 < 12:
                m = json.loads(await asyncio.wait_for(ws.recv(), 12))
                for p in m.get("serverContent", {}).get("modelTurn", {}).get("parts", []):
                    if "inlineData" in p: audio += base64.b64decode(p["inlineData"]["data"])
        except (asyncio.TimeoutError, websockets.ConnectionClosed) as e: pass
        t.cancel()
        return len(audio), round(f0(audio)), gender(audio)

async def main():
    src = tts("Sage natürlich: Guten Morgen! Heute ist ein schöner Tag. Wie geht es dir?", sys.argv[1] if len(sys.argv) > 1 else "Charon")
    a = array.array('h', src); pcm16 = array.array('h', [a[int(i * 1.5)] for i in range(int(len(a) / 1.5))]).tobytes()
    print("eingang F0", sys.argv[1:], round(f0(src)), "Hz")
    for voice in [None, "Kore", "Aoede", "Puck", "Charon"]:
        print(voice, await run(pcm16, "zh-Hans", voice))
if __name__ == "__main__":
    asyncio.run(main())
