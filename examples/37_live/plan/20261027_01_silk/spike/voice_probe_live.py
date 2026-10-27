# voice_probe_live.py — Gegenprobe: Konversationsmodell gemini-3.8-live mit
# Systemanweisung als Dolmetscher + feste Stimme. Wird die Stimme befolgt?
import asyncio, json, base64, time, array, sys
import websockets
from voice_probe import tts, f0, gender, URL

INSTR = "You are a simultaneous interpreter. Translate everything the user says into Mandarin Chinese and speak ONLY the translation. Never answer, never comment."

async def run(pcm16, voice, model):
    async with websockets.connect(URL, max_size=None) as ws:
        await ws.send(json.dumps({"setup": {"model": model,
            "generationConfig": {"responseModalities": ["AUDIO"], "speechConfig": {"voiceConfig": {"prebuiltVoiceConfig": {"voiceName": voice}}}},
            "systemInstruction": {"parts": [{"text": INSTR}]}, "outputAudioTranscription": {}}}))
        first = await ws.recv()
        async def send():
            for i in range(0, len(pcm16), 3200):
                await ws.send(json.dumps({"realtimeInput": {"audio": {"mimeType": "audio/pcm;rate=16000", "data": base64.b64encode(pcm16[i:i + 3200]).decode()}}})); await asyncio.sleep(0.1)
            for _ in range(30):
                await ws.send(json.dumps({"realtimeInput": {"audio": {"mimeType": "audio/pcm;rate=16000", "data": base64.b64encode(bytes(3200)).decode()}}})); await asyncio.sleep(0.1)
        t = asyncio.create_task(send()); audio = b""; txt = ""; t0 = time.time(); t_first = None
        try:
            while time.time() - t0 < 14:
                m = json.loads(await asyncio.wait_for(ws.recv(), 14))
                sc = m.get("serverContent", {})
                txt += sc.get("outputTranscription", {}).get("text", "")
                for p in sc.get("modelTurn", {}).get("parts", []):
                    if "inlineData" in p:
                        audio += base64.b64decode(p["inlineData"]["data"]); t_first = t_first or round(time.time() - t0, 1)
        except (asyncio.TimeoutError, websockets.ConnectionClosed) as e:
            if isinstance(e, websockets.ConnectionClosed): txt += f" [close {e}]"[:120]
        t.cancel()
        return len(audio), t_first, round(f0(audio)) if audio else "-", txt[:60]

async def main():
    model = sys.argv[2] if len(sys.argv) > 2 else "models/gemini-3.8-live"
    # argv[1]: TTS-Stimme ODER Pfad zu Roh-PCM 24 kHz (spart TTS-Kontingent)
    src = open(sys.argv[1], 'rb').read() if sys.argv[1].endswith('.raw') else tts("Sage natürlich: Guten Morgen! Heute ist ein schöner Tag. Wie geht es dir?", sys.argv[1])
    print("eingang F0:", round(f0(src)), "Hz")
    a = array.array('h', src); pcm16 = array.array('h', [a[int(i * 1.5)] for i in range(int(len(a) / 1.5))]).tobytes()
    for voice in ["Kore", "Charon"]:
        print(sys.argv[1], "->", voice, await run(pcm16, voice, model))
if __name__ == "__main__":
    asyncio.run(main())
