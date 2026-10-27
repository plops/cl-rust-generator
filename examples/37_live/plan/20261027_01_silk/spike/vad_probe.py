# vad_probe.py — gemini-3.8-live als Dolmetscher bei DURCHGEHENDER Sprache:
# Wie viel uebersetztes Audio kommt an, wie frueh, mit welchen
# realtimeInputConfig-Varianten? Eingang: 30 s Mandarin (mic-zh.wav).
import asyncio, json, base64, time, sys, subprocess
import websockets
from voice_probe import URL, f0

import os
INSTR_SIMPLE = ("You are a simultaneous interpreter. The user speaks Mandarin Chinese. "
         "Translate everything the user says into German and speak ONLY the German translation. "
         "Never answer, never comment, never ask questions.")
# Wie src/05_gemini.rs live_instruction("zh-Hans","de") (Stand 2026-10-27)
INSTR_SERVER = ("You are a simultaneous interpreter. The user speaks Simplified Chinese. "
         "Translate everything the user says from Simplified Chinese into German "
         "and speak ONLY the German translation. Never answer, never "
         "comment, never ask questions. Never say or write placeholder "
         "text such as 'translation pending', 'no speech' or similar. If "
         "you hear only silence or noise, stay silent and output nothing.")
INSTR = INSTR_SERVER if os.environ.get("INSTR") == "server" else INSTR_SIMPLE
VOICE = os.environ.get("VOICE", "Charon")
WAV = os.environ.get("WAV", "/tmp/live-hil/mic-zh.wav")
SECS = 30
SEG = float(sys.argv.pop(1)) if len(sys.argv) > 2 and sys.argv[1].replace(".", "").isdigit() else 3.0
pcm16 = subprocess.run(["ffmpeg", "-v", "error", "-i", WAV, "-t", str(SECS),
                        "-ac", "1", "-ar", "16000", "-f", "s16le", "-"], capture_output=True, check=True).stdout

VARIANTS = {
    "translate": "translate",   # Vergleich: Uebersetzungsmodell
    "default": None,
    "no_interrupt": {"activityHandling": "NO_INTERRUPTION"},
    # Eigene Segmentierung: VAD aus, alle SEG s activityEnd/activityStart.
    "manual3s": {"automaticActivityDetection": {"disabled": True}, "activityHandling": "NO_INTERRUPTION"},
    "no_interrupt+fast_end": {"activityHandling": "NO_INTERRUPTION",
        "automaticActivityDetection": {"endOfSpeechSensitivity": "END_SENSITIVITY_HIGH", "silenceDurationMs": 300}},
}

async def run(name, ric):
    setup = {"model": "models/gemini-3.8-live",
             "generationConfig": {"responseModalities": ["AUDIO"],
                                  "speechConfig": {"voiceConfig": {"prebuiltVoiceConfig": {"voiceName": VOICE}}}},
             "systemInstruction": {"parts": [{"text": INSTR}]},
             "outputAudioTranscription": {}}
    if os.environ.get("INTRANS"): setup["inputAudioTranscription"] = {}
    if ric == "translate":
        setup = {"model": "models/gemini-3.5-live-translate-preview",
                 "generationConfig": {"responseModalities": ["AUDIO"], "translationConfig": {"targetLanguageCode": "de"}},
                 "outputAudioTranscription": {}}
    elif ric:
        setup["realtimeInputConfig"] = ric
    async with websockets.connect(URL, max_size=None) as ws:
        await ws.send(json.dumps({"setup": setup}))
        first = (await ws.recv())[:40]
        t0 = time.time()
        manual = bool(ric) and ric != "translate" and ric.get("automaticActivityDetection", {}).get("disabled")
        async def send():
            if manual: await ws.send(json.dumps({"realtimeInput": {"activityStart": {}}}))
            for i in range(0, len(pcm16), 3200):
                if manual and i and i % int(3200 * 10 * SEG) == 0:  # alle SEG s Segment schliessen
                    await ws.send(json.dumps({"realtimeInput": {"activityEnd": {}}}))
                    await ws.send(json.dumps({"realtimeInput": {"activityStart": {}}}))
                await ws.send(json.dumps({"realtimeInput": {"audio": {"mimeType": "audio/pcm;rate=16000", "data": base64.b64encode(pcm16[i:i + 3200]).decode()}}}))
                await asyncio.sleep(0.1)
            if manual: await ws.send(json.dumps({"realtimeInput": {"activityEnd": {}}}))
            for _ in range(50):  # 5 s Stille am Ende
                await ws.send(json.dumps({"realtimeInput": {"audio": {"mimeType": "audio/pcm;rate=16000", "data": base64.b64encode(bytes(3200)).decode()}}}))
                await asyncio.sleep(0.1)
        task = asyncio.create_task(send())
        audio = b""; first_at = None; txt = ""; interrupted = 0
        try:
            while time.time() - t0 < SECS + 8:
                m = json.loads(await asyncio.wait_for(ws.recv(), SECS + 8))
                sc = m.get("serverContent", {})
                interrupted += 1 if sc.get("interrupted") else 0
                txt += sc.get("outputTranscription", {}).get("text", "")
                for p in sc.get("modelTurn", {}).get("parts", []):
                    if "inlineData" in p:
                        audio += base64.b64decode(p["inlineData"]["data"])
                        first_at = first_at or round(time.time() - t0, 1)
        except asyncio.TimeoutError:
            pass
        except websockets.ConnectionClosed as e:
            txt += f" [close {e.code} {e.reason[:80]}]"
        task.cancel()
        print(f"{name:24s} setup={first!r} audio={len(audio)/48000:.1f}s erstes={first_at}s unterbrochen={interrupted} F0={round(f0(audio)) if audio else '-'}")
        print("   text:", txt[:220].replace("\n", " "))

async def main():
    for name in (sys.argv[1:] or VARIANTS):
        await run(name, VARIANTS[name])

if __name__ == "__main__":
    asyncio.run(main())
