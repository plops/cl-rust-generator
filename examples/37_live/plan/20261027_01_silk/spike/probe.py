import asyncio, json, base64, os, sys, urllib.request, websockets, time
KEY=open('/workspace/src/cl-rust-generator/api_key.txt').read().strip()
def tts(text):
    body=json.dumps({"contents":[{"parts":[{"text":text}]}],"generationConfig":{"responseModalities":["AUDIO"],"speechConfig":{"voiceConfig":{"prebuiltVoiceConfig":{"voiceName":"Charon"}}}}}).encode()
    r=urllib.request.urlopen(urllib.request.Request("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-tts:generateContent?key="+KEY,body,{"Content-Type":"application/json"}),timeout=60)
    j=json.load(r); return base64.b64decode(j["candidates"][0]["content"]["parts"][0]["inlineData"]["data"])
async def main():
    pcm24=tts("Sage natürlich: Guten Morgen! Heute ist ein schöner Tag. Wie geht es dir?")
    open('/tmp/de24.raw','wb').write(pcm24)
    import array
    a=array.array('h',pcm24); b=array.array('h',[a[int(i*1.5)] for i in range(int(len(a)/1.5))])
    pcm16=b.tobytes()
    url="wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1beta.GenerativeService.BidiGenerateContent?key="+KEY
    async with websockets.connect(url,max_size=None) as ws:
        await ws.send(json.dumps({"setup":{"model":"models/gemini-3.5-live-translate-preview","generationConfig":{"responseModalities":["AUDIO"],"translationConfig":{"targetLanguageCode":"zh-Hans"}},"inputAudioTranscription":{},"outputAudioTranscription":{}}}))
        print("setup reply:", (await ws.recv())[:200])
        async def sender():
            for i in range(0,len(pcm16),3200):
                await ws.send(json.dumps({"realtimeInput":{"audio":{"mimeType":"audio/pcm;rate=16000","data":base64.b64encode(pcm16[i:i+3200]).decode()}}}))
                await asyncio.sleep(0.1)
            sil=bytes(3200)
            for i in range(30):
                await ws.send(json.dumps({"realtimeInput":{"audio":{"mimeType":"audio/pcm;rate=16000","data":base64.b64encode(sil).decode()}}})); await asyncio.sleep(0.1)
        t=asyncio.create_task(sender()); audio=0; tin=[]; tout=[]
        t0=time.time()
        try:
            while time.time()-t0<14:
                m=json.loads(await asyncio.wait_for(ws.recv(),timeout=14))
                sc=m.get("serverContent",{})
                if "inputTranscription" in sc: tin.append(sc["inputTranscription"].get("text",""))
                if "outputTranscription" in sc: tout.append(sc["outputTranscription"].get("text",""))
                for p in sc.get("modelTurn",{}).get("parts",[]):
                    if "inlineData" in p: audio+=len(base64.b64decode(p["inlineData"]["data"])); 
                    if audio and 'first' not in locals(): first=time.time()-t0; print("first audio at",round(first,2))
                if not sc and "setupComplete" not in m: print("other:",str(m)[:200])
        except asyncio.TimeoutError: pass
        print("audio bytes",audio,"in:","".join(tin),"out:","".join(tout))
asyncio.run(main())
