use opus_pure::{Application, Bandwidth, OpusDecoder, OpusEncoder, Signal, packet};
fn main() {
    for (rate, frame_ms) in [(24000, 60), (16000, 60), (16000, 20)] {
        let mut enc = OpusEncoder::new(rate, 1, Application::Voip).unwrap();
        enc.bitrate_bps = 12000;
        enc.max_bandwidth = Bandwidth::Wideband;
        enc.signal_type = Some(Signal::Voice);
        let n = (rate as usize) * frame_ms / 1000;
        let mut dec = OpusDecoder::new(16000, 1).unwrap();
        let mut total = 0; let mut modes = std::collections::HashMap::new();
        let mut out = vec![0u8; 1500]; let mut pcm = vec![0f32; 5760];
        let frames = 3000 / frame_ms; // 3 s
        let t0 = std::time::Instant::now();
        for f in 0..frames {
            let input: Vec<f32> = (0..n).map(|i| { let t = (f*n+i) as f32 / rate as f32; 0.3*(2.0*3.14159*220.0*t).sin()*(1.0+ (2.0*3.14159*3.0*t).sin())*0.5 }).collect();
            let len = enc.encode(&input, n, &mut out).unwrap();
            total += len;
            *modes.entry(format!("{:?}", packet::mode(&out[..len]).unwrap())).or_insert(0) += 1;
            let got = dec.decode(&out[..len], 5760, &mut pcm).unwrap();
            if f == 0 { println!("decoded {got} samples @16k"); }
        }
        println!("rate={rate} frame={frame_ms}ms bytes={total} kbit/s={:.1} modes={modes:?} enc+dec {:?}", total as f64*8.0/3000.0, t0.elapsed());
    }
}
