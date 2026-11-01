// LbwCore.swift — Swift-Hülle um die C-ABI (mobile/core/include/lbw_core.h).
//
// Eine Instanz = eine Verbindung (SSH-Kette + Protokoll v3 laufen in Rust).
// Nur vom Main-Thread benutzen; `poll()` im Bildtakt (CADisplayLink).

import Foundation

struct RemoteText {
    let id: UInt64
    let x: Int, y: Int, w: Int, h: Int
    let fg: (UInt8, UInt8, UInt8)
    let bg: (UInt8, UInt8, UInt8)
    let text: String
}

final class LbwCore {
    static let width = Int(LBW_WIDTH)
    static let height = Int(LBW_HEIGHT)

    private var h: OpaquePointer?
    /// RGBA8-Canvas 1280×720, gültig nach einem FRAME-Bit.
    private(set) var canvas = [UInt8](repeating: 0, count: LbwCore.width * LbwCore.height * 4)

    init?(json: String) {
        h = json.withCString { lbw_new($0) }
        if h == nil { return nil }
    }

    deinit { lbw_free(h) }

    /// Bits: LBW_FRAME | LBW_TEXT | LBW_UP.
    func poll() -> Int32 {
        canvas.withUnsafeMutableBufferPointer { lbw_poll(h, $0.baseAddress, $0.count) }
    }

    private func read(_ f: (OpaquePointer?, UnsafeMutablePointer<UInt8>?, Int) -> Int) -> [UInt8] {
        let n = f(h, nil, 0)
        var b = [UInt8](repeating: 0, count: n)
        _ = b.withUnsafeMutableBufferPointer { f(h, $0.baseAddress, n) }
        return b
    }

    var status: String { String(decoding: read(lbw_status), as: UTF8.self) }

    /// Hostkey-Fingerprints pro Hop (TOFU), leer bis die Kette einmal stand.
    var pins: [String] {
        String(decoding: read(lbw_pins), as: UTF8.self).split(separator: "\n").map(String.init)
    }

    /// Texte aus dem Blob (u32 n | je: u64 id, u16 x y w h, u8 fg[3] bg[3], u16 len, UTF-8).
    var texts: [RemoteText] {
        let b = read(lbw_texts)
        var o = 0
        func u16() -> Int { defer { o += 2 }; return Int(b[o]) | Int(b[o + 1]) << 8 }
        func u8() -> UInt8 { defer { o += 1 }; return b[o] }
        guard b.count >= 4 else { return [] }
        let n = Int(b[0]) | Int(b[1]) << 8 | Int(b[2]) << 16 | Int(b[3]) << 24
        o = 4
        var out: [RemoteText] = []
        for _ in 0..<n {
            guard o + 24 <= b.count else { break } // 8 id + 8 Box + 6 Farben + 2 Länge
            var id: UInt64 = 0
            for i in 0..<8 { id |= UInt64(b[o + i]) << (8 * UInt64(i)) }
            o += 8
            let (x, y, w, hh) = (u16(), u16(), u16(), u16())
            let fg = (u8(), u8(), u8()), bg = (u8(), u8(), u8())
            let len = u16()
            guard o + len <= b.count else { break }
            let s = String(decoding: b[o..<o + len], as: UTF8.self)
            o += len
            out.append(RemoteText(id: id, x: x, y: y, w: w, h: hh, fg: fg, bg: bg, text: s))
        }
        return out
    }

    func key(_ name: String) { name.withCString { lbw_key(h, $0) } }
    func text(_ s: String) { s.withCString { lbw_text(h, $0) } }
    func mouse(_ x: Int, _ y: Int) { lbw_mouse(h, Int32(x), Int32(y)) }
    func button(_ b: Int32, _ down: Bool) { lbw_button(h, b, down) }
    func click(_ b: Int32 = 1) { button(b, true); button(b, false) }
    func wheel(_ dy: Int32) { lbw_wheel(h, dy) }
}
