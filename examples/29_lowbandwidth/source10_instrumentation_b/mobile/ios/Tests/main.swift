// Linux-Prüfling für Sources/LbwCore.swift (ohne UIKit): verbindet direkt
// mit einem lbw-server (Port = Argument), pollt bis Bild + Text da sind,
// prüft den Text-Parser und schickt Eingaben. Gebaut/gestartet von
// mobile/core/tests/c_header.rs (falls swiftc vorhanden). Exit 0 = OK.
import Foundation

let port = CommandLine.arguments.count > 1 ? CommandLine.arguments[1] : "7878"
guard LbwCore(json: "kaputt") == nil else { print("FEHLER: kaputte Konfig akzeptiert"); exit(1) }
guard let core = LbwCore(json: "{\"direct\":\"127.0.0.1:\(port)\"}") else { print("FEHLER: init"); exit(1) }
var up = false, frame = false, text = ""
for _ in 0..<500 where !(up && frame && !text.isEmpty) {
    let f = core.poll()
    up = up || f & Int32(LBW_UP) != 0
    frame = frame || (f & Int32(LBW_FRAME) != 0 && core.canvas[0] > 150)
    if let t = core.texts.first { text = "\(t.text)@\(t.x),\(t.y) \(t.w)x\(t.h)" }
    usleep(20_000)
}
core.text("s\n")
core.key("Esc")
core.mouse(5, 6)
core.click()
usleep(300_000)
print("swift: up=\(up) frame=\(frame) text=\(text) status=\(core.status)")
exit(up && frame && !text.isEmpty ? 0 : 1)
