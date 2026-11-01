// ScreenView.swift — entfernter Bildschirm: Canvas + Texte, Touch, Tastatur.
//
// Touch: Tippen = Linksklick an der Stelle, Ziehen = Maus bewegen,
// langes Drücken = Rechtsklick, zwei Finger ziehen = Bild auf/ab.
// Die Softwaretastatur (UIKeyInput) schickt Zeichen; Löschen = Backspace.

import SwiftUI
import UIKit

final class RemoteView: UIView, UIKeyInput {
    var core: LbwCore?
    var onUp: (() -> Void)?
    private var image: CGImage?
    private var texts: [RemoteText] = []
    private var link: CADisplayLink?
    private var wasUp = false
    private var panAccum: CGFloat = 0

    override init(frame: CGRect) {
        super.init(frame: frame)
        backgroundColor = .black
        isMultipleTouchEnabled = true
        addGestureRecognizer(UITapGestureRecognizer(target: self, action: #selector(tap(_:))))
        let pan = UIPanGestureRecognizer(target: self, action: #selector(pan(_:)))
        pan.maximumNumberOfTouches = 1
        addGestureRecognizer(pan)
        let scroll = UIPanGestureRecognizer(target: self, action: #selector(scroll(_:)))
        scroll.minimumNumberOfTouches = 2
        addGestureRecognizer(scroll)
        addGestureRecognizer(UILongPressGestureRecognizer(target: self, action: #selector(longPress(_:))))
    }

    required init?(coder: NSCoder) { fatalError("nicht benutzt") }

    override func didMoveToWindow() {
        super.didMoveToWindow()
        link?.invalidate()
        link = nil
        if window != nil {
            let l = CADisplayLink(target: self, selector: #selector(tick))
            l.preferredFramesPerSecond = 30
            l.add(to: .main, forMode: .common)
            link = l
        }
    }

    @objc private func tick() {
        guard let core else { return }
        let f = core.poll()
        if f & Int32(LBW_FRAME) != 0 { image = makeImage(core.canvas) }
        if f & Int32(LBW_TEXT) != 0 { texts = core.texts }
        let up = f & Int32(LBW_UP) != 0
        if up && !wasUp { onUp?() }
        wasUp = up
        if f & (Int32(LBW_FRAME) | Int32(LBW_TEXT)) != 0 { setNeedsDisplay() }
    }

    private func makeImage(_ rgba: [UInt8]) -> CGImage? {
        let (w, h) = (LbwCore.width, LbwCore.height)
        guard let p = CGDataProvider(data: Data(rgba) as CFData) else { return nil }
        return CGImage(width: w, height: h, bitsPerComponent: 8, bitsPerPixel: 32, bytesPerRow: w * 4,
                       space: CGColorSpaceCreateDeviceRGB(),
                       bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.noneSkipLast.rawValue),
                       provider: p, decode: nil, shouldInterpolate: true, intent: .defaultIntent)
    }

    /// Server-Pixel ↔ Ansicht (Seitenverhältnis erhalten, zentriert).
    private var fit: (scale: CGFloat, ox: CGFloat, oy: CGFloat) {
        let s = min(bounds.width / CGFloat(LbwCore.width), bounds.height / CGFloat(LbwCore.height))
        return (s, (bounds.width - CGFloat(LbwCore.width) * s) / 2, (bounds.height - CGFloat(LbwCore.height) * s) / 2)
    }

    private func server(_ p: CGPoint) -> (Int, Int) {
        let f = fit
        let x = Int(((p.x - f.ox) / f.scale).rounded()), y = Int(((p.y - f.oy) / f.scale).rounded())
        return (min(max(x, 0), LbwCore.width - 1), min(max(y, 0), LbwCore.height - 1))
    }

    override func draw(_ rect: CGRect) {
        guard let ctx = UIGraphicsGetCurrentContext() else { return }
        let f = fit
        ctx.translateBy(x: f.ox, y: f.oy)
        ctx.scaleBy(x: f.scale, y: f.scale)
        if let image {
            // CGContext zeichnet Bilder mit y nach oben — lokal spiegeln.
            ctx.saveGState()
            ctx.translateBy(x: 0, y: CGFloat(LbwCore.height))
            ctx.scaleBy(x: 1, y: -1)
            ctx.draw(image, in: CGRect(x: 0, y: 0, width: LbwCore.width, height: LbwCore.height))
            ctx.restoreGState()
        }
        for t in texts {
            let r = CGRect(x: t.x, y: t.y, width: t.w, height: t.h)
            ctx.setFillColor(UIColor(red: CGFloat(t.bg.0) / 255, green: CGFloat(t.bg.1) / 255,
                                     blue: CGFloat(t.bg.2) / 255, alpha: 1).cgColor)
            ctx.fill(r)
            let font = UIFont.monospacedSystemFont(ofSize: max(6, CGFloat(t.h) * 0.75), weight: .regular)
            let fg = UIColor(red: CGFloat(t.fg.0) / 255, green: CGFloat(t.fg.1) / 255, blue: CGFloat(t.fg.2) / 255, alpha: 1)
            (t.text as NSString).draw(in: r, withAttributes: [.font: font, .foregroundColor: fg])
        }
    }

    @objc private func tap(_ g: UITapGestureRecognizer) {
        let (x, y) = server(g.location(in: self))
        core?.mouse(x, y)
        core?.click()
    }

    @objc private func pan(_ g: UIPanGestureRecognizer) {
        let (x, y) = server(g.location(in: self))
        core?.mouse(x, y)
    }

    @objc private func scroll(_ g: UIPanGestureRecognizer) {
        panAccum += g.translation(in: self).y
        g.setTranslation(.zero, in: self)
        if abs(panAccum) > 80 {
            core?.wheel(panAccum > 0 ? -1 : 1)
            panAccum = 0
        }
    }

    @objc private func longPress(_ g: UILongPressGestureRecognizer) {
        guard g.state == .began else { return }
        let (x, y) = server(g.location(in: self))
        core?.mouse(x, y)
        core?.click(3)
    }

    // MARK: UIKeyInput — Softwaretastatur
    override var canBecomeFirstResponder: Bool { true }
    var hasText: Bool { true }
    func insertText(_ text: String) { core?.text(text) } // "\n" → Enter im Kern
    func deleteBackward() { core?.key("Backspace") }
    var autocorrectionType: UITextAutocorrectionType { get { .no } set {} }
    var autocapitalizationType: UITextAutocapitalizationType { get { .none } set {} }
}

struct ScreenView: UIViewRepresentable {
    let core: LbwCore
    @Binding var keyboard: Bool
    var onUp: () -> Void

    func makeUIView(context: Context) -> RemoteView {
        let v = RemoteView(frame: .zero)
        v.core = core
        v.onUp = onUp
        return v
    }

    func updateUIView(_ v: RemoteView, context: Context) {
        if keyboard { v.becomeFirstResponder() } else { v.resignFirstResponder() }
    }
}
