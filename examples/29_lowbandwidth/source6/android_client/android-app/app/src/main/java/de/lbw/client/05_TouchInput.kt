// 05_TouchInput — touch gestures → mouse/viewport actions (pure, JVM-tested).
//
// The view feeds raw pointer data (view pixels, milliseconds); no Android
// types, so the state machine runs in plain unit tests.
//
//            1 finger                         2 fingers
// TRACKPAD   drag = move pointer (relative)   pinch = zoom, drag = pan
//            tap = left click                 or wheel (vertical, not zoomed)
//            long press = drag (hold left)    tap = right click
// DIRECT     tap = move there + left click    same as trackpad
//            drag = move with left held
//            long press = right click
// SELECT     drag = selection rectangle → onSelect
package de.lbw.client

import kotlin.math.abs
import kotlin.math.hypot

enum class TouchMode { TRACKPAD, DIRECT, SELECT }

interface MouseSink {
    fun mouse(x: Int, y: Int, force: Boolean)
    fun button(button: Int, down: Boolean)
    fun wheel(dy: Int)
    fun select(x0: Int, y0: Int, x1: Int, y1: Int)
}

class TouchInput(private val vp: Viewport, private val sink: MouseSink) {
    var mode = TouchMode.TRACKPAD

    /** Pointer position in server pixels (drawn as a cursor in TRACKPAD). */
    var pointerX = 320f
        private set
    var pointerY = 320f
        private set

    /** Active selection in server pixels (x0, y0, x1, y1) or null. */
    var selection: FloatArray? = null
        private set

    /** Finger movement in view px → pointer movement in view px. */
    var trackpadSpeed = 1.3f

    private var downX = 0f
    private var downY = 0f
    private var downT = 0L
    private var lastX = 0f
    private var lastY = 0f
    private var maxPointers = 0
    private var moved = false
    private var dragging = false
    private var longFired = false
    private var twoFinger: TwoFinger? = null

    private class TwoFinger(var cx: Float, var cy: Float, val span0: Float, var span: Float) {
        var zooming = false
        var wheelAcc = 0f
    }

    fun setScene(w: Int, h: Int) {
        pointerX = pointerX.coerceIn(0f, (w - 1).toFloat())
        pointerY = pointerY.coerceIn(0f, (h - 1).toFloat())
    }

    /** First finger down. */
    fun down(x: Float, y: Float, t: Long) {
        downX = x; downY = y; downT = t
        lastX = x; lastY = y
        maxPointers = 1
        moved = false
        dragging = false
        longFired = false
        twoFinger = null
        if (mode == TouchMode.SELECT) {
            val (sx, sy) = vp.toScene(x, y)
            selection = floatArrayOf(sx, sy, sx, sy)
        }
    }

    /**
     * Pointer update. [n] = fingers down; for n ≥ 2 (x, y) is the centroid
     * and [span] the mean distance of the fingers to it.
     */
    fun move(n: Int, x: Float, y: Float, span: Float, t: Long) {
        if (n >= 2) {
            twoFingerMove(x, y, span)
            return
        }
        if (twoFinger != null) return // one finger left over after a pinch
        if (!moved && hypot(x - downX, y - downY) > SLOP) moved = true
        val dx = x - lastX
        val dy = y - lastY
        lastX = x; lastY = y
        if (!moved) return
        when (mode) {
            TouchMode.TRACKPAD -> movePointerBy(dx * trackpadSpeed / vp.scale, dy * trackpadSpeed / vp.scale)
            TouchMode.DIRECT -> {
                if (!dragging && !longFired) {
                    val (sx, sy) = vp.toScene(downX, downY)
                    pointerTo(sx, sy, force = true)
                    sink.button(CoreBridge.BUTTON_LEFT, true)
                    dragging = true
                }
                val (sx, sy) = vp.toScene(x, y)
                pointerTo(sx, sy, force = false)
            }
            TouchMode.SELECT -> {
                val (sx, sy) = vp.toScene(x, y)
                selection?.let { it[2] = sx; it[3] = sy }
            }
        }
    }

    private fun twoFingerMove(x: Float, y: Float, span: Float) {
        maxPointers = 2
        if (dragging) {
            sink.button(CoreBridge.BUTTON_LEFT, false)
            dragging = false
        }
        if (mode == TouchMode.SELECT) selection = null
        val g = twoFinger ?: TwoFinger(x, y, span, span).also { twoFinger = it; return }
        val dx = x - g.cx
        val dy = y - g.cy
        if (!g.zooming && g.span0 > 0 && abs(span / g.span0 - 1f) > PINCH_THRESHOLD) g.zooming = true
        if (hypot(x - downX, y - downY) > SLOP) moved = true
        val zoomedIn = vp.scale > vp.fitScale * 1.01f
        if (g.zooming || zoomedIn) {
            if (g.span > 0 && span > 0) vp.zoomBy(span / g.span, x, y)
            vp.panBy(dx, dy)
        } else {
            g.wheelAcc += dy
            while (abs(g.wheelAcc) >= WHEEL_STEP) {
                // Natural scrolling: fingers down = content follows = wheel up (+1)
                val notch = if (g.wheelAcc > 0) 1 else -1
                sink.wheel(notch)
                g.wheelAcc -= notch * WHEEL_STEP
            }
        }
        g.cx = x; g.cy = y; g.span = span
    }

    /** Called by the view [LONG_PRESS_MS] after down; ignored if outdated. */
    fun longPress(t: Long) {
        if (moved || longFired || maxPointers != 1 || t - downT < LONG_PRESS_MS) return
        longFired = true
        when (mode) {
            TouchMode.TRACKPAD -> {
                sink.mouse(pointerX.toInt(), pointerY.toInt(), true)
                sink.button(CoreBridge.BUTTON_LEFT, true)
                dragging = true
                moved = true // following moves drag
            }
            TouchMode.DIRECT -> {
                val (sx, sy) = vp.toScene(downX, downY)
                pointerTo(sx, sy, force = true)
                click(CoreBridge.BUTTON_RIGHT)
            }
            TouchMode.SELECT -> Unit
        }
    }

    /** Last finger up. */
    fun up(t: Long) {
        val tap = !moved && !longFired && t - downT < TAP_MS
        when {
            dragging -> {
                sink.mouse(pointerX.toInt(), pointerY.toInt(), true)
                sink.button(CoreBridge.BUTTON_LEFT, false)
            }
            tap && maxPointers >= 2 -> {
                sink.mouse(pointerX.toInt(), pointerY.toInt(), true)
                click(CoreBridge.BUTTON_RIGHT)
            }
            tap && mode == TouchMode.TRACKPAD -> {
                sink.mouse(pointerX.toInt(), pointerY.toInt(), true)
                click(CoreBridge.BUTTON_LEFT)
            }
            tap && mode == TouchMode.DIRECT -> {
                val (sx, sy) = vp.toScene(downX, downY)
                pointerTo(sx, sy, force = true)
                click(CoreBridge.BUTTON_LEFT)
            }
            mode == TouchMode.SELECT && twoFinger == null -> selection?.let {
                if (moved) sink.select(it[0].toInt(), it[1].toInt(), it[2].toInt(), it[3].toInt())
            }
        }
        dragging = false
        twoFinger = null
        if (mode != TouchMode.SELECT) selection = null
    }

    fun clearSelection() {
        selection = null
    }

    private fun click(b: Int) {
        sink.button(b, true)
        sink.button(b, false)
    }

    private fun movePointerBy(dx: Float, dy: Float) {
        pointerTo(pointerX + dx, pointerY + dy, force = false)
        vp.reveal(pointerX, pointerY)
    }

    private fun pointerTo(x: Float, y: Float, force: Boolean) {
        pointerX = x.coerceIn(0f, vp.sceneW - 1)
        pointerY = y.coerceIn(0f, vp.sceneH - 1)
        sink.mouse(pointerX.toInt(), pointerY.toInt(), force)
    }

    companion object {
        const val SLOP = 12f
        const val TAP_MS = 250L
        const val LONG_PRESS_MS = 450L
        const val WHEEL_STEP = 40f
        const val PINCH_THRESHOLD = 0.08f
    }
}
