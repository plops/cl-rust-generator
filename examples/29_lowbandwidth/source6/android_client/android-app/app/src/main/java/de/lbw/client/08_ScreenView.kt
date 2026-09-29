// 08_ScreenView — draws the remote screen and turns touch/keys into input.
//
// Per vsync: Core.poll() → Bitmap.copyPixelsFromBuffer (one memcpy from the
// direct buffer, nothing on the Java heap) → server texts drawn with Unifont
// on top, all in server coordinates under the viewport matrix. Bitmap
// scaling uses nearest neighbour (filter off), like the desktop client.
package de.lbw.client

import android.annotation.SuppressLint
import android.content.Context
import android.graphics.Bitmap
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.graphics.Path
import android.graphics.Typeface
import android.util.Log
import android.util.TypedValue
import android.view.KeyEvent
import android.view.MotionEvent
import android.view.View
import android.view.inputmethod.EditorInfo
import android.view.inputmethod.InputConnection
import android.view.inputmethod.InputMethodManager
import kotlin.math.hypot

@SuppressLint("ViewConstructor")
class ScreenView(ctx: Context, private val font: Typeface, private val mods: StickyMods) : View(ctx), MouseSink {

    var core: Core? = null
        set(c) {
            field = c
            bitmap = null
            texts = emptyList()
            wasUp = false
            if (c != null) postOnAnimation(frameLoop)
            invalidate()
        }

    /** Extra HUD text (SSH tunnel state), set by the activity. */
    var tunnelState: String = ""
    var hud = false
    var onSelected: (String) -> Unit = {}
    var onModsUsed: () -> Unit = {}

    val vp = Viewport()
    val touch = TouchInput(vp, this)
    val keys = KeyInput({ core }, mods) { onModsUsed() }

    private var bitmap: Bitmap? = null
    private var texts: List<TextItem> = emptyList()
    private var status = ""
    private var statusAt = 0L
    private var wasUp = false

    private val bitmapPaint = Paint().apply { isFilterBitmap = false }
    private val bgPaint = Paint()
    private val textPaint = Paint(Paint.ANTI_ALIAS_FLAG).apply { typeface = font }
    private val selPaint = Paint().apply { color = Color.argb(64, 50, 130, 255) }
    private val selLine = Paint().apply { color = Color.rgb(50, 130, 255); style = Paint.Style.STROKE }
    private val hudBg = Paint().apply { color = Color.argb(190, 0, 0, 0) }
    private val hudText = Paint(Paint.ANTI_ALIAS_FLAG).apply { typeface = font; color = Color.YELLOW }
    private val cursorFill = Paint(Paint.ANTI_ALIAS_FLAG).apply { color = Color.WHITE }
    private val cursorLine = Paint(Paint.ANTI_ALIAS_FLAG).apply { color = Color.BLACK; style = Paint.Style.STROKE }
    private val cursorPath = Path()

    init {
        isFocusable = true
        isFocusableInTouchMode = true
        setBackgroundColor(Color.BLACK)
    }

    private val frameLoop = object : Runnable {
        override fun run() {
            val c = core ?: return
            if (!c.isOpen) return
            if (pollOnce(c)) invalidate()
            postOnAnimation(this)
        }
    }

    /** Applies network events; true if something visible changed. */
    private fun pollOnce(c: Core): Boolean {
        val f = c.poll()
        var changed = false
        if (f and CoreBridge.SIZE != 0 || bitmap == null) {
            bitmap = Bitmap.createBitmap(c.width, c.height, Bitmap.Config.ARGB_8888)
            vp.setSizes(width, height, c.width, c.height)
            touch.setScene(c.width, c.height)
            Log.i(TAG, "scene ${c.width}x${c.height}")
            changed = true
        }
        if (f and CoreBridge.FRAME != 0) {
            c.frame.rewind()
            bitmap?.copyPixelsFromBuffer(c.frame)
            changed = true
        }
        if (f and CoreBridge.TEXT != 0) {
            texts = c.texts()
            Log.i(TAG, "texts ${texts.size}: ${texts.take(20).joinToString(" | ") { it.text.take(40) }}")
            changed = true
        }
        val up = f and CoreBridge.UP != 0
        if (up != wasUp) {
            wasUp = up
            Log.i(TAG, if (up) "link up" else "link down")
            changed = true
        }
        val now = System.currentTimeMillis()
        if (now - statusAt > 500) {
            statusAt = now
            status = c.status()
            changed = changed || hud || !up
        }
        return changed
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        core?.let { vp.setSizes(w, h, it.width, it.height) }
        // Screen position for scripted taps (scripts/emulator_e2e.sh)
        post {
            val loc = IntArray(2).also { getLocationOnScreen(it) }
            Log.i(TAG, "view ${w}x$h@${loc[0]},${loc[1]}")
        }
    }

    override fun onDraw(canvas: Canvas) {
        val bm = bitmap ?: return drawHud(canvas)
        canvas.save()
        canvas.translate(vp.offX, vp.offY)
        canvas.scale(vp.scale, vp.scale)
        canvas.drawBitmap(bm, 0f, 0f, bitmapPaint)
        for (t in texts) drawText(canvas, t)
        touch.selection?.let { s ->
            val l = minOf(s[0], s[2]); val r = maxOf(s[0], s[2])
            val tp = minOf(s[1], s[3]); val b = maxOf(s[1], s[3])
            canvas.drawRect(l, tp, r, b, selPaint)
            selLine.strokeWidth = 1.5f / vp.scale
            canvas.drawRect(l, tp, r, b, selLine)
        }
        canvas.restore()
        if (touch.mode == TouchMode.TRACKPAD) drawCursor(canvas)
        drawHud(canvas)
    }

    private fun drawText(canvas: Canvas, t: TextItem) {
        bgPaint.color = t.bg
        canvas.drawRect(t.x.toFloat(), t.y.toFloat(), (t.x + t.w).toFloat(), (t.y + t.h).toFloat(), bgPaint)
        textPaint.color = t.fg
        textPaint.textScaleX = 1f
        textPaint.textSize = TextItems.textSizeFor(t.h)
        textPaint.textScaleX = TextItems.scaleXFor(t.w, textPaint.measureText(t.text))
        val fm = textPaint.fontMetrics
        val baseline = t.y + (t.h - (fm.descent - fm.ascent)) / 2 - fm.ascent
        canvas.drawText(t.text, t.x + 1f, baseline, textPaint)
    }

    private fun drawCursor(canvas: Canvas) {
        val (x, y) = vp.toView(touch.pointerX, touch.pointerY)
        val s = 18f * resources.displayMetrics.density / 2.5f
        cursorPath.reset()
        cursorPath.moveTo(x, y)
        cursorPath.lineTo(x, y + s * 1.6f)
        cursorPath.lineTo(x + s * 0.45f, y + s * 1.2f)
        cursorPath.lineTo(x + s * 1.1f, y + s * 1.1f)
        cursorPath.close()
        cursorLine.strokeWidth = 2f
        canvas.drawPath(cursorPath, cursorFill)
        canvas.drawPath(cursorPath, cursorLine)
    }

    private fun drawHud(canvas: Canvas) {
        val c = core
        if (!(hud || c == null || !wasUp)) return
        val line = listOf(if (c == null) "nicht verbunden" else status, tunnelState)
            .filter { it.isNotEmpty() }.joinToString(" | ")
        hudText.textSize = TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_SP, 13f, resources.displayMetrics)
        val h = hudText.textSize * 1.5f
        canvas.drawRect(0f, height - h, width.toFloat(), height.toFloat(), hudBg)
        canvas.drawText(line, 6f, height - h * 0.3f, hudText)
    }

    // ---- touch --------------------------------------------------------------

    private val longPress = Runnable {
        touch.longPress(System.currentTimeMillis())
        invalidate()
    }

    @SuppressLint("ClickableViewAccessibility")
    override fun onTouchEvent(e: MotionEvent): Boolean {
        val t = System.currentTimeMillis()
        when (e.actionMasked) {
            MotionEvent.ACTION_DOWN -> {
                touch.down(e.x, e.y, t)
                postDelayed(longPress, TouchInput.LONG_PRESS_MS)
            }
            MotionEvent.ACTION_POINTER_DOWN, MotionEvent.ACTION_MOVE -> {
                var cx = 0f; var cy = 0f
                val n = e.pointerCount
                for (i in 0 until n) { cx += e.getX(i); cy += e.getY(i) }
                cx /= n; cy /= n
                var span = 0f
                for (i in 0 until n) span += hypot(e.getX(i) - cx, e.getY(i) - cy)
                touch.move(n, cx, cy, span / n, t)
            }
            MotionEvent.ACTION_UP, MotionEvent.ACTION_CANCEL -> {
                removeCallbacks(longPress)
                touch.up(t)
            }
        }
        invalidate()
        return true
    }

    /** Real mouse (DeX, Chromebook, USB): hover moves, wheel scrolls. */
    override fun onGenericMotionEvent(e: MotionEvent): Boolean {
        val c = core ?: return false
        return when (e.actionMasked) {
            MotionEvent.ACTION_HOVER_MOVE -> {
                val (sx, sy) = vp.toScene(e.x, e.y)
                c.mouse(sx.toInt(), sy.toInt())
                true
            }
            MotionEvent.ACTION_SCROLL -> {
                val v = e.getAxisValue(MotionEvent.AXIS_VSCROLL)
                c.wheel(if (v > 0) 1 else if (v < 0) -1 else 0)
                true
            }
            else -> super.onGenericMotionEvent(e)
        }
    }

    // MouseSink → Core
    override fun mouse(x: Int, y: Int, force: Boolean) { core?.mouse(x, y, force) }
    override fun button(button: Int, down: Boolean) { core?.button(button, down) }
    override fun wheel(dy: Int) { core?.wheel(dy) }
    override fun select(x0: Int, y0: Int, x1: Int, y1: Int) {
        val s = core?.select(x0, y0, x1, y1) ?: ""
        Log.i(TAG, "select ${s.length} chars")
        onSelected(s)
    }

    // ---- keyboard -----------------------------------------------------------

    fun showKeyboard(show: Boolean) {
        val imm = context.getSystemService(InputMethodManager::class.java)
        if (show) {
            requestFocus()
            imm.showSoftInput(this, 0)
        } else {
            imm.hideSoftInputFromWindow(windowToken, 0)
        }
    }

    override fun onCheckIsTextEditor() = true

    override fun onCreateInputConnection(out: EditorInfo): InputConnection = keys.connection(this, out)

    override fun onKeyDown(keyCode: Int, e: KeyEvent): Boolean =
        keys.onKeyDown(keyCode, e) || super.onKeyDown(keyCode, e)

    companion object {
        const val TAG = "lbw"
    }
}
