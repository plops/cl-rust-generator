// 01_CoreBridge — JNI declarations for liblbw_core.so and a safe wrapper.
//
// The signatures must match rust-core/src/04_jni.rs exactly
// (Java_de_lbw_client_CoreBridge_<name>). Strings travel as UTF-8 byte[],
// the RGBA canvas through a direct ByteBuffer (no Java heap copy).
package de.lbw.client

import java.nio.ByteBuffer
import java.nio.ByteOrder

object CoreBridge {
    const val FRAME = 1
    const val TEXT = 2
    const val SIZE = 4
    const val UP = 8

    const val MOD_SHIFT = 1
    const val MOD_CTRL = 2
    const val MOD_ALT = 4
    const val MOD_SUPER = 8

    const val BUTTON_LEFT = 1
    const val BUTTON_MIDDLE = 2
    const val BUTTON_RIGHT = 3

    /** Loads the native library once; returns the error instead of throwing. */
    val loadError: Throwable? by lazy {
        try {
            System.loadLibrary("lbw_core")
            null
        } catch (t: Throwable) {
            t
        }
    }

    @JvmStatic external fun nativeNew(addr: ByteArray, deadAfterS: Int): Long
    @JvmStatic external fun nativeFree(h: Long)
    @JvmStatic external fun nativePoll(h: Long, frame: ByteBuffer?): Int
    @JvmStatic external fun nativeSize(h: Long): Int
    @JvmStatic external fun nativeTexts(h: Long): ByteArray?
    @JvmStatic external fun nativeStatus(h: Long): ByteArray?
    @JvmStatic external fun nativeKey(h: Long, keysym: Int, mods: Int)
    @JvmStatic external fun nativeAndroidKey(h: Long, keyCode: Int, metaState: Int): Boolean
    @JvmStatic external fun nativeChar(h: Long, codepoint: Int, mods: Int): Boolean
    @JvmStatic external fun nativePaste(h: Long, utf8: ByteArray)
    @JvmStatic external fun nativeMouse(h: Long, x: Int, y: Int, force: Boolean)
    @JvmStatic external fun nativeButton(h: Long, button: Int, down: Boolean)
    @JvmStatic external fun nativeWheel(h: Long, dy: Int)
    @JvmStatic external fun nativeSelect(h: Long, x0: Int, y0: Int, x1: Int, y1: Int): ByteArray?
}

/**
 * One connection to an lbw-server (`host:port`). Reconnects on its own;
 * [close] stops the network thread and closes the socket immediately.
 * Not thread-safe by design: call from the UI thread only.
 */
class Core(addr: String, deadAfterS: Int = 90) : AutoCloseable {
    private var h: Long = CoreBridge.nativeNew(addr.toByteArray(Charsets.UTF_8), deadAfterS)

    var width = 0
        private set
    var height = 0
        private set

    /** RGBA8888 canvas, `width * height * 4` bytes, valid after a FRAME flag. */
    var frame: ByteBuffer = ByteBuffer.allocateDirect(4)
        private set

    init {
        resize()
    }

    val isOpen get() = h != 0L

    private fun resize() {
        val s = CoreBridge.nativeSize(h)
        width = s ushr 16
        height = s and 0xffff
        frame = ByteBuffer.allocateDirect(maxOf(4, width * height * 4)).order(ByteOrder.nativeOrder())
    }

    /**
     * Applies pending network events. On a size change the buffer is
     * reallocated and polled again, so the result may contain SIZE|FRAME.
     */
    fun poll(): Int {
        if (h == 0L) return 0
        var f = CoreBridge.nativePoll(h, frame)
        if (f and CoreBridge.SIZE != 0) {
            resize()
            f = f or CoreBridge.nativePoll(h, frame)
        }
        frame.rewind()
        return f
    }

    fun texts(): List<TextItem> =
        if (h == 0L) emptyList() else TextItems.parse(CoreBridge.nativeTexts(h) ?: ByteArray(4))

    fun status(): String =
        if (h == 0L) "" else CoreBridge.nativeStatus(h)?.toString(Charsets.UTF_8) ?: ""

    fun key(keysym: Int, mods: Int) {
        if (h != 0L) CoreBridge.nativeKey(h, keysym, mods)
    }

    fun androidKey(keyCode: Int, metaState: Int): Boolean =
        h != 0L && CoreBridge.nativeAndroidKey(h, keyCode, metaState)

    fun char(codepoint: Int, mods: Int = 0): Boolean =
        h != 0L && CoreBridge.nativeChar(h, codepoint, mods)

    /** Sends every code point of [s] (IME commitText). */
    fun text(s: String, mods: Int = 0) {
        var i = 0
        while (i < s.length) {
            val cp = s.codePointAt(i)
            char(cp, mods)
            i += Character.charCount(cp)
        }
    }

    fun paste(s: String) {
        if (h != 0L && s.isNotEmpty()) CoreBridge.nativePaste(h, s.toByteArray(Charsets.UTF_8))
    }

    fun mouse(x: Int, y: Int, force: Boolean = false) {
        if (h != 0L) CoreBridge.nativeMouse(h, x, y, force)
    }

    fun button(button: Int, down: Boolean) {
        if (h != 0L) CoreBridge.nativeButton(h, button, down)
    }

    fun click(button: Int = CoreBridge.BUTTON_LEFT) {
        button(button, true)
        button(button, false)
    }

    fun wheel(dy: Int) {
        if (h != 0L && dy != 0) CoreBridge.nativeWheel(h, dy)
    }

    fun select(x0: Int, y0: Int, x1: Int, y1: Int): String =
        if (h == 0L) "" else CoreBridge.nativeSelect(h, x0, y0, x1, y1)?.toString(Charsets.UTF_8) ?: ""

    override fun close() {
        if (h != 0L) {
            CoreBridge.nativeFree(h)
            h = 0L
        }
    }
}
