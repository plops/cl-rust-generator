// 03_TextItems — text blob parser and Unifont fitting (pure, JVM-tested).
//
// Blob layout (little endian, rust-core/src/02_blob.rs):
// u32 count, per item: u32 id | u16 x y w h | u8 fg[3] | u8 bg[3] | u16 len | UTF-8
package de.lbw.client

import java.nio.ByteBuffer
import java.nio.ByteOrder

/** One server text element; rectangle in server pixels, colours as ARGB. */
data class TextItem(
    val id: Int,
    val x: Int,
    val y: Int,
    val w: Int,
    val h: Int,
    val fg: Int,
    val bg: Int,
    val text: String,
)

object TextItems {
    /** id + x y w h + fg + bg + len */
    private const val ITEM_HEADER = 4 + 8 + 6 + 2

    /** Parses the blob; a truncated blob yields the complete items only. */
    fun parse(blob: ByteArray): List<TextItem> {
        val b = ByteBuffer.wrap(blob).order(ByteOrder.LITTLE_ENDIAN)
        if (b.remaining() < 4) return emptyList()
        val n = b.int
        val out = ArrayList<TextItem>(n.coerceIn(0, 4096))
        repeat(n) {
            if (b.remaining() < ITEM_HEADER) return out
            val id = b.int
            val x = b.u16()
            val y = b.u16()
            val w = b.u16()
            val h = b.u16()
            val fg = b.rgb()
            val bg = b.rgb()
            val len = b.u16()
            if (b.remaining() < len) return out
            val s = String(blob, b.position(), len, Charsets.UTF_8)
            b.position(b.position() + len)
            out += TextItem(id, x, y, w, h, fg, bg, s)
        }
        return out
    }

    private fun ByteBuffer.u16() = short.toInt() and 0xffff

    private fun ByteBuffer.rgb(): Int {
        val r = get().toInt() and 0xff
        val g = get().toInt() and 0xff
        val bl = get().toInt() and 0xff
        return (0xff shl 24) or (r shl 16) or (g shl 8) or bl
    }

    /**
     * Unifont size (server pixels) for a box of height [h]: typical
     * terminal/GUI lines (11..22 px) use the native 16 px grid scaled to
     * the box, everything else 0.8 × h (as in the desktop client).
     */
    fun textSizeFor(h: Int): Float = when (h) {
        in 11..22 -> h * 0.9f
        else -> (h * 0.8f).coerceIn(8f, 96f)
    }

    /** Horizontal stretch so [measured] px fill the box width minus padding. */
    fun scaleXFor(boxW: Int, measured: Float, pad: Float = 1f): Float =
        if (measured <= 0f) 1f else ((boxW - 2 * pad) / measured).coerceIn(0.4f, 2.5f)
}
