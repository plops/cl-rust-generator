package de.lbw.client

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class TextItemsTest {
    // Same bytes as rust-core 02_blob::tests::blob_layout_is_stable
    private val blob = byteArrayOf(
        1, 0, 0, 0,
        4, 3, 2, 1,
        1, 0, 2, 0, 44, 1, 16, 0,
        9, 8, 7, 1, 2, 3,
        3, 0, 'H'.code.toByte(), 0xc3.toByte(), 0xa4.toByte(),
    )

    @Test
    fun parsesRustLayout() {
        val t = TextItems.parse(blob).single()
        assertEquals(0x01020304, t.id)
        assertEquals(listOf(1, 2, 300, 16), listOf(t.x, t.y, t.w, t.h))
        assertEquals(0xff090807.toInt(), t.fg)
        assertEquals(0xff010203.toInt(), t.bg)
        assertEquals("Hä", t.text)
    }

    @Test
    fun truncatedOrEmptyBlobIsSafe() {
        assertTrue(TextItems.parse(ByteArray(0)).isEmpty())
        assertTrue(TextItems.parse(byteArrayOf(0, 0, 0, 0)).isEmpty())
        assertTrue(TextItems.parse(blob.copyOf(blob.size - 1)).isEmpty())
        val two = blob.copyOf().also { it[0] = 2 }
        assertEquals(1, TextItems.parse(two).size)
    }

    @Test
    fun fontFitsBox() {
        assertEquals(14.4f, TextItems.textSizeFor(16), 0.01f)
        assertEquals(8f, TextItems.textSizeFor(4), 0.01f)
        assertEquals(96f, TextItems.textSizeFor(500), 0.01f)
        assertEquals(1f, TextItems.scaleXFor(102, 100f), 0.001f)
        assertEquals(2.5f, TextItems.scaleXFor(1000, 10f), 0.001f)
        assertEquals(0.4f, TextItems.scaleXFor(10, 1000f), 0.001f)
        assertEquals(1f, TextItems.scaleXFor(10, 0f), 0.001f)
    }
}
