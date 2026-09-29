package de.lbw.client

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class TouchInputTest {
    private class Rec : MouseSink {
        val ev = mutableListOf<String>()
        override fun mouse(x: Int, y: Int, force: Boolean) { ev += "m$x,$y" }
        override fun button(button: Int, down: Boolean) { ev += "b$button${if (down) "d" else "u"}" }
        override fun wheel(dy: Int) { ev += "w$dy" }
        override fun select(x0: Int, y0: Int, x1: Int, y1: Int) { ev += "s$x0,$y0,$x1,$y1" }
        fun buttons() = ev.filter { it.startsWith("b") || it.startsWith("w") || it.startsWith("s") }
    }

    // view 640x640 = scene 640x640 at scale 1
    private fun setup(mode: TouchMode): Pair<TouchInput, Rec> {
        val vp = Viewport().apply { setSizes(640, 640, 640, 640) }
        val r = Rec()
        return TouchInput(vp, r).apply { this.mode = mode } to r
    }

    @Test
    fun trackpadTapClicksAtPointer() {
        val (t, r) = setup(TouchMode.TRACKPAD)
        t.down(10f, 10f, 0)
        t.up(100)
        assertEquals(listOf("m320,320", "b1d", "b1u"), r.ev)
    }

    @Test
    fun trackpadDragMovesRelative() {
        val (t, r) = setup(TouchMode.TRACKPAD)
        t.trackpadSpeed = 1f
        t.down(100f, 100f, 0)
        t.move(1, 120f, 100f, 0f, 10) // past slop: first delta counts
        t.move(1, 150f, 130f, 0f, 20)
        t.up(400)
        assertEquals(370f, t.pointerX, 0.01f)
        assertEquals(350f, t.pointerY, 0.01f)
        assertTrue(r.buttons().isEmpty())
    }

    @Test
    fun trackpadLongPressDrags() {
        val (t, r) = setup(TouchMode.TRACKPAD)
        t.down(100f, 100f, 0)
        t.longPress(TouchInput.LONG_PRESS_MS)
        t.move(1, 110f, 100f, 0f, 600)
        t.up(700)
        assertEquals(listOf("b1d", "b1u"), r.buttons())
    }

    @Test
    fun longPressIgnoredAfterMove() {
        val (t, r) = setup(TouchMode.TRACKPAD)
        t.down(100f, 100f, 0)
        t.move(1, 200f, 100f, 0f, 50)
        t.longPress(TouchInput.LONG_PRESS_MS)
        t.up(600)
        assertTrue(r.buttons().isEmpty())
    }

    @Test
    fun directTapMovesAndClicks() {
        val (t, r) = setup(TouchMode.DIRECT)
        t.down(50f, 60f, 0)
        t.up(80)
        assertEquals(listOf("m50,60", "b1d", "b1u"), r.ev)
    }

    @Test
    fun directDragHoldsButton() {
        val (t, r) = setup(TouchMode.DIRECT)
        t.down(50f, 60f, 0)
        t.move(1, 80f, 60f, 0f, 30)
        t.up(300)
        assertEquals(listOf("m50,60", "b1d", "m80,60", "m80,60", "b1u"), r.ev)
    }

    @Test
    fun directLongPressIsRightClick() {
        val (t, r) = setup(TouchMode.DIRECT)
        t.down(50f, 60f, 0)
        t.longPress(500)
        t.up(700)
        assertEquals(listOf("b3d", "b3u"), r.buttons())
    }

    @Test
    fun twoFingerTapIsRightClick() {
        val (t, r) = setup(TouchMode.TRACKPAD)
        t.down(100f, 100f, 0)
        t.move(2, 120f, 100f, 20f, 10)
        t.up(120)
        assertEquals(listOf("b3d", "b3u"), r.buttons())
    }

    @Test
    fun twoFingerDragScrollsNaturally() {
        val (t, r) = setup(TouchMode.TRACKPAD)
        t.down(100f, 100f, 0)
        t.move(2, 100f, 100f, 50f, 10)
        t.move(2, 100f, 190f, 50f, 20) // fingers down 90 px → 2 notches up
        t.up(500)
        assertEquals(listOf("w1", "w1"), r.buttons())
    }

    @Test
    fun pinchZooms() {
        val vp = Viewport().apply { setSizes(640, 640, 640, 640) }
        val t = TouchInput(vp, Rec())
        t.down(300f, 300f, 0)
        t.move(2, 320f, 320f, 50f, 10)
        t.move(2, 320f, 320f, 100f, 20)
        t.up(300)
        assertEquals(2f, vp.scale, 0.01f)
    }

    @Test
    fun selectionReportsSceneRect() {
        val (t, r) = setup(TouchMode.SELECT)
        t.down(10f, 20f, 0)
        t.move(1, 200f, 100f, 0f, 50)
        t.up(400)
        assertEquals(listOf("s10,20,200,100"), r.ev)
        t.clearSelection()
        assertNull(t.selection)
    }
}
