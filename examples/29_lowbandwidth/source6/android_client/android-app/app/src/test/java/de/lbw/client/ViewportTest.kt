package de.lbw.client

import org.junit.Assert.assertEquals
import org.junit.Test

class ViewportTest {
    private fun vp() = Viewport().apply { setSizes(1000, 500, 640, 640) }

    @Test
    fun fitsAndCentres() {
        val v = vp()
        assertEquals(500f / 640f, v.scale, 1e-4f)
        assertEquals((1000 - 500) / 2f, v.offX, 1e-3f)
        assertEquals(0f, v.offY, 1e-3f)
        val (sx, sy) = v.toScene(500f, 250f)
        assertEquals(320f, sx, 1e-3f)
        assertEquals(320f, sy, 1e-3f)
    }

    @Test
    fun zoomKeepsFocusFixedAndIsClamped() {
        val v = vp()
        val before = v.toScene(600f, 100f)
        v.zoomBy(3f, 600f, 100f)
        val after = v.toScene(600f, 100f)
        assertEquals(before.first, after.first, 1e-2f)
        assertEquals(before.second, after.second, 1e-2f)
        v.zoomBy(1000f, 0f, 0f)
        assertEquals(v.maxScale, v.scale, 1e-4f)
        v.zoomBy(0.0001f, 0f, 0f)
        assertEquals(v.fitScale, v.scale, 1e-4f)
    }

    @Test
    fun panStopsAtEdges() {
        val v = vp()
        v.zoomBy(4f, 500f, 250f)
        v.panBy(1e6f, 1e6f)
        assertEquals(0f, v.offX, 1e-3f)
        assertEquals(0f, v.offY, 1e-3f)
        v.panBy(-1e6f, -1e6f)
        assertEquals(1000f - 640f * v.scale, v.offX, 1e-2f)
        assertEquals(500f - 640f * v.scale, v.offY, 1e-2f)
    }

    @Test
    fun revealBringsPointIntoView() {
        val v = vp()
        v.zoomBy(4f, 0f, 0f)
        v.reveal(600f, 600f)
        val (x, y) = v.toView(600f, 600f)
        assert(x in 0f..1000f && y in 0f..500f) { "$x,$y" }
    }

    @Test
    fun sceneResizeRefits() {
        val v = vp()
        v.zoomBy(4f, 0f, 0f)
        v.setSizes(1000, 500, 1280, 720)
        assertEquals(minOf(1000f / 1280f, 500f / 720f), v.scale, 1e-4f)
    }
}
