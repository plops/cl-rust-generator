// 04_Viewport — zoom/pan between server pixels and view pixels (pure).
//
// view = scene * scale + off. The scene always stays reachable: when it is
// smaller than the view it is centred, otherwise its edges can't leave it.
package de.lbw.client

class Viewport {
    var viewW = 1f
        private set
    var viewH = 1f
        private set
    var sceneW = 1f
        private set
    var sceneH = 1f
        private set
    var scale = 1f
        private set
    var offX = 0f
        private set
    var offY = 0f
        private set

    /** Scale at which the whole scene fits into the view. */
    val fitScale get() = minOf(viewW / sceneW, viewH / sceneH)
    val maxScale get() = maxOf(fitScale, 1f) * MAX_ZOOM

    /** New view or scene size; keeps the zoom if possible, else fits. */
    fun setSizes(vw: Int, vh: Int, sw: Int, sh: Int) {
        val changed = sw.toFloat() != sceneW || sh.toFloat() != sceneH
        viewW = vw.coerceAtLeast(1).toFloat()
        viewH = vh.coerceAtLeast(1).toFloat()
        sceneW = sw.coerceAtLeast(1).toFloat()
        sceneH = sh.coerceAtLeast(1).toFloat()
        if (changed) fit() else clamp()
    }

    fun fit() {
        scale = fitScale
        offX = (viewW - sceneW * scale) / 2
        offY = (viewH - sceneH * scale) / 2
    }

    /** Zooms by [factor] keeping view point (fx, fy) fixed. */
    fun zoomBy(factor: Float, fx: Float, fy: Float) {
        val s = (scale * factor).coerceIn(fitScale, maxScale)
        val k = s / scale
        offX = fx - (fx - offX) * k
        offY = fy - (fy - offY) * k
        scale = s
        clamp()
    }

    fun panBy(dx: Float, dy: Float) {
        offX += dx
        offY += dy
        clamp()
    }

    /** Pans minimally so scene point (x, y) is inside the view with [margin] px. */
    fun reveal(x: Float, y: Float, margin: Float = 24f) {
        val (vx, vy) = toView(x, y)
        if (vx < margin) offX += margin - vx
        if (vx > viewW - margin) offX -= vx - (viewW - margin)
        if (vy < margin) offY += margin - vy
        if (vy > viewH - margin) offY -= vy - (viewH - margin)
        clamp()
    }

    fun toScene(vx: Float, vy: Float) = Pair((vx - offX) / scale, (vy - offY) / scale)

    fun toView(sx: Float, sy: Float) = Pair(sx * scale + offX, sy * scale + offY)

    private fun clamp() {
        offX = axis(offX, viewW, sceneW * scale)
        offY = axis(offY, viewH, sceneH * scale)
    }

    private fun axis(off: Float, view: Float, content: Float): Float =
        if (content <= view) (view - content) / 2 else off.coerceIn(view - content, 0f)

    companion object {
        const val MAX_ZOOM = 8f
    }
}
