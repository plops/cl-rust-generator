package de.lbw.client

import org.junit.Assert.assertEquals
import org.junit.Test

class StickyModsTest {
    @Test
    fun armLockRelease() {
        val m = StickyMods()
        m.tap(CoreBridge.MOD_CTRL)
        assertEquals(CoreBridge.MOD_CTRL, m.consume())
        assertEquals(0, m.consume())
        m.tap(CoreBridge.MOD_ALT)
        m.tap(CoreBridge.MOD_ALT)
        assertEquals(CoreBridge.MOD_ALT, m.consume())
        assertEquals(CoreBridge.MOD_ALT, m.consume())
        m.tap(CoreBridge.MOD_ALT)
        assertEquals(0, m.active)
    }

    @Test
    fun metaMatchesAndroidConstants() {
        val m = StickyMods()
        assertEquals(0x1 or 0x2 or 0x1000 or 0x1_0000, m.toMeta(15))
    }
}
