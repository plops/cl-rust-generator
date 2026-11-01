package de.lbw.client

import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * source10_b: pins are positional per hop. source11 dropped missing pins
 * (mapNotNull), so an unpinned jump shifted the target's pin onto the
 * jump — the chain then failed with "wrong host key" forever.
 */
class EngineJsonTest {
    private fun hop(host: String, pin: String?) = HopParams(host, 22, "u", "KEY", pin)

    @Test
    fun missingPinKeepsPositions() {
        val p = ConnectParams(
            profile = "p", jumps = listOf(hop("jump", null)), target = hop("ziel", "SHA256:z"),
            targetSshdHost = "127.0.0.1", targetSshdPort = 2332, lbwPort = 7878, mode = TouchMode.TRACKPAD,
        )
        val j = p.toEngineJson()
        assertTrue(j, j.contains(""""pins":["","SHA256:z"]"""))
        assertTrue(j, j.contains(""""hops":["u@jump:22","u@ziel:22"]"""))
        assertTrue(j, !j.contains("token"))
    }
}
