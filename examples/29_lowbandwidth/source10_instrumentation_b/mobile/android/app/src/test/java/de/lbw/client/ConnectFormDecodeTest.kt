package de.lbw.client

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * ConnectForm.decodeKeyExtra without any device: base64 key extras
 * (the emulator E2E path) decode to PEM, garbage yields null.
 */
class ConnectFormDecodeTest {
    private val pem = "-----BEGIN OPENSSH PRIVATE KEY-----\nZmFrZXk=\n-----END OPENSSH PRIVATE KEY-----\n"

    @Test
    fun b64RoundTrip() {
        val b64 = java.util.Base64.getEncoder().encodeToString(pem.toByteArray(Charsets.UTF_8))
        assertEquals(pem, ConnectForm.decodeKeyExtra(b64))
    }

    @Test
    fun garbageYieldsNull() {
        assertNull(ConnectForm.decodeKeyExtra(null))
        assertNull(ConnectForm.decodeKeyExtra("   "))
        assertNull(ConnectForm.decodeKeyExtra("!!!kein-base64!!!"))
        val notKey = java.util.Base64.getEncoder().encodeToString("hallo".toByteArray())
        assertNull(ConnectForm.decodeKeyExtra(notKey))
    }

    @Test
    fun pinKeyIsStable() {
        val h = HopParams("127.0.0.1", 2332, "kiel", "", null)
        assertTrue(ConnectForm.pinKey("p", h).startsWith("pin:p:kiel@127.0.0.1:2332"))
    }
}
