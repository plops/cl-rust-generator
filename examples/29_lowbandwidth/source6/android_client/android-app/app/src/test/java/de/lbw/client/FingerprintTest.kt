package de.lbw.client

import org.junit.Assert.assertEquals
import org.junit.Test
import java.util.Base64

class FingerprintTest {
    @Test
    fun matchesSshKeygen() {
        // ssh-keygen -lf host_ed25519.pub
        val blob = Base64.getDecoder().decode("AAAAC3NzaC1lZDI1NTE5AAAAIIIwcxH7Z8Q5M217h/eEir8TnuXSRkDwMrjY3iIP/XUb")
        assertEquals("SHA256:ScF/wEhG8c8UkhK7Rhe2f1Sb3vlvpey14evY8lA6gPQ", SshTunnel.fingerprintOf(blob))
    }
}
