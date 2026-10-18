package de.lbw.client

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue
import org.junit.Test

/**
 * Loads the host build of liblbw_android_core (cargo build -p
 * lbw-android-core) and checks the JNI surface without any server: bad
 * config yields a closed Core, key/char mapping works offline.
 */
class CoreBridgeHostTest {
    private fun lib() {
        assumeTrue(
            "liblbw_android_core not built: ${CoreBridge.loadError}",
            CoreBridge.loadError == null,
        )
    }

    @Test
    fun versionString() {
        lib()
        val v = CoreBridge.version()?.toString(Charsets.UTF_8)
        assertTrue(v?.startsWith("lbw11") == true)
    }

    @Test
    fun badConfigStaysClosed() {
        lib()
        Core("{no json").use { c ->
            assertFalse(c.isOpen)
            assertEquals(0, c.poll())
            assertTrue(c.texts().isEmpty())
        }
    }

    @Test
    fun pinsEmptyBeforeChainStands() {
        lib()
        // Unreachable port: chain never stands, so no pins are learned.
        val json = """{"hops":["u@127.0.0.1:1"],"identities":["x"],"pins":[],""" +
            """"target_sshd_host":"127.0.0.1","target_sshd_port":1,"lbw_port":1,"token":"t"}"""
        Core(json).use { c ->
            assertTrue(c.isOpen)
            assertTrue(c.pins().isEmpty())
        }
    }

    @Test
    fun keymapWorksWithoutConnection() {
        lib()
        // Unreachable port: worker retries in the background, mapping is local.
        val json = """{"hops":["u@127.0.0.1:1"],"identities":["x"],"pins":[],""" +
            """"target_sshd_host":"127.0.0.1","target_sshd_port":1,"lbw_port":1,"token":"t"}"""
        Core(json).use { c ->
            assertTrue(c.isOpen)
            assertTrue(c.androidKey(66, 0)) // Enter
            assertFalse(c.androidKey(29, 0)) // A -> char path
            assertTrue(c.char('ß'.code))
            assertFalse(c.char(0x1))
            c.key("Esc")
            c.text("hi")
            c.paste("p")
            c.mouse(10, 20)
            c.button(CoreBridge.BUTTON_LEFT, true)
            c.button(CoreBridge.BUTTON_LEFT, false)
            c.wheel(1)
            assertTrue(c.status().isNotEmpty())
        }
    }
}
