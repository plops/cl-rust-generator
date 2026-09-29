package de.lbw.client

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue
import org.junit.Before
import org.junit.Test
import java.net.ServerSocket
import java.net.SocketTimeoutException

/**
 * Loads the host build of liblbw_core (cargo build -p lbw-core) and checks
 * that every JNI signature resolves and the engine really connects.
 * Skipped when the library has not been built.
 */
class CoreBridgeHostTest {
    @Before
    fun needsLib() {
        assumeTrue("liblbw_core not built: ${CoreBridge.loadError}", CoreBridge.loadError == null)
    }

    @Test
    fun unconnectedEngineAnswersEveryCall() {
        Core("127.0.0.1:1", 5).use { c ->
            assertEquals(640, c.width)
            assertEquals(640, c.height)
            assertEquals(640 * 640 * 4, c.frame.capacity())
            assertEquals(0, c.poll() and CoreBridge.UP)
            assertTrue(c.status(), c.status().startsWith("verbinde") || c.status().startsWith("getrennt"))
            assertTrue(c.texts().isEmpty())
            assertEquals("", c.select(0, 0, 100, 100))
            c.key(0xff0d, 0)
            assertTrue(c.androidKey(66, 0)) // KEYCODE_ENTER
            assertTrue(!c.androidKey(0, 0))
            assertTrue(c.char('a'.code))
            c.text("ö😀")
            c.paste("x")
            c.mouse(10, 10, true)
            c.click()
            c.wheel(-3)
        }
    }

    @Test
    fun connectsSendsAndClosesOnClose() {
        ServerSocket(0).use { srv ->
            srv.soTimeout = 5000
            val c = Core("127.0.0.1:${srv.localPort}", 30)
            srv.accept().use { s ->
                s.soTimeout = 3000
                c.mouse(5, 5, true)
                val input = s.getInputStream()
                val buf = ByteArray(256)
                assertTrue("client sent nothing", input.read(buf) > 0)
                c.close()
                assertTrue(!c.isOpen)
                val t0 = System.nanoTime()
                try {
                    while (input.read(buf) >= 0) {
                        // drain until EOF
                    }
                } catch (e: SocketTimeoutException) {
                    throw AssertionError("socket not closed after Core.close()")
                }
                assertTrue((System.nanoTime() - t0) < 2_000_000_000L)
            }
        }
    }
}
