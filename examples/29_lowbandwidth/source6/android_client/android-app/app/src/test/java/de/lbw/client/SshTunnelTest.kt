package de.lbw.client

import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue
import org.junit.Before
import org.junit.Test
import java.io.File
import java.net.ServerSocket
import java.net.Socket
import kotlin.concurrent.thread

/**
 * Integration test against a real sshd (scripts/test_sshd.sh start).
 * Skipped unless LBW_SSHD_PORT / LBW_SSHD_USER / LBW_SSHD_KEY are set.
 */
class SshTunnelTest {
    private val port = System.getenv("LBW_SSHD_PORT")?.toIntOrNull()
    private val user = System.getenv("LBW_SSHD_USER")
    private val keyFile = System.getenv("LBW_SSHD_KEY")?.let(::File)
    private lateinit var echo: ServerSocket

    @Before
    fun setUp() {
        assumeTrue("no test sshd", port != null && user != null && keyFile?.canRead() == true)
        echo = ServerSocket(0)
        thread(isDaemon = true) {
            while (!echo.isClosed) {
                val s = runCatching { echo.accept() }.getOrNull() ?: break
                thread(isDaemon = true) { s.use { it.getInputStream().copyTo(it.getOutputStream()) } }
            }
        }
    }

    @After
    fun tearDown() {
        if (::echo.isInitialized) echo.close()
    }

    private fun cfg(pin: String? = null) = SshTunnel.Config(
        host = "127.0.0.1", port = port!!, user = user!!,
        privateKey = keyFile!!.readText(), remotePort = echo.localPort,
        pinnedFingerprint = pin, keepaliveMs = 2_000,
    )

    private fun roundTrip(localPort: Int, msg: String) {
        Socket("127.0.0.1", localPort).use { s ->
            s.soTimeout = 5_000
            val out = msg.toByteArray()
            s.getOutputStream().write(out)
            val buf = ByteArray(out.size)
            var n = 0
            while (n < buf.size) n += s.getInputStream().read(buf, n, buf.size - n).also { check(it > 0) }
            assertEquals(msg, String(buf))
        }
    }

    @Test
    fun forwardsAndRestoresSamePortAfterDrop() {
        val states = mutableListOf<String>()
        SshTunnel(cfg()) { synchronized(states) { states += it } }.use { t ->
            val p = t.start()
            assertTrue(p > 0)
            assertTrue(t.fingerprint!!.startsWith("SHA256:"))
            roundTrip(p, "hallo über ssh")
            t.simulateDrop()
            val deadline = System.currentTimeMillis() + 15_000
            while (System.currentTimeMillis() < deadline) {
                Thread.sleep(200)
                if (t.isUp && synchronized(states) { states.count { it.startsWith("SSH: 127") } } >= 2) break
            }
            assertTrue("tunnel not restored: $states", t.isUp)
            assertEquals(p, t.localPort)
            roundTrip(p, "wieder da")
        }
    }

    @Test
    fun rejectsChangedHostKeyBeforeAuth() {
        val bad = "SHA256:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        val e = runCatching { SshTunnel(cfg(pin = bad)).use { it.start() } }.exceptionOrNull()
        assertTrue("$e", e is SshTunnel.HostKeyMismatch)
    }

    @Test
    fun acceptsPinnedHostKey() {
        val fp = SshTunnel(cfg()).use { it.start(); it.fingerprint!! }
        SshTunnel(cfg(pin = fp)).use { roundTrip(it.start(), "gepinnt") }
    }

    @Test
    fun passwordAuth() {
        val pwUser = System.getenv("LBW_SSHD_PWUSER")
        val pw = System.getenv("LBW_SSHD_PASSWORD")
        assumeTrue("no password user", pwUser != null && pw != null)
        val c = cfg().copy(user = pwUser!!, privateKey = null, password = pw)
        SshTunnel(c).use { roundTrip(it.start(), "passwort") }
        val wrong = runCatching { SshTunnel(c.copy(password = "falsch")).use { it.start() } }
        assertTrue(wrong.isFailure)
    }
}
