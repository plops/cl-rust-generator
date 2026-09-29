// 02_SshTunnel — SSH local port forward (JSch) with keepalive and watchdog.
//
// 127.0.0.1:<localPort> → ssh host → remoteHost:remotePort (lbw-server).
// The local port stays the same across reconnects, so the Rust core simply
// reconnects to the same address. Host keys are pinned "trust on first use":
// the SHA256 fingerprint (OpenSSH format) is checked during key exchange,
// before a password or key is ever sent.
package de.lbw.client

import com.jcraft.jsch.HostKey
import com.jcraft.jsch.HostKeyRepository
import com.jcraft.jsch.JSch
import com.jcraft.jsch.Session
import com.jcraft.jsch.UserInfo
import java.security.MessageDigest
import java.util.Base64

class SshTunnel(
    private val cfg: Config,
    private val onState: (String) -> Unit = {},
) : AutoCloseable {

    data class Config(
        val host: String,
        val port: Int = 22,
        val user: String,
        val password: String? = null,
        /** OpenSSH/PEM private key text (RSA, ECDSA, Ed25519). */
        val privateKey: String? = null,
        val passphrase: String? = null,
        val remoteHost: String = "127.0.0.1",
        val remotePort: Int = 7878,
        /** Expected `SHA256:…` host key fingerprint; null = accept and report. */
        val pinnedFingerprint: String? = null,
        val keepaliveMs: Int = 15_000,
        val connectTimeoutMs: Int = 10_000,
    )

    class HostKeyMismatch(val seen: String, val expected: String) :
        Exception("Host-Key geändert: $seen (erwartet $expected)")

    @Volatile var localPort = 0
        private set

    /** Fingerprint of the server's host key after the first connect. */
    @Volatile var fingerprint: String? = null
        private set

    @Volatile var state = "SSH: aus"
        private set

    @Volatile private var session: Session? = null
    @Volatile private var closed = false
    private var watchdog: Thread? = null

    val isUp get() = session?.isConnected == true

    /**
     * Connects synchronously (call off the UI thread) and starts the
     * watchdog. Throws on bad credentials or host key, so the user sees it.
     * Returns the local port.
     */
    fun start(): Int {
        connect()
        watchdog = Thread(::watch, "lbw-ssh-watchdog").apply {
            isDaemon = true
            start()
        }
        return localPort
    }

    private fun connect() {
        setState("SSH: verbinde ${cfg.user}@${cfg.host}:${cfg.port} …")
        val jsch = JSch()
        jsch.hostKeyRepository = PinningRepository()
        cfg.privateKey?.takeIf { it.isNotBlank() }?.let {
            jsch.addIdentity("key", it.trim().toByteArray(), null, cfg.passphrase?.toByteArray())
        }
        val s = jsch.getSession(cfg.user, cfg.host, cfg.port)
        cfg.password?.takeIf { it.isNotEmpty() }?.let { s.setPassword(it.toByteArray()) }
        s.userInfo = NoPrompt
        // "yes": the repository decides; unknown keys are accepted there (TOFU)
        s.setConfig("StrictHostKeyChecking", "yes")
        s.setConfig("PreferredAuthentications", "publickey,keyboard-interactive,password")
        s.setConfig("compression.s2c", "none")
        s.setServerAliveInterval(cfg.keepaliveMs)
        s.setServerAliveCountMax(3)
        try {
            s.connect(cfg.connectTimeoutMs)
            localPort = s.setPortForwardingL("127.0.0.1", localPort, cfg.remoteHost, cfg.remotePort)
        } catch (e: Exception) {
            s.disconnect()
            throw (e.cause as? HostKeyMismatch) ?: e
        }
        session = s
        setState("SSH: ${cfg.host} → 127.0.0.1:$localPort")
    }

    private fun watch() {
        var backoffMs = 1_000L
        while (!closed) {
            try {
                Thread.sleep(if (isUp) 2_000L else backoffMs)
            } catch (_: InterruptedException) {
                break
            }
            if (closed || isUp) {
                backoffMs = 1_000L
                continue
            }
            session?.disconnect()
            session = null
            try {
                connect()
                backoffMs = 1_000L
            } catch (e: Exception) {
                setState("SSH: getrennt (${e.message}), neuer Versuch in ${backoffMs / 1000} s")
                backoffMs = (backoffMs * 2).coerceAtMost(30_000L)
            }
        }
    }

    /** For tests: drops the SSH connection as a network loss would. */
    fun simulateDrop() {
        session?.disconnect()
    }

    private fun setState(s: String) {
        state = s
        onState(s)
    }

    override fun close() {
        closed = true
        watchdog?.interrupt()
        session?.disconnect()
        session = null
        setState("SSH: aus")
    }

    private inner class PinningRepository : HostKeyRepository {
        override fun check(host: String?, key: ByteArray): Int {
            val fp = fingerprintOf(key)
            val expected = cfg.pinnedFingerprint ?: fingerprint
            if (expected != null && expected != fp) {
                throw HostKeyMismatch(fp, expected)
            }
            fingerprint = fp
            return HostKeyRepository.OK
        }

        override fun add(hostkey: HostKey?, ui: UserInfo?) = Unit
        override fun remove(host: String?, type: String?) = Unit
        override fun remove(host: String?, type: String?, key: ByteArray?) = Unit
        override fun getKnownHostsRepositoryID() = "lbw-pinning"
        override fun getHostKey(): Array<HostKey> = emptyArray()
        override fun getHostKey(host: String?, type: String?): Array<HostKey> = emptyArray()
    }

    private object NoPrompt : UserInfo {
        override fun getPassphrase(): String? = null
        override fun getPassword(): String? = null
        override fun promptPassword(message: String?) = false
        override fun promptPassphrase(message: String?) = false
        override fun promptYesNo(message: String?) = false
        override fun showMessage(message: String?) = Unit
    }

    companion object {
        /** OpenSSH style: `SHA256:` + unpadded base64 of the key blob hash. */
        fun fingerprintOf(key: ByteArray): String =
            "SHA256:" + Base64.getEncoder().withoutPadding()
                .encodeToString(MessageDigest.getInstance("SHA-256").digest(key))
    }
}
