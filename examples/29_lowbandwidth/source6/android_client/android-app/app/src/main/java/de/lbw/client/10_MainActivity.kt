// 10_MainActivity — session screen, connection and lifecycle.
//
// ConnectForm → (optional SSH tunnel on a background thread) → Core → ScreenView.
// onStop closes everything (socket, tunnel); onStart reconnects with the
// same parameters (the password lives only in memory, never in prefs).
// Intent extras allow scripted starts (see scripts/emulator_e2e.sh):
//   --es addr H:P | --es ssh_host H --ei ssh_port P --es ssh_user U
//   --es ssh_password PW --ei remote_port P --es mode trackpad|direct|select
//   --ez hud true --ez autoconnect true
package de.lbw.client

import android.annotation.SuppressLint
import android.app.Activity
import android.content.ClipData
import android.content.ClipboardManager
import android.graphics.Color
import android.graphics.Typeface
import android.os.Build
import android.os.Bundle
import android.util.Log
import android.view.WindowInsets
import android.view.WindowManager
import android.widget.LinearLayout
import android.widget.Toast
import android.window.OnBackInvokedDispatcher

class MainActivity : Activity() {
    private val prefs by lazy { getSharedPreferences("lbw", MODE_PRIVATE) }
    private val font by lazy {
        runCatching { Typeface.createFromAsset(assets, "fonts/unifont.otf") }.getOrDefault(Typeface.MONOSPACE)
    }
    private val mods = StickyMods()
    private lateinit var root: LinearLayout
    private lateinit var session: LinearLayout
    private lateinit var screen: ScreenView
    private lateinit var keybar: VirtualKeybar
    private lateinit var connectForm: ConnectForm

    private var active: ConnectParams? = null
    private var core: Core? = null
    private var tunnel: SshTunnel? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        window.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON)
        root = LinearLayout(this).apply { orientation = LinearLayout.VERTICAL; setBackgroundColor(Color.BLACK) }
        // Edge-to-edge is enforced from targetSdk 35: pad for bars and IME
        root.setOnApplyWindowInsetsListener { v, insets ->
            if (Build.VERSION.SDK_INT >= 30) {
                val i = insets.getInsets(WindowInsets.Type.systemBars() or WindowInsets.Type.ime())
                v.setPadding(i.left, i.top, i.right, i.bottom)
            } else {
                @Suppress("DEPRECATION")
                v.setPadding(
                    insets.systemWindowInsetLeft, insets.systemWindowInsetTop,
                    insets.systemWindowInsetRight, insets.systemWindowInsetBottom,
                )
            }
            insets
        }
        connectForm = ConnectForm(this, prefs) { connect(it) }
        buildSession()
        root.addView(connectForm.view, LinearLayout.LayoutParams(-1, -1))
        setContentView(root)
        if (Build.VERSION.SDK_INT >= 33) {
            onBackInvokedDispatcher.registerOnBackInvokedCallback(OnBackInvokedDispatcher.PRIORITY_DEFAULT) { back() }
        }
        intent.extras?.let { x ->
            connectForm.applyExtras(x)
            screen.hud = x.getBoolean("hud", false)
            if (x.getBoolean("autoconnect", false)) connect(connectForm.read())
        }
    }

    // API 33+ routes back via onBackInvokedDispatcher (enableOnBackInvokedCallback in the manifest)
    @SuppressLint("GestureBackNavigation")
    @Deprecated("Only used below API 33")
    override fun onBackPressed() = back()

    private fun back() {
        if (active != null) {
            Log.i(ScreenView.TAG, "back: session closed")
            disconnect()
            active = null
            showForm()
        } else {
            finish()
        }
    }

    // ---- UI -----------------------------------------------------------------

    private fun buildSession() {
        screen = ScreenView(this, font, mods)
        screen.onSelected = { s ->
            getSystemService(ClipboardManager::class.java).setPrimaryClip(ClipData.newPlainText("lbw", s))
            Toast.makeText(this, "${s.length} Zeichen kopiert", Toast.LENGTH_SHORT).show()
        }
        keybar = VirtualKeybar(this, mods, object : KeybarActions {
            override fun key(keysym: Int) = screen.keys.key(keysym)
            override fun toggleKeyboard() = screen.showKeyboard(true)
            override fun cycleMode(): TouchMode {
                val m = TouchMode.entries[(screen.touch.mode.ordinal + 1) % TouchMode.entries.size]
                setMode(m)
                return m
            }
            override fun paste() {
                val clip = getSystemService(ClipboardManager::class.java).primaryClip
                val s = clip?.takeIf { it.itemCount > 0 }?.getItemAt(0)?.coerceToText(this@MainActivity)?.toString()
                if (!s.isNullOrEmpty()) core?.paste(s)
            }
            override fun toggleHud() {
                screen.hud = !screen.hud
                screen.invalidate()
            }
            override fun fit() {
                screen.vp.fit()
                screen.invalidate()
            }
            override fun wheel(dy: Int) {
                core?.wheel(dy)
            }
            override fun modsChanged() = Unit
        })
        screen.onModsUsed = { keybar.refresh() }
        session = LinearLayout(this).apply {
            orientation = LinearLayout.VERTICAL
            addView(screen, LinearLayout.LayoutParams(-1, 0, 1f))
            addView(keybar, LinearLayout.LayoutParams(-1, -2))
        }
    }

    private fun setMode(m: TouchMode) {
        screen.touch.mode = m
        screen.touch.clearSelection()
        keybar.setMode(m)
        prefs.edit().putString("mode", m.name).apply()
        screen.invalidate()
    }

    private fun showForm() {
        root.removeAllViews()
        root.addView(connectForm.view, LinearLayout.LayoutParams(-1, -1))
    }

    private fun showSession() {
        root.removeAllViews()
        root.addView(session, LinearLayout.LayoutParams(-1, -1))
        screen.requestFocus()
    }

    // ---- connection ---------------------------------------------------------

    private fun connect(p: ConnectParams) {
        connectForm.save()
        active = p
        setMode(p.mode)
        showSession()
        val ssh = p.ssh ?: return startCore(p.addr)
        screen.tunnelState = "SSH: verbinde …"
        val cfg = ssh.copy(pinnedFingerprint = prefs.getString(ConnectForm.pinKey(ssh), null))
        val t = SshTunnel(cfg) { st -> runOnUiThread { screen.tunnelState = st; screen.invalidate() } }
        tunnel = t
        Thread({
            try {
                val port = t.start()
                t.fingerprint?.let { fp -> prefs.edit().putString(ConnectForm.pinKey(ssh), fp).apply() }
                Log.i(ScreenView.TAG, "ssh tunnel up: 127.0.0.1:$port fp=${t.fingerprint}")
                runOnUiThread { if (tunnel === t) startCore("127.0.0.1:$port") }
            } catch (e: Exception) {
                Log.w(ScreenView.TAG, "ssh failed: $e")
                runOnUiThread {
                    if (tunnel === t) {
                        disconnect()
                        active = null
                        connectForm.status.text = "SSH-Fehler: ${e.message}"
                        showForm()
                    }
                }
            }
        }, "lbw-ssh-connect").start()
    }

    private fun startCore(addr: String) {
        CoreBridge.loadError?.let {
            connectForm.status.text = "Native Bibliothek fehlt: $it"
            active = null
            showForm()
            return
        }
        Log.i(ScreenView.TAG, "connect $addr")
        val c = Core(addr)
        core = c
        screen.core = c
    }

    private fun disconnect() {
        screen.core = null
        core?.close()
        core = null
        val t = tunnel ?: return
        tunnel = null
        // JSch disconnect may block on the socket: never on the UI thread
        Thread({ t.close() }, "lbw-ssh-close").start()
    }

    override fun onStart() {
        super.onStart()
        val p = active
        if (p != null && core == null && tunnel == null) {
            Log.i(ScreenView.TAG, "start: reconnecting")
            connect(p)
        }
    }

    override fun onStop() {
        super.onStop()
        if (active != null) Log.i(ScreenView.TAG, "stop: closing connection")
        disconnect()
    }

    override fun onDestroy() {
        disconnect()
        super.onDestroy()
    }
}
