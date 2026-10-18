// 10_MainActivity — session screen, connection and lifecycle.
//
// ConnectForm → Core (SSH chain inside Rust) → ScreenView.
// onStop closes everything; onStart reconnects with the
// same parameters (the password lives only in memory, never in prefs).
// Intent extras allow scripted starts (see scripts/emulator_e2e.sh):
//   --es profile NAME --es jump_host H --ei jump_port P --es jump_user U
//   --es jump_key PEM --es target_host H --ei target_port P --es target_user U
//   --es target_key PEM --es target_sshd_host H --ei target_sshd_port P
//   --ei lbw_port P --es lbw_token T --es mode trackpad|direct|select
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
            override fun key(name: String) = screen.keys.key(name)
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
        screen.onLinkUp = { savePins() }
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
        startCore(p)
    }

    // Kein Java-Tunnel mehr: Die Rust-Engine baut die SSH-Kette selbst
    // (russh, moderne Keys inkl. Ed25519) und verbindet sich neu.
    private fun startCore(p: ConnectParams) {
        CoreBridge.loadError?.let {
            connectForm.status.text = "Native Bibliothek fehlt: $it"
            active = null
            showForm()
            return
        }
        Log.i(ScreenView.TAG, "connect ${p.profile.ifEmpty { p.target.host }}")
        val c = Core(p.toEngineJson())
        if (!c.isOpen) {
            connectForm.status.text = "Engine startet nicht (Config prüfen)"
            active = null
            showForm()
            return
        }
        core = c
        screen.core = c
    }

    private fun disconnect() {
        screen.core = null
        core?.close()
        core = null
    }

    /**
     * Stores the learned/verified host-key fingerprints (TOFU) under
     * pinKey() per hop, so the next start verifies instead of learning.
     */
    private fun savePins() {
        val p = active ?: return
        val pins = core?.pins() ?: return
        val hops = p.jumps + p.target
        if (pins.size != hops.size) {
            Log.w(ScreenView.TAG, "pins: ${pins.size} für ${hops.size} Hops — nicht gespeichert")
            return
        }
        val e = prefs.edit()
        for ((hop, pin) in hops.zip(pins)) {
            if (pin.isNotEmpty()) e.putString(ConnectForm.pinKey(p.profile, hop), pin)
        }
        e.apply()
        Log.i(ScreenView.TAG, "pins: ${pins.size} Host-Keys gespeichert")
    }

    override fun onStart() {
        super.onStart()
        val p = active
        if (p != null && core == null) {
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
