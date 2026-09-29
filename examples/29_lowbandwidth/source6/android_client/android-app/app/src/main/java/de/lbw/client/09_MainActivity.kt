// 09_MainActivity — connection form, session screen and lifecycle.
//
// Form → (optional SSH tunnel on a background thread) → Core → ScreenView.
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
import android.text.InputType
import android.util.Log
import android.view.WindowInsets
import android.view.WindowManager
import android.widget.Button
import android.widget.CheckBox
import android.widget.EditText
import android.widget.LinearLayout
import android.widget.ScrollView
import android.widget.TextView
import android.widget.Toast
import android.window.OnBackInvokedDispatcher

class MainActivity : Activity() {
    private data class Params(val addr: String, val ssh: SshTunnel.Config?, val mode: TouchMode)

    private val prefs by lazy { getSharedPreferences("lbw", MODE_PRIVATE) }
    private val font by lazy {
        runCatching { Typeface.createFromAsset(assets, "fonts/unifont.otf") }.getOrDefault(Typeface.MONOSPACE)
    }
    private val mods = StickyMods()
    private lateinit var root: LinearLayout
    private lateinit var form: ScrollView
    private lateinit var session: LinearLayout
    private lateinit var screen: ScreenView
    private lateinit var keybar: VirtualKeybar
    private lateinit var status: TextView
    private val f = mutableMapOf<String, EditText>()
    private lateinit var useSsh: CheckBox

    private var active: Params? = null
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
        buildForm()
        buildSession()
        root.addView(form, LinearLayout.LayoutParams(-1, -1))
        setContentView(root)
        if (Build.VERSION.SDK_INT >= 33) {
            onBackInvokedDispatcher.registerOnBackInvokedCallback(OnBackInvokedDispatcher.PRIORITY_DEFAULT) { back() }
        }
        applyExtras()
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

    private fun field(parent: LinearLayout, key: String, hint: String, def: String, type: Int, lines: Int = 1) {
        parent.addView(TextView(this).apply { text = hint; setTextColor(Color.LTGRAY) })
        val e = EditText(this).apply {
            setText(if (key == "ssh_password") "" else prefs.getString(key, def))
            inputType = type
            setTextColor(Color.WHITE)
            if (lines > 1) {
                minLines = lines
                maxLines = lines
                setHorizontallyScrolling(false)
            }
        }
        f[key] = e
        parent.addView(e)
    }

    private fun buildForm() {
        val text = InputType.TYPE_CLASS_TEXT or InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS
        val uri = text or InputType.TYPE_TEXT_VARIATION_URI
        val num = InputType.TYPE_CLASS_NUMBER
        val col = LinearLayout(this).apply { orientation = LinearLayout.VERTICAL; setPadding(32, 32, 32, 32) }
        col.addView(TextView(this).apply { this.text = "LBW Remote Desktop"; textSize = 22f; setTextColor(Color.WHITE) })
        field(col, "addr", "Server (host:port, direkt)", "10.0.2.2:7878", uri)
        useSsh = CheckBox(this).apply {
            this.text = "Über SSH-Tunnel"
            setTextColor(Color.WHITE)
            isChecked = prefs.getBoolean("ssh", false)
        }
        col.addView(useSsh)
        field(col, "ssh_host", "SSH-Host", "", uri)
        field(col, "ssh_port", "SSH-Port", "22", num)
        field(col, "ssh_user", "SSH-Benutzer", "", text)
        field(col, "ssh_password", "Passwort (wird nicht gespeichert)", "", text or InputType.TYPE_TEXT_VARIATION_PASSWORD)
        field(col, "ssh_key", "Privater Schlüssel (optional, PEM/OpenSSH)", "", text or InputType.TYPE_TEXT_FLAG_MULTI_LINE, 3)
        field(col, "remote_port", "lbw-server-Port auf dem SSH-Host", "7878", num)
        col.addView(Button(this).apply { this.text = "Verbinden"; setOnClickListener { connect(readForm()) } })
        col.addView(Button(this).apply {
            this.text = "Gespeicherten Host-Key vergessen"
            setOnClickListener {
                prefs.edit().remove(pinKey(readForm().ssh)).apply()
                status.text = "Host-Key vergessen"
            }
        })
        status = TextView(this).apply { setTextColor(Color.rgb(255, 180, 80)); setPadding(0, 24, 0, 0) }
        col.addView(status)
        form = ScrollView(this).apply { addView(col) }
    }

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
        root.addView(form, LinearLayout.LayoutParams(-1, -1))
    }

    private fun showSession() {
        root.removeAllViews()
        root.addView(session, LinearLayout.LayoutParams(-1, -1))
        screen.requestFocus()
    }

    // ---- parameters ---------------------------------------------------------

    private fun readForm(): Params {
        fun s(k: String) = f[k]!!.text.toString().trim()
        val ssh = if (useSsh.isChecked) {
            SshTunnel.Config(
                host = s("ssh_host"), port = s("ssh_port").toIntOrNull() ?: 22, user = s("ssh_user"),
                password = s("ssh_password").ifEmpty { null }, privateKey = s("ssh_key").ifEmpty { null },
                remotePort = s("remote_port").toIntOrNull() ?: 7878,
            )
        } else {
            null
        }
        val mode = runCatching { TouchMode.valueOf(prefs.getString("mode", "")!!) }.getOrDefault(TouchMode.TRACKPAD)
        return Params(s("addr"), ssh, mode)
    }

    private fun saveForm() {
        val e = prefs.edit()
        for ((k, v) in f) if (k != "ssh_password") e.putString(k, v.text.toString())
        e.putBoolean("ssh", useSsh.isChecked).apply()
    }

    private fun applyExtras() {
        val x = intent.extras ?: return
        x.getString("addr")?.let { f["addr"]!!.setText(it); useSsh.isChecked = false }
        x.getString("ssh_host")?.let { f["ssh_host"]!!.setText(it); useSsh.isChecked = true }
        if (x.containsKey("ssh_port")) f["ssh_port"]!!.setText(x.getInt("ssh_port").toString())
        x.getString("ssh_user")?.let { f["ssh_user"]!!.setText(it) }
        x.getString("ssh_password")?.let { f["ssh_password"]!!.setText(it) }
        if (x.containsKey("remote_port")) f["remote_port"]!!.setText(x.getInt("remote_port").toString())
        x.getString("mode")?.let { m -> prefs.edit().putString("mode", m.uppercase()).apply() }
        screen.hud = x.getBoolean("hud", false)
        if (x.getBoolean("autoconnect", false)) connect(readForm())
    }

    private fun pinKey(c: SshTunnel.Config?) = "pin:${c?.user}@${c?.host}:${c?.port}"

    // ---- connection ---------------------------------------------------------

    private fun connect(p: Params) {
        saveForm()
        active = p
        setMode(p.mode)
        showSession()
        val ssh = p.ssh ?: return startCore(p.addr)
        screen.tunnelState = "SSH: verbinde …"
        val cfg = ssh.copy(pinnedFingerprint = prefs.getString(pinKey(ssh), null))
        val t = SshTunnel(cfg) { st -> runOnUiThread { screen.tunnelState = st; screen.invalidate() } }
        tunnel = t
        Thread({
            try {
                val port = t.start()
                t.fingerprint?.let { fp -> prefs.edit().putString(pinKey(ssh), fp).apply() }
                Log.i(ScreenView.TAG, "ssh tunnel up: 127.0.0.1:$port fp=${t.fingerprint}")
                runOnUiThread { if (tunnel === t) startCore("127.0.0.1:$port") }
            } catch (e: Exception) {
                Log.w(ScreenView.TAG, "ssh failed: $e")
                runOnUiThread {
                    if (tunnel === t) {
                        disconnect()
                        active = null
                        status.text = "SSH-Fehler: ${e.message}"
                        showForm()
                    }
                }
            }
        }, "lbw-ssh-connect").start()
    }

    private fun startCore(addr: String) {
        CoreBridge.loadError?.let {
            status.text = "Native Bibliothek fehlt: $it"
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
