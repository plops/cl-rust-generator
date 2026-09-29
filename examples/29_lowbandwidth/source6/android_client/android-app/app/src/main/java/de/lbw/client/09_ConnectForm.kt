// 09_ConnectForm — connection form: fields, prefs and intent extras.
//
// Everything except the SSH password is kept in SharedPreferences "lbw";
// TOFU host-key pins live there too under pinKey(). Intent extras
// (see 10_MainActivity) prefill the fields for scripted starts.
package de.lbw.client

import android.content.Context
import android.content.SharedPreferences
import android.graphics.Color
import android.os.Bundle
import android.text.InputType
import android.widget.Button
import android.widget.CheckBox
import android.widget.EditText
import android.widget.LinearLayout
import android.widget.ScrollView
import android.widget.TextView

data class ConnectParams(val addr: String, val ssh: SshTunnel.Config?, val mode: TouchMode)

class ConnectForm(
    private val ctx: Context,
    private val prefs: SharedPreferences,
    onConnect: (ConnectParams) -> Unit,
) {
    private val f = mutableMapOf<String, EditText>()
    private val useSsh: CheckBox
    val status: TextView
    val view: ScrollView

    init {
        val text = InputType.TYPE_CLASS_TEXT or InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS
        val uri = text or InputType.TYPE_TEXT_VARIATION_URI
        val num = InputType.TYPE_CLASS_NUMBER
        val col = LinearLayout(ctx).apply { orientation = LinearLayout.VERTICAL; setPadding(32, 32, 32, 32) }
        col.addView(TextView(ctx).apply { this.text = "LBW Remote Desktop"; textSize = 22f; setTextColor(Color.WHITE) })
        field(col, "addr", "Server (host:port, direkt)", "10.0.2.2:7878", uri)
        useSsh = CheckBox(ctx).apply {
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
        col.addView(Button(ctx).apply { this.text = "Verbinden"; setOnClickListener { onConnect(read()) } })
        status = TextView(ctx).apply { setTextColor(Color.rgb(255, 180, 80)); setPadding(0, 24, 0, 0) }
        col.addView(Button(ctx).apply {
            this.text = "Gespeicherten Host-Key vergessen"
            setOnClickListener {
                prefs.edit().remove(pinKey(read().ssh)).apply()
                status.text = "Host-Key vergessen"
            }
        })
        col.addView(status)
        view = ScrollView(ctx).apply { addView(col) }
    }

    private fun field(parent: LinearLayout, key: String, hint: String, def: String, type: Int, lines: Int = 1) {
        parent.addView(TextView(ctx).apply { text = hint; setTextColor(Color.LTGRAY) })
        val e = EditText(ctx).apply {
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

    fun read(): ConnectParams {
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
        return ConnectParams(s("addr"), ssh, mode)
    }

    fun save() {
        val e = prefs.edit()
        for ((k, v) in f) if (k != "ssh_password") e.putString(k, v.text.toString())
        e.putBoolean("ssh", useSsh.isChecked).apply()
    }

    fun applyExtras(x: Bundle) {
        x.getString("addr")?.let { f["addr"]!!.setText(it); useSsh.isChecked = false }
        x.getString("ssh_host")?.let { f["ssh_host"]!!.setText(it); useSsh.isChecked = true }
        if (x.containsKey("ssh_port")) f["ssh_port"]!!.setText(x.getInt("ssh_port").toString())
        x.getString("ssh_user")?.let { f["ssh_user"]!!.setText(it) }
        x.getString("ssh_password")?.let { f["ssh_password"]!!.setText(it) }
        if (x.containsKey("remote_port")) f["remote_port"]!!.setText(x.getInt("remote_port").toString())
        x.getString("mode")?.let { m -> prefs.edit().putString("mode", m.uppercase()).apply() }
    }

    companion object {
        fun pinKey(c: SshTunnel.Config?) = "pin:${c?.user}@${c?.host}:${c?.port}"
    }
}
