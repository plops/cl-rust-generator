// 09_ConnectForm — connection form with profiles and per-hop settings.
//
// Everything is configurable — no hardcoded hosts or ports: each profile
// stores jump host/port/user/key, target host/port/user/key, the target
// sshd address as seen from the jump (your 127.0.0.1:2332), the LBW port
// and the LBW token. Host-key pins (TOFU) live under pinKey() per hop.
// Private keys and the token stay in SharedPreferences "lbw" (app-private);
// the token field is never shown in plain text. Intent extras (see
// 10_MainActivity) prefill the fields for scripted starts.
package de.lbw.client

import android.content.Context
import android.content.SharedPreferences
import android.graphics.Color
import android.os.Bundle
import android.text.InputType
import android.widget.Button
import android.widget.EditText
import android.widget.LinearLayout
import android.widget.ScrollView
import android.widget.TextView

/** One SSH hop (jump or target) with its own identity. */
data class HopParams(
    val host: String,
    val port: Int,
    val user: String,
    val identity: String,
    val pin: String?,
)

data class ConnectParams(
    val profile: String,
    val jumps: List<HopParams>,
    val target: HopParams,
    val targetSshdHost: String,
    val targetSshdPort: Int,
    val lbwPort: Int,
    val token: String,
    val mode: TouchMode,
)

/** EngineCfg JSON for Rust (see android/rust-core/src/01_engine.rs). */
fun ConnectParams.toEngineJson(): String {
    fun q(s: String) = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\""
    val hops = (jumps + target).joinToString(",") { q("${it.user}@${it.host}:${it.port}") }
    val ids = (jumps + target).joinToString(",") { q(it.identity) }
    val pins = (jumps + target).mapNotNull { it.pin?.takeIf(String::isNotEmpty)?.let(::q) }
    return """{"hops":[$hops],"identities":[$ids],"pins":[${pins.joinToString(",")}],""" +
        """"target_sshd_host":${q(targetSshdHost)},"target_sshd_port":$targetSshdPort,""" +
        """"lbw_port":$lbwPort,"token":${q(token)}}"""
}

class ConnectForm(
    private val ctx: Context,
    private val prefs: SharedPreferences,
    onConnect: (ConnectParams) -> Unit,
) {
    private val f = mutableMapOf<String, EditText>()
    val status: TextView
    val view: ScrollView

    init {
        val text = InputType.TYPE_CLASS_TEXT or InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS
        val uri = text or InputType.TYPE_TEXT_VARIATION_URI
        val num = InputType.TYPE_CLASS_NUMBER
        val pw = text or InputType.TYPE_TEXT_VARIATION_PASSWORD
        val col = LinearLayout(ctx).apply { orientation = LinearLayout.VERTICAL; setPadding(32, 32, 32, 32) }
        col.addView(TextView(ctx).apply { this.text = "LBW Remote Desktop"; textSize = 22f; setTextColor(Color.WHITE) })
        profileRow(col, onConnect)
        field(col, "jump_host", "Jump-Host (leer = direkt)", "", uri)
        field(col, "jump_port", "Jump-Port", "22", num)
        field(col, "jump_user", "Jump-Benutzer", "", text)
        field(col, "jump_key", "Jump-Key (PEM/OpenSSH, inkl. Ed25519)", "", text or InputType.TYPE_TEXT_FLAG_MULTI_LINE, 3)
        field(col, "target_host", "Ziel-Host", "", uri)
        field(col, "target_port", "Ziel-SSH-Port", "22", num)
        field(col, "target_user", "Ziel-Benutzer", "", text)
        field(col, "target_key", "Ziel-Key (PEM/OpenSSH, inkl. Ed25519)", "", text or InputType.TYPE_TEXT_FLAG_MULTI_LINE, 3)
        field(col, "target_sshd_host", "Ziel-sshd aus Jump-Sicht", "127.0.0.1", uri)
        field(col, "target_sshd_port", "Ziel-sshd-Port aus Jump-Sicht", "2332", num)
        field(col, "lbw_port", "LBW-Port am Ziel", "7878", num)
        field(col, "lbw_token", "LBW-Token", "", pw)
        col.addView(Button(ctx).apply { this.text = "Verbinden"; setOnClickListener { onConnect(read()) } })
        status = TextView(ctx).apply { setTextColor(Color.rgb(255, 180, 80)); setPadding(0, 24, 0, 0) }
        col.addView(Button(ctx).apply {
            this.text = "Gespeicherte Host-Keys vergessen"
            setOnClickListener {
                val p = read()
                for (h in p.jumps + p.target) prefs.edit().remove(pinKey(p.profile, h)).apply()
                status.text = "Host-Keys vergessen"
            }
        })
        col.addView(status)
        view = ScrollView(ctx).apply { addView(col) }
        loadProfile(prefs.getString("profile", "") ?: "")
    }

    private fun profileRow(col: LinearLayout, onConnect: (ConnectParams) -> Unit) {
        field(col, "profile", "Profil", "", InputType.TYPE_CLASS_TEXT or InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS)
        val row = LinearLayout(ctx).apply { orientation = LinearLayout.HORIZONTAL }
        row.addView(Button(ctx).apply {
            this.text = "Laden"; setOnClickListener { loadProfile(f["profile"]!!.text.toString().trim()) }
        })
        row.addView(Button(ctx).apply {
            this.text = "Speichern"; setOnClickListener { saveProfile(); status.text = "Profil gespeichert" }
        })
        row.addView(Button(ctx).apply {
            this.text = "Löschen"; setOnClickListener {
                prefs.edit().remove("profile:" + f["profile"]!!.text.toString().trim()).apply()
                status.text = "Profil gelöscht"
            }
        })
        col.addView(row)
    }

    private fun field(parent: LinearLayout, key: String, hint: String, def: String, type: Int, lines: Int = 1) {
        parent.addView(TextView(ctx).apply { text = hint; setTextColor(Color.LTGRAY) })
        val e = EditText(ctx).apply {
            setText(prefs.getString(key, def))
            inputType = type
            setTextColor(Color.WHITE)
            if (lines > 1) {
                minLines = lines
                maxLines = lines + 2
                setHorizontallyScrolling(false)
            }
        }
        f[key] = e
        parent.addView(e)
    }

    fun read(): ConnectParams {
        fun s(k: String) = f[k]!!.text.toString().trim()
        val profile = s("profile")
        val jumps = if (s("jump_host").isEmpty()) {
            emptyList()
        } else {
            listOf(
                HopParams(
                    host = s("jump_host"), port = s("jump_port").toIntOrNull() ?: 22,
                    user = s("jump_user"), identity = f["jump_key"]!!.text.toString(),
                    pin = prefs.getString(
                        pinKey(profile, HopParams(s("jump_host"), s("jump_port").toIntOrNull() ?: 22, s("jump_user"), "", null)), null,
                    ),
                ),
            )
        }
        val target = HopParams(
            host = s("target_host"), port = s("target_port").toIntOrNull() ?: 22,
            user = s("target_user"), identity = f["target_key"]!!.text.toString(),
            pin = prefs.getString(
                pinKey(profile, HopParams(s("target_host"), s("target_port").toIntOrNull() ?: 22, s("target_user"), "", null)), null,
            ),
        )
        val mode = runCatching { TouchMode.valueOf(prefs.getString("mode", "")!!) }.getOrDefault(TouchMode.TRACKPAD)
        return ConnectParams(
            profile = profile, jumps = jumps, target = target,
            targetSshdHost = s("target_sshd_host"),
            targetSshdPort = s("target_sshd_port").toIntOrNull() ?: 2332,
            lbwPort = s("lbw_port").toIntOrNull() ?: 7878,
            token = f["lbw_token"]!!.text.toString(), mode = mode,
        )
    }

    fun save() = saveProfile()

    private fun saveProfile() {
        val e = prefs.edit()
        e.putString("profile", f["profile"]!!.text.toString().trim())
        for ((k, v) in f) e.putString("profile:${f["profile"]!!.text.toString().trim()}:$k", v.text.toString())
        e.apply()
    }

    private fun loadProfile(name: String) {
        if (name.isEmpty()) return
        for ((k, v) in f) {
            prefs.getString("profile:$name:$k", null)?.let { v.setText(it) }
        }
        f["profile"]!!.setText(name)
    }

    fun applyExtras(x: Bundle) {
        fun es(k: String, fk: String) {
            x.getString(k)?.let { f[fk]!!.setText(it) }
        }
        fun ei(k: String, fk: String) {
            if (x.containsKey(k)) f[fk]!!.setText(x.getInt(k).toString())
        }
        fun eb64(k: String, fk: String) {
            decodeKeyExtra(x.getString(k))?.let { f[fk]!!.setText(it) }
        }
        es("profile", "profile")
        es("jump_host", "jump_host"); ei("jump_port", "jump_port")
        es("jump_user", "jump_user"); es("jump_key", "jump_key")
        eb64("jump_key_b64", "jump_key")
        es("target_host", "target_host"); ei("target_port", "target_port")
        es("target_user", "target_user"); es("target_key", "target_key")
        eb64("target_key_b64", "target_key")
        es("target_sshd_host", "target_sshd_host"); ei("target_sshd_port", "target_sshd_port")
        ei("lbw_port", "lbw_port")
        es("lbw_token", "lbw_token")
        x.getString("mode")?.let { m -> prefs.edit().putString("mode", m.uppercase()).apply() }
    }

    companion object {
        fun pinKey(profile: String, h: HopParams) = "pin:$profile:${h.user}@${h.host}:${h.port}"

        /**
         * Base64-Key-Extra (`adb shell am start` transportiert keine
         * mehrzeiligen `--es`-Werte: die Geräte-Shell sähe jede PEM-Zeile
         * als eigenes Kommando). Reine Funktion, JVM-testbar.
         */
        fun decodeKeyExtra(b64: String?): String? = runCatching {
            b64?.takeIf { it.isNotBlank() }?.let {
                java.util.Base64.getMimeDecoder().decode(it.trim())
                    .toString(Charsets.UTF_8).takeIf { s -> "PRIVATE KEY" in s }
            }
        }.getOrNull()
    }
}
