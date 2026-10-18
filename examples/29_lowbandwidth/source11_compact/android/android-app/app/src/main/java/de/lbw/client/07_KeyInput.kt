// 07_KeyInput — soft keyboard (InputConnection) and hardware keys → Core.
//
// Special keys go through the Rust keymap (nativeAndroidKey); everything
// else is sent as a character. Sticky modifiers from the keybar apply to
// the next key or character and are then used up.
package de.lbw.client

import android.text.InputType
import android.view.KeyEvent
import android.view.View
import android.view.inputmethod.BaseInputConnection
import android.view.inputmethod.EditorInfo
import android.view.inputmethod.InputConnection

class KeyInput(private val core: () -> Core?, private val mods: StickyMods, private val onModsUsed: () -> Unit) {

    fun key(name: String) {
        core()?.key(name)
        mods.consume()
        onModsUsed()
    }

    /** Text from the IME or a hardware key, with sticky modifiers applied. */
    fun typeText(s: String, extraMods: Int = 0) {
        val c = core() ?: return
        var i = 0
        while (i < s.length) {
            var cp = s.codePointAt(i)
            i += Character.charCount(cp)
            val m = mods.consume() or extraMods
            if (m != 0 && m and COMBO == 0) cp = Character.toUpperCase(cp)
            c.char(cp)
        }
        onModsUsed()
    }

    /** IME without suggestions/composition: every commit is sent at once. */
    fun connection(view: View, out: EditorInfo): InputConnection {
        out.inputType = InputType.TYPE_CLASS_TEXT or InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD or
            InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS
        out.imeOptions = EditorInfo.IME_FLAG_NO_FULLSCREEN or EditorInfo.IME_FLAG_NO_EXTRACT_UI or
            EditorInfo.IME_ACTION_NONE
        return object : BaseInputConnection(view, false) {
            override fun commitText(text: CharSequence, newCursorPosition: Int): Boolean {
                typeText(text.toString())
                return true
            }

            override fun deleteSurroundingText(beforeLength: Int, afterLength: Int): Boolean {
                repeat(beforeLength.coerceAtLeast(1)) { key(Keysym.BACKSPACE) }
                return true
            }

            override fun performEditorAction(actionCode: Int): Boolean {
                key(Keysym.RETURN)
                return true
            }
        }
    }

    /** Hardware/IME key event; false = not ours (Back, volume, modifiers). */
    fun onKeyDown(keyCode: Int, e: KeyEvent): Boolean {
        val c = core() ?: return false
        if (keyCode == KeyEvent.KEYCODE_BACK || keyCode == KeyEvent.KEYCODE_VOLUME_UP ||
            keyCode == KeyEvent.KEYCODE_VOLUME_DOWN || KeyEvent.isModifierKey(keyCode)
        ) {
            return false
        }
        if (c.androidKey(keyCode, e.metaState or mods.toMeta(mods.active))) {
            mods.consume()
            onModsUsed()
            return true
        }
        val plainMeta = e.metaState and
            (KeyEvent.META_CTRL_MASK or KeyEvent.META_ALT_MASK or KeyEvent.META_META_MASK).inv()
        val cp = e.getUnicodeChar(plainMeta)
        if (cp <= 0) return false
        typeText(String(Character.toChars(cp)), metaMods(e.metaState))
        return true
    }

    private fun metaMods(meta: Int): Int {
        var m = 0
        if (meta and KeyEvent.META_CTRL_ON != 0) m = m or COMBO_CTRL
        if (meta and KeyEvent.META_ALT_ON != 0) m = m or COMBO_ALT
        if (meta and KeyEvent.META_META_ON != 0) m = m or COMBO_META
        return m
    }

    companion object {
        private const val COMBO_CTRL = 2
        private const val COMBO_ALT = 4
        private const val COMBO_META = 8
        private const val COMBO = COMBO_CTRL or COMBO_ALT or COMBO_META
    }
}
