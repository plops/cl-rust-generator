// 06_VirtualKeybar — row of keys missing on soft keyboards + sticky modifiers.
//
// Ctrl/Alt/Shift/Super: one tap arms the modifier for the next key, a second
// tap locks it, a third releases it. The bar also carries app actions
// (keyboard, mouse mode, paste, HUD, fit).
package de.lbw.client

import android.content.Context
import android.content.res.ColorStateList
import android.graphics.Color
import android.graphics.drawable.GradientDrawable
import android.graphics.drawable.RippleDrawable
import android.view.Gravity
import android.view.View
import android.widget.Button
import android.widget.HorizontalScrollView
import android.widget.LinearLayout

/** Sticky modifier state (pure, JVM-tested). Bits = CoreBridge.MOD_*. */
class StickyMods {
    var armed = 0
        private set
    var locked = 0
        private set

    val active get() = armed or locked

    /** Tap on a modifier key: off → armed → locked → off. */
    fun tap(bit: Int) {
        when {
            locked and bit != 0 -> locked = locked and bit.inv()
            armed and bit != 0 -> {
                armed = armed and bit.inv()
                locked = locked or bit
            }
            else -> armed = armed or bit
        }
    }

    /** Modifiers for the next key; armed ones are used up. */
    fun consume(): Int {
        val m = active
        armed = 0
        return m
    }

    /** Android meta state for [CoreBridge.nativeAndroidKey]. */
    fun toMeta(mods: Int): Int {
        var m = 0
        if (mods and CoreBridge.MOD_SHIFT != 0) m = m or 0x1
        if (mods and CoreBridge.MOD_ALT != 0) m = m or 0x2
        if (mods and CoreBridge.MOD_CTRL != 0) m = m or 0x1000
        if (mods and CoreBridge.MOD_SUPER != 0) m = m or 0x1_0000
        return m
    }
}

object Keysym {
    const val ESC = 0xff1b
    const val TAB = 0xff09
    const val BACKSPACE = 0xff08
    const val RETURN = 0xff0d
    const val LEFT = 0xff51
    const val UP = 0xff52
    const val RIGHT = 0xff53
    const val DOWN = 0xff54
    const val HOME = 0xff50
    const val END = 0xff57
    const val PAGE_UP = 0xff55
    const val PAGE_DOWN = 0xff56
    const val INSERT = 0xff63
    const val DELETE = 0xffff
    fun f(n: Int) = 0xffbe + (n - 1)
}

/** Actions of the bar that are not plain keys. */
interface KeybarActions {
    fun key(keysym: Int)
    fun toggleKeyboard()
    fun cycleMode(): TouchMode
    fun paste()
    fun toggleHud()
    fun fit()
    fun wheel(dy: Int)
    fun modsChanged()
}

class VirtualKeybar(ctx: Context, private val mods: StickyMods, private val act: KeybarActions) :
    HorizontalScrollView(ctx) {

    private val row = LinearLayout(ctx).apply { orientation = LinearLayout.HORIZONTAL }
    private val modButtons = mutableListOf<Pair<Button, Int>>()
    private val modeButton: Button
    private var fnPage = false
    private val fnButtons = mutableListOf<Button>()

    init {
        isHorizontalScrollBarEnabled = false
        setBackgroundColor(Color.rgb(20, 20, 26))
        addView(row)
        button("⌨") { act.toggleKeyboard() }
        modeButton = button(modeLabel(TouchMode.TRACKPAD)) { modeButton.text = modeLabel(act.cycleMode()) }
        button("Esc") { act.key(Keysym.ESC) }
        button("Tab") { act.key(Keysym.TAB) }
        mod("Ctrl", CoreBridge.MOD_CTRL)
        mod("Alt", CoreBridge.MOD_ALT)
        mod("⇧", CoreBridge.MOD_SHIFT)
        mod("❖", CoreBridge.MOD_SUPER)
        button("←") { act.key(Keysym.LEFT) }
        button("↓") { act.key(Keysym.DOWN) }
        button("↑") { act.key(Keysym.UP) }
        button("→") { act.key(Keysym.RIGHT) }
        button("⤒") { act.wheel(1) }
        button("⤓") { act.wheel(-1) }
        button("Fn") { toggleFn() }
        for (n in 1..12) fnButtons += button("F$n") { act.key(Keysym.f(n)) }
        button("Pos1") { act.key(Keysym.HOME) }
        button("Ende") { act.key(Keysym.END) }
        button("Bild↑") { act.key(Keysym.PAGE_UP) }
        button("Bild↓") { act.key(Keysym.PAGE_DOWN) }
        button("Einf") { act.key(Keysym.INSERT) }
        button("Entf") { act.key(Keysym.DELETE) }
        button("Einfügen") { act.paste() }
        button("⤢") { act.fit() }
        button("HUD") { act.toggleHud() }
        fnButtons.forEach { it.visibility = View.GONE }
        refresh()
    }

    private fun modeLabel(m: TouchMode) = when (m) {
        TouchMode.TRACKPAD -> "Trackpad"
        TouchMode.DIRECT -> "Direkt"
        TouchMode.SELECT -> "Auswahl"
    }

    fun setMode(m: TouchMode) {
        modeButton.text = modeLabel(m)
    }

    private fun toggleFn() {
        fnPage = !fnPage
        fnButtons.forEach { it.visibility = if (fnPage) View.VISIBLE else View.GONE }
    }

    private fun button(label: String, onClick: () -> Unit): Button {
        val b = Button(context).apply {
            text = label
            isAllCaps = false
            minWidth = 0
            minimumWidth = 0
            minHeight = 0
            minimumHeight = 0
            textSize = 14f
            gravity = Gravity.CENTER
            setPadding(dp(10), dp(6), dp(10), dp(6))
            setTextColor(Color.WHITE)
            background = keyBackground(KEY_COLOR)
            // Keep the IME open: the bar must never take the focus
            isFocusable = false
            setOnClickListener { onClick() }
        }
        row.addView(b, LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, dp(40)).apply { setMargins(dp(2), dp(3), dp(2), dp(3)) })
        return b
    }

    private fun mod(label: String, bit: Int) {
        val b = button(label) {
            mods.tap(bit)
            refresh()
            act.modsChanged()
        }
        modButtons += b to bit
    }

    /** Updates modifier highlighting (armed = blue, locked = orange). */
    fun refresh() {
        for ((b, bit) in modButtons) {
            b.background = keyBackground(
                when {
                    mods.locked and bit != 0 -> Color.rgb(230, 140, 20)
                    mods.armed and bit != 0 -> Color.rgb(40, 110, 230)
                    else -> KEY_COLOR
                },
            )
        }
    }

    // Explicit drawable: theme button tints live inside the default drawable and
    // are lost as soon as backgroundTintList is touched (blank white keys).
    private fun keyBackground(color: Int) = RippleDrawable(
        ColorStateList.valueOf(Color.argb(90, 255, 255, 255)),
        GradientDrawable().apply { cornerRadius = dp(4).toFloat(); setColor(color) },
        null,
    )

    private fun dp(v: Int) = (v * resources.displayMetrics.density).toInt()

    private companion object {
        val KEY_COLOR = Color.rgb(70, 70, 78)
    }
}
