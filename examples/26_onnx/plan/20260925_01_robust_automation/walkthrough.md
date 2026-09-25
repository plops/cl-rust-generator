# Walkthrough — 20260925_01_robust_automation

Robuste Automation in `examples/26_onnx/source6/`: Fuzzy-OCR-Matcher
+ Fenster-Fokus-Automatik + Refactors. Alle Schritte R1–R4/T1 aus
`task.md` sind umgesetzt, alle Gates grün, kein Commit (Repo-Regel).

## Implementiert vs. Plan/Prototyp

- **R1 (`07_match.rs` + Umbenennung):** `norm_ocr_str`,
  `levenshtein_substring`, `fuzzy_ocr_match` wie im Prototyp, inkl.
  beider Prototyp-Tests wortwörtlich (mit `Cancel`/`Confirm`-Negativ).
  `evaluate` nutzt `fuzzy_ocr_match`; Pattern werden roh gespeichert.
  Umbenennung `07_rules`→`08_rules`, `08_tui`→`09_tui`,
  `09_canvas`→`10_canvas` (+ Doku-Verweise, `scripts/README.md`).
- **R2 (Fokus + Klick-Pfad):** `focus_at` (`translate_coordinates` →
  Kind/Root, `set_input_focus(PARENT)`, `_NET_ACTIVE_WINDOW`
  `[1, NOW, 0, 0, 0]`, best-effort), Button-Events mit `(x, y)`,
  `FOCUS_SETTLE_MS = 100`, `Sink::click_and_type`-Default, Engine nutzt
  sie; `CLICK_TYPE_DELAY_MS` + Post-Type-Sleep entfernt.
- **R3 (Refactors):** `norm_rgb`-Helper (02), `load_dict` per
  `trim_matches` (04), `as_u32`-Kettenform (06). Keine neuen Tests —
  bestehende Suite (1:1-Byte-Gate, Dict-Test, TOML-Fehler-Tests) blieb
  grün und ist das Orakel.
- **R4 (`rules.metaai.toml`):** eine `click_and_type`-Regel, Pattern
  `Ask Meta AI...`, Red-Panda-Prompt, Cooldown 5 s. Parse-Nachweis per
  Exit-Code: ohne Display Exit 1 (TOML ok), kaputte TOML Exit 2.

## Abweichungen vom Prototyp (mit Begründung)

1. Modulname `mtch` in `main.rs` (`match` ist Rust-Schlüsselwort;
   Dateiname bleibt `07_match.rs`).
2. `focus_at` mit `let-else`-Early-Returns statt verschachteltem
   `if-let` — gleiche Best-effort-Semantik, flacher lesbar.
3. `click_and_type`-Default referenziert `FOCUS_SETTLE_MS` statt
   hartkodierter 100 (eine Wahrheitsquelle für die Pause).
4. Ignored-XTEST-Test erweitert: läuft über `focus_at` +
   `Sink::click_and_type` (assertet `Ok` + `skipped == 0`).
5. Codebase spricht Deutsch — Kommentare/Fehlermeldungen bleiben
   deutsch statt Prototyp-Englisch; `as_u32`-Kette fasst zwei
   Fehlertexte zu einem zusammen (Tests prüfen nur die Variante).
6. `main.rs` hat 305 Zeilen (+3 durch die `mod mtch`-Deklaration):
   Verdrahtungs-Datei, kein nummeriertes Modul; `~300` gilt für Module.

## Messungen

- `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
  `cargo test`: **39 passed, 0 failed, 1 ignored** (37 alt + 2 neue
  Matcher-Tests; alle alten ohne Anpassung grün).
- Ignored-XTEST-Test unter Xvfb: **ok (0,37 s)** — Fokus-Sequenz,
  Klick, Tippen, `click_and_type` ohne Fehler.
- `smoke_xvfb.sh`: **PASS** (XTEST, OCR-Batch, Fehlerpfade 0/2/1).
- `test_duckai.sh`: **PASS** (Anonymitäts-Hinweis + Witz-Antwort).
- Alle Dateien ≤ 300 Zeilen außer `main.rs` (305, s. oben).

## Docker-Pakete

Unverändert gegenüber dem Vorgängerplan (Xvfb, xterm, Chrome-for-Testing,
scrot, xdotool) — nichts Neues installiert, keine neue Cargo-Dep.

## Learnings

- `pkill -f "Xvfb :97"` trifft die eigene Shell (Pattern steht in der
  eigenen Kommandozeile) — Prozesse per `pgrep -a` verifizieren statt
  blind zu killen.
- Der Exit-Code-Trick (1 = TOML ok / X11 fehlt, 2 = TOML kaputt) macht
  den TOML-Nachweis ohne Display trivial — kein Extra-Harness nötig.
- Die Default-Methode im `Sink`-Trait trägt die 100-ms-Pause an genau
  einer Stelle; `FakeSink` erbt sie (Test dauert 100 ms länger, bleibt
  deterministisch ohne `Instant`-Warterei).

## Erweiterungen (offen, aus den Open Questions)

1. Fokus-Erfolg im Dashboard sichtbar machen (Zähler wie `Skip`)?
   Default bisher: nein (silent best-effort).
2. Toleranz pro Regel per TOML (`max_edits` optional)? Default bisher:
   nein (globale längenproportionale Regel reicht).
3. Manuell mit Bildschirm (optional): Firefox + `rules.metaai.toml`,
   `a` drücken, Platzhalter-Match + Fokus-Wechsel beobachten.
