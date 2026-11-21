# dnb-tracker

Terminal-Tracker für Drum and Bass und Phonk: ratatui-Pattern-Editor,
eigener DSP-Code (Oszillatoren, Biquad, Hüllkurven, Sättigung),
cpal-Ausgabe, lock-freie Kommunikation per `rtrb`.

```sh
sudo apt-get install libasound2-dev     # ALSA-Header (Ubuntu/Debian)
cargo run --release                      # TUI mit DnB-Demo, speichert nach song.txt
cargo run --release -- mysong.txt --demo phonk
cargo run --release -- --demo dnb --play --seconds 20    # ohne TUI
cargo run --release -- --demo phonk --render phonk.wav --seconds 30
cargo run --release -- --list-devices    # Ausgabegerät mit --device <teilstring>
```

## Tasten

| Taste | Funktion |
|---|---|
| `Esc` | Edit-Modus an/aus (aus = nur anspielen, "jam") |
| `z s x d c v g b h n j m` / `q 2 w 3 e r 5 t 6 y 7 u i 9 o 0 p` | Klaviatur (zwei Oktaven) |
| `1` | Note-Off (Notenspalte, Edit-Modus) |
| `0-9 a-f` | Hex in Instrument-/Lautstärke-/Effekt-Argument-Spalte |
| `C D G R K T` | Effektbefehl in Effektspalte |
| `Entf` / `Backspace` | Feld löschen |
| Pfeile, `Tab`/`Shift+Tab`, `PgUp`/`PgDn`, `Home`/`End` | Cursor |
| `Space` / `Enter` | Song abspielen/stoppen / aktuelles Pattern loopen |
| `F1`/`F2` Oktave · `F3`/`F4` Instrument · `F5`/`F6` Pattern · `F7`/`F8` BPM · `F9`/`F10` Step · `F12` Mute |
| `=` / `-` | aktuelles Pattern an Order-Liste anhängen / letzten Eintrag entfernen |
| `Ctrl+←`/`Ctrl+→` | Order-Position |
| `Ctrl+S` / `Ctrl+Q` | Speichern / Beenden |

## Effekte

`Cxx` Cutoff (40 Hz…18 kHz exponentiell) · `Dxx` Drive · `Gxx` Glide
(xx·5 ms) · `Rxx` Retrigger alle x Ticks · `Kxx` Note-Cut nach xx Ticks ·
`Txx` Tempo (BPM hex).

## Tests

```sh
cargo test && cargo clippy --all-targets -- -D warnings && cargo fmt --check
TRACKER_SAMPLES=/pfad/zu/wavs cargo test --test real_samples -- --nocapture
```

Architektur und Hintergründe: `../plan/20261121_01_tracker/walkthrough.md`.
