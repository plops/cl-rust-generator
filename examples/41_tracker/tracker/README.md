# dnb-tracker

Terminal-Tracker für Drum and Bass und Phonk: ratatui-Pattern-Editor,
eigener DSP-Code (Oszillatoren, Biquad, Hüllkurven, Sättigung),
cpal-Ausgabe, lock-freie Kommunikation per `rtrb`.

```sh
sudo apt-get install libasound2-dev     # ALSA-Header (Ubuntu/Debian)
cargo run --release                      # TUI mit synth-dnb, speichert nach song.txt
cargo run --release -- mysong.txt --demo synth-phonk
cargo run --release -- --demo synth-dnb --play --seconds 20    # ohne TUI
cargo run --release -- --demo synth-phonk --render phonk.wav --seconds 30
cargo run --release -- songs/mixed_amen_jungle.txt             # braucht samples/, s. u.
cargo run --release -- --list-devices    # Ausgabegerät mit --device <teilstring>
```

## Demos: Synth oder Samples?

Der Header der TUI (und die CLI-Ausgabe `song: … [badge]`) zeigt, woraus ein Song
klingt. Das Badge wird aus den tatsächlich gespielten Instrumenten berechnet
(`Song::sources`), nicht aus dem Namen: `synth`, `samples` oder `synth+samples`.

| Demo | Badge | Herkunft |
|---|---|---|
| `--demo synth-dnb` (Two-Step Reese, 174 BPM) | `synth` | komplett berechnet |
| `--demo synth-phonk` (Cowbell Drift, 140 BPM) | `synth` | komplett berechnet |
| `songs/mixed_amen_jungle.txt` (175 BPM) | `synth+samples` | Amen-Break (Oxx-Chops) + Synth-Reese/Lead |
| `songs/mixed_drift_phonk.txt` (125 BPM) | `synth+samples` | Phonk-Kit-WAVs + Synth-Pad |

Die Sample-Daten sind nicht im Repo (Fremddaten, ~55 MB) und liegen git-ignoriert in `samples/`:

```sh
mkdir -p samples/amen samples/phonk_src
unzip -j rhythm-lab.com_amen_vol.1.zip 'rhythm-lab.com_amen_vol.1/WAV/*' -d samples/amen
git clone --depth 1 https://github.com/GareBear99/Phonk_Producer_Toolkit samples/phonk_src/repo
cargo run --example write_songs      # songs/mixed_*.txt neu erzeugen (optional)
```

Eigene Samples: in der Song-Datei `instr 0A kind=sample sample=pfad.wav root=F-1 name=…`
(`root` = Note, bei der das Sample unverändert klingt; Pfad relativ zur Song-Datei).

## Tasten

| Taste | Funktion |
|---|---|
| `Esc` | Edit-Modus an/aus (aus = nur anspielen, "jam") |
| `z s x d c v g b h n j m` / `q 2 w 3 e r 5 t 6 y 7 u i 9 o 0 p` | Klaviatur (zwei Oktaven) |
| `1` | Note-Off (Notenspalte, Edit-Modus) |
| `0-9 a-f` | Hex in Instrument-/Lautstärke-/Effekt-Argument-Spalte |
| `C D G R K T O` | Effektbefehl in Effektspalte |
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
`Txx` Tempo (BPM hex) · `Oxx` Sample-Start bei Slice xx/32 (ein 2-Takt-Break → 16tel);
`Rxx` in einer Zeile *ohne* Note rollt den laufenden Slice weiter.

## Tests

```sh
cargo test && cargo clippy --all-targets -- -D warnings && cargo fmt --check
TRACKER_SAMPLES=/pfad/zu/wavs cargo test --test real_samples -- --nocapture
```

Werkzeuge: `cargo run --release --example stems -- songs/mixed_amen_jungle.txt /tmp/stems`
rendert jeden Kanal solo; `../plan/20261121_01_tracker/spikes/spectrum.py` misst Bänder.

Architektur und Hintergründe: `../plan/20261121_01_tracker/walkthrough.md`.
