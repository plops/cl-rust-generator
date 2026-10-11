# Tasks: Interaktiver Viewer — `02_interactive`

Seriell; jeder Schritt endet gruen (Test + `fmt --check` + `clippy`).

## U0 — DeepWiki + Doku-Basis

- [ ] DeepWiki `stephenberry/opus-pure` abfragen (erledigt, s. plan.md
      §5); `source1/deps.md` aktualisieren (Ausnahme streichen,
      `OpusMSDecoder`/`read_packet_into`-Notizen).
- [ ] `plan/20261011_02_interactive/{prompt,plan,task}.md` anlegen.
- [ ] `prompt.txt` + `opus-vis.md` aus 01_init committen (erledigt).

Verifikation: `git log` zeigt Doku-Commits.

## U1 — `render_range` (Implementierung + Tests)

- [ ] `06_render`: `render_range(a, cols, t0, t1, plot_w)` + Wrapper
      `render()`; Achsen relativ; `SICHT`-Zeile bei Zoom;
      `freq_of_bin()` pub.
- [ ] Unit-Tests: Vollbereich byte-identisch zu `render()`; Zoom-Ticks;
      Streifen-Farbe im Bereich.

Verifikation: `cargo test -p opus-vis render` gruen.

## U2 — Viewer (Implementierung + Tests)

- [ ] `09_show`: `View` (zoom/pan/clamp), `hover_text()`,
      `show_interactive()` (dirty-Render, Overlay, Steuerung §2).
- [ ] `08_cli`: Analysis+Cols an Viewer, Hilfe-Text Steuerung.
- [ ] Unit-Tests: View-Math, Hover-Format.

Verifikation: `cargo test --all-features` gruen (33 + neue).

## U3 — HIL-Nachweis Interaktion

- [ ] `xdotool` installieren; `scripts/interact.sh` (xvfb: Rad-Zoom,
      Drag-Pan, Tasten, Screenshots nach /tmp).
- [ ] Screenshots sichten (Zoom wirkt, Panel lesbar, kein Schrott).
- [ ] `smoke.sh`: `interact.sh`-Aufruf mit Skip wenn `xdotool` fehlt.

Verifikation: `interact.sh` + `smoke.sh` gruen.

## U4 — Hygiene + Walkthrough + Commits

- [ ] `cargo fmt`, `clippy -D warnings`, `cargo test --all-features`,
      `cargo upgrade --dry-run` (kein Handlungsbedarf erwartet).
- [ ] `walkthrough.md` (deutsch, Mermaid, Glossar-Ergänzung, §1–4).
- [ ] README-Steuerung + Commits je Task.

Verifikation: alle Checks gruen, `git log` zeigt U0–U4.
