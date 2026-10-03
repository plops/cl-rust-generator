# Generator für `source8_transpiled`

Dieser Ordner enthält die Common-Lisp-Quellen, aus denen der komplette
Rust-Workspace erzeugt wird (Transpiler:
[cl-rust-generator](../../../../..), `write-source`/`emit-rs`).

## Aufruf (aus dem Repo-Root)

```sh
sbcl --eval '(ql:register-local-projects)' \
     --load examples/29_lowbandwidth/source8_transpiled/gen/gen.lisp --quit
```

Der Lauf ist idempotent und deterministisch: Dateien werden nur bei
Inhaltsänderung neu geschrieben (danach läuft `rustfmt --edition 2024`).

## Dateien

| Datei | Inhalt |
|---|---|
| `00_util.lisp` | Pfade (`s8-path`), `pub_`, `doc`, `testmod`, `+key-table+` (+ `server-key-arms`, `client-key-pairs`), `write-text-file`. Zuerst laden. |
| `common.lisp` | `lbw-common`: Typen, Framing, YUV, `lib.rs`, Manifest (T1). |
| `server.lisp` | `lbw-server`: Manifest, `01_config`, `02_capture`, `03_ocr`, `04_tiles`, `05_av1`, `06_input`, `07_session`, `lib.rs`, `main.rs`, Integrationstests (T2–T4). Eine Datei pro Crate — kein Zeilen-Split (die 600-Zeilen-Regel gilt nur für Rust-Output). |
| `client.lisp` | `lbw-client`: Manifest, Module, `examples/probe.rs`, Tests (T5–T6). |
| `texts.lisp` | Nicht-Rust-Ausgaben (`Cargo.toml`, Skripte, README …). |
| `gen.lisp` | Einstieg: lädt alles, ruft `write-source` je Datei. |

Konventionen: Lisp-`-` → Rust-`_`, `--` → `::`; Bezeichner exakt wie in
Rust schreiben (Readtable `:invert`); Floats, Generics, Lebensdauern,
`self`, Struct-Muster und Let-Chains als Strings.
