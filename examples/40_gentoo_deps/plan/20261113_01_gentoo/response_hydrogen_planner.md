## 1. Datei: `./plan/20261113_01_gentoo/deps.md`



---

## 2. Datei: `./plan/20261113_01_gentoo/plan.md`


---

## 4. Commit-Strategie & Conventional-Commit-Regeln

### Git-Workflow
1. Ausgangspunkt ist immer ein aktueller `master`-Branch.
2. Erstelle einen dedizierten Feature-Branch, z. B.:
   `git checkout -b feature/gentoo-dep-tui`
3. Nach jedem abgeschlossenen **HIL-Nachweis** eines Modus (oder nach Abschluss einer TUI-/Doku-Phase) wird erst **nach bestandenen Gates** (`cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`) sauber committet.

### Format der Commit Messages (verbindlich)
Jeder Commit muss dem **Conventional-Commits-Standard** folgen und eine **umfassende Beschreibung im Body** enthalten:

```text
<type>(<scope>): <kurze Beschreibung im Imperativ, max. 72 Zeichen>

<Body: Absätze mit klarer Struktur>
- Was: Welche Module/Modi wurden implementiert oder geändert?
- Warum: Welche Architekturentscheidung oder Randbedingung liegt zugrunde?
- Tests & Validierung: Welche Host-Tests und welcher HIL-Nachweis (mit konkreten
  Laufzeiten/Zahlen/Befehlen auf dem Gentoo-Host) wurden erfolgreich ausgeführt?
```

**Erlaubte `<type>`-Werte:** `feat`, `fix`, `refactor`, `test`, `docs`, `chore`, `build`.  
**Erlaubte `<scope>`-Beispiele:** `scaffold`, `index`, `graph`, `cascade`, `delta`, `plan`, `verify`, `tui`, `hil`, `docs`.

#### Beispiel-Commit (Referenz für den Agenten)
```text
feat(index): implement repo index with manifest and binhost size loaders

Implementiert die Daten-Pipeline in 01_types.rs bis 05_repo_index.rs inklusive
CLI-Modus `index-check`. Das Laden von metadata/md5-cache erfolgt parallel
über rayon und walkdir hinter einer eigenen RepoIndex-Abstraktion über
portage-metadata und portage-atom.

Entscheidungen:
- Paketversionen werden vorab nach KEYWORDS (amd64) und package.mask
  gefiltert, damit 9999-Live-Ebuilds ohne Manifest/Binpkg ausgeschlossen sind.
- Binhost-SIZE und Manifest-DIST-Größen werden getrennt geführt, um das
  Tarball-vs-Runtime-Problem (z. B. sys-kernel/linux-headers) abzubilden.

Validierung:
- Host-Tests: Synthetisches tempfile-Repo mit Live-Ebuild, Maskierung und
  Manifest-Summen bestanden (`cargo test`).
- Gates: `cargo fmt --check` und `cargo clippy --all-targets -- -D warnings` grün.
- HIL-Nachweis: `index-check` gegen Snapshot des Gentoo-Hosts eingelesen
  (X Pakete in Y ms, 0 Panics).
```

---

## 5. Qualitäts-Gates, HIL-Definition & Stopp-Regel

### Was bedeutet „HIL-Nachweis“ (Host-in-the-Loop) in diesem Projekt?
Da die Engine für echte Gentoo-Docker-Builds deterministisch verlässlich sein muss, bedeutet jeder **HIL-Nachweis**:
1. Ausführen des jeweiligen CLI-Modus gegen **reale Daten des Gentoo-Hosts** (`ssh kiel@localhost -i ~/.ssh/from_android` oder ein gezielt gezogener Read-Only-Snapshot des Gentoo-Trees `/var/db/repos/gentoo`, `/etc/portage`, `/var/lib/portage/world` + Binhost-`Packages` im Container).
2. Wo anwendbar (`graph`, `verify`), Abgleich der Ergebnisse gegen Portages echtes Orakel (`emerge --pretend --emptytree --with-bdeps=n @world`).
3. Regressionstests in `tests/hil_regression.rs` lesen den Pfad über `GENTOO_HIL_REPO` (oder überspringen sauber mit Hinweis, wenn die Umgebungsvariable nicht gesetzt ist, sodass `cargo test` auch ohne Datensatz immer grün bleibt).

### Verbindliche Gates nach jedem Task
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
```

### Abbruch- & Eskalationsregel
Lässt sich ein technisches Problem (z. B. Parser-Bug in Upstream-Crate, unauflösbarer SSH-/Host-Fehler oder Compile-Blockade) in **maximal zwei gezielten Versuchen** nicht lösen, **sofort anhalten**, den exakten Fehlerzustand inkl. Reproduktion in der Ausgabe/im Bericht festhalten und nicht blind weiterarbeiten.


## 3. Datei: `./plan/20261113_01_gentoo/tasks.md`


