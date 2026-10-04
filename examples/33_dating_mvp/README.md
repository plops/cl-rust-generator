# Anti-Tinder MVP

Kennst du das? Dating-Apps, die dich mit Endlos-Swiping bei der Stange
halten wollen — je länger du suchst, desto besser für sie. Dieses Projekt
dreht den Spieß um: **Eine Partnerbörse, die dich möglichst schnell wieder
loswerden will.** Anmelden, ein ehrliches Profil anlegen, jeden Tag
**maximal 5 Vorschläge** bekommen — und wenn es beidseitig funkt, geht es
per Signal weiter. Mission erfüllt, App vergessen.

Das steckt dahinter:

* **Matching statt Swiping:** Ein Score von 0–100 % aus Persönlichkeit
  ([MBTI](https://de.wikipedia.org/wiki/Myers-Briggs-Typenindikator)-Typ),
  Hobbys und Lebenszielen (z. B. Familienplanung als Dealbreaker).
* **Tresor statt Glaskasten:** Einkommen, Vermögen & Co. liegen
  **verschlüsselt** in der Datenbank und dienen nur dem Algorithmus —
  andere sehen dafür nur ein 🔒-Symbol.
* **Kein Chat, kein Spam:** Kein In-App-Chat (Signal übernimmt), dafür
  10-€-Einmalhürde gegen Bots und ein Spam-Filter für Profiltexte.

Stack: Rust mit `axum` (Web) + `sqlx`/PostgreSQL (Datenbank) +
`askama`/`HTMX` (HTML, das sich ohne Neuladen aktualisiert). In 5 Minuten
läuft das bei dir lokal — versprochen, siehe [Schnellstart](#schnellstart).

## Dein Weg als Nutzer

Stell dir Anna vor: Sie registriert sich, zahlt einmalig 10 € im Testmodus
(es fließt kein echtes Geld), legt ihr Profil an — und findet am nächsten
Morgen 5 Vorschläge vor. Einen likt sie, er likt zurück, und schon steht
sein Signal-Kontakt da. So sieht der Weg aus:

```mermaid
flowchart TD
    A["1. Registrieren"] --> B["2. Paywall\n10 Euro, Testmodus"]
    B --> C["3. Profil anlegen\nplus Tresor-Felder"]
    C --> D{"AI-Check sauber?"}
    D -- "nein, z.B. Werbelink" --> C
    D -- ja --> E["4. Top 5 Matches\nHTMX lädt die Cards"]
    E --> F["5. Liken"]
    F --> H{"Gegen-Like?"}
    H -- nein --> E
    H -- ja --> I["6. Chat via Signal"]
```

## Wie es unter der Haube aussieht

Drei Akteure, klare Rollen: Der Browser redet mit dem Axum-Server (in
Produktion über nginx als Türsteher mit TLS-Verschlüsselung), der Server
mit PostgreSQL. Ein Hintergrund-Job rechnet einmal täglich die Top 5 pro
Nutzer aus:

```mermaid
flowchart LR
    B["Browser\nHTMX plus PicoCSS"] --> N["nginx, nur Prod\nTLS-Ende und Proxy"]
    N --> A["Axum auf Port 3000\nRouten und Templates"]
    B -. "lokal direkt" .-> A
    A <--> PG[("PostgreSQL\nTresor-Felder verschlüsselt")]
    A --> J["Top-5-Job\nalle 24 Stunden"]
    J --> PG
```

## Beispiel: Woher kommen die 60 %?

Anna (30, INFJ, klettert und kocht gern) und Ben (32, ENFP, klettert und
liest gern), beide mit Kinderwunsch. Der Algorithmus rechnet so:

| Baustein | Rechnung | Punkte |
|---|---|---|
| Dealbreaker | Alter ok (±10), Wunsch gegenseitig, Familienplanung gleich → weiter | — |
| MBTI | INFJ und ENFP sind beide „Diplomaten“ (NF-Gruppe) | 28/35 |
| Hobbys | Gemeinsam: {Klettern} von {Klettern, Kochen, Lesen} = ⅓ (Jaccard-Anteil) | 12/35 |
| Tresor-Passung | Erwartung „egal“ + benachbarte Vermögensstufen | 20/30 |
| **Gesamt** | | **60 %** |

Der Score ist übrigens **richtungabhängig**: Was Anna von Ben erwartet,
zählt aus ihrer Sicht — umgekehrt kann es anders aussehen. Genau richtig
für einseitige Wünsche.

## Voraussetzungen

* Rust-Toolchain ( Edition 2024, `cargo` ) — Build & Server
* Docker — PostgreSQL 16 als Container
* `uv` — nur für die E2E-Browser-Tests

## Schnellstart

**1. Datenbank starten:**

```bash
docker run -d --name dating_mvp_db \
  -e POSTGRES_USER=dating \
  -e POSTGRES_PASSWORD=dating_secret_dev \
  -e POSTGRES_DB=dating_mvp \
  -p 5432:5432 postgres:16-alpine
```

Alternativ (mit Compose-Plugin): `docker compose up -d db`.
Hinweis: Läuft deine Shell selbst in einem Container, ist der DB-Host
nicht `localhost`, sondern die Container-IP (z. B. `172.17.0.3`, siehe
`docker inspect dating_mvp_db`).

**2. Umgebung konfigurieren:**

```bash
cp -n .env.example .env
python3 -c "import secrets,base64;print(base64.b64encode(secrets.token_bytes(32)).decode())"
# Ausgabe als DATA_ENCRYPTION_KEY in .env eintragen
```

**3. Server starten (Migrationen laufen automatisch):**

```bash
cargo run
```

Wenn alles klappt, siehst du u. a. diese Zeilen — dann
**http://localhost:3000 öffnen**:

```text
INFO dating_mvp: database connected, migrations applied
INFO dating_mvp: listening addr=0.0.0.0:3000
```

**4. Werde Anna:** Spiele den [Weg von oben](#dein-weg-als-nutzer) einmal
selbst durch — registrieren, im Testmodus „zahlen“, Profil mit ein paar
🔒-Feldern anlegen. Tipp für den vollen Effekt: Lege in einem zweiten
Browser(fenster) Ben an (männlich, sucht weiblich, ähnliche Hobbys), dann
seht ihr euch gegenseitig in den Matches. Bei Gegenliebe erscheint
**„Chat via Signal“** mit den Kontakten beider Seiten.

## Tests

```bash
cargo test                                   # 37 Unit-Tests (Scoring, Crypto, Modelle, …)
TEST_DATABASE_URL=postgres://dating:dating_secret_dev@localhost:5432/dating_mvp_test cargo test db::
cargo fmt --check && cargo clippy --all-targets -- -D warnings

cd e2e_tests
uv run playwright install chromium          # einmalig (ggf. danach: uv run playwright install-deps chromium)
E2E_DATABASE_URL=postgres://dating:dating_secret_dev@localhost:5432/dating_mvp_test uv run pytest
```

Die E2E-Suite startet einen eigenen Server auf Port 3101 mit separater
Test-Datenbank und prüft 5 Browser-Flows: Registrierung/Paywall/Login,
Tresor-Geheimhaltung, HTMX-Matches, Signal-Austausch, Spam-Ablehnung.
Grün sieht so aus:

```text
test result: ok. 37 passed; 0 failed     # cargo test
.....                           [100%]   # pytest
5 passed in 8.05s
```

## Konfiguration (`.env`)

| Variable | Default | Bedeutung |
|---|---|---|
| `DATABASE_URL` | — (Pflicht) | Postgres-Verbindung |
| `DATA_ENCRYPTION_KEY` | — (Pflicht) | 32 Bytes als Base64; verschlüsselt Tresor-Felder |
| `PORT` | `3000` | Server-Port |
| `SESSION_SECRET` | Dev-Wert | HMAC-Signatur der Login-Cookies (prod: rotieren!) |
| `RATE_LIMIT_PER_SECOND` / `RATE_LIMIT_BURST` | `10` / `30` | Rate-Limit pro IP |
| `MATCH_JOB_INTERVAL_HOURS` | `24` | Takt des Top-5-Jobs |
| `AI_MODE` | `mock` | `mock` oder `http` (LLM via `AI_API_URL`/`AI_API_KEY`) |
| `STRIPE_SECRET_KEY` | — (Mock) | Echter Key schaltet später den Echt-Checkout scharf |
| `RUST_LOG` / `LOG_FORMAT` | `info` | Logging; `LOG_FORMAT=json` für Prod |

## Projektstruktur

```text
src/            nummerierte Module (01_config … 12_routes, via #[path] eingebunden)
templates/      Askama-HTML (HTMX-Partial für Matches, PicoCSS)
static/         htmx.min.js (lokal vendored, kein CDN nötig)
migrations/     SQL-Schema (users, profiles, likes, daily_matches)
e2e_tests/      uv-Projekt mit Playwright-Suite
deps.md         Abhängigkeiten in org/project-Notation
plan/20261004_01_init/  plan.md, task.md, walkthrough.md, nginx.conf-Beispiel
```

Details stehen in [plan.md](plan/20261004_01_init/plan.md) und
[plan/walkthrough.md](plan/20261004_01_init/walkthrough.md).

## Variante: PostgreSQL nativ auf dem Server (ohne Docker)

Auf einem Hetzner-Server (Ubuntu/Debian) brauchst du kein Docker für die
Datenbank — Postgres läuft auch direkt als Systemdienst. Die App merkt
keinen Unterschied, nur die `DATABASE_URL` zeigt auf `localhost`.

**Einrichtung (Single-Server, App + DB auf einer Maschine):**

```bash
sudo apt update && sudo apt install -y postgresql
sudo -u postgres psql -c "CREATE USER dating WITH PASSWORD 'starkes-passwort-hier';"
sudo -u postgres psql -c "CREATE DATABASE dating_mvp OWNER dating;"
sudo -u postgres psql -c "CREATE DATABASE dating_mvp_test OWNER dating;"
```

Danach in `.env`:

```text
DATABASE_URL=postgres://dating:starkes-passwort-hier@localhost:5432/dating_mvp
```

Das war's — Migrationen legt die App beim Start selbst an. Zwei Dinge
noch für Prod: `.env` per `chmod 600 .env` absichern und ein
Backup einrichten (z. B. nächtlicher `pg_dump` per Cron +
`unattended-upgrades` für Sicherheitsupdates). Lauscht Postgres nur auf
`localhost` (Debian-Default), ist es von außen nicht erreichbar — genau
richtig für Single-Server.

**Vor- und Nachteile im Vergleich:**

| Aspekt | Nativ (Systemdienst) | Docker (`postgres:16-alpine`) |
|---|---|---|
| Setup | `apt install`, Hetzner-Standardweg, kein Daemon nötig | Ein Befehl, Version gepinnt, überall gleich |
| Dev/Prod-Parität | Schlechter: lokal evtl. Docker, prod nativ → Drift möglich | Sehr gut: identisches Image überall |
| Updates | `unattended-upgrades`, aber Major-Upgrades (z. B. PG 16→17) brauchen `pg_upgrade` + Planung | Image-Tag wechseln, Container neu ziehen |
| Isolation | Keine: teilt sich alles mit dem System, keine Limits | Eigener Container, leicht zurückzusetzen/neu aufzusetzen |
| Backups | Bordmittel (`pg_dump`, `barman`), viele Anleitungen | Gleicher Dump **plus** Volume-Disziplin (`dating_mvp_pgdata` sichern!) |
| Mehrere Instanzen | Aufwendig (zweite Cluster-Version, Ports) | Trivial (weiterer Container) |
| Overhead | Keiner | Vernachlässigbar (eine Schicht mehr beim Debuggen) |

**Empfehlung:** Für dieses MVP als Solo-Projekt auf einer Hetzner-Maschine
ist **nativ** der wartungsärmste Weg (weniger Moving Parts, Updates kommen
mit dem System). Sobald mehrere Umgebungen, exakte Reproduzierbarkeit oder
Team-Setups wichtig werden, gewinnt **Docker** durch die deklarative,
gepinnte Umgebung (`docker-compose.yml` liegt bei). Der Schnellstart oben
nutzt Docker, damit er auf jedem Rechner identisch läuft.

## Produktion: nginx + HTTPS

Prinzip: **nginx terminiert TLS** (Let's-Encrypt-Zertifikate via
Certbot) und proxyt auf die App unter `http://127.0.0.1:3000`. Die App
selbst spricht nur HTTP — das reicht hinter dem Proxy.

Minimaler Serverblock (`/etc/nginx/sites-available/dating`, Domain
anpassen, dann `certbot --nginx -d deine-domain.de` für die
`ssl_certificate`-Zeilen):

```nginx
server {
    listen 80;
    listen [::]:80;
    server_name deine-domain.de;
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl;
    listen [::]:443 ssl;
    server_name deine-domain.de;

    ssl_certificate /etc/letsencrypt/live/deine-domain.de/fullchain.pem; # managed by Certbot
    ssl_certificate_key /etc/letsencrypt/live/deine-domain.de/privkey.pem; # managed by Certbot

    location / {
        proxy_pass http://127.0.0.1:3000;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

Danach: `nginx -t && systemctl reload nginx`.

Ein **reales, größeres Beispiel** (HTTP→HTTPS-Redirects, gzip,
GeoIP-Header, mehrere Subdomains) liegt unter
[plan/20261004_01_init/nginx.conf](plan/20261004_01_init/nginx.conf) —
dort findest du u. a. das Redirect-Muster mit Certbot-Kommentaren und
die Proxy-Header, die auch der Block oben nutzt.

**Prod-Hinweise, bitte nicht überspringen:**

* **Port 3000 nicht öffentlich machen** — die App lauscht auf
  `0.0.0.0:3000`. Hinter nginx per Firewall/Security-Group nur
  `127.0.0.1` zulassen (oder die Bind-Adresse in `main.rs` einengen).
* **Rate-Limit hinterm Proxy:** Das Limit sieht per Default nur die
  Proxy-IP (`127.0.0.1`), alle Nutzer teilen sich also einen Bucket.
  Follow-up im Code: auf `SmartIpKeyExtractor` umstellen und in nginx
  `X-Forwarded-For $remote_addr` setzen (nicht appenden — sonst kann
  jeder das Limit mit eigenem Header umgehen; siehe Kommentar am
  `/karte`-Block der Beispiel-Config).
* **Cookies härten:** `Secure`-Flag und CSRF-Token sind für Prod
  einzuplanen (MVP: `HttpOnly` + `SameSite=Lax`).
* **Secrets sichern:** `DATA_ENCRYPTION_KEY` geht nie verloren —
  Verlust macht alle Tresor-Daten unlesbar. `SESSION_SECRET` in Prod
  rotieren, `LOG_FORMAT=json` setzen.
