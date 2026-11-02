//! `sar_gui_server` — lokaler Test-Prototyp (Schritte 2, 4–5, GUI Option A).
//!
//! Entscheidung: Das Frontend bleibt als String-Konstante in dieses Binary
//! eingebettet (keine separaten `static/`-Dateien), damit der Prototyp aus
//! genau einer Datei besteht und ohne Asset-Pfade läuft. Das Schritt-3-Layout
//! (links Leaflet-Karte via CDN/OSM, rechts Bedienfeld) ist in `INDEX_HTML`
//! umgesetzt; Leaflet/OSM werden erst zur Laufzeit aus dem Netz geladen.
//! Ohne Netz bleibt die Karte leer (Hinweis im GUI), Koordinateneingabe und
//! alle API-Funktionen bleiben verfügbar. Kein Framework/Bundler, nur
//! Vanilla-JS mit `setInterval`-Polling auf `GET /api/jobs/{id}`.
//!
//! Stand: Server-Skeleton mit `GET /` und `GET /api/status` plus
//! `POST /api/locate` (Schritt 4), `POST /api/focus` und
//! `GET /api/jobs/{id}` (Schritt 5, Job-System mit Burst-Cache),
//! `GET /api/chip/{id}` als Chip-PNG mit Fadenkreuz (Schritt 6),
//! `GET /api/compare` für den Master/Slave-Vergleich (Drift über die
//! Chip-Mediane, Ampel via `geoloc::bewerte_zeitreihe`) und
//! `GET /api/report` (Schritt 7, `geoloc::wochenbericht`-Text aus den
//! abgeschlossenen Fokus-Jobs). Die Routen nutzen
//! `geoloc::locate_asset`, `ingest::load_l0_dataset`,
//! `rda::focus_tops_burst_cpu` sowie `tops_alpha`/`focused_line`
//! auf den hier konfigurierten Datensätzen (`--master`/`--slave`).
//!
//! Nur `std` + `sar_source2` + `image` (Chip-PNG), kein Web-Framework.
//! Bindet nur Loopback. Keine Persistenz: Neustart verwirft Jobs und Cache.

use image::ImageEncoder;
use image::codecs::png::{CompressionType, FilterType, PngEncoder};
use sar_source2::geoloc::{
    AmpelStatus, HgtTile, ReportEintrag, bewerte_zeitreihe, chip_stats, focused_line, locate_asset,
    parse_poeorb_eof, tops_alpha, wochenbericht,
};
use sar_source2::ingest::{L0Dataset, load_l0_dataset};
use sar_source2::rda::focus_tops_burst_cpu;
use sar_source2::types::{BurstBlock, Complex32};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Cursor, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::Duration;

/// Taiwan-Master als Default-Datensatz (relativ zum Startverzeichnis).
const DEFAULT_DATASET: &str = "../data/insar/S1C_IW_RAW__0SDV_20260928T215149_20260928T215222_009653_01336F_D2EE.SAFE/s1c-iw-raw-s-vv-20260928t215149-20260928t215222-009653-01336f.dat";

/// Standard-Chip-Fenster (Halbbreiten): ±16 Zeilen × ±20 Spalten.
/// Pro Fokus-Job über `halfAz`/`halfRg` im UI editierbar (Schritt 6).
const DEFAULT_HALF_AZ: usize = 16;
const DEFAULT_HALF_RG: usize = 20;

/// Obergrenze für editierbare Chip-Halbbreiten (Bindung an vernünftige
/// PNG-Größen; der Chip wird am Bildrand beschnitten).
const MAX_HALF: usize = 200;

/// Standard-Schwellen [dB] für die Vergleichs-Ampel (Schritt 6, Prototyp —
/// vor Produktiveinsatz an beschrifteten Zeitreihen kalibrieren, siehe
/// `geoloc::bewerte_zeitreihe`).
const DEFAULT_WARN_DB: f64 = 3.0;
const DEFAULT_ALARM_DB: f64 = 6.0;

const INDEX_HTML: &str = r##"<!DOCTYPE html>
<html lang="de">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>SAR GUI Prototyp</title>
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
<style>
body { font-family: system-ui, sans-serif; margin: 0; color: #222; }
header, footer { padding: 0.6rem 1rem; background: #f2f2f2; }
header h1 { font-size: 1.2rem; margin: 0; }
main { display: flex; gap: 1rem; padding: 1rem; }
#karte { flex: 3; min-width: 0; }
#map { height: 480px; border: 1px solid #999; background: #e8e8e8; }
#panel { flex: 2; min-width: 16rem; }
#panel section { border: 1px solid #ccc; padding: 0.6rem; margin-bottom: 0.8rem; }
label { display: block; margin: 0.3rem 0; }
input { width: 10rem; }
table { border-collapse: collapse; width: 100%; font-size: 0.85rem; }
th, td { border: 1px solid #bbb; padding: 0.2rem 0.4rem; text-align: right; }
tr.bester { background: #e6f4e6; }
#fehler { color: #a00; }
#status { font-weight: bold; }
#chipbild { max-width: 100%; image-rendering: pixelated; border: 1px solid #999; background: #000; }
.vergleich-aus { opacity: 0.45; }
.hinweis { font-size: 0.85rem; color: #555; }
fieldset { border: 1px solid #ccc; margin-bottom: 0.8rem; }
@media (max-width: 50rem) { main { flex-direction: column; } }
</style>
</head>
<body>
<header>
<h1>SAR GUI Prototyp (Test/Demo, keine Produktions-Web-App)</h1>
<p>Hinweis: Jobs und Cache verfallen bei Neustart des Servers.</p>
</header>
<main>
<section id="karte" aria-label="Karte">
<div id="map"></div>
<p id="maphinweis" hidden>Karte kann gerade nicht geladen werden (z.&nbsp;B. offline).
Koordinateneingabe und alle Funktionen bleiben verfügbar.</p>
</section>
<aside id="panel">
<section aria-label="Status">
<h2>Status: <span id="status">lädt …</span></h2>
<p id="serverinfo"></p>
<p id="fehler" role="alert"></p>
</section>
<section aria-label="Asset">
<h2>Punkt-Asset</h2>
<label>Breite (lat, -90…90): <input id="lat" type="number" step="any" value="22.62"></label>
<label>Länge (lon, -180…180): <input id="lon" type="number" step="any" value="120.31"></label>
<label>Höhe in m (leer = DEM-Vorschlag): <input id="hoehe" type="number" step="any" placeholder="leer = DEM-Vorschlag"></label>
<fieldset><legend>Datensatz (getrennt lokalisieren/fokussieren)</legend>
<label><input id="ds-master" type="radio" name="datensatz" value="master" checked> Master</label>
<label><input id="ds-slave" type="radio" name="datensatz" value="slave"> Slave</label>
<span id="slavehinweis" class="hinweis"></span>
</fieldset>
<label>Name für Report: <input id="assetname" type="text" value="Kaohsiung-Hafen"></label>
<p>
<button id="btn-lokalisieren" type="button">Lokalisieren</button>
<button id="btn-demo" type="button">Demo: Kaohsiung 22.62/120.31</button>
<button id="btn-neu" type="button">Neues Asset</button>
</p>
</section>
<section aria-label="Burst-Kandidaten">
<h2>Burst-Kandidaten</h2>
<p id="kandidatenhinweis">Noch keine Lokalisierung.</p>
<table id="tabelle" hidden>
<thead><tr><th>#</th><th>Swath</th><th>Burst</th><th>az</th><th>rg</th>
<th>tZd (s)</th><th>Inzidenz (°)</th><th>Layover?</th><th></th></tr></thead>
<tbody id="tbody"></tbody>
</table>
</section>
<section aria-label="Fokus-Job">
<h2>Fokus-Job</h2>
<label>Chip ±Zeilen (halfAz): <input id="halfaz" type="number" step="1" min="1" max="200" value="16"></label>
<label>Chip ±Spalten (halfRg): <input id="halfrg" type="number" step="1" min="1" max="200" value="20"></label>
<p id="jobstatus">Kein Job gestartet.</p>
<pre id="jobdetail"></pre>
</section>
<section aria-label="Chip-Anzeige">
<h2>Chip-Anzeige</h2>
<p><img id="chipbild" alt="Chip-Ausschnitt (Slant-Range)" hidden></p>
<p id="chiplegende" class="hinweis">Noch kein Chip.</p>
<p class="hinweis">Chip-Ausschnitt in Slant-Range-Geometrie (Azimut × Schrägentfernung),
Fadenkreuz = fokussierte Mitte — kein Karten-Overlay.</p>
</section>
<section id="vergleich" aria-label="Master-Slave-Vergleich">
<h2>Master/Slave-Vergleich</h2>
<p class="hinweis">Prototyp: Schwellen nicht kalibriert (Warnung 3 dB / Alarm 6 dB,
editierbar). Drift = Slave-Median minus Master-Median; Ampel nach
<code>bewerte_zeitreihe</code> mit Median-Basis.</p>
<label>Warnung [dB]: <input id="warn" type="number" step="any" value="3"></label>
<label>Alarm [dB]: <input id="alarm" type="number" step="any" value="6"></label>
<p><button id="btn-vergleich" type="button">Vergleichen</button></p>
<p id="vergleichstatus">Noch kein Vergleich.</p>
<pre id="vergleichdetail"></pre>
</section>
<section aria-label="Wochenreport">
<h2>Wochenreport</h2>
<p><button id="btn-report" type="button">Report laden</button></p>
<pre id="reporttext">Noch kein Report.</pre>
</section>
</aside>
</main>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
(function () {
"use strict";
var latEl = document.getElementById("lat");
var lonEl = document.getElementById("lon");
var hEl = document.getElementById("hoehe");
var statusEl = document.getElementById("status");
var fehlerEl = document.getElementById("fehler");
var infoEl = document.getElementById("serverinfo");
var hinweisEl = document.getElementById("kandidatenhinweis");
var tabelleEl = document.getElementById("tabelle");
var tbodyEl = document.getElementById("tbody");
var jobstatusEl = document.getElementById("jobstatus");
var jobdetailEl = document.getElementById("jobdetail");
var chipbildEl = document.getElementById("chipbild");
var chiplegendeEl = document.getElementById("chiplegende");
var vergleichEl = document.getElementById("vergleich");
var vergleichstatusEl = document.getElementById("vergleichstatus");
var vergleichdetailEl = document.getElementById("vergleichdetail");
var reporttextEl = document.getElementById("reporttext");
var slaveHinweisEl = document.getElementById("slavehinweis");
var karte = null, marker = null, pollTimer = null, treffer = [];
var letzterDatensatz = "master";
var letzteJobs = { master: null, slave: null };
var slaveVerfuegbar = false;

function setzeStatus(t) { statusEl.textContent = t; }
function zeigeFehler(t) { fehlerEl.textContent = t || ""; }
function zahlOderLeer(el) {
  var v = el.value.trim();
  if (v === "") return null;
  var n = Number(v);
  return isFinite(n) ? n : NaN;
}

function gewaehlterDatensatz() {
  var s = document.querySelector('input[name="datensatz"]:checked');
  return s ? s.value : "master";
}

fetch("/api/status").then(function (r) { return r.json(); }).then(function (j) {
  infoEl.textContent = "Datensatz: " + (j.dataset || "?") +
    (j.slaveDataset ? " | Slave: " + j.slaveDataset : " | Slave: —") +
    " | Ephemeriden: " + (j.ephemerisAvailable ? "ja" : "nein") +
    " | DEM-Kacheln: " + (j.demTilesAvailable ? "ja" : "nein");
  setzeStatus(j.state === "bereit" ? "datenbereit" : String(j.state));
  slaveVerfuegbar = !!j.slaveAvailable;
  var slaveRadio = document.getElementById("ds-slave");
  slaveRadio.disabled = !slaveVerfuegbar;
  if (!slaveVerfuegbar) {
    document.getElementById("ds-master").checked = true;
    slaveHinweisEl.textContent = "(kein Slave geladen — nur ein Datensatz)";
    vergleichEl.classList.add("vergleich-aus");
    document.getElementById("btn-vergleich").disabled = true;
    vergleichstatusEl.textContent = "Vergleich ausgegraut: nur ein Datensatz geladen (Server mit --slave starten).";
  } else {
    slaveHinweisEl.textContent = "";
  }
}).catch(function () {
  setzeStatus("fehler");
  zeigeFehler("Der Server antwortet nicht. Läuft sar_gui_server auf Loopback?");
});

try {
  if (typeof L === "undefined") throw new Error("Leaflet fehlt (offline?)");
  karte = L.map("map").setView([22.62, 120.31], 9);
  L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    { maxZoom: 19, attribution: "© OpenStreetMap" }).addTo(karte);
  karte.on("click", function (e) {
    latEl.value = e.latlng.lat.toFixed(5);
    lonEl.value = e.latlng.lng.toFixed(5);
    setzeMarker(e.latlng.lat, e.latlng.lng);
  });
} catch (e) {
  document.getElementById("maphinweis").hidden = false;
}
function setzeMarker(lat, lon) {
  if (!karte) return;
  if (marker) marker.setLatLng([lat, lon]);
  else marker = L.marker([lat, lon]).addTo(karte);
  karte.setView([lat, lon], karte.getZoom());
}

function lokalisieren() {
  zeigeFehler("");
  var lat = Number(latEl.value), lon = Number(lonEl.value);
  if (!isFinite(lat) || lat < -90 || lat > 90) {
    zeigeFehler("Die Breite (lat) muss zwischen -90 und 90 Grad liegen."); return;
  }
  if (!isFinite(lon) || lon < -180 || lon > 180) {
    zeigeFehler("Die Länge (lon) muss zwischen -180 und 180 Grad liegen."); return;
  }
  var h = zahlOderLeer(hEl);
  if (h !== null && !isFinite(h)) {
    zeigeFehler("Die Höhe (h) muss eine endliche Zahl in Metern sein oder leer bleiben (DEM-Vorschlag)."); return;
  }
  var body = { lat: lat, lon: lon, datasetId: gewaehlterDatensatz() };
  if (h !== null) body.h = h;
  setzeStatus("lädt …");
  fetch("/api/locate", { method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body) }).then(function (r) {
    return r.json().then(function (j) { return { ok: r.ok, j: j }; });
  }).then(function (x) {
    if (!x.ok) { zeigeFehler(x.j.error || "Lokalisierung fehlgeschlagen."); setzeStatus("fehler"); return; }
    treffer = x.j.hits || [];
    letzterDatensatz = x.j.datasetId || gewaehlterDatensatz();
    setzeStatus("datenbereit");
    setzeMarker(lat, lon);
    zeigeTreffer();
  }).catch(function () {
    setzeStatus("fehler");
    zeigeFehler("Der Server antwortet nicht. Läuft sar_gui_server auf Loopback?");
  });
}

function zeigeTreffer() {
  tbodyEl.innerHTML = "";
  if (!treffer.length) {
    tabelleEl.hidden = true;
    hinweisEl.textContent = "Keine Burst-Kandidaten für diesen Punkt (außerhalb der Szene?).";
    return;
  }
  tabelleEl.hidden = false;
  hinweisEl.textContent = treffer.length + " Kandidat(en), bester zuerst.";
  treffer.forEach(function (t, i) {
    var tr = document.createElement("tr");
    if (i === 0) tr.className = "bester";
    var zellen = [String(i + 1), String(t.swathNumber), String(t.burstIndex),
      Number(t.azEstimated).toFixed(1), Number(t.rg).toFixed(1),
      Number(t.tZd).toFixed(3), Number(t.incidenceDeg).toFixed(1),
      t.layoverRiskHeuristic ? "ja (Faustregel)" : "nein"];
    zellen.forEach(function (z) {
      var td = document.createElement("td"); td.textContent = z; tr.appendChild(td);
    });
    var aktion = document.createElement("td");
    var btn = document.createElement("button");
    btn.type = "button";
    btn.textContent = i === 0 ? "Fokus starten (bester)" : "Fokus starten";
    btn.addEventListener("click", function () { fokusStarten(t); });
    aktion.appendChild(btn); tr.appendChild(aktion);
    tbodyEl.appendChild(tr);
  });
}

function chipHalb(id, fallback) {
  var n = Number(document.getElementById(id).value);
  if (!isFinite(n)) return fallback;
  n = Math.round(n);
  if (n < 1) return 1;
  if (n > 200) return 200;
  return n;
}

function fokusStarten(t) {
  zeigeFehler("");
  stoppePolling();
  setzeStatus("fokus-läuft");
  jobstatusEl.textContent = "Fokus läuft …";
  jobdetailEl.textContent = "";
  chipbildEl.hidden = true;
  chiplegendeEl.textContent = "Fokus läuft …";
  var ds = letzterDatensatz || "master";
  var asset = document.getElementById("assetname").value.trim() || "Punkt";
  fetch("/api/focus", { method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ datasetId: ds, swathNumber: t.swathNumber,
      burstIndex: t.burstIndex, az: t.azEstimated, rg: t.rg, tZd: t.tZd,
      halfAz: chipHalb("halfaz", 16), halfRg: chipHalb("halfrg", 20),
      asset: asset }) }
  ).then(function (r) {
    return r.json().then(function (j) { return { ok: r.ok, j: j }; });
  }).then(function (x) {
    if (!x.ok) {
      setzeStatus("fehler");
      jobstatusEl.textContent = "Fokus fehlgeschlagen.";
      zeigeFehler(x.j.error || "Fokus konnte nicht gestartet werden."); return;
    }
    frageJob(x.j.jobId);
  }).catch(function () {
    setzeStatus("fehler");
    jobstatusEl.textContent = "Fokus fehlgeschlagen.";
    zeigeFehler("Der Server antwortet nicht. Läuft sar_gui_server auf Loopback?");
  });
}

function frageJob(id) {
  stoppePolling();
  pollTimer = setInterval(function () {
    fetch("/api/jobs/" + encodeURIComponent(id)).then(function (r) {
      return r.json().then(function (j) { return { ok: r.ok, j: j }; });
    }).then(function (x) {
      if (!x.ok) {
        stoppePolling(); setzeStatus("fehler");
        jobstatusEl.textContent = "Unbekannter Job.";
        zeigeFehler(x.j.error || "Unbekannte Job-Kennung."); return;
      }
      var j = x.j;
      if (j.status === "done") {
        stoppePolling(); setzeStatus("ergebnis-da");
        jobstatusEl.textContent = "Ergebnis da.";
        jobdetailEl.textContent = JSON.stringify(j, null, 2);
        letzteJobs[j.datasetId || letzterDatensatz] = id;
        if (j.chip && j.chip.pngUrl) {
          chipbildEl.src = j.chip.pngUrl;
          chipbildEl.hidden = false;
          chiplegendeEl.textContent = "Mitte Zeile " + Number(j.chip.centerRow).toFixed(1) +
            " / Spalte " + Number(j.chip.centerCol).toFixed(1) +
            " (±" + j.chip.halfAz + " × ±" + j.chip.halfRg + ").";
        }
      } else if (j.status === "error") {
        stoppePolling(); setzeStatus("fehler");
        jobstatusEl.textContent = "Fokus fehlgeschlagen.";
        zeigeFehler(j.error || "Der Fokus-Job ist fehlgeschlagen.");
      } else {
        jobstatusEl.textContent = "Fokus läuft … (" + j.status + ")";
      }
    }).catch(function () {
      stoppePolling(); setzeStatus("fehler");
      zeigeFehler("Der Server antwortet nicht. Läuft sar_gui_server auf Loopback?");
    });
  }, 1000);
}

function stoppePolling() {
  if (pollTimer !== null) { clearInterval(pollTimer); pollTimer = null; }
}

document.getElementById("btn-lokalisieren").addEventListener("click", lokalisieren);
document.getElementById("btn-demo").addEventListener("click", function () {
  latEl.value = "22.62"; lonEl.value = "120.31"; hEl.value = "";
  lokalisieren();
});
document.getElementById("btn-neu").addEventListener("click", function () {
  stoppePolling(); zeigeFehler("");
  latEl.value = ""; lonEl.value = ""; hEl.value = "";
  treffer = []; tbodyEl.innerHTML = ""; tabelleEl.hidden = true;
  hinweisEl.textContent = "Noch keine Lokalisierung.";
  jobstatusEl.textContent = "Kein Job gestartet."; jobdetailEl.textContent = "";
  chipbildEl.hidden = true; chiplegendeEl.textContent = "Noch kein Chip.";
  letzteJobs = { master: null, slave: null };
  setzeStatus("datenbereit");
  if (marker && karte) { karte.removeLayer(marker); marker = null; }
});

function schwelle(id, fallback) {
  var n = Number(document.getElementById(id).value);
  return isFinite(n) && n >= 0 ? n : fallback;
}

document.getElementById("btn-vergleich").addEventListener("click", function () {
  zeigeFehler("");
  if (!letzteJobs.master || !letzteJobs.slave) {
    vergleichstatusEl.textContent = "Bitte erst je einen Fokus-Job für Master und Slave starten.";
    return;
  }
  var warn = schwelle("warn", 3), alarm = schwelle("alarm", 6);
  vergleichstatusEl.textContent = "Vergleich läuft …";
  fetch("/api/compare?master=" + encodeURIComponent(letzteJobs.master) +
    "&slave=" + encodeURIComponent(letzteJobs.slave) +
    "&warn=" + encodeURIComponent(warn) + "&alarm=" + encodeURIComponent(alarm)
  ).then(function (r) {
    return r.json().then(function (j) { return { ok: r.ok, j: j }; });
  }).then(function (x) {
    if (!x.ok) {
      vergleichstatusEl.textContent = "Vergleich fehlgeschlagen.";
      zeigeFehler(x.j.error || "Vergleich fehlgeschlagen."); return;
    }
    vergleichstatusEl.textContent = "Ampel: " + x.j.ampel + " (Prototyp).";
    vergleichdetailEl.textContent = JSON.stringify(x.j, null, 2);
  }).catch(function () {
    vergleichstatusEl.textContent = "Vergleich fehlgeschlagen.";
    zeigeFehler("Der Server antwortet nicht. Läuft sar_gui_server auf Loopback?");
  });
});

document.getElementById("btn-report").addEventListener("click", function () {
  zeigeFehler("");
  var asset = document.getElementById("assetname").value.trim() || "Punkt";
  var warn = schwelle("warn", 3), alarm = schwelle("alarm", 6);
  reporttextEl.textContent = "Report lädt …";
  fetch("/api/report?asset=" + encodeURIComponent(asset) +
    "&warn=" + encodeURIComponent(warn) + "&alarm=" + encodeURIComponent(alarm)
  ).then(function (r) { return r.text(); }
  ).then(function (t) { reporttextEl.textContent = t; }
  ).catch(function () {
    reporttextEl.textContent = "Noch kein Report.";
    zeigeFehler("Der Server antwortet nicht. Läuft sar_gui_server auf Loopback?");
  });
});
})();
</script>
</body>
</html>"##;

struct Config {
    dataset: Option<String>,
    slave: Option<String>,
    host: String,
    port: u16,
}

/// Ein geladener Datensatz mit seiner Id (`master`, `slave` oder Pfad).
struct DatasetEntry {
    id: String,
    path: String,
    data: Arc<L0Dataset>,
}

/// Fokusergebnis eines Bursts (Cache-Inhalt pro Burst).
#[derive(Clone)]
struct CachedFocus {
    img: Arc<Vec<Complex32>>,
    n_az_fft: usize,
    n_rg: usize,
    kt_hz_s: f64,
    t_mid_s: f64,
    pri_s: f64,
}

/// Ergebnis eines Fokus-Jobs (für `GET /api/jobs/{id}` bei `done`).
#[derive(Clone)]
struct FocusResult {
    dataset_id: String,
    asset: String,
    swath_number: u8,
    burst_index: usize,
    focused_row: f64,
    range_col: f64,
    half_az: usize,
    half_rg: usize,
    median_db: f64,
    p90_db: f64,
    n: usize,
    alpha: f64,
    kt_hz_s: f64,
    /// Chip-Ausschnitt als PNG-Bytes (Graustufen, Fadenkreuz in Rot).
    chip_png: Vec<u8>,
}

/// Job-Zustand: `pending` → `running` → `done` | `error`.
struct Job {
    status: String,
    error: Option<String>,
    result: Option<FocusResult>,
}

struct JobStore {
    next_id: u64,
    jobs: HashMap<String, Job>,
    cache: HashMap<String, CachedFocus>,
}

impl JobStore {
    fn new() -> Self {
        Self {
            next_id: 1,
            jobs: HashMap::new(),
            cache: HashMap::new(),
        }
    }
}

/// Geteilter Server-Zustand: geladene Datensätze + Jobs/Cache.
struct AppState {
    datasets: Vec<DatasetEntry>,
    jobs: Mutex<JobStore>,
}

impl AppState {
    fn find_dataset(&self, id: &str) -> Option<&DatasetEntry> {
        if id == "master" {
            return self.datasets.first();
        }
        self.datasets.iter().find(|d| d.id == id || d.path == id)
    }

    fn find_burst(
        &self,
        id: &str,
        swath: u8,
        burst: usize,
    ) -> Option<(Arc<L0Dataset>, BurstBlock)> {
        let entry = self.find_dataset(id)?;
        let ds = Arc::clone(&entry.data);
        let block = ds
            .swaths
            .iter()
            .find(|s| s.swath_number == swath)?
            .bursts
            .iter()
            .find(|b| b.burst_index == burst)?
            .clone();
        Some((ds, block))
    }
}

fn print_usage() {
    eprintln!(
        "Verwendung: sar_gui_server [--dataset <pfad>] [--master <pfad>] \
         [--slave <pfad>] [--host 127.0.0.1] [--port 8787]"
    );
}

fn parse_args() -> Result<Config, String> {
    let mut dataset: Option<String> = None;
    let mut slave: Option<String> = None;
    let mut host = String::from("127.0.0.1");
    let mut port: u16 = 8787;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--dataset" | "--master" => {
                let v = args.next().ok_or("Fehlender Wert für --dataset/--master")?;
                dataset = Some(v);
            }
            "--slave" => {
                let v = args.next().ok_or("Fehlender Wert für --slave")?;
                slave = Some(v);
            }
            "--host" => {
                host = args.next().ok_or("Fehlender Wert für --host")?;
            }
            "--port" => {
                let v = args.next().ok_or("Fehlender Wert für --port")?;
                port = v
                    .parse::<u16>()
                    .map_err(|_| "Ungültiger --port (1-65535)")?;
            }
            "--help" | "-h" => {
                print_usage();
                std::process::exit(0);
            }
            other => return Err(format!("Unbekanntes Argument: {other}")),
        }
    }
    // Nur Loopback zulassen.
    if host != "127.0.0.1" && host != "::1" && host != "localhost" {
        return Err("Nur Loopback erlaubt (--host 127.0.0.1, ::1 oder localhost)".into());
    }
    if dataset.is_none() {
        // Default: Taiwan-Master, falls vorhanden.
        if Path::new(DEFAULT_DATASET).is_file() {
            dataset = Some(DEFAULT_DATASET.into());
        } else {
            // Auch vom Repository-Root aus startbar.
            let alt = format!("data/{}", &DEFAULT_DATASET["../".len()..]);
            if Path::new(&alt).is_file() {
                dataset = Some(alt);
            }
        }
    }
    Ok(Config {
        dataset,
        slave,
        host,
        port,
    })
}

/// Sucht das DEM-Verzeichnis (Start aus `source2/` oder Repository-Root).
fn dem_dir() -> Option<PathBuf> {
    for cand in ["../data/dem", "data/dem"] {
        let p = PathBuf::from(cand);
        if p.is_dir() {
            return Some(p);
        }
    }
    None
}

/// Sucht das Orbit-Verzeichnis mit präzisen Ephemeriden (`.EOF`).
fn aux_orbit_dir() -> Option<PathBuf> {
    for cand in ["../data/aux_orbits", "data/aux_orbits"] {
        let p = PathBuf::from(cand);
        if p.is_dir() {
            return Some(p);
        }
    }
    None
}

/// Prüft, ob mindestens eine DEM-Kachel per `HgtTile::load` lesbar ist.
fn dem_tiles_available() -> bool {
    let Some(dir) = dem_dir() else {
        return false;
    };
    let Ok(entries) = std::fs::read_dir(dir) else {
        return false;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|e| e == "hgt") && HgtTile::load(&path).is_ok() {
            return true;
        }
    }
    false
}

/// Prüft, ob präzise Ephemeriden (`.EOF`) per `parse_poeorb_eof` lesbar sind.
fn ephemeris_available() -> bool {
    let Some(dir) = aux_orbit_dir() else {
        return false;
    };
    let Ok(entries) = std::fs::read_dir(dir) else {
        return false;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|e| e == "EOF")
            && parse_poeorb_eof(&path).is_ok_and(|v| !v.is_empty())
        {
            return true;
        }
    }
    false
}

fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out
}

fn status_json(state: &AppState) -> String {
    let (dataset_label, dataset_ok) = match state.datasets.first() {
        Some(d) => (d.path.clone(), Path::new(&d.path).is_file()),
        None => ("?".to_string(), false),
    };
    let state_str = if dataset_ok { "bereit" } else { "fehler" };
    let slave = state.datasets.get(1);
    let slave_available = slave.is_some();
    let slave_label = slave.map_or(String::new(), |d| d.path.clone());
    format!(
        "{{ \"state\": \"{state_str}\", \"dataset\": \"{}\", \
         \"slaveAvailable\": {slave_available}, \"slaveDataset\": \"{}\", \
         \"ephemerisAvailable\": {}, \"demTilesAvailable\": {} }}",
        json_escape(&dataset_label),
        json_escape(&slave_label),
        ephemeris_available(),
        dem_tiles_available(),
    )
}

fn error_json(msg: &str) -> String {
    format!("{{ \"error\": \"{}\" }}", json_escape(msg))
}

/// Sucht `"key"` im JSON-Body und liest die folgende Zahl (auch negativ,
/// mit Bruchteil/Exponent). Gibt `None` bei fehlendem/ungültigem Wert.
fn json_number(body: &str, key: &str) -> Option<f64> {
    let pat = format!("\"{key}\"");
    let mut rest = body;
    loop {
        let i = rest.find(pat.as_str())?;
        rest = &rest[i + pat.len()..];
        let after_key = rest.trim_start();
        if !after_key.starts_with(':') {
            continue;
        }
        let mut num = after_key[1..].trim_start();
        if num.starts_with("null") {
            return None;
        }
        let mut end = 0usize;
        for (k, c) in num.char_indices() {
            if c.is_ascii_digit() || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E' {
                end = k + c.len_utf8();
            } else {
                break;
            }
        }
        if end == 0 {
            return None;
        }
        num = &num[..end];
        if let Ok(v) = num.parse::<f64>() {
            return Some(v);
        }
        return None;
    }
}

/// Sucht `"key": "<wert>"` im JSON-Body (ohne Escapes im Wert).
fn json_string(body: &str, key: &str) -> Option<String> {
    let pat = format!("\"{key}\"");
    let mut rest = body;
    loop {
        let i = rest.find(pat.as_str())?;
        rest = &rest[i + pat.len()..];
        let after_key = rest.trim_start();
        if !after_key.starts_with(':') {
            continue;
        }
        let val = after_key[1..].trim_start();
        if !val.starts_with('"') {
            return None;
        }
        let end = val[1..].find('"')?;
        return Some(val[1..1 + end].to_string());
    }
}

/// DEM-Höhe am Punkt (bilinear aus der passenden `.hgt`-Kachel), `None`
/// ohne Kachel. Fehler sind für den Prototyp still: Fallback ist 0 m.
fn dem_height(lat: f64, lon: f64) -> Option<f64> {
    let dir = dem_dir()?;
    let entries = std::fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.extension().is_some_and(|e| e == "hgt") {
            continue;
        }
        let Ok(tile) = HgtTile::load(&path) else {
            continue;
        };
        let (la0, lo0) = (f64::from(tile.lat0), f64::from(tile.lon0));
        if lat >= la0 && lat < la0 + 1.0 && lon >= lo0 && lon < lo0 + 1.0 {
            let h = tile.height(lat, lon);
            if h.is_finite() {
                return Some(h);
            }
        }
    }
    None
}

/// Rendert den Chip-Ausschnitt (`±half_az` × `±half_rg` um `az`/`rg`) als
/// RGB-PNG: logarithmierte Leistung in Graustufen (2-%- bis 98-%-Perzentil),
/// Fadenkreuz in Rot auf der fokussierten Mitte. Der Ausschnitt liegt in
/// Slant-Range-Geometrie (Azimut × Schrägentfernung) — kein Karten-Overlay.
fn render_chip_png(
    img: &[Complex32],
    n_az: usize,
    n_rg: usize,
    az: f64,
    rg: f64,
    half_az: usize,
    half_rg: usize,
) -> Option<Vec<u8>> {
    if img.len() != n_az * n_rg || n_az == 0 || n_rg == 0 {
        return None;
    }
    let r0 = (az.round() as i64 - half_az as i64).max(0) as usize;
    let r1 = (az.round() as i64 + half_az as i64)
        .min(n_az as i64 - 1)
        .max(0) as usize;
    let c0 = (rg.round() as i64 - half_rg as i64).max(0) as usize;
    let c1 = (rg.round() as i64 + half_rg as i64)
        .min(n_rg as i64 - 1)
        .max(0) as usize;
    if r1 < r0 || c1 < c0 {
        return None;
    }
    let h = r1 - r0 + 1;
    let w = c1 - c0 + 1;
    let mut db: Vec<f64> = Vec::with_capacity(h * w);
    for r in r0..=r1 {
        for c in c0..=c1 {
            let p = img[r * n_rg + c];
            let power = f64::from(p.re) * f64::from(p.re) + f64::from(p.im) * f64::from(p.im);
            db.push(10.0 * power.max(1e-30).log10());
        }
    }
    let mut sorted = db.clone();
    sorted.sort_by(f64::total_cmp);
    let lo = sorted[(sorted.len() * 2) / 100];
    let hi = sorted[(sorted.len() * 98) / 100].max(lo + 1e-3);
    let center_r = (az.round() as i64 - r0 as i64) as usize;
    let center_c = (rg.round() as i64 - c0 as i64) as usize;
    let mut rgb = Vec::with_capacity(h * w * 3);
    for r in 0..h {
        for c in 0..w {
            let g = (((db[r * w + c] - lo) / (hi - lo)).clamp(0.0, 1.0) * 255.0) as u8;
            if r == center_r || c == center_c {
                rgb.extend_from_slice(&[220, 30, 30]);
            } else {
                rgb.extend_from_slice(&[g, g, g]);
            }
        }
    }
    let mut buf = Vec::new();
    {
        let writer = Cursor::new(&mut buf);
        let encoder =
            PngEncoder::new_with_quality(writer, CompressionType::Fast, FilterType::NoFilter);
        encoder
            .write_image(&rgb, w as u32, h as u32, image::ExtendedColorType::Rgb8)
            .ok()?;
    }
    Some(buf)
}

/// `POST /api/locate`: parsen, validieren, `locate_asset`, Kandidaten-JSON
/// (bester zuerst — Reihenfolge von `locate_asset` wird übernommen).
/// `datasetId` ist optional (Default `master`); damit lassen sich Master und
/// Slave getrennt lokalisieren.
fn handle_locate(state: &AppState, body: &str) -> (u16, &'static str, String) {
    let Some(lat) = json_number(body, "lat") else {
        return (
            400,
            "Bad Request",
            error_json("Bitte gib eine Breite (lat) als Zahl an."),
        );
    };
    let Some(lon) = json_number(body, "lon") else {
        return (
            400,
            "Bad Request",
            error_json("Bitte gib eine Länge (lon) als Zahl an."),
        );
    };
    if !lat.is_finite() || lat < -90.0 || lat > 90.0 {
        return (
            400,
            "Bad Request",
            error_json("Die Breite (lat) muss zwischen -90 und 90 Grad liegen."),
        );
    }
    if !lon.is_finite() || lon < -180.0 || lon > 180.0 {
        return (
            400,
            "Bad Request",
            error_json("Die Länge (lon) muss zwischen -180 und 180 Grad liegen."),
        );
    }
    let h = match json_number(body, "h") {
        None => dem_height(lat, lon).unwrap_or(0.0),
        Some(v) if v.is_finite() => v,
        Some(_) => {
            return (
                400,
                "Bad Request",
                error_json("Die Höhe (h) muss eine endliche Zahl in Metern sein."),
            );
        }
    };
    let dataset_id = json_string(body, "datasetId").unwrap_or_else(|| "master".into());
    let Some(entry) = state.find_dataset(&dataset_id) else {
        return (
            404,
            "Not Found",
            error_json("Unbekannte Datensatz-Kennung (datasetId) — ggf. fehlt --slave."),
        );
    };
    let ds = &entry.data;
    let hits = locate_asset(&ds.ephemeris, ds, lat, lon, h);
    let mut out = format!(
        "{{ \"datasetId\": \"{}\", \"hits\": [",
        json_escape(&dataset_id)
    );
    for (i, hit) in hits.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&format!(
            "{{ \"swathNumber\": {}, \"burstIndex\": {}, \"azEstimated\": {:.3}, \
             \"rg\": {:.3}, \"tZd\": {:.6}, \"incidenceDeg\": {:.3}, \
             \"layoverRiskHeuristic\": {} }}",
            hit.swath_number,
            hit.burst_index,
            hit.az,
            hit.rg,
            hit.t_zd_s,
            hit.incidence_deg,
            hit.layover_risk
        ));
    }
    out.push_str("] }");
    (200, "OK", out)
}

/// Legt einen Fokus-Job an und startet den Worker-Thread. Gibt die
/// HTTP-Antwort (`202` + Job-Id oder Fehler-JSON) zurück.
fn handle_focus(state: &Arc<AppState>, body: &str) -> (u16, &'static str, String) {
    let Some(dataset_id) = json_string(body, "datasetId") else {
        return (
            400,
            "Bad Request",
            error_json("Bitte gib eine Datensatz-Kennung (datasetId) an."),
        );
    };
    let (Some(swath_f), Some(burst_f), Some(az), Some(rg), Some(t_zd)) = (
        json_number(body, "swathNumber"),
        json_number(body, "burstIndex"),
        json_number(body, "az"),
        json_number(body, "rg"),
        json_number(body, "tZd"),
    ) else {
        return (
            400,
            "Bad Request",
            error_json("Bitte gib swathNumber, burstIndex, az, rg und tZd als Zahlen an."),
        );
    };
    if !az.is_finite() || !rg.is_finite() || !t_zd.is_finite() {
        return (
            400,
            "Bad Request",
            error_json("az, rg und tZd müssen endliche Zahlen sein."),
        );
    }
    if !swath_f.is_finite() || swath_f < 0.0 || swath_f > 255.0 || swath_f.fract() != 0.0 {
        return (
            400,
            "Bad Request",
            error_json("swathNumber muss eine ganze Zahl zwischen 0 und 255 sein."),
        );
    }
    if !burst_f.is_finite() || burst_f < 0.0 || burst_f.fract() != 0.0 {
        return (
            400,
            "Bad Request",
            error_json("burstIndex muss eine ganze Zahl ab 0 sein."),
        );
    }
    let (swath_number, burst_index) = (swath_f as u8, burst_f as usize);
    // Chip-Fenster: optional, Default ±16 × ±20, Obergrenze `MAX_HALF`.
    let half_az = match json_number(body, "halfAz") {
        None => DEFAULT_HALF_AZ,
        Some(v) if v.is_finite() && v >= 1.0 && v <= MAX_HALF as f64 => v as usize,
        Some(_) => {
            return (
                400,
                "Bad Request",
                error_json("halfAz muss eine ganze Zahl zwischen 1 und 200 sein."),
            );
        }
    };
    let half_rg = match json_number(body, "halfRg") {
        None => DEFAULT_HALF_RG,
        Some(v) if v.is_finite() && v >= 1.0 && v <= MAX_HALF as f64 => v as usize,
        Some(_) => {
            return (
                400,
                "Bad Request",
                error_json("halfRg muss eine ganze Zahl zwischen 1 und 200 sein."),
            );
        }
    };
    let asset = json_string(body, "asset")
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "Punkt".into());
    let Some(entry) = state.find_dataset(&dataset_id) else {
        return (
            404,
            "Not Found",
            error_json("Unbekannte Datensatz-Kennung (datasetId)."),
        );
    };
    let burst_exists = entry
        .data
        .swaths
        .iter()
        .find(|s| s.swath_number == swath_number)
        .is_some_and(|s| s.bursts.iter().any(|b| b.burst_index == burst_index));
    if !burst_exists {
        return (
            404,
            "Not Found",
            error_json("Diesen Burst gibt es in dem Datensatz nicht."),
        );
    }
    let job_id = {
        let mut store = state.jobs.lock().expect("Job-Speicher belegt");
        let id = format!("job-{}", store.next_id);
        store.next_id += 1;
        store.jobs.insert(
            id.clone(),
            Job {
                status: "pending".into(),
                error: None,
                result: None,
            },
        );
        id
    };
    let worker_state = Arc::clone(state);
    let worker_id = job_id.clone();
    let worker_req = FocusRequest {
        dataset_id,
        swath_number,
        burst_index,
        rg,
        t_zd,
        half_az,
        half_rg,
        asset,
    };
    std::thread::spawn(move || {
        run_focus_job(&worker_state, &worker_id, worker_req);
    });
    (
        202,
        "Accepted",
        format!("{{ \"jobId\": \"{}\" }}", json_escape(&job_id)),
    )
}

/// Eingaben eines Fokus-Jobs (aus `POST /api/focus`).
struct FocusRequest {
    dataset_id: String,
    swath_number: u8,
    burst_index: usize,
    rg: f64,
    t_zd: f64,
    half_az: usize,
    half_rg: usize,
    asset: String,
}

/// Worker eines Fokus-Jobs: Burst aus dem Cache oder frisch über
/// `focus_tops_burst_cpu` fokussieren, exakte Zeile über
/// `tops_alpha` + `focused_line` (nicht `AssetHit.az` allein),
/// Chip-Statistik über `chip_stats`.
fn run_focus_job(state: &Arc<AppState>, job_id: &str, req: FocusRequest) {
    let dataset_id = req.dataset_id.as_str();
    let (swath_number, burst_index, rg, t_zd, half_az, half_rg) = (
        req.swath_number,
        req.burst_index,
        req.rg,
        req.t_zd,
        req.half_az,
        req.half_rg,
    );
    let asset = req.asset;
    {
        let mut store = state.jobs.lock().expect("Job-Speicher belegt");
        if let Some(job) = store.jobs.get_mut(job_id) {
            job.status = "running".into();
        }
    }
    let fail = |state: &Arc<AppState>, msg: String| {
        let mut store = state.jobs.lock().expect("Job-Speicher belegt");
        if let Some(job) = store.jobs.get_mut(job_id) {
            job.status = "error".into();
            job.error = Some(msg);
        }
    };
    let cache_key = format!("{dataset_id}|{swath_number}|{burst_index}");
    // Cache zuerst: keine doppelte Fokussierung desselben Bursts.
    let cached = {
        state
            .jobs
            .lock()
            .expect("Job-Speicher belegt")
            .cache
            .get(&cache_key)
            .cloned()
    };
    let (focused, block) = match cached {
        Some(c) => {
            let Some((_, b)) = state.find_burst(dataset_id, swath_number, burst_index) else {
                fail(state, "Diesen Burst gibt es in dem Datensatz nicht.".into());
                return;
            };
            (
                c, // Burst-Metadaten (veff, Pulszeiten) kommen aus dem Datensatz.
                b,
            )
        }
        None => {
            let Some((_, b)) = state.find_burst(dataset_id, swath_number, burst_index) else {
                fail(state, "Diesen Burst gibt es in dem Datensatz nicht.".into());
                return;
            };
            let (img, n_az_fft, n_rg, steering) = focus_tops_burst_cpu(&b, true);
            let Some(&t_first) = b.pulse_times.first() else {
                fail(state, "Der Burst hat keine Pulszeiten.".into());
                return;
            };
            let Some(&t_last) = b.pulse_times.last() else {
                fail(state, "Der Burst hat keine Pulszeiten.".into());
                return;
            };
            let entry = CachedFocus {
                img: Arc::new(img),
                n_az_fft,
                n_rg,
                kt_hz_s: steering.kt_hz_s,
                t_mid_s: 0.5 * (t_first + t_last),
                pri_s: b.params.pri_s,
            };
            state
                .jobs
                .lock()
                .expect("Job-Speicher belegt")
                .cache
                .insert(cache_key, entry.clone());
            (entry, b)
        }
    };
    // Exakte fokussierte Zeile aus der Zero-Doppler-Zeit (α-Stauchung!).
    let rg_idx = rg.round().clamp(0.0, (block.n_rg.max(1) - 1) as f64) as usize;
    let veff = block.veff.get(rg_idx).copied().unwrap_or(0.0);
    if !veff.is_finite() || veff <= 0.0 {
        fail(state, "Der Burst hat keine gültige Geschwindigkeit.".into());
        return;
    }
    let slant = block.params.slant_range(rg_idx);
    let alpha = tops_alpha(focused.kt_hz_s, veff, slant);
    let row = focused_line(
        focused.n_az_fft,
        focused.t_mid_s,
        focused.pri_s,
        alpha,
        t_zd,
    );
    if !row.is_finite()
        || row < 0.0
        || row >= focused.n_az_fft as f64
        || rg < 0.0
        || rg >= focused.n_rg as f64
    {
        fail(
            state,
            "Der Punkt liegt außerhalb des fokussierten Bursts.".into(),
        );
        return;
    }
    match chip_stats(
        &focused.img,
        focused.n_az_fft,
        focused.n_rg,
        row,
        rg,
        half_az,
        half_rg,
    ) {
        Some(stats) => {
            let chip_png = render_chip_png(
                &focused.img,
                focused.n_az_fft,
                focused.n_rg,
                row,
                rg,
                half_az,
                half_rg,
            )
            .unwrap_or_default();
            let dataset_id_owned = dataset_id.to_string();
            let mut store = state.jobs.lock().expect("Job-Speicher belegt");
            if let Some(job) = store.jobs.get_mut(job_id) {
                job.status = "done".into();
                job.result = Some(FocusResult {
                    dataset_id: dataset_id_owned,
                    asset,
                    swath_number,
                    burst_index,
                    focused_row: row,
                    range_col: rg,
                    half_az,
                    half_rg,
                    median_db: stats.median_db,
                    p90_db: stats.p90_db,
                    n: stats.n,
                    alpha,
                    kt_hz_s: focused.kt_hz_s,
                    chip_png,
                });
            }
        }
        None => fail(
            state,
            "Der Chip liegt am Bildrand — bitte einen Punkt weiter innen wählen.".into(),
        ),
    }
}

/// `GET /api/jobs/{id}`: Jobstatus als JSON (`pending`/`running`/`done`/`error`).
fn handle_job(state: &AppState, job_id: &str) -> (u16, &'static str, String) {
    let store = state.jobs.lock().expect("Job-Speicher belegt");
    let Some(job) = store.jobs.get(job_id) else {
        return (
            404,
            "Not Found",
            error_json("Unbekannte Job-Kennung — Jobs verfallen beim Neustart."),
        );
    };
    let body = match job.status.as_str() {
        "done" => {
            let r = job.result.as_ref().expect("done ohne Ergebnis");
            format!(
                "{{ \"status\": \"done\", \"datasetId\": \"{}\", \"asset\": \"{}\", \
                 \"chipStats\": {{ \"medianDb\": {:.3}, \"p90Db\": {:.3}, \"n\": {} }}, \
                 \"chip\": {{ \"swathNumber\": {}, \"burstIndex\": {}, \
                 \"centerRow\": {:.3}, \"centerCol\": {:.3}, \
                 \"focusedRow\": {:.3}, \"rangeCol\": {:.3}, \
                 \"halfAz\": {}, \"halfRg\": {}, \
                 \"pngUrl\": \"/api/chip/{}\" }}, \
                 \"tops\": {{ \"alpha\": {:.6}, \"ktHzS\": {:.3} }} }}",
                json_escape(&r.dataset_id),
                json_escape(&r.asset),
                r.median_db,
                r.p90_db,
                r.n,
                r.swath_number,
                r.burst_index,
                r.focused_row,
                r.range_col,
                r.focused_row,
                r.range_col,
                r.half_az,
                r.half_rg,
                json_escape(job_id),
                r.alpha,
                r.kt_hz_s
            )
        }
        "error" => format!(
            "{{ \"status\": \"error\", \"error\": \"{}\" }}",
            json_escape(job.error.as_deref().unwrap_or("Unbekannter Fehler."))
        ),
        other => format!("{{ \"status\": \"{other}\" }}"),
    };
    (200, "OK", body)
}

/// Holt einen abgeschlossenen Job (`done`) samt Ergebnis; sonst `None`.
fn done_result(state: &AppState, job_id: &str) -> Option<FocusResult> {
    let store = state.jobs.lock().expect("Job-Speicher belegt");
    let job = store.jobs.get(job_id)?;
    if job.status != "done" {
        return None;
    }
    job.result.clone()
}

/// `GET /api/chip/{id}`: Chip-PNG des Jobs (siehe `render_chip_png`).
fn handle_chip(state: &AppState, job_id: &str) -> Option<Vec<u8>> {
    let result = done_result(state, job_id)?;
    if result.chip_png.is_empty() {
        return None;
    }
    Some(result.chip_png)
}

/// Dekodiert `%XX` und `+` in Query-Werten (ASCII-reichend für den Prototyp).
fn url_decode(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%'
            && i + 2 < bytes.len()
            && let Ok(hex) = u8::from_str_radix(&s[i + 1..i + 3], 16)
        {
            out.push(hex as char);
            i += 3;
            continue;
        }
        if bytes[i] == b'+' {
            out.push(' ');
        } else {
            out.push(bytes[i] as char);
        }
        i += 1;
    }
    out
}

/// Liest einen Query-Parameter (`?a=1&b=2`) als String.
fn query_param(path: &str, key: &str) -> Option<String> {
    let q = path.split_once('?')?.1;
    for pair in q.split('&') {
        let (k, v) = pair.split_once('=').unwrap_or((pair, ""));
        if k == key {
            return Some(url_decode(v));
        }
    }
    None
}

/// Liest einen Query-Parameter als `f64` (endlich, nicht-negativ).
fn query_float(path: &str, key: &str, fallback: f64) -> f64 {
    match query_param(path, key).as_deref().map(str::parse::<f64>) {
        Some(Ok(v)) if v.is_finite() && v >= 0.0 => v,
        _ => fallback,
    }
}

/// `GET /api/compare?master={id}&slave={id}&warn=&alarm=`: vergleicht zwei
/// abgeschlossene Fokus-Jobs. Drift = Slave-Median minus Master-Median
/// (Median-Basis); Ampel via `geoloc::bewerte_zeitreihe` über
/// `[master, slave]`. Schwellen sind Prototyp-Werte (editierbar, Standard
/// Warnung 3 dB / Alarm 6 dB, nicht kalibriert — siehe `bewerte_zeitreihe`).
fn handle_compare(state: &AppState, path: &str) -> (u16, &'static str, String) {
    let (Some(master_id), Some(slave_id)) =
        (query_param(path, "master"), query_param(path, "slave"))
    else {
        return (
            400,
            "Bad Request",
            error_json("Bitte master- und slave-Job-Kennung als Query angeben."),
        );
    };
    let warn = query_float(path, "warn", DEFAULT_WARN_DB);
    let alarm = query_float(path, "alarm", DEFAULT_ALARM_DB);
    let (Some(master), Some(slave)) = (
        done_result(state, &master_id),
        done_result(state, &slave_id),
    ) else {
        return (
            404,
            "Not Found",
            error_json("Beide Jobs müssen abgeschlossen (done) sein."),
        );
    };
    let medians = [master.median_db, slave.median_db];
    let ampel = bewerte_zeitreihe(&medians, warn, alarm);
    let status = match ampel.status {
        AmpelStatus::Gruen => "grün",
        AmpelStatus::Gelb => "gelb",
        AmpelStatus::Rot => "rot",
    };
    let drift = slave.median_db - master.median_db;
    (
        200,
        "OK",
        format!(
            "{{ \"ampel\": \"{status}\", \"driftDb\": {drift:.3}, \
             \"warnDb\": {warn:.3}, \"alarmDb\": {alarm:.3}, \
             \"detail\": \"{}\", \"prototyp\": true, \
             \"master\": {{ \"jobId\": \"{}\", \"medianDb\": {:.3}, \"p90Db\": {:.3} }}, \
             \"slave\": {{ \"jobId\": \"{}\", \"medianDb\": {:.3}, \"p90Db\": {:.3} }} }}",
            json_escape(&ampel.detail),
            json_escape(&master_id),
            master.median_db,
            master.p90_db,
            json_escape(&slave_id),
            slave.median_db,
            slave.p90_db,
        ),
    )
}

/// `GET /api/report?asset=&warn=&alarm=`: Wochenreport als Markdown-Text
/// (`geoloc::wochenbericht`) aus den abgeschlossenen Fokus-Jobs dieses Assets
/// (älteste zuerst). Bei weniger als zwei Einträgen steht ein Hinweis im
/// Text — eine Drift-Aussage braucht mindestens zwei Messungen.
fn handle_report(state: &AppState, path: &str) -> (u16, &'static str, String) {
    let asset = query_param(path, "asset")
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "Punkt".into());
    let warn = query_float(path, "warn", DEFAULT_WARN_DB);
    let alarm = query_float(path, "alarm", DEFAULT_ALARM_DB);
    let store = state.jobs.lock().expect("Job-Speicher belegt");
    let mut ids: Vec<&String> = store.jobs.keys().collect();
    ids.sort();
    let mut eintraege = Vec::new();
    for id in ids {
        let job = &store.jobs[id];
        if job.status != "done" {
            continue;
        }
        if let Some(r) = &job.result
            && r.asset == asset
        {
            eintraege.push(ReportEintrag {
                datum: format!("Messung {} ({})", eintraege.len() + 1, id),
                median_db: r.median_db,
                p90_db: r.p90_db,
            });
        }
    }
    drop(store);
    let mut bericht = wochenbericht(&asset, &eintraege, warn, alarm);
    if eintraege.len() < 2 {
        bericht.push_str(
            "\nHinweis: zu wenige Einträge — bitte erst mindestens zwei \
             Fokus-Jobs für dieses Asset starten (z. B. Master und Slave).\n",
        );
    }
    (200, "OK", bericht)
}

fn respond(stream: &mut TcpStream, code: u16, reason: &str, content_type: &str, body: &str) {
    respond_bytes(stream, code, reason, content_type, body.as_bytes());
}

fn respond_bytes(stream: &mut TcpStream, code: u16, reason: &str, content_type: &str, body: &[u8]) {
    let head = format!(
        "HTTP/1.1 {code} {reason}\r\nContent-Type: {content_type}\r\n\
         Content-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
    );
    let _ = stream.write_all(head.as_bytes());
    let _ = stream.write_all(body);
}

fn handle(stream: &mut TcpStream, state: &Arc<AppState>) {
    let Ok(clone) = stream.try_clone() else {
        return;
    };
    let mut reader = BufReader::new(clone);
    let mut request_line = String::new();
    if reader.read_line(&mut request_line).is_err() {
        return;
    }
    let mut parts = request_line.split_whitespace();
    let (method, path) = (
        parts.next().unwrap_or(""),
        parts.next().unwrap_or("").to_string(),
    );
    // Header lesen, Body exakt nach Content-Length einlesen.
    let mut content_length = 0usize;
    loop {
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => break,
            Ok(_) => {
                if line == "\r\n" || line == "\n" || line.trim().is_empty() {
                    break;
                }
                if let Some(v) = line
                    .strip_prefix("Content-Length:")
                    .or_else(|| line.strip_prefix("content-length:"))
                {
                    content_length = v.trim().parse().unwrap_or(0);
                }
            }
            Err(_) => break,
        }
    }
    let mut body = vec![0u8; content_length.min(1 << 20)];
    let mut read = 0usize;
    while read < body.len() {
        match reader.read(&mut body[read..]) {
            Ok(0) => break,
            Ok(n) => read += n,
            Err(_) => break,
        }
    }
    body.truncate(read);
    drop(reader);
    let body_str = String::from_utf8_lossy(&body).into_owned();

    match (method, path.as_str()) {
        ("GET", "/") => respond(stream, 200, "OK", "text/html; charset=utf-8", INDEX_HTML),
        ("GET", "/api/status") => {
            respond(stream, 200, "OK", "application/json", &status_json(state))
        }
        ("POST", "/api/locate") => {
            let (code, reason, out) = handle_locate(state, &body_str);
            respond(stream, code, reason, "application/json", &out);
        }
        ("POST", "/api/focus") => {
            let (code, reason, out) = handle_focus(state, &body_str);
            respond(stream, code, reason, "application/json", &out);
        }
        ("GET", p) if p.starts_with("/api/jobs/") => {
            let id = &p["/api/jobs/".len()..];
            if id.is_empty() || id.contains('/') || id.contains('?') {
                respond(
                    stream,
                    404,
                    "Not Found",
                    "application/json",
                    &error_json("Unbekannte Job-Kennung."),
                );
            } else {
                let (code, reason, out) = handle_job(state, id);
                respond(stream, code, reason, "application/json", &out);
            }
        }
        ("GET", p) if p.starts_with("/api/chip/") => {
            let id = p["/api/chip/".len()..].split('?').next().unwrap_or("");
            if id.is_empty() || id.contains('/') {
                respond(
                    stream,
                    404,
                    "Not Found",
                    "application/json",
                    &error_json("Unbekannte Job-Kennung."),
                );
            } else {
                match handle_chip(state, id) {
                    Some(png) => respond_bytes(stream, 200, "OK", "image/png", &png),
                    None => respond(
                        stream,
                        404,
                        "Not Found",
                        "application/json",
                        &error_json("Kein Chip für diesen Job — erst fokussieren."),
                    ),
                }
            }
        }
        ("GET", p) if p == "/api/compare" || p.starts_with("/api/compare?") => {
            let (code, reason, out) = handle_compare(state, p);
            respond(stream, code, reason, "application/json", &out);
        }
        ("GET", p) if p == "/api/report" || p.starts_with("/api/report?") => {
            let (code, reason, out) = handle_report(state, p);
            respond(stream, code, reason, "text/plain; charset=utf-8", &out);
        }
        ("GET", _) => respond(
            stream,
            404,
            "Not Found",
            "application/json",
            &error_json("Unbekannter Pfad"),
        ),
        ("POST", _) => respond(
            stream,
            404,
            "Not Found",
            "application/json",
            &error_json("Unbekannter Pfad"),
        ),
        _ => respond(
            stream,
            405,
            "Method Not Allowed",
            "application/json",
            &error_json("Diese Methode wird hier nicht unterstützt"),
        ),
    }
}

/// Lädt einen Datensatz (`master` oder `slave`); Fehler werden gemeldet,
/// blockieren aber nicht den Serverstart (Status zeigt dann `fehler`).
fn try_load(id: &str, path: &str) -> Option<DatasetEntry> {
    match load_l0_dataset(Path::new(path), None, None) {
        Ok(data) => {
            println!(
                "Datensatz {id} geladen: {path} ({} Bursts gesamt)",
                data.swaths.iter().map(|s| s.bursts.len()).sum::<usize>()
            );
            Some(DatasetEntry {
                id: id.into(),
                path: path.into(),
                data: Arc::new(data),
            })
        }
        Err(e) => {
            eprintln!("Warnung: Datensatz {id} ({path}) ließ sich nicht laden: {e}");
            None
        }
    }
}

fn main() {
    let cfg = parse_args().unwrap_or_else(|e| {
        eprintln!("Fehler: {e}");
        print_usage();
        std::process::exit(2);
    });
    let mut datasets = Vec::new();
    if let Some(p) = &cfg.dataset
        && let Some(entry) = try_load("master", p)
    {
        datasets.push(entry);
    }
    if let Some(p) = &cfg.slave
        && p != cfg.dataset.as_deref().unwrap_or("")
        && let Some(entry) = try_load("slave", p)
    {
        datasets.push(entry);
    }
    let state = Arc::new(AppState {
        datasets,
        jobs: Mutex::new(JobStore::new()),
    });
    let addr = format!("{}:{}", cfg.host, cfg.port);
    let listener = TcpListener::bind(&addr).unwrap_or_else(|e| {
        eprintln!("Fehler: Binden an {addr} scheiterte: {e}");
        std::process::exit(1);
    });
    println!("sar_gui_server hört auf http://{addr}/");
    match state.datasets.first() {
        Some(d) => println!("Datensatz: {}", d.path),
        None => println!("Datensatz: ? (kein Default gefunden)"),
    }
    if state.datasets.len() > 1 {
        println!("Slave: {}", state.datasets[1].path);
    }
    for stream in listener.incoming() {
        match stream {
            Ok(mut s) => {
                let _ = s.set_read_timeout(Some(Duration::from_secs(30)));
                let st = Arc::clone(&state);
                // Pro Verbindung ein Thread: Server bleibt erreichbar,
                // Jobs laufen ohnehin in eigenen Threads (Schritt 5).
                std::thread::spawn(move || handle(&mut s, &st));
            }
            Err(e) => eprintln!("Verbindungsfehler: {e}"),
        }
    }
}
