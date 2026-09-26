# Live-Gesichtserkennung vom X11-Desktop: Architektur, Deep Dives & Post-Mortem

*Projekt `20260926_01_face` — Binary `x11_face_reid` in `examples/26_onnx/source7/`,
Stand 2026-09-26. Alle Gates grün.*

## Executive Summary

Wir haben eine Echtzeit-Pipeline gebaut, die direkt vom X11-Bildschirm liest,
Gesichter detektiert, sie auf ein normiertes Porträt alignt, per ArcFace in
einen 512-dimensionalen Raum einbettet und Personen über Sitzungen hinweg
wiedererkennt — alles in einem einzigen Rust-Binary mit fünf externen Crates
und ohne eine einzige schwere Systemabhängigkeit. Auf einem AMD Threadripper
PRO 7955WX läuft die komplette Schleife aus Capture, Inferenz und Rendering
mit rund 96 Bildern pro Sekunde auf der CPU; ein CUDA-Pfad existiert, validiert
sich selbst per Probe-Inferenz und fällt ohne cuDNN stabil auf CPU zurück.
Die drei spannendsten Geschichten dieses Projekts: ein 4×4-Gauß-Trick, der eine
komplette Mathe-Bibliothek ersetzt, winzige Geisterboxen, die einen fehlenden
Multiplikationsfaktor verrieten, und eine ONNX-Runtime-Lüge, die erst beim
ersten Frame aufflog.

## 1. Vision & Pipeline-Architektur

### Das Problem

Gesichtserkennung auf dem Desktop klingt nach einem gelösten Problem — bis man
die Latenz-, Abhängigkeits- und Persistenz-Bedingungen ernst nimmt. Wir wollten
kein Python-Skript, das über Screenshots stolpert, keine C++-Vektordatenbank,
die beim Kompilieren zickt, und keinen neuronalen Overkill für ein paar
tausend Vektoren. Das Ziel lautete: den X11-Root-Window nativ mit 640×640
Pixeln abgreifen, Gesichter finden, sie wiedererkennen und das Ergebnis in
einem Fenster zeigen, das gleichzeitig Debug-Konsole und Produkt-Demo ist.
Und das Ganze auf zwei Zielplattformen: einer RTX-A4000-Workstation mit CUDA
und einem Ryzen-Laptop, auf dem nur AVX2 und viele Threads helfen.

Die Herausforderung steckt in den Übergängen. Jedes Glied der Kette spricht
eine andere Sprache: X11 liefert BGRA-Pixel im Z-Pixmap-Format, SCRFD will
planar-normalisierte Float-Tensoren, ArcFace erwartet ein exakt aligntes
112×112-Porträt, und die Re-ID-Schicht denkt in Kosinus-Abständen auf einer
512-dimensionalen Hyperkugel. Ein sauberer Datenfluss ohne Kopier-Orgien und
ohne Format-Verwirrung ist deshalb die eigentliche Architekturleistung.

### Der Datenfluss: vom X11-Pixel zum Embedding

Jeder Frame durchläuft sechs Stationen. Zuerst holt [02_screen_capture.rs](/workspace/src/cl-rust-generator/examples/26_onnx/source7/src/02_screen_capture.rs)
per `xproto::get_image` exakt die 640×640 Pixel oben links vom Root-Window —
nativ, ohne Resize, denn 640 ist zufällig auch die Lieblingsgröße von SCRFD.
Die BGRA-Bytes werden in einem einzigen Durchlauf nach RGB gewandelt. Dann
übernimmt [04_scrfd_detector.rs](/workspace/src/cl-rust-generator/examples/26_onnx/source7/src/04_scrfd_detector.rs):
Das Bild wird zu `(x − 127.5) / 128` normalisiert, planar in NCHW-Ordnung
abgelegt und durch `det_500m.onnx` (2,5 MB) geschoben. Aus den neun
Output-Tensoren — Scores, Box-Regressionen und Keypoint-Regressionen auf je
drei Pyramiden-Ebenen — decodiert reines Rust Bounding-Boxes plus fünf
Landmarks (Augen, Nase, Mundwinkel) und bereinigt sie per Greedy-NMS
(Konfidenzschwelle 0.5, IoU-Schwelle 0.4, exakt wie im Referenzcode).

Für jede überlebende Detektion schätzt [03_alignment.rs](/workspace/src/cl-rust-generator/examples/26_onnx/source7/src/03_alignment.rs)
eine Ähnlichkeitstransformation auf das offizielle ArcFace-Referenztemplate
und warpt das Gesicht bilinear auf 112×112 Pixel. Diesen Crop frisst
[05_arcface_embed.rs](/workspace/src/cl-rust-generator/examples/26_onnx/source7/src/05_arcface_embed.rs):
nach `(x − 127.5) / 127.5`-Normalisierung liefert `w600k_mbf.onnx` (13,6 MB)
einen 512D-Vektor, der zwingend L2-normalisiert wird — erst dadurch wird aus
der teuren Kosinus-Ähnlichkeit ein schlichtes Skalarprodukt. Den Abschluss
bildet [06_face_database.rs](/workspace/src/cl-rust-generator/examples/26_onnx/source7/src/06_face_database.rs):
Jedes Embedding wird gegen eine Exemplar-Bank gehalten, einer Person
zugeordnet oder als neue Person angelegt, und die Datenbank persistiert per
`serde` und `bincode` nach `faces_db.bin`. Orchestriert wird alles von
[07_engine.rs](/workspace/src/cl-rust-generator/examples/26_onnx/source7/src/07_engine.rs),
einer generischen `Engine<D, E>`, die über Traits entkoppelt ist — der Grund,
warum sich die komplette Pipeline-Logik ohne ein einziges ONNX-Modell testen
lässt.

### Das Fenster: Feed, Sidebar, HUD

Wer das Programm startet, sieht ein 820×640-Fenster, das in zwei Welten
zerfällt. Links dominiert der 640×640-Live-Feed: das abgegriffene
Desktop-Rechteck, überlagert mit grünen Bounding-Boxes, fünf gelben
Keypoint-Punkten pro Gesicht und einem Label wie `ID 3 0.91` — Personen-ID
plus Match-Konfidenz. Rechts schließt eine 180 Pixel schmale, dunkle Sidebar
an, die von oben nach unten drei Dinge erzählt. Ganz oben das „live"-Panel:
der aktuell alignte 112×112-Crop, also exakt das Porträt, das gerade durch
ArcFace lief — ein unbestechlicher Blick in die Seele des Alignments.
Darunter die Galerie: pro bekannter Person das erste gespeicherte Thumbnail
(auf 56×56 skaliert) neben ihrer ID, das visuelle Gedächtnis der Datenbank.
Ganz unten das HUD in vier Zeilen: Personen, Exemplare, FPS und der aktive
Execution Provider. Diese letzte Zeile — `provider: CPU` oder `provider:
CUDA` — ist kein Dekor, sondern ein Ehrlichkeitsversprechen: Sie zeigt immer
den tatsächlich bewiesenen Inferenzpfad, wie Deep Dive 3 erklären wird.

## 2. Ingenieursentscheidungen & Deep Dives

### Deep Dive 1: Der 4×4-Gauß-Kniff statt schwerer SVD

Das 5-Punkt-Alignment ist der mathematisch anspruchsvollste Baustein, der kein
neuronales Netz ist. Gegeben sind fünf erkannte Landmarks im Foto und fünf
Referenzpunkte des ArcFace-Templates — etwa `[38.2946, 51.6963]` für das linke
Auge — gesucht ist die Ähnlichkeitstransformation, die beide Punktmengen
optimal zur Deckung bringt: Skalierung, Rotation und Translation, aber kein
Scheren. Der Lehrbuchweg heißt Umeyama-Algorithmus mit Singulärwertzerlegung
(SVD). In Rust bedeutet das: `nalgebra` oder einen vergleichbaren Brocken als
Abhängigkeit einheiraten, nur um einmal pro Gesicht eine 2×2-Matrix zu
zerlegen. Für ein Projekt, dessen Credo „minimale Abhängigkeiten" lautet, war
das keine Option.

Der Kniff besteht darin, die Ähnlichkeitstransformation so zu parametrisieren,
dass sie linear wird. Schreibt man Skalierung $s$ und Rotation $θ$ als
$a = s·cos θ$ und $b = s·sin θ$, lautet die Abbildung schlicht:

$$x' = a·x − b·y + t_x \qquad y' = b·x + a·y + t_y$$

Vier Unbekannte ($a$, $b$, $t_x$, $t_y$), und jede der fünf Punktkorrespondenzen
liefert zwei lineare Gleichungen — zehn Gleichungen für vier Unbekannte, ein
klassisches überbestimmtes System. Die Design-Matrix liest sich fast wie Poesie:
Für die $x$-Koordinate gilt die Zeile $[x, −y, 1, 0] → x'$, für die
$y$-Koordinate $[y, x, 0, 1] → y'$. Daraus baut `estimate_similarity` die
Normalgleichung $(A^TA)·p = A^Tb$, ein symmetrisches 4×4-System, das ein
handgeschriebener Gauß-Algorithmus mit Pivotisierung in rund dreißig Zeilen
löst — in `f64` für die numerische Ruhe, mit `None` als ehrlicher Antwort bei
degenerierten Landmarks (etwa fünf identischen Punkten).

Der Warp selbst invertiert diese 2×3-Matrix analytisch — bei einer Ähnlichkeit
genügt $det = a² + b²$ — und tastet dann jedes der 112×112 Zielpixel bilinear
aus dem Quellbild ab, mit schwarzem Rand wie `cv2.warpAffine(borderValue=0)`.
Der Test `fit_recovers_known_transform` dreht den Spieß um: Er erfindet eine
bekannte Trafo (Skale 2, 30 Grad, Translation), rechnet rückwärts die
Quellpunkte aus und verlangt, dass der Fit sie auf $10^{-2}$ genau
reproduziert. Mathematik ohne Mathe-Crate — der Stolz des Moduls.

### Deep Dive 2: Das Rätsel der 5-Pixel-Geisterboxen

Der erste Ende-zu-Ende-Lauf mit echten Modellen war ein Triumph und ein
Desaster zugleich. Der Triumph: SCRFD feuerte auf dem Ross-Porträt mit
Konfidenzen bis 0.82 — das Modell sah also Gesichter. Das Desaster: Die
Boxen maßen 5×8 Pixel. Vier winzige Rechtecke mit exzellenten Scores,
dicht beieinander, keines davon ein Gesicht. Kein Crash, keine NaNs, nur
selbstbewusster Unsinn.

Die Erklärung liegt im Multi-Stride-Anchor-Prinzip von SCRFD. Das Netz sieht
das Bild auf drei Pyramiden-Ebenen gleichzeitig: Bei Stride 8 teilt es die
640 Pixel in ein 80×80-Raster, bei Stride 16 in 40×40, bei Stride 32 in
20×20. Jede Rasterzelle ist ein Anker-Center, verdoppelt auf zwei Anker pro
Position (`_num_anchors = 2`), insgesamt also 12800 + 3200 + 800 = 16800
Kandidaten. Das Netz sagt nun nicht absolute Koordinaten voraus, sondern
Distanzen vom Anker-Center zu den vier Box-Kanten — aber in Einheiten von
*Rastersschritten*, nicht Pixeln. Der Referenzcode multipliziert deshalb vor
dem Decodieren: `bbox_preds * stride`. Genau diese Zeile fehlte bei uns. Die
Folge war exakt um den Faktor 8 bis 32 zu klein: Ein Gesicht von 158×243
Pixeln schrumpfte auf Geisterboxen von 5×8 Pixeln, während die Scores —
stride-unabhängig — strahlend hoch blieben. Besonders perfide: Der Fehler sah
in Unit-Tests mit synthetischen Tensoren plausibel aus, weil dort niemand die
absolute Größe hinterfragt hatte.

Der Fix war eine Zeile pro Koordinate — `c[0] - b4[0] * st` statt
`c[0] - b4[0]`, analog für Keypoints — doch die Lehre sitzt tiefer und steht
seitdem als Regressionstest im Code: Der E2E-Test mit dem Ross-Porträt pinnt
*exakt eine* Box an (201, 111 bis 359, 354, Score 0.82). Jede künftige
Decode-Regression, die wieder Pixelkrümel oder Duplikate produziert, lässt
diesen Test sofort rot werden. Der einzige ehrliche Decode-Test ist ein echtes
Gesicht.

### Deep Dive 3: Wenn ONNX Runtime lügt — die cuDNN-Falle

Die Hardware-Strategie klang simpel: CUDA zuerst versuchen, bei Fehlern auf
multi-threaded CPU zurückfallen. Die Implementierung folgte dem
Python-Referenzcode (`providers=[CUDA, CPU]`) und sah zunächst korrekt aus:
Session mit CUDA-Provider bauen, bei `Err` eben CPU nehmen. Der CUDA-Build
kompilierte, der Commit meldete `Ok` — und der erste Frame explodierte mit
einem harten Fehler aus den Tiefen der ONNX Runtime:

> `Conv_0 ... NOT_IMPLEMENTED: cuDNN is unavailable ... dlopen failed for
> libcudnn.so`

Der Commit lügt. Die Session-Erstellung prüft lediglich, dass der CUDA-Provider
registrierbar ist; ob seine Kernabhängigkeit cuDNN tatsächlich geladen werden
kann, stellt sich erst heraus, wenn der erste Faltungsknoten (Conv) wirklich
rechnet — also mitten im Hot-Path, wo bei uns ein `unwrap` wartete. Auf der
Workstation mit vollständigem CUDA-Stack wäre das nie aufgefallen; im Docker-
Container mit reinen Runtime-Libs (CUDA 13.x, ohne Compiler, ohne cuDNN) war
es ein garantierter Crash statt des versprochenen Fallbacks.

Die Lösung heißt `try_cuda` und folgt einem einfachen Prinzip: Behaupte
nichts, was du nicht bewiesen hast. Nach dem Commit führt die Funktion eine
Probe-Inferenz mit einem Null-Tensor in exakt der Input-Form des jeweiligen
Modells aus — `[1, 3, 640, 640]` für SCRFD, `[1, 3, 112, 112]` für ArcFace.
Gelingt sie, ist CUDA echt und die Session wird mit dem Label `"CUDA"`
übernommen; scheitert sie an irgendeiner Stelle, fällt die Funktion auf
`None` zurück und der Caller baut still eine CPU-Session. Die Kosten: eine
zusätzliche Inferenz beim Start, also Millisekunden. Der Lohn: Der CUDA-Build
lief im cuDNN-losen Container mit `provider=CPU` und Exit-Code 0 durch den
Xvfb-Smoke — ein Fallback, der seinen Namen verdient, inklusive ehrlicher
Anzeige im HUD.

Zwei kleinere `ort`-Erkenntnisse fielen auf dem Weg ab und sind es wert,
festgehalten zu werden: In `ort 2.0.0-rc.13` heißen die Provider-Typen
`CPU` und `CUDA` (nicht `CPUExecutionProvider`), und `BuilderResult` trägt
einen wiederherstellbaren Fehlertyp, der im `and_then` die Form
`Ok(b.with_execution_providers(...)?)` erzwingt. Beide Fakten stammen aus der
Registry-Quelle, denn die offizielle Doku-Seite antwortete mit 404. Und weil
`commit_from_file` das `std`-Feature von `ort` verlangt, lesen wir Modelle
schlicht in den Speicher und nutzen `commit_from_memory` — bei 2,5 und
13,6 MB ohne jeden Nachteil, ganz wie in `source2`.

### Weitere Entscheidungen im Kurzporträt

Nicht jede Entscheidung verdient einen eigenen Akt, aber drei verdienen einen
Absatz. Erstens: Die `Detector`-/`Embedder`-Trait-Implementierungen für die
echten ONNX-Typen leben in [07_engine.rs](/workspace/src/cl-rust-generator/examples/26_onnx/source7/src/07_engine.rs),
nicht in `main.rs` — nur so kann der E2E-Test die echten Sessions durch die
echte Engine jagen. Zweitens: Das Binary druckt am Ende eine Stats-Zeile
(`stats frames=… faces=… persons=… exemplars=… provider=…`), die headless
Läufe auswertbar macht und nebenbei Automations-Nutzern dient. Drittens: Die
Datei-Regel des Prompts — eine Zuständigkeit pro Datei, maximal rund 300
Zeilen — ist eingehalten; die größte Datei (`04_scrfd_detector.rs`) misst
270 Zeilen, der Rest liegt bei höchstens 260.

## 3. Re-ID, Online-Lernen & Persistenz

### Drei Zonen, eine Philosophie

Das Herz der Wiedererkennung ist keine Datenbank im klassischen Sinn, sondern
eine Exemplar-Bank mit einer klaren Philosophie: Niemals mitteln, immer
sammeln. Jede Person wird durch bis zu fünf konkrete Beobachtungen
repräsentiert — je ein 512D-Vektor plus den zugehörigen 112×112-RGB-Thumbnail.
Warum keine Mittelwerte? Weil ein gemitteltes Gesicht ein Niemandsgesicht ist:
Der Durchschnitt aus Frontal- und Profilansicht liegt im Niemandsland des
Embedding-Raums und matcht auf alles ein bisschen und auf nichts richtig.
Separate Exemplare dagegen bewahren die echte Varianz — Brille auf, Brille ab,
Blick nach links — und der Best-Match über alle Exemplare (`S_max`, schlicht
das maximale Skalarprodukt dank L2-Normierung) findet die ähnlichste je
gesehene Ansicht.

Um `S_max` herum definiert der Prompt drei Zonen, die das Online-Lernen
steuern. Oberhalb von 0.65 ist die Person bekannt — doch innerhalb dieser Zone
wartet eine zweite, subtilere Entscheidung: Zwischen 0.65 und 0.88 gilt die
Beobachtung als *diversifiziert*, also als neuer Blickwinkel, der als Exemplar
gespeichert wird (bei vollem Haus nach FIFO: das älteste fliegt). Über 0.88
dagegen ist das Bild redundant — fast identisch mit etwas, das wir schon haben
— und wird verworfen, um die Bank nicht mit Duplikaten zu fluten. Unterhalb
von 0.45 beginnt jemand Neues: neue ID, erstes Exemplar. Und dazwischen, von
0.45 bis 0.65, liegt die Ambivalenz-Zone des Zweifels: Das Gesicht wird
weiter getrackt — Box und Crop erscheinen in der UI — aber nichts wird in die
Datenbank geschrieben. Diese Zurückhaltung ist Absicht: Ein falscher
Datenbankeintrag vergiftet alle Zukunft, ein ausgelassener Frame kostet nichts.

### Das Serde-512D-Dilemma

Die Persistenz klingt trivial — `serde` und `bincode` nach `faces_db.bin` —
und war es fast. Fast, denn Rusts `serde` implementiert Array-Serialisierung
nur bis Länge 32, und unser Embedding ist `[f32; 512]`. Der Compiler meldete
nüchtern `the trait bound [f32; 512]: Serialize is not satisfied`, und die
saubere Antwort war Handarbeit: [01_types.rs](/workspace/src/cl-rust-generator/examples/26_onnx/source7/src/01_types.rs)
serialisiert `Embedding512` als Sequenz von 512 Floats und deserialisiert sie
über einen eigenen Visitor, der exakt 512 Elemente verlangt. Dazu kam eine
zweite Stolperfalle: `bincode` 2.x spricht von Haus aus seine eigenen
`Encode`/`Decode`-Traits, nicht Serde — erst das `serde`-Cargo-Feature
öffnet `bincode::serde::encode_to_vec` und `decode_from_slice`. Der Lohn ist
eine Roundtrip-garantierte Datenbank, deren Test Personen samt Thumbnails
byte-identisch durch TempDir schreibt und zurückliest.

## 4. Verifikation & Test-Philosophie

Unsere Testpyramide hat vier Stockwerke, und jedes beantwortet eine andere
Frage. Ganz unten stehen zwanzig Unit-Tests, die reine Mathematik ohne jede
Hardware prüfen: Erholt der Umeyama-Fit eine bekannte Trafo? Reproduziert der
Identitäts-Warp exakt die Pixel? Stimmen die Ankerzahlen (12800/3200/800) und
die Duplikation pro Position? Rechnet das Decode exakt Boxen und Keypoints aus
synthetischen Tensoren? Überlebt im NMS-Beispiel die stärkste Box? Bleibt der
Null-Vektor NaN-frei? Bilden die Schwellen 0.44/0.45/0.64/0.65/0.88/0.89 exakt
die Zonen ab? Deckt die FIFO bei fünf Exemplaren? Jeder dieser Tests läuft in
Millisekunden und braucht weder Display noch Modell.

Darüber sitzen zwei Modell-Vertragstests (`tests/with_models.rs`, per Default
ignoriert), die das echte `.onnx` nach seinem Layout befragen: SCRFD muss
genau neun Outputs mit den exakten Längen liefern, ArcFace genau 512
Nicht-Null-Werte. Sie schützen vor dem Albtraum jeder ONNX-Pipeline — einem
neu exportierten Modell mit stillschweigend vertauschten Outputs. Das dritte
Stockwerk ist der E2E-Test `real_models_detect_and_reidentify_face`: echte
Sessions, echtes Ross-Porträt als eingebettetes PPM-Asset, ein Frame durch
die komplette Engine — und die harte Behauptung, dass genau ein Gesicht mit
Person 0 herauskommt und der zweite Frame redundant wiedererkannt wird. Ganz
oben schließlich der `xvfb-run`-Smoke des fertigen Binaries: fünf Frames unter
virtuellem X-Server, Exit 0, Stats-Zeile auf stdout.

Bemerkenswert ist, was die Tests *nicht* fangen konnten — und was doch: Die
Geisterboxen-Geschichte aus Deep Dive 2 überlebte alle synthetischen Tests,
weil diese die absolute Boxgröße nie hinterfragten; erst das Echtgesicht
sprach das Urteil. Umgekehrt bewies der Schwellen-Test seine Existenz-
berechtigung, als er mich zweimal auf falsch berechnete Testvektoren hinwies:
Der Best-Match läuft über *alle* Exemplare, und meine handgerechneten
Erwartungen hatten das zweite Exemplar vergessen — der Produktcode hatte von
Anfang an recht. Tests, die den Autor belehren, sind die besten Tests.

## 5. Performance-Analyse & Hardware-Realität

Die Zahlen, Release-Build auf dem Threadripper PRO 7955WX, CPU-Pfad: 100
Frames aus Capture, Detektion und Rendering in 1036 Millisekunden Wandzeit
einschließlich Startup — effektiv rund 96 FPS. Der E2E-Test mit Gesicht
(zwei Sessions laden plus zwei volle Frames mit Alignment, Embedding und
DB-Update) braucht 0,09 Sekunden. Das ist kein Messfehler, sondern die
Konsequenz bewusster Modellwahl: `det_500m` trägt seinen Namen, weil es mit
500 Mega-FLOPs auskommt — ein Federgewicht, das auf 32 Threads mit AVX2
praktisch verfliegt — und das MobileFaceNet-Embedding auf 112×112 Pixeln ist
kaum mehr als ein Husten. Der Engpass liegt längst nicht mehr in der
Inferenz, sondern wie schon in `source2/doc.md` analysiert im synchronen
X11-Grab und im V-Sync des Fensters.

Ehrlichkeit verlangt das Eingeständnis: Eine CUDA-Messung gibt es nicht. Im
Container fehlt `libcudnn.so`, der CUDA-EP scheitert reproduzierbar am ersten
Faltungsknoten, und der Fallback greift — nachgewiesen, nicht vermutet
(`provider=CPU` im Smoke des CUDA-Builds). Für die Workstation mit
vollständigem CUDA-Stack genügt `--features cuda`, und die Erwartung lautet
mehrere hundert FPS Inferenz — wobei dann erst recht X11 und V-Sync
limitieren. Für den Ryzen-Laptop ist die Botschaft noch besser: Was hier auf
der CPU 96 FPS schafft, braucht keine GPU, um flüssig zu sein. Die
Multi-Thread-Konfiguration (`available_parallelism` Intra-Op-Threads) nimmt
mit, was der Laptop hergibt, und die Architektur — native 640 ohne Resize,
keine Kopien, keine Wartezeiten — ist auf beiden Plattformen dieselbe.

## 6. Learnings & Ausblick für das Dockerfile

Vier Lektionen bleiben, sortiert nach Bissigkeit. Erstens: *Traue keinem
Commit.* Ob ein Execution Provider wirklich rechnen kann, beweist nur eine
Probe-Inferenz — alles andere kracht im Hot-Path statt beim Start. Zweitens:
*Der einzige ehrliche Decode-Test ist ein echtes Gesicht.* Synthetische
Tensoren prüfen Arithmetik, aber erst ein Porträt mit bekannter Antwort
prüft Semantik. Drittens: *Bei `ort` gilt die Registry-Quelle.* Typnamen und
Fehlertypen der Version 2.0.0-rc.13 waren nur im heruntergeladenen Quelltext
verlässlich zu finden; die Web-Doku antwortete mit 404. Viertens, eher
hausmeisterlich: Rust 1.98 will `as_chunks` statt `chunks_exact` mit
Konstanten und `is_multiple_of` statt Modulo-Vergleich — Lints, die man einmal
lernt und nie wieder sieht.

Für das Dockerfile fallen zwei Installationserinnerungen an. Zur Laufzeit
braucht das miniquad-X11-Backend `libxkbcommon0`, `libgl1`, `libxi6` und
`libxcursor1` (plus die mitgezogene `libxfixes3`) — ohne sie stirbt das Binary
beim Start mit `DlOpenError`. Nur zur Test-Asset-Erzeugung diente `python3-pil`
(PNG→PPM, wohlgemerkt über System-`/usr/bin/python3`, nicht das Workspace-
venv); für Laufzeit-Images ist es optional. Bereits vorhanden und genutzt
wurden `xvfb`/`xvfb-run`, die CUDA-Runtime-Libs der Version 13 (ohne Compiler
und ohne cuDNN) sowie Rust 1.98.1. Die Modelle — `det_500m.onnx` (2,5 MB) und
`w600k_mbf.onnx` (13,6 MB) aus dem Release v0.0.1 von
`yakhyo/face-reidentification` — lädt `source7/download_models.sh` per curl
mit Größenprüfung nach; im Git landen sie dank `.gitignore` nie, verifiziert
per `git check-ignore`.

Was bleibt? Ein System, das tut, was es verspricht, und verspricht, was es
beweisen kann — vom Gauß-Fit über die Geisterboxen bis zum ehrlichen
`provider=CPU`. Die Pipeline ist klein genug, um sie an einem Nachmittag zu
lesen (1529 Zeilen über acht Module), und getestet genug, um sie anzufassen.
Der nächste Schritt, wenn die Workstation cuDNN bekommt, ist ein Einzeiler:
`--features cuda`. Und dann sehen wir, wie viele hundert FPS ein 500-MFLOP-
Modell auf einer RTX A4000 wirklich schafft.
