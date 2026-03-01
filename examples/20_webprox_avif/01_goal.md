Ähnlich wie ../19_webprox Soll dieses Projekt ein Cloud Server verwenden, der einen Browser laufen lässt und Webseiten dort rendert, auf einen Clienten, der eine sehr langsame Internetanbindung hat, die Webseite anzuzeigen.

Aber dieses Projekt hier soll von grafischer Natur sein. Es soll die geränderte Webseite als hochkomprimiertes Bild übertragen. 

Die Idee ist dabei, den effizienten AV1 Encoder RAV1E zu verwenden. 
https://github.com/xiph/rav1e
als decoder
https://github.com/memorysafety/rav1d

Auch inkrementelle Updates und Scrolling der Webseite sollen darüber abgewickelt werden. 

Der Client soll mit einer möglichst einfachen GUI die Bilder anzeigen. Ich denke, das beste Projekt dafür ist:
https://github.com/not-fl3/miniquad

Wahrscheinlich brauchen wir auch Text Output (also Fonts) und andere Dinge, zum Beispiel Rechtecke zu zeichnen. Deshalb wird die folgende Bibliothek wahrscheinlich auch nützlich sein. 
https://github.com/not-fl3/macroquad

Nutze bei der Planung das DeepWiki MCP oder die dokumentation vom deepwiki https://deepwiki.com/not-fl3/macroquad (die verzeichnisstruktur ist equivalent zu github) m erkundungen ueber die Bibliotheken zu machen und um die beste Loesung zu finden.

  
Bei dieser kann man wahrscheinlich aber sehr viele Features abschalten, die Dependencies gering zu halten. Denn eines meiner Ziele ist bloat zu vermeiden. Ein anderes ist cross-platform zu sein und CI builds mit github actions zu ermoeglichen.
Fuer die ersten iterationen beschraenken wir uns jedoch auf linux fuer server und client.

Wie beim ../19_webprox Projekt soll auch hier der Server mittels gRPC angebunden sein. Die Datenkommunikation soll optional über Zertifikate verschlüsselt werden.

Der Browser auf dem Cloud-Server soll normalerweise im Headless-Modus  laufen, aber für debugging-Zwecke soll auch ein normaler Modus, wo man das Bild sieht, eingeschaltet werden können. Schaue dir das ../19_webprox Projekt genauer an und schlage Optionen vor, die sinnvoll sind, wie zum Beispiel für Konfigurationshandling und sowas.

Erstelle einen Plan für die neue Implementierung dieser Idee. Gehe dabei bereits auf die Strukturierung des Programms ein. Wir wollen eine gute Maintainable Architektur verwenden. Unter Nutzung von Programming Patterns wollen wir die einzelnen Module voneinander entkoppeln. Und der Plan soll das bereits ordentlich erläutern und diskutieren. 

Auch die Testinfrastruktur soll im Plan bedacht werden. Da wir KI-Agenten für die Entwicklung benutzen, ist es nützlich, jedem der Programme ausreichende Kommandozeilenparameter zu geben. Der Client sollte zum Beispiel eine Anfrage absetzen und die empfangenen Informationen als Text oder Bild in eine Datei abspeichern, sodass der Agent die Resultate überprüfen kann.  
Damit sollte es recht einfach möglich sein, Integrationstests umzusetzen, die der Agent selbstständig ausführt. Und die dann später vielleicht auch automatisiert werden können, ohne agenten Einsatz. 

Vielleicht nicht im ersten Implementierungsschritt, aber vorgesehen sollte es schon sein, ist die Volltextsuche. Vielleicht sollte die vom Browser übernommen werden. Oder es sollte eine Möglichkeit geben, über das GRPC Protokoll den Text der Webseite zu übertragen. Wobei darin weder das Problem besteht, dass es nicht besonders einfach ist, eine Webseite in Text umzuwandeln. Für die Suche könnte es aber ausreichen. 

In ähnlichen Zusammenhang sollten die Bilddaten mit Metadaten erweitert werden. Wir haben ja wahrscheinlich die Titelseite, aber die Position von Links und die Links selbst sollten auch mit angezeigt werden. Während die Bildwarten nur einen beschränkten Ausschnitt der Webseite anzeigen, könnten die Links alle beim ersten Mal schon gesendet werden, sodass die GUI beim Scrollen zumindest die Links anzeigt, bevor das Bild kommt. Aber diese Zusatzdaten sollten alle vom GAPC-P Protokoll im Klienten ein- oder ausgeschalten werden können. 

Der Bildstream von Chrome sollte im Server so roh wie moeglich gelesen werden.

cd  /home/kiel/stage/cl-rust-generator/examples/20_webprox_avif/


declare -a FILES=(
../19_webprox/rust_grpc_remote_browser_proxy.md
../19_webprox/plan.md
../19_webprox/README.md
../19_webprox/CHANGELOG.md
../19_webprox/Cargo.toml
../19_webprox/cloud-proxy-server/Cargo.toml
../19_webprox/minimal-tui-client/Cargo.toml
../19_webprox/proto-definitions/Cargo.toml
../19_webprox/doc/010_release_process.md
../19_webprox/doc/050_case_study.md
../19_webprox/doc/051_case_study_compression.md
../19_webprox/doc/080_house_keeping.md
../19_webprox/proto-definitions/proto/browser.proto
../19_webprox/cloud-proxy-server/tests/integration.rs
../19_webprox/chrome-test/src/main.rs
../19_webprox/chrome-test/Cargo.toml
../19_webprox/setup01_build.sh
../19_webprox/setup02_clean.sh
../../.github/workflows/release-19-webprox.yml
)
for i in "${FILES[@]}"; do
        echo "// start of $i"
        cat "$i"
done | xclip