Ähnlich wie ../19_webprox Soll dieses Projekt ein Cloud Server verwenden, der einen Browser laufen lässt und Webseiten dort rendert, auf einen Clienten, der eine sehr langsame Internetanbindung hat, die Webseite anzuzeigen.

Aber dieses Projekt hier soll von grafischer Natur sein. Es soll die geränderte Webseite als hochkomprimiertes Bild übertragen. 

Die Idee ist dabei, den effizienten AV1 Encoder RAV1E zu verwenden. 
https://github.com/xiph/rav1e

Auch inkrementelle Updates und Scrolling der Webseite sollen darüber abgewickelt werden. 

Der Client soll mit einer möglichst einfachen GUI die Bilder anzeigen. Ich denke, das beste Projekt dafür ist:
https://github.com/not-fl3/miniquad

Wahrscheinlich brauchen wir auch Text Output (also Fonts) und andere Dinge, zum Beispiel Rechtecke zu zeichnen. Deshalb wird die folgende Bibliothek wahrscheinlich auch nützlich sein. 
https://github.com/not-fl3/macroquad

Nutze bei der Planung das DeepWiki MCP um erkundungen ueber die Bibliotheken zu machen und um die beste Loesung zu finden.

  
Bei dieser kann man wahrscheinlich aber sehr viele Features abschalten, die Dependencies gering zu halten.

Wie beim ../19_webprox Projekt soll auch hier der Server mittels g angebunden sein. Die Datenkommunikation soll optional über Zertifikate verschlüsselt werden.

Der Browser auf dem Cloud-Server soll im Headless-Modus normalerweise laufen, aber für die Bugging-Zwecke soll auch ein normaler Modus, wo man das Bild sieht, eingeschaltet werden können. Schaue dir das ../19_webprox Projekt genauer an und schlage Optionen vor, die sinnvoll sind, wie zum Beispiel für Konfigurationshandling und sowas.

Erstelle einen Plan für die neue Implementierung dieser Idee. Gehe dabei bereits auf die Strukturierung des Programms ein. Wir wollen eine gute Maintainable Architektur verwenden. Unter Nutzung von Programming Patterns wollen wir die einzelnen Module voneinander entkoppeln. Und der Plan soll das bereits ordentlich erläutern und diskutieren. 

cd  /home/kiel/stage/cl-rust-generator/examples/20_webprox_avif/


declare -a FILES=(
../19_webprox/rust_grpc_remote_browser_proxy.md
)
for i in "${FILES[@]}"; do
        echo "// start of $i"
        cat "$i"
done | xclip