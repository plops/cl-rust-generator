for i in ./Cargo.toml \
	     ./client/Cargo.toml client/src/*.rs client/src/bin/*.rs client/tests/*.rs \
	     ./common/Cargo.toml common/src/*.rs \
	     ./log/Cargo.toml log/src/*.rs log/src/bin/*.rs log/tests/*.rs \
	     ./server/Cargo.toml server/src/*.rs server/tests/*.rs
do
    echo "// start of "$i
    cat $i
done

echo "schaue den code an. das ziel ist mit moeglichst wenig code eine remote control loesung fuer schmale verbindungen zu bauen. wir haben aufloesung auf 1280x720 fixiert (protokoll v2) und lassen den ocr-detektor per onnx cuda-ep auf der gpu laufen, den erkenner auf cpu (hybrid, gemessen). neu in source10: instrumentierung per --record in .lbwlog-dateien (lbw-log-crate), offline-analyse mit lbw-logstat und headless-replay mit lbw-replay. ist der code so minimal wie es geht? pruefe insbesondere, ob das log-format und die statistik-abdeckung ausreichen, um einen 6kb/s-kanal zu charakterisieren und input/text/bild-latenzen zu messen."
