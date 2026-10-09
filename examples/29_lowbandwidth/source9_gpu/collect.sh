for i in ./Cargo.toml \
	     ./client/Cargo.toml client/src/*.rs \
	     ./common/Cargo.toml common/src/*.rs \
	     ./server/Cargo.toml server/src/*.rs
do
    echo "// start of "$i
    cat $i
done

echo "schaue den code an. das ziel ist mit moeglichst wenig code eine remote control loesung fuer schmale verbindungen zu bauen. wir haben aufloesung auf 1280x720 fixiert (protokoll v2) und lassen den ocr-detektor per onnx cuda-ep auf der gpu laufen, den erkenner auf cpu (hybrid, gemessen). ist der code so minimal wie es geht? ausserdem schaue dir an wie wir av1 codieren (eine bounding-box pro frame) und ob das padding des detektor-inputs auf 1280x736 statt tiling die richtige wahl ist."
