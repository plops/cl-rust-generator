for i in ./Cargo.toml \
	     ./client/Cargo.toml client/src/*.rs \
	     ./common/Cargo.toml common/src/*.rs \
	     ./server/Cargo.toml server/src/*.rs
do
    echo "// start of "$i
    cat $i
done

echo "schaue den code an. das ziel ist mit moeglichst wenig code eine remote control loesung fuer 6kB/s verbindungen zu bauen. ist der code so minimal wie es geht (wir haben aufloesung auf 640x640 fixiert). vielleicht koennte man noch code sparen indem wir keinen fallback fuer fehlende modelle erlauben. ausserdem schaue dir an wie wir av1 codieren. frueher setzten wir mal die tiles zum groesstmoeglichen rechteck zusammen um datenrate zu minimieren, ist es wie es jetzt ist okay oder was ist der beste weg?"
