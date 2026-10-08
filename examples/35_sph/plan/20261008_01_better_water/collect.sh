for i in plan/20261006_*/walkthrough.md \
	     *.toml \
	     src/*.rs \
	     tests/*.rs
do
    echo "// start of "$i
    cat $i
done

cat <<EOF
das wasser sieht irgendwie nicht zusammenhaengend aus, es fliegt auseinander
validiere dass die implementierung korrekt ist, bzw erklaere was falsch ist
EOF


