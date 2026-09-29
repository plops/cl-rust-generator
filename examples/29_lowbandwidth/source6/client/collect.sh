for i in *.toml src/*.rs;
do
    echo "// start of "$i
    cat $i
done
