for i in Cargo.toml src/*.rs
do
    echo "// start of "$i
    cat $i
done
