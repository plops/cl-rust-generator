for i in Cargo.toml src/main.rs
do
    echo "// start of "$i
    cat $i
done

echo "
factor the code to a good minimum. we don't need to parse txt dictionaries for example.
compile the dictionary in together with the weights.

here are the relevant files:


kiel@localhost ~/stage/cl-rust-generator/examples/26_onnx/source4 $ find .
.
./PP-OCRv6_small_det.onnx
./Cargo.toml
./PP-OCRv6_small_rec.onnx
./Cargo.lock
./inference.yml
./src/main.rs
"
