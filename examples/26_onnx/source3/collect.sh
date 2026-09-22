for i in ../source2/Cargo.toml ../source2/src0/main.rs ~/src/tract/README.md /home/kiel/src/tract/examples/face_detection_yolov8onnx_example/src/main.rs /home/kiel/src/tract/examples/face_detection_yolov8onnx_example/Cargo.toml
do
    echo "// start of "$i
    cat $i
done

echo "
your task is to convert ../source2/src0/main.rs to use tract instead of ort. the idea is to create a minimal dependency software that works on cpu. also write the new Cargo.toml"
