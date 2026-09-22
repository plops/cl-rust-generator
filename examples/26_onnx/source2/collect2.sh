for i in Cargo.toml src/main.rs ../source1/src/main.rs ../source1/Cargo.toml
do
    echo "// start of "$i
    cat $i
done

echo "
your task is to adapt src/main.rs to apply the paddlepaddle ocr AI weights and visualize the results on the image. for the rendering of the text you can take code from source1 but keep dependencies minimal."
