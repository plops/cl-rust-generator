for i in Cargo.toml src/main.rs
do
    echo "// start of "$i
    cat $i
done

echo "
 want to have simple code with low dependencies. review the source code and improve/refactor it if you can. i also want to add a feature: print out the detected strings but avoid duplication (so if the frames don't change dont print the string again). ideally there should also be a change detector to not send frames into the AI algorithm if the image hasn't changed"
