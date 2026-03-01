cd  ~/stage/cl-rust-generator/examples/20_webprox_avif/

declare -a FILES=(
        01_goal.md
        03_plan.md
        04_implementation_plan.md
        05_architecture_and_validation.md
        06_implementation_deviation.md
        macroquad-client/src/main.rs
        macroquad-client/Cargo.toml
        Cargo.toml
)
for i in "${FILES[@]}"; do
        echo "// start of $i"
        cat "$i"
done | xclip


for i in "${FILES[@]}"; do
        echo "// start of $i"
        ls "$i"
done


the client implementation has a major flaw. grpc uses tokio which clashes with the threading model of macroquad. the client needs to have a separate visualisation thread which communicates with the grpc thread via channels or queues. create an implementation plan 09... 
that documents the new approach which will fix this behaviour.

also i note that the main.rs file is currently one very large file
consider the 05_architecture_and_validation.md instructions and split the main.rs file into smaller files at reasonable boundaries.