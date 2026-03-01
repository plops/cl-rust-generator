fn main() -> Result<(), Box<dyn std::error::Error>> {
    tonic_build::configure()
        .build_server(true)
        .build_client(true)
        .out_dir("src")
        .compile_protos(&["proto/browser_stream.proto"], &["proto"])?;
    Ok(())
}
