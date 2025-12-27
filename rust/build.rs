//! Build script for compiling Protocol Buffer definitions

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Tell cargo to recompile if the proto files change
    println!("cargo:rerun-if-changed=proto/messages.proto");
    println!("cargo:rerun-if-changed=proto/services.proto");

    // Configure tonic-build
    tonic_build::configure()
        .build_server(true)
        .build_client(true)
        .out_dir("src/generated")
        .compile(
            &["proto/services.proto"],
            &["proto"],
        )?;

    Ok(())
}
