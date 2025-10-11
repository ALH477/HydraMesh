use std::io::Result;

fn main() -> Result<()> {
    std::fs::create_dir_all("src/proto")?;
    tonic_build::configure()
        .out_dir("src/proto")
        .compile(&["proto/messages.proto", "proto/services.proto"], &["proto"])?;
    Ok(())
}
