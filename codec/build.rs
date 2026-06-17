// SPDX-License-Identifier: LGPL-3.0-only
// Build script for the DCF wire/audio codec.
//
// With the `pm` feature, compile the committed Faust→C phase-mod synthesis
// (faust/dcf_pm_codec.gen.c) and link it so audio::pm can FFI into it. Without
// the feature this is a no-op and the crate has no C dependency.

fn main() {
    if std::env::var("CARGO_FEATURE_PM").is_ok() {
        println!("cargo:rerun-if-changed=faust/dcf_pm_codec.gen.c");
        cc::Build::new()
            .file("faust/dcf_pm_codec.gen.c")
            .flag_if_supported("-w") // generated Faust C is noisy; silence it
            .compile("dcf_pm_codec");
        println!("cargo:rustc-link-lib=m"); // sinf/cosf/powf in the Faust synth
    }
}
