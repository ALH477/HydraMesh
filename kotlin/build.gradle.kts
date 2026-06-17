// SPDX-License-Identifier: LGPL-3.0-only
//
// Kotlin/JVM module for the DCF DeModFrame wire codec.
//   gradle run    -> certifies the codec against ../Documentation/golden_vectors.json
//   gradle build  -> compiles the codec + UDP transport (DcfNode)
//
// The dependency-free certification (no JSON library, no test framework) lives in
// dcf.CertifyKt; CI also runs it directly via `kotlinc` (see certify-kotlin in
// .github/workflows/wire-certify.yml).

plugins {
    kotlin("jvm") version "2.0.21"
    application
}

repositories {
    mavenCentral()
}

kotlin {
    jvmToolchain(21)
}

application {
    mainClass.set("dcf.CertifyKt")
}
