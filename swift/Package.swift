// swift-tools-version:5.7
// SPDX-License-Identifier: LGPL-3.0-only
import PackageDescription

let package = Package(
    name: "DCFWire",
    products: [
        .library(name: "DCFWire", targets: ["DCFWire"]),
    ],
    targets: [
        .target(name: "DCFWire", path: "Sources/DCFWire"),
        .testTarget(
            name: "DCFWireTests",
            dependencies: ["DCFWire"],
            path: "Tests/DCFWireTests"
        ),
    ]
)
