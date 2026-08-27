// swift-tools-version:5.7
import PackageDescription

// NaijaUI is a renderer-neutral UI protocol client for the NaijaScript engine.
//
// The package is split into two targets:
//   * NaijaUI     - pure-Swift library. The Codable protocol model and the
//                   retained-tree patch engine build on ANY platform with a
//                   Swift toolchain. The SwiftUI renderer lives here too but is
//                   compiled only behind `#if canImport(SwiftUI)`, so the
//                   library still builds where SwiftUI is unavailable (e.g.
//                   Linux CI).
//   * Conformance - executable that replays golden fixtures and deep-compares
//                   the resulting tree against `final_tree`. It depends only on
//                   the pure-Swift portion of NaijaUI, so it runs anywhere.
let package = Package(
    name: "NaijaUI",
    products: [
        .library(name: "NaijaUI", targets: ["NaijaUI"]),
        .executable(name: "Conformance", targets: ["Conformance"]),
    ],
    targets: [
        .target(name: "NaijaUI"),
        .executableTarget(
            name: "Conformance",
            dependencies: ["NaijaUI"]
        ),
    ]
)
