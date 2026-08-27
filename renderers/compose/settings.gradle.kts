rootProject.name = "naijascript-compose-renderer"

pluginManagement {
    repositories {
        gradlePluginPortal()
        mavenCentral()
        google()
        maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
    }
}

dependencyResolutionManagement {
    repositories {
        mavenCentral()
        google()
        maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
    }
}

// The root project is a pure-JVM module: protocol + engine + conformance runner.
// It has ZERO Compose/Android dependencies so `./gradlew conformance` runs
// against a plain JVM classpath.
//
// The `:ui` subproject holds the Jetpack Compose (Compose Desktop / JVM)
// renderer. It depends on the root module for the shared protocol model but is
// never on the conformance runner's classpath.
include(":ui")
