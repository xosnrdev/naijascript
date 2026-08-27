plugins {
    kotlin("jvm") version "2.0.21"
    // The Compose compiler plugin is required for Kotlin 2.0+.
    id("org.jetbrains.kotlin.plugin.compose") version "2.0.21"
    // JetBrains Compose (Compose Multiplatform / Desktop). This is a pure-JVM
    // Compose target: it needs NO Android SDK.
    id("org.jetbrains.compose") version "1.7.1"
}

repositories {
    mavenCentral()
    google()
    maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
}

dependencies {
    // Reuse the shared protocol model + retained-tree engine from the root module.
    implementation(project(":"))

    // Compose runtime/UI surface needed to compile the @Composable renderer.
    implementation(compose.runtime)
    implementation(compose.foundation)
    implementation(compose.material)
    implementation(compose.ui)
}

kotlin {
    jvmToolchain(17)
}
