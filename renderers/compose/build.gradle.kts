plugins {
    kotlin("jvm") version "2.0.21"
    kotlin("plugin.serialization") version "2.0.21"
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.7.3")
    testImplementation(kotlin("test"))
}

kotlin {
    jvmToolchain(17)
}

application {
    // Pure-JVM conformance entry point.
    mainClass.set("conformance.MainKt")
}

// `./gradlew conformance` is a friendly alias for running the conformance
// runner. It only uses the root (pure-JVM) runtime classpath, so it never
// pulls in Compose or Android.
tasks.register<JavaExec>("conformance") {
    group = "verification"
    description = "Applies golden fixtures (mount + patches) and deep-compares to final_tree."
    classpath = sourceSets["main"].runtimeClasspath
    mainClass.set("conformance.MainKt")
    // Fixtures live at <repo>/conformance/fixtures; this project dir is
    // <repo>/renderers/compose, so the runner resolves "../../conformance/fixtures".
    workingDir = projectDir
}

tasks.named<JavaExec>("run") {
    workingDir = projectDir
}

tasks.test {
    useJUnitPlatform()
}
