plugins {
    alias(libs.plugins.android.application)
}

// Single source of truth: [workspace.package] version in source6/Cargo.toml.
// versionCode = major*10000 + minor*100 + patch (minor, patch < 100).
val lbwVersion: String = rootProject.file("../../Cargo.toml").readLines()
    .dropWhile { it.trim() != "[workspace.package]" }
    .first { it.trim().startsWith("version") }
    .substringAfter('"').substringBefore('"')
val lbwVersionCode: Int = lbwVersion.substringBefore('-').split('.').map(String::toInt)
    .let { (major, minor, patch) -> major * 10000 + minor * 100 + patch }

// Release signing from the environment (CI: GitHub secrets, see source6/RELEASE.md).
// Without LBW_KEYSTORE the release APK is signed with the debug key.
fun env(name: String): String? = providers.environmentVariable(name).orNull?.takeIf { it.isNotEmpty() }

android {
    namespace = "de.lbw.client"
    compileSdk = 36
    ndkVersion = "30.0.16248370"

    defaultConfig {
        applicationId = "de.lbw.client"
        minSdk = 26
        targetSdk = 36
        versionCode = lbwVersionCode
        versionName = lbwVersion
        ndk { abiFilters += listOf("arm64-v8a", "x86_64") }
    }

    signingConfigs {
        env("LBW_KEYSTORE")?.let { ks ->
            create("release") {
                storeFile = file(ks)
                storePassword = env("LBW_KEYSTORE_PASSWORD")
                keyAlias = env("LBW_KEY_ALIAS")
                keyPassword = env("LBW_KEY_PASSWORD") ?: env("LBW_KEYSTORE_PASSWORD")
            }
        }
    }

    buildTypes {
        release {
            // No R8: JNI entry points are looked up by name
            isMinifyEnabled = false
            signingConfig = signingConfigs.findByName("release") ?: signingConfigs.getByName("debug")
        }
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }

    packaging {
        // liblbw_core.so is already stripped by cargo (release profile)
        jniLibs.keepDebugSymbols += "**/liblbw_core.so"
    }

    testOptions {
        unitTests.isReturnDefaultValues = true
    }
}

kotlin {
    compilerOptions {
        jvmTarget = org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_17
    }
}

dependencies {
    implementation(libs.jsch)
    testImplementation(libs.junit)
}

// JVM unit tests load the host build of lbw-core (cargo build -p lbw-core)
val hostLibDir = rootProject.file("../../target/debug").absolutePath
tasks.withType<Test>().configureEach {
    systemProperty("java.library.path", hostLibDir)
    systemProperty("lbw.hostLib", hostLibDir)
    inputs.dir(hostLibDir).optional().withPropertyName("hostLib")
    // SshTunnelTest runs only with a test sshd; re-run when that changes
    for (v in listOf("LBW_SSHD_PORT", "LBW_SSHD_USER", "LBW_SSHD_KEY", "LBW_SSHD_PWUSER", "LBW_SSHD_PASSWORD")) {
        inputs.property(v, providers.environmentVariable(v).orElse(""))
    }
    testLogging {
        events("passed", "skipped", "failed")
        exceptionFormat = org.gradle.api.tasks.testing.logging.TestExceptionFormat.FULL
        showStandardStreams = false
    }
}
