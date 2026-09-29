plugins {
    alias(libs.plugins.android.application)
}

android {
    namespace = "de.lbw.client"
    compileSdk = 36
    ndkVersion = "30.0.16248370"

    defaultConfig {
        applicationId = "de.lbw.client"
        minSdk = 26
        targetSdk = 36
        versionCode = 1
        versionName = "0.6.0"
        ndk { abiFilters += listOf("arm64-v8a", "x86_64") }
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            // Unsigned by default; CI uses the debug build
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
