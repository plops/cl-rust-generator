buildscript {
    dependencies {
        // Kotlin compiler version for AGP's built-in Kotlin support
        classpath(libs.kotlin.gradle.plugin)
    }
}
plugins {
    alias(libs.plugins.android.application) apply false
}
