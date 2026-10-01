wie kann ich wgpu in mein rust projekt einbinden aber minimale abhaengigkeiten erzeugen? ich moechte nicht hunderte abhaengigkeiten reinziehen, die ich nicht brauche. ich arbeite auf linux mit vulkan. 

## Kurzantwort

Deaktiviere die Default-Features und aktiviere nur `vulkan` (plus ggf. `wgsl`):

```toml
[dependencies]
wgpu = { version = "30", default-features = false, features = ["vulkan", "wgsl"] }
```

Damit vermeidest du vor allem den GLES-Backend-Stapel (`glow`, `glutin`, `khronos-egl` u. a.) sowie die WASM/WebGPU-Abhängigkeiten. [1](#0-0) 

---

## Details

### Was die Defaults reinziehen

Die Default-Features von `wgpu` sind `std`, `dx12`, `metal`, `gles`, `vulkan`, `wgsl` und `webgpu`. [1](#0-0)  Auf Linux aktivieren davon real nur `vulkan` und `gles` Code — die `dx12`/`metal`-Abhängigkeiten sind zwar target-konditioniert und werden auf Linux nicht kompiliert, aber `gles` zieht auf Linux tatsächlich `glow`, `glutin_wgl_sys`, `khronos-egl` usw. in den Dependency-Graph. [2](#0-1) [3](#0-2) 

### Vulkan nur auf Linux

Das `vulkan`-Feature aktiviert den Backend-Pfad nur auf Windows/Linux/Android/FreeBSD — auf Linux genau das, was du willst, mit `ash`, `gpu-allocator` usw. [4](#0-3) [5](#0-4)  Die Weiterleitung läuft über `wgpu` → `wgpu-core` → `wgpu-core-deps-linux-android-bsd` → `wgpu-hal`. [6](#0-5) [7](#0-6) 

### Shader-Sprache wählen

- `wgsl` brauchst du, wenn du WGSL-Shader lädst (meistens der Fall). [8](#0-7) 
- Nur vorcompilierte SPIR-V? Dann `features = ["vulkan", "spirv"]` statt `wgsl` — spart den WGSL-Parser in `naga`. [9](#0-8) 

### Hinweis zu `wgpu-core`/`wgpu-hal` direkt

Du könntest theoretisch direkt auf `wgpu-hal` aufsetzen (unsichere HAL, deutlich weniger Komfort) — für die meisten Projekte ist `wgpu` mit minimalen Features der richtige Weg. [10](#0-9) 

## Notes

- Die Workspace-Dependency im Repo selbst zeigt das Muster: `wgpu` wird dort mit `default-features = false` und expliziter Feature-Liste eingebunden. [11](#0-10) 
- MSRV von `wgpu` ist 1.87. [12](#0-11) 
- `cargo tree -p wgpu --edges features` hilft dir, den resultierenden Dependency-Graph zu prüfen.

Wiki pages you might want to explore:
- [Overview (gfx-rs/wgpu)](/wiki/gfx-rs/wgpu#1)
- [Crates Overview (gfx-rs/wgpu)](/wiki/gfx-rs/wgpu#1.2)
- [Build System and Workspace (gfx-rs/wgpu)](/wiki/gfx-rs/wgpu#6.4)

