# optics

A headless, differentiable 3D optical ray tracer. It traces a bundle of rays
through a sequence of spherical/planar refracting surfaces, measures the RMS
spot size on the image plane, and optimizes lens parameters (radius, thickness,
material index) by exact gradient descent. Gradients are computed with
forward-mode automatic differentiation (dual numbers), not finite differences.

Outputs: printed spot diagrams and focal metrics, a versioned `system.json` for
a Three.js viewer, and an interactive terminal telemetry view.

## Build and test

```sh
cargo build --release
cargo test            # unit + integration + doc-tests
cargo clippy
```

## CLI usage

```
optics trace    --config <toml>                              trace bundle, print spots
optics efl      --config <toml>                              effective focal length (mm)
optics optimize --config <toml> [--iters N] [--lr F] [--write-back <toml>]
optics export   --config <toml> --out <json>                 three.js system.json
optics tui      [--config <toml>]                            telemetry (q/Esc quits)
optics --help
```

`--config` defaults to `assets/sample.toml` when omitted.

### Examples

Trace the sample doublet and print where each ray lands:

```sh
cargo run -- trace --config assets/sample.toml
```

```
rays: 10/10 arrived, loss = 0.123456
ray 0: Image x=0.0000 y=0.0000 z=45.0000 l=0.5876
...
```

Report the effective focal length, back focus, and defocus:

```sh
cargo run -- efl --config assets/cooke.toml
```

Optimize the variables flagged in the config, overriding iteration count and
learning rate, then write the optimized prescription back to a new file:

```sh
cargo run -- optimize --config assets/sample.toml --iters 50 --lr 0.002 \
    --write-back optimized.toml
```

Export a traced system to JSON for the viewer:

```sh
cargo run -- export --config assets/double_gauss.toml --out system.json
```

Launch the interactive telemetry view (runs a descent first, then shows the
variable inspector and loss curve; press `q`, `Esc`, or `Enter` to exit):

```sh
cargo run -- tui --config assets/sample.toml
```

## TOML configuration schema

A configuration has one `[source]` table, an optional `[optimize]` table, and
an ordered list of `[[surfaces]]`. All lengths are in millimetres; wavelengths
are in micrometres (um).

### `[source]`

| Key                 | Type       | Default    | Meaning |
|---------------------|------------|------------|---------|
| `ray_count`         | integer    | `10`       | Rays sampled over the pupil. Laid out on a `ceil(sqrt(n))` square grid, then truncated to this count. |
| `grid_radius`       | float      | *(unset)*  | Pupil radius. Takes precedence over `aperture_diameter`. |
| `aperture_diameter` | float      | *(unset)*  | Full clear aperture; pupil radius is half of it. |
| `wavelengths`       | float list | `[0.5876]` | Wavelengths (um) traced independently. Multiple entries give a polychromatic spot / loss. |

If neither `grid_radius` nor `aperture_diameter` is set, the pupil radius
falls back to a built-in default (`5.0` mm).

### `[optimize]`

| Key             | Type    | Default | Meaning |
|-----------------|---------|---------|---------|
| `learning_rate` | float   | `0.001` | Gradient-descent step size (starting step; the optimizer backtracks if a step would raise the loss). |
| `iters`         | integer | `20`    | Maximum descent iterations. Descent stops early once the loss change drops below the convergence tolerance. |

### `[[surfaces]]`

Surfaces are listed in the order light meets them. The first surface vertex is
at `z = 0`; each `thickness` is the axial gap to the next vertex, and the
**last** `thickness` is the gap from the final surface to the image plane.

| Key        | Type        | Default   | Meaning |
|------------|-------------|-----------|---------|
| `name`     | string      | required  | Human-readable label. |
| `radius`   | float       | required  | Radius of curvature. `0` denotes a planar surface. Sign follows the usual convention (center of curvature at `z0 + radius`). |
| `thickness`| float       | required  | Axial gap to the next vertex (or to the image plane for the last surface). |
| `material` | float       | `1.0`     | Refractive index of the medium **after** this surface (`n_d` when dispersive). Defaults to air, matching patent tables that list only glass. |
| `cauchy_b` | float       | `0.0`     | Cauchy dispersion coefficient (um^2): `n(l) = material + cauchy_b / l^2`. `0` disables dispersion for the surface. |
| `optimize` | string list | `[]`      | Parameters to optimize: any of `"radius"`, `"thickness"`, `"material"`. |
| `diameter` | float       | *(unset)* | Clear aperture for the exported profile. Defaults to the pupil diameter. |
| `stop`     | bool        | `false`   | Aperture stop: rays landing beyond the pupil radius here are vignetted. |

### Conventions

- The first ray segment travels in air (`n = 1`); each surface's `material` is
  the index of the medium immediately after it.
- Rays start collimated along `+z`, launched from `z0[0] - 10` mm, sampled
  across the pupil grid.
- A `radius` of `0` is intersected as the plane `z = vertex`.
- Refraction returns no ray on total internal reflection; such rays are
  reported as `Tir` rather than producing NaNs.

### Minimal example

```toml
[source]
ray_count = 10
grid_radius = 5.0

[optimize]
learning_rate = 0.001
iters = 20

[[surfaces]]
name = "Front Element"
radius = 50.0
thickness = 5.0
material = 1.5168   # N-BK7 index
optimize = ["radius"]

[[surfaces]]
name = "Back Element"
radius = -100.0
thickness = 40.0    # last thickness = gap to the image plane
material = 1.0      # air
```

Bundled prescriptions live in `assets/`: `sample.toml` (doublet),
`landscape.toml` (meniscus with remote stop), `cooke.toml` (Cooke triplet,
polychromatic), and `double_gauss.toml` (six-element double Gauss).

## Using the crate as a library

The core pipeline is available programmatically. See the runnable examples on
`load_toml`, `trace_system`, `efl`, `descend`, and `to_json` in the API docs
(`cargo doc --open`).

```rust
use optics::{load_toml, trace_system, efl, descend, to_json, RayEnd};

let setup = load_toml(&std::fs::read_to_string("assets/sample.toml")?)?;

// Trace and inspect.
let paths = trace_system(&setup.surfaces, &setup);
let arrived = paths.iter().filter(|p| p.end == RayEnd::Image).count();
println!("{arrived}/{} rays reached the image plane", paths.len());

// Paraxial focal length.
if let Some(f) = efl(&setup) {
    println!("EFL = {f:.3} mm");
}

// Optimize the flagged variables and export the result.
let (optimized, history) = descend(&setup)?;
println!("loss {:.6} -> {:.6}", history[0], history.last().unwrap());
let opt_paths = trace_system(&optimized.surfaces, &optimized);
std::fs::write("system.json", to_json(&optimized, &opt_paths))?;
# Ok::<(), Box<dyn std::error::Error>>(())
```

## JSON export format (`system.json`)

`optics export` writes a versioned document (`SCHEMA_VERSION = 1`) intended for
a Three.js viewer:

```jsonc
{
  "version": 1,
  "loss": 0.1234,                       // RMS spot loss of the traced system
  "wavelengths": [0.5876],              // echoed in config order, for coloring
  "variables": [                        // optimized parameter snapshot
    { "surface": "Front Element", "key": "radius", "value": 50.12 }
  ],
  "surfaces": [                         // one lens profile per surface
    {
      "name": "Front Element",
      "vertex_z": 0.0,
      "radius": 50.0,
      "profile": [[0.0, 0.0], [/* r, z */]]   // axis-to-rim polyline
    }
  ],
  "segments": [                         // ray paths as point pairs
    [[x0, y0, z0], [x1, y1, z1]]
  ]
}
```

Consuming it in Three.js:

- Each `surfaces[i].profile` is an `[r, z]` polyline. Revolve it about the
  optical (`z`) axis with `THREE.LatheGeometry` to render the lens body.
- `segments` is a flat list of consecutive `[p0, p1]` point pairs, grouped by
  wavelength in configuration order — feed the flattened vertices straight into
  `THREE.LineSegments` and color by wavelength using the `wavelengths` echo.

```js
// Sketch: build ray lines from segments.
const positions = [];
for (const [a, b] of data.segments) { positions.push(...a, ...b); }
const geom = new THREE.BufferGeometry();
geom.setAttribute("position", new THREE.Float32BufferAttribute(positions, 3));
const lines = new THREE.LineSegments(geom, new THREE.LineBasicMaterial());
scene.add(lines);
```

## How it works

Data flows `dual -> linalg -> ray -> system -> trace -> optimize -> export/tui`:

- **`dual`** — forward-mode autodiff. A `Dual` carries a value and its
  derivative w.r.t. one seeded input; operators apply the calculus chain rule.
- **`linalg`** — minimal `Vec3`/`Point3` over `Dual`, so derivatives propagate
  through every geometric operation.
- **`ray`** — surface intersection (sphere or plane) and vector-form Snell
  refraction (renormalized; `None` on total internal reflection).
- **`system`** — TOML schema, vertex layout from thicknesses, pupil sampling,
  and Cauchy dispersion.
- **`trace`** — sequential tracing per wavelength; marginal-ray EFL/back-focus.
- **`optimize`** — RMS spot loss, exact per-variable gradients (one seeded
  trace each), and guarded gradient descent with early stop.
- **`export` / `tui`** — `system.json` for Three.js and a `ratatui` telemetry
  view.
