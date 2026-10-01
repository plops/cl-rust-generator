// Treemap-Rechtecke mit Cushion-Shading: parabolische Aufhellung zur Mitte,
// dunkle Innenkante, optionaler Highlight-Modus für das Hover-Rechteck.
// Quad-Ecken entstehen arithmetisch aus vertex_index (kein Vertex-Buffer).

struct Uniforms {
    screen: vec2f,
};

@group(0) @binding(0) var<uniform> uni: Uniforms;

struct Item {
    pos: vec2f,
    size: vec2f,
    color: vec3f,
    flags: u32,
};

@group(0) @binding(1) var<storage, read> items: array<Item>;

struct VsOut {
    @builtin(position) pos: vec4f,
    @location(0) uv: vec2f,
    @location(1) size_px: vec2f,
    @location(2) color: vec3f,
    @location(3) @interpolate(flat) flags: u32,
};

@vertex
fn vs(@builtin(vertex_index) vi: u32, @builtin(instance_index) ii: u32) -> VsOut {
    let it = items[ii];
    // Dreiecksliste: (0,0) (1,0) (0,1) / (1,0) (1,1) (0,1).
    let cx = f32(vi == 1u || vi == 3u || vi == 4u);
    let cy = f32(vi == 2u || vi == 4u || vi == 5u);
    let corner = vec2f(cx, cy);
    let px = it.pos + corner * it.size;
    let ndc = vec2f(px.x / uni.screen.x * 2.0 - 1.0, 1.0 - px.y / uni.screen.y * 2.0);
    return VsOut(vec4f(ndc, 0.0, 1.0), corner, it.size, it.color, it.flags);
}

const FLAG_FLAT: u32 = 1u;
const FLAG_HI: u32 = 2u;

@fragment
fn fs(in: VsOut) -> @location(0) vec4f {
    let flat = (in.flags & FLAG_FLAT) != 0u;
    let hi = (in.flags & FLAG_HI) != 0u;
    // Invertiertes Detail für kleine Rechtecke: Cushion + 1.5-px-Kante würden
    // alles unter ~3 px schwarz färben. Daher unter 3 px volle Dateifarbe,
    // darüber weicher Übergang bis 8 px zu Cushion + Kante.
    let detail = smoothstep(3.0, 8.0, min(in.size_px.x, in.size_px.y));
    var base = in.color;
    if (!flat) {
        // Cushion: Mitte heller (parabolisch), Ränder dunkler.
        let n = in.uv * 2.0 - 1.0;
        let d = max(0.0, (1.0 - n.x * n.x) * (1.0 - n.y * n.y));
        base *= mix(1.0, 0.68 + 0.42 * d, detail);
    }
    if (hi) {
        base = base * 0.5 + vec3f(0.5);
    }
    // 1.5-px-Innenkante: dunkel, im Highlight-Modus weiß.
    let m = min(
        min(in.uv.x, 1.0 - in.uv.x) * in.size_px.x,
        min(in.uv.y, 1.0 - in.uv.y) * in.size_px.y
    );
    let edge = mix(1.0, smoothstep(0.0, 1.5, m), detail);
    var border = base * 0.45;
    if (hi) {
        border = vec3f(1.0);
    }
    return vec4f(mix(border, base, edge), 1.0);
}
