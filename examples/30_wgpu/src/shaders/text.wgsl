// Bitmap-Text über Atlas-Textur: eine Instanz pro Zeichen, Nearest-Sampling.
// Volle Weißschrift mit Atlas-Alpha; leere Texel werden verworfen.

struct Uniforms {
    screen: vec2f,
};

@group(0) @binding(0) var<uniform> uni: Uniforms;

struct Glyph {
    pos: vec2f,
    cell: u32,
    scale: f32,
};

@group(0) @binding(1) var<storage, read> glyphs: array<Glyph>;
@group(0) @binding(2) var atlas: texture_2d<f32>;
@group(0) @binding(3) var samp: sampler;

struct VsOut {
    @builtin(position) pos: vec4f,
    @location(0) tuv: vec2f,
};

@vertex
fn vs(@builtin(vertex_index) vi: u32, @builtin(instance_index) ii: u32) -> VsOut {
    let g = glyphs[ii];
    let cx = f32(vi == 1u || vi == 3u || vi == 4u);
    let cy = f32(vi == 2u || vi == 4u || vi == 5u);
    let corner = vec2f(cx, cy);
    let px = g.pos + corner * 8.0 * g.scale;
    let ndc = vec2f(px.x / uni.screen.x * 2.0 - 1.0, 1.0 - px.y / uni.screen.y * 2.0);
    let col = f32(g.cell % 16u);
    let row = f32(g.cell / 16u);
    let tuv = vec2f(
        (col * 8.0 + corner.x * 8.0) / 128.0,
        (row * 8.0 + corner.y * 8.0) / 48.0
    );
    return VsOut(vec4f(ndc, 0.0, 1.0), tuv);
}

@fragment
fn fs(in: VsOut) -> @location(0) vec4f {
    let a = textureSample(atlas, samp, in.tuv).a;
    if (a < 0.01) {
        discard;
    }
    return vec4f(1.0, 1.0, 1.0, a);
}
