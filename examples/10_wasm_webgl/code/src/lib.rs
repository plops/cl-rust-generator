#[allow(unused_parens)]
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{WebGLProgram, WebGLShader, WebGlRenderingContext};
type Result<T> = std::result::Result<T, JsValue>;
pub fn start() -> Result<()> {
    let document = web_sys::window().unwrap().document().unwrap();
    let canvas_el = document.get_element_by_id("canvas").unwrap();
    let canvas = canvas.dyn_into::<web_sys::HtmlCanvasElement>()?;
    let context = canvas
        .get_context("webgl")?
        .unwrap()
        .dyn_into::<WebGlRenderingContext>()?;
    return Ok(());
}