#[allow(unused_parens)]
use wasm_bindgen::prelude::*;
use web_sys::Document;
type Result<T> = std::result::Result<T, JsValue>;
fn get_document() -> Result<Document> {
    let window = web_sys::window().unwrap();
    return Ok(window.document().unwrap());
}
#[wasm_bindgen]
pub fn run() -> Result<()> {
    let document = get_document()?;
    let body = document.body().unwrap();
    mount_app(&document, &body)?;
    return Ok(());
}
fn mount_app(document: &web_sys::Document, body: &web_sys::HtmlElement) -> Result<()> {
    mount_title(&document, &body)?;
    return Ok(());
}
fn mount_title(document: &web_sys::Document, body: &web_sys::HtmlElement) -> Result<()> {
    let title = document.create_element("h1").expect("title");
    let title_text = document.create_text_node("DOT");
    title
        .append_child(&title_text)
        .expect("append child to title");
    body.append_child(&title).expect("append title to body");
    return Ok(());
}
