extern crate azul;
use azul::{
    prelude::*,
    widgets::{button::Button, label::Label},
};
struct MyDataModel {
    counter: usize,
}
fn update_counter(event: CallbackInfo<MyDataModel>) -> UpdateScreen {
    (event.state.data.counter) += (1);
    return Redraw;
}
impl Layout for MyDataModel {
    fn layout(&self, _info: LayoutInfo<Self>) -> Dom<Self> {
        let label = Label::new(format!("{}", self.counter)).dom();
        let button = Button::with_label("counter")
            .dom()
            .with_callback(On::MouseUp, update_counter);
        let mut dom = Dom::div().with_child(label).with_child(button);
        return dom;
    }
}
fn main() {
    let mut app = App::new(MyDataModel { counter: 0 }, AppConfig::default()).unwrap();
    let window = app
        .create_window(WindowCreateOptions::default(), css::native())
        .unwrap();
    app.run(window).unwrap();
}
