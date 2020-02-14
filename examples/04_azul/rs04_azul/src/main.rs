extern crate azul;
use azul::{
    prelude::*,
    widgets::{button::Button, label::Label},
};
struct MyDataModel {
    counter: usize,
}
fn update_counter(event: CallbackInfo<MyDataModel>) {
    (event.state.data.counter) += (1);
    Redraw
}
impl Layout for MyDataModel {
    fn layout(&self, _info: LayoutInfo<Self>) -> Dom<Self> {
        let mut label = Label::new(format!("{}", self.counter)).dom();
        let mut button = Button::with_label("counter")
            .dom()
            .with_callback(On::MouseUp, update_counter);
        Dom::div().with_child(label).with_child(button);
        return Dom::div();
    }
}
fn main() {
    let mut app = App::new(MyDataModel { counter: 0 }, AppConfig::default()).unwrap();
    let mut window = app
        .create_window(WindowCreateOptions::default(), css::native())
        .unwrap();
    app.run(window).unwrap();
}
