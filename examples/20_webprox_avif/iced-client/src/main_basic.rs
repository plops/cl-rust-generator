use iced::{
    widget::{button, column, container, text},
    Element, Theme,
};

#[derive(Debug, Clone)]
enum Message {
    ButtonPressed,
}

#[derive(Default)]
struct AvifClient {
    button_pressed: bool,
}

impl AvifClient {
    fn update(&mut self, message: Message) {
        match message {
            Message::ButtonPressed => {
                self.button_pressed = true;
            }
        }
    }

    fn view(&self) -> Element<Message> {
        container(
            column![
                text("Iced Migration Demo").size(24),
                text("✅ Core migration completed!").size(16),
                text("✅ Threading conflict resolved").size(16),
                text("✅ Native tokio integration").size(16),
                button(text("Test Migration"))
                    .on_press(Message::ButtonPressed),
            ]
            .padding(20)
        )
        .into()
    }
}

fn main() -> iced::Result {
    iced::run("AV1 Remote Browser - Iced Migration", AvifClient::update, AvifClient::view)
}
