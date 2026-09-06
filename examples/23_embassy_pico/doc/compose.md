https://medium.com/@carlmkadie/device-envoy-making-embedded-fun-31534917414b
device-envoy-rp: Making Embedded Pico Fun
With Rust, Embassy, and Composable Device Abstractions
Carl M. Kadie
Carl M. Kadie
18 min read
·
Feb 24, 2026

--
Press enter or click to view image in full size
Conway’s Game of Life — Source: all figures by the author

This article uses video demos to introduce device-envoy-rp, a new Rust crate. Its goal is simple: make embedded programming more enjoyable. device-envoy is built on Embassy and on a design pattern called device abstractions. We’ll define that pattern in the next section, after the first example.

    For a video version of this article, see this talk to the Seattle Rust User Group. Also available, the ESP32-version of this article.

The library is experimental and focused on microcontroller applications. Its key dependency, Embassy, brings Rust’s async facilities to bare metal. That makes it possible to write structured, concurrent programs on a small, inexpensive microcontroller without an operating system.

Today, device-envoy runs on two microcontroller families: Raspberry Pi Pico and Pico 2, with and without Wi-Fi, and ESP32-family chips based on both RISC-V and Xtensa. The ideas are not tied to any one platform. This article focuses on the Pico family, but device-envoy offers the same features on both.
What device-envoy Provides

device-envoy provides reusable device abstractions. Each encapsulates hardware ownership and async coordination behind a focused API. The current device abstractions:

    LED Strips & Panels: NeoPixel-style (WS2812) LED arrays with 2D text rendering, animation, embedded-graphics support, and efficient options for power limiting and color correction.
    WiFi (Pico W): Connect to the Internet with automatic credentials management; on boot, opens a web form if WiFi credentials aren’t saved, then connects seamlessly to a stored network; requires Pico W and is not supported on non-W boards.
    Audio Player: Play audio clips over I²S hardware with runtime sequencing, volume control, and compression.
    Flash Storage: Type-safe, on-board persistent storage backed by flash.
    IR Remote: Remote control decoder implementing the NEC protocol. Returns custom enum variants.
    Servo Control: Servo positioning and animation.
    Button Input: Button handling with debouncing and clean event semantics.
    LCD Display: Text display support for HD44780-compatible character LCDs.
    RFID Reader: Card detection and reading using the MFRC522 module.
    Clock Sync: Network time synchronization utilities.
    LED4 Display: 4-digit, 7-segment LED display control with optional animation and blinking.
    Single LED: Single LED control with animation support.

Full API documentation is available on docs.rs.

The rest of this article introduces device-envoy through a sequence of demos. Most demos are followed by the code that powers them. Each section introduces one device abstraction.

Here is the path we will follow:

    LED Strips: Just Turn on the Lights
    LED Strip: Animation Running in the Background
    LED Strip: Scaling to 96 LEDs
    LED Panel: Text
    LED Panel: 2D Graphics
    WiFi Auto: Joining the Network
    Clocks Demo: Three Picos, Three Displays (no code shown)
    Conway’s Game of Life: Computation on Display (no code shown)
    Conclusion: What device-envoy Gives You
    Conclusion: Call to Action

By the end, I hope you feel confident using device-envoy and have a clear sense of how to apply these ideas in your own projects.

Let’s start with the first demo.
LED Strips: Just Turn on the Lights

The first demo is intentionally simple. On a desktop machine, a cargo command compiles the firmware and flashes it over USB to the Pico. The board’s debug probe flickers briefly, and then an addressable LED strip lights up in an alternating blue and light-gray pattern.

At first, it is surprisingly hard to distinguish the two colors. The LEDs are bright and directional. To make the pattern clear, we place a sheet of black LED acrylic in front of the strip. With the acrylic in place, the alternating pattern becomes obvious: blue, light gray, blue, light gray, and so on.
Pico runs firmware. The 1D LED strip shows an alternating blue and light-gray pattern behind black LED acrylic.

Let’s look at the code. In this first example, we’ll look at the full program. In later examples, only the new or interesting parts will be shown.

The program begins with the usual #![no_std] setup and imports:

#![allow(missing_docs)]
#![no_std]
#![no_main]
#![cfg(not(feature = "host"))]

use core::{convert::Infallible, future, panic};
use device_kit::{
    Result,
    led_strip::{Frame1d, colors, led_strip},
};
use embassy_executor::Spawner;
use {defmt_rtt as _, panic_probe as _};

Next, we define a device abstraction using device envoy’s led_strip! macro. This defines the LedStrip8 struct type, which controls an 8-LED strip on PIN_0. We’ll see how it is used in a moment.

led_strip! {
    LedStrip8 {
        pin: PIN_0,
        len: 8,
    }
}

Next, we use two levels of main, which allows inner_main to return a Result.

// Nice trick: Two "mains" lets us use Results.
#[embassy_executor::main]
async fn main(spawner: Spawner) -> ! {
    let err = inner_main(spawner).await.unwrap_err();
    panic!("{err}");
}

async fn inner_main(spawner: Spawner) -> Result<Infallible> {

Next, we initialize the Pico hardware and construct led_strip8, an instance of the LedStrip8 struct, the device abstraction. During construction, you pass the resources it needs: the pin, a PIO instance, and a DMA channel. The details of PIO and DMA are not important here. What matters is that the device abstraction now owns these resources exclusively.

let p = embassy_rp::init(Default::default());
let led_strip8 = LedStrip8::new(p.PIN_0, p.PIO0, p.DMA_CH0, spawner)?;

Next, create a 1D frame, which is simply an array of RGB values. Fill it with alternating colors:

    let mut frame1d = Frame1d::new(); // just an owned array of RGB pixels
    let palette = [colors::BLUE, colors::LIGHT_GRAY];
    for pixel_index in 0..frame1d.len() {
        frame1d[pixel_index] = palette[pixel_index % 2];
    }

Finally, write the frame to the strip. The LEDs light up with the alternating pattern, which remains until it is replaced. After writing the frame, the program is free to do other work. In this demo, it simply waits forever.

    led_strip8.write_frame(frame1d)?;
    future::pending().await // run forever
}

    Aside: What “Device Abstraction” Means

    This LED strip is the first example of a device abstraction. From the application’s point of view, a device abstraction is simply a normal Rust struct with a focused API. Some methods are synchronous; others are async.

    Behind the scenes, the struct communicates through Embassy channels or signals. An Embassy task runs in a loop processing those messages. That task owns the hardware resources and maintains whatever internal state it needs. It lives in static memory for the lifetime of the firmware.

    For more background on this pattern, see How Rust & Embassy Shine on Embedded Devices.

API documentation for LED strips is available on docs.rs.

The LED strip example shows the basic pattern. Now that it can display a static frame, the next step is to make it blink.
LED Strip: Animation Running in the Background

The next step is to make the LEDs blink.

In the demo, the strip alternates between two patterns. Instead of a static blue–light gray pattern, it flips back and forth: blue–gray, then gray–blue.
LED strip alternating between two frames using the animate method. The animation runs in the background while the program continues executing.

The structure of the program is almost identical to the previous example. We define the LED strip device abstraction the same way and construct the LedStrip8 instance by passing it ownership of the pin, a PIO instance, and a DMA channel.

The difference is that we now create two frames instead of one.

The first frame is exactly what we built before: alternating blue and light gray. The second frame is offset by one position, so where the first frame is blue, the second is light gray, and vice versa.

    let palette = [colors::BLUE, colors::LIGHT_GRAY];
    let mut frame0 = Frame1d::new();
    let mut frame1 = frame0.clone();
    for pixel_index in 0..frame0.len() {
        frame0[pixel_index] = palette[pixel_index % 2];
        frame1[pixel_index] = palette[(pixel_index + 1) % 2];
    }

The new method is animate.

Instead of calling write_frame, we pass animate a sequence of (frame, duration) pairs. The strip displays the first frame for the specified duration, then the second frame for its duration, and then repeats the sequence forever.

    const FRAME_DURATION: Duration = Duration::from_millis(150);
    led_strip8.animate([(frame0, FRAME_DURATION), (frame1, FRAME_DURATION)])?;

The animation runs in the background. After calling animate, the program is free to do other work. It could watch a button, respond to network events, or update another device abstraction.

Now let’s scale this up from eight LEDs to ninety-six and see what changes.
LED Strip: Scaling to 96 LEDs

In this demo, the LED strip has 96 pixels. A single gray pixel moves along the strip, one position at a time, against a blue background.
A 96-pixel LED strip with a single gray pixel moving along a blue background.

At 96 LEDs, practical concerns appear quickly. If every LED runs at full brightness, the strip can draw about six amps. That current must be supplied directly to the strip. Powering it through the Pico board risks damaging the board or causing unstable behavior.

For that reason, the library applies a conservative current limit of 300 mA by default. For this demo, the limit is increased to 500 mA.

Color constants such as colors::BLUE and colors::LIGHT_GRAY come from the smart-leds crate, which defines a common RGB8 color type and named colors. These are linear 8-bit RGB values. Because human brightness perception is non-linear, applying them directly to LEDs can produce colors that look dim or unbalanced. Gamma correction compensates for that. The library provides a default gamma curve and an alternate curve compatible with the smart-leds default correction.

At this scale, there is also a tradeoff between animation convenience and memory use. A background animation that cycles 96 RGB values through 96 frames requires 27,648 bytes. For this demo, the animation is generated in the foreground, one frame at a time, requiring only 288 bytes.

We continue to use the led_strip! macro, but with different parameters. The strip now has length 96, and this example shows that the pin, PIO instance, DMA channel, current limit, and gamma curve are configurable.

led_strip! {
    pub LedStrip96 { // can add 'pub' to make struct public
        pin: PIN_4,
        len: 96,
        // Optionals
        pio: PIO1,     // which of 2 or 3 PIO resources to use
        dma: DMA_CH5,  // which of 12 DMA resources to use
        max_current: Current::Milliamps(500), // default is 300ma
        gamma: Gamma::SmartLeds, // compatibility curve (= 2.8)
        max_frames: 0, // Disable animation; write_frame() still works
    }
    // Gamma correction and current limiting are folded into a single
    // lookup table (one table lookup per RGB channel at runtime).
}

Construction works as before. We construct an instance by passing it the required hardware resources.

    // Must match the pin, pio, dma in LedStrip96 above to avoid
    // compilation error.
    let led_strip96 = LedStrip96::new(p.PIN_4, p.PIO1, p.DMA_CH5, spawner)?;

Encoding the hardware choices in types turns many configuration mistakes into compile-time errors. If LedStrip96 is declared to use PIN_4 and the code tries to construct it with a different pin, the compiler rejects the program.

To produce the moving pixel, the demo starts with a frame initialized to all blue. It then loops over each pixel position. On each iteration, it sets one pixel to gray, writes the full 96-pixel frame, waits briefly, and then restores that pixel to blue before moving to the next position.

    let mut frame1d = Frame1d::filled(colors::BLUE);
    loop {
        for dot_index in 0..LedStrip96::LEN {
            frame1d[dot_index] = colors::LIGHT_GRAY;
            led_strip96.write_frame(frame1d)?;
            Timer::after(Duration::from_millis(50)).await;
            frame1d[dot_index] = colors::BLUE;
        }
    }

Both current limiting and gamma correction compile down to simple lookup tables. They are applied efficiently at runtime.

This example scales the strip up, but it also exposes a new problem. Once the display is large, drawing anything more structured than a moving dot becomes painful. If the physical wiring follows a serpentine pattern, writing text or drawing shapes requires awkward coordinate math.

Fixing this is the next step. Instead of thinking in terms of a 1D index, we want to think in 2D coordinates and draw text.
LED Panel: Text

The next demo treats the LEDs as a panel. Electrically, they are still just an LED strip.

The panels’ pixels follow a common serpentine wiring pattern: left to right on one row, then right to left on the next. I connected two 12×4 panels to form a 12×8 display.

In the demo, the word GO appears in white and then in color.
The word “GO” rendered on a 12×8 LED panel using a compile-time layout and built-in font support.

Instead of thinking in terms of a 1D index, we now want to think in 2D coordinates.

The first step is to describe the physical wiring as a compile-time layout. The library provides a small layout system that maps logical (x, y) coordinates to the underlying 1D LED order.

In this case, a single 12×4 panel uses a standard serpentine column-major pattern. Two such layouts are combined vertically to form a 12×8 panel. The combined layout is then rotated to match the physical mounting orientation.

// Two 12x4 panels stacked vertically to create a 12x8 display.
const LED_LAYOUT_12X4: LedLayout<48, 12, 4> = LedLayout::serpentine_column_major();
const LED_LAYOUT_12X8: LedLayout<96, 12, 8> = LED_LAYOUT_12X4.combine_v(LED_LAYOUT_12X4);
const LED_LAYOUT_12X8_ROTATED: LedLayout<96, 8, 12> = LED_LAYOUT_12X8.rotate_cw();

All of this happens at compile time. There is no runtime cost for describing the layout.

If you prefer, you can also define a layout explicitly by listing the (x, y) coordinate of every pixel. The layout system supports that as well.

Once the layout is defined, we use a new macro to create a 2D LED device abstraction:

// Define a struct `Led12x8` to control a 12x8 LED panel on PIN_4
led2d! {
    Led12x8 {
        pin: PIN_4,
        led_layout: LED_LAYOUT_12X8_ROTATED,
        // Use a 4x6 pixel font with no gap between characters
        font: Led2dFont::Font4x6Trim,
    }
}

The library includes 40 bitmap fonts. The “trimmed” versions pack characters tightly, which is useful on small displays.

In the program, we construct the panel and then call write_text:

    // Text supports "\n" for multiple lines.
    // Colors are per-character, repeat as needed, and default to white.
    led12x8
        .write_text(
            "Go\nGo",
            &[colors::LIGHT_GRAY, colors::LIGHT_GRAY,
              colors::ORANGE,colors::HOT_PINK,
            ],
        )
        .await?;

There is one color per character. If more colors are provided than needed, the extras are ignored. If fewer are provided, they repeat. If no colors are provided, the text defaults to white.

The important change is conceptual. The wiring is still 1D, but the program now thinks in 2D. The layout abstraction handles the translation from (x, y) coordinates to the correct physical LED index.

    Aside: API documentation for LED panels is available on docs.rs.

Once that translation exists, drawing text and shapes becomes manageable.
LED Panel: 2D Graphics

Once the LEDs are treated as a 2D panel, text is only the beginning. The next demo draws simple graphics: points and lines.
Text, points, and lines rendered on a 12×8 LED panel using a 2D frame and embedded-graphics-style drawing.

The key change is that the program no longer manipulates pixels through a 1D index. Instead, it draws into a 2D frame and lets the device abstraction map (x, y) coordinates to the underlying serpentine wiring.

The library integrates with the embedded-graphics crate, which provides a common drawing API for embedded displays.

At a high level, the flow looks like this:

    Create a 2D frame.
    Draw shapes into it using a graphics API.
    Write the frame to the LED panel.

The example below combines text rendering, direct pixel manipulation, and graphics primitives in a single frame.


    // A 2D array of pixels.
    let mut frame2d = Frame2d::new();

    // Can write text to a frame instead of directly to the LED panel.
    let text_colors = [colors::ORANGE, colors::HOT_PINK];
    led12x8.write_text_to_frame("Go", &text_colors, &mut frame2d)?;

    // Can read and write the frame's pixels directly.
    // Index with tuple (x, y). Origin (0,0) is top-left.
    // Fill in the letter "o" with 4 pixels.
    frame2d[(5, 3)] = colors::HOT_PINK;
    frame2d[(6, 3)] = colors::HOT_PINK;
    frame2d[(5, 4)] = colors::HOT_PINK;
    frame2d[(6, 4)] = colors::HOT_PINK;

    // With embedded-graphics, you can draw any shapes you want (and text). 
    // - We use smart-leds' `RGB8` color type throughout device-envoy.
    //   embedded-graphics uses its own `Rgb888`, so we convert.
    let line_style = PrimitiveStyle::with_stroke(colors::RED.to_rgb888(), 1);

    // These geometry points are compile-time constants.
    const MID_Y: i32 = Led12x8::HEIGHT as i32 / 2;
    const MID_LEFT: Point = Point::new(Led12x8::BOTTOM_LEFT.x, MID_Y);
    const MID_RIGHT: Point = Point::new(Led12x8::BOTTOM_RIGHT.x, MID_Y);

    Line::new(MID_LEFT, Led12x8::BOTTOM_RIGHT)
        .into_styled(line_style)
        .draw(&mut frame2d)?;
    Line::new(MID_RIGHT, Led12x8::BOTTOM_LEFT)
        .into_styled(line_style)
        .draw(&mut frame2d)?;
     // Write the frame to the LED panel. It stays until you replace it.
    led12x8.write_frame(frame2d)?;

The panel device abstraction separates geometry from wiring. The physical hardware is still a single serpentine chain of LEDs, but the program can treat it like a small display. Drawing logic stays readable because it uses coordinates rather than a wiring-dependent 1D index.

Additional panel and strip features not demonstrated here:

    Panel animations are supported, not just static frames.
    Up to 8 independent LedStrip or LedPanel instances can run on a Pico 1 using a related third macro: led_strips!.
    Up to 12 independent instances can run on a Pico 2.

So far, everything has focused on LEDs. Embedded systems are rarely just displays. Next, we connect the device to the network.
WiFi Auto: Joining the Network

The next demo uses a Raspberry Pi Pico W. Unlike the earlier examples, this device connects to WiFi.

On first boot, if no credentials are stored, the device starts a temporary Wi-Fi network. You connect to it from a phone or laptop, enter your home network name and password into a simple web form, and submit. The device stores those credentials in flash and reboots. On the next boot, it connects automatically.
First boot opens a captive portal. After entering credentials, the device reconnects automatically on subsequent boots.

From the application’s perspective, WiFi provisioning is a device abstraction just like the LED strip. The demo also shows something important: application-level code can react to connection events and update the LED panel while WiFi is starting.

The provisioning flow is intentionally simple:

    If credentials exist in flash, connect to the saved network.
    If not, start an access point and serve a small configuration page.
    Save credentials and reboot.

In the case of WifiAuto, we start by creating typed persistent storage using FlashArray. The details are not important here. With that in hand, we construct a WifiAuto instance:

// Flash stores WiFi credentials after first setup
    static FLASH_ARRAY_STATIC: FlashArrayStatic = FlashArray::<1>::new_static();
    let flash_array = FlashArray::new(&FLASH_ARRAY_STATIC, p.FLASH)?;
    let [wifi_credentials_flash_block] = flash_array; 

    // Create a WifiAuto instance.
    // A button is used to force reconfiguration via setup web page.
    // Pico W uses the CYW43 chip wired to fixed GPIOs; we pass those here.
    let wifi_auto = WifiAuto::new(
        p.PIN_23,  // internal CYW43 pins (fixed)
        p.PIN_24,
        p.PIN_25,
        p.PIN_29,
        p.PIO1,    // Needs a PIO resource
        p.DMA_CH1, // Needs a DMA resource
        wifi_credentials_flash_block,
        p.PIN_15,  // Button for forced reconfiguration
        PressedTo::Ground,
        "PicoDemo",// Setup SSID
        [],        // Any custom fields
        spawner,
    )?;

Not shown here, the custom_fields parameter lets you add additional HTML form fields to the setup page. For example, in the clock demos, the setup page lists time zones and lets the user select one.

WifiAuto has a single high-level method: connect. It consumes the WifiAuto instance and returns the network stack and the button.

When calling connect, you provide an event handler. It can be empty. More usefully, it can update whatever user interface your device provides to reflect the current WiFi connection state. In other words, the application reacts to connection events instead of managing a WiFi state machine.

// Try to connect.
    // Will launch setup web page as needed.
    // Will reset Pico as needed.
    // wifi_auto consumed. Returns network stack and button.
    let led8x12_ref = &led8x12; // Borrow so can use without owning.
    let (stack, _button) = wifi_auto
        .connect(|event| async move {
            match event {
                // Join setup network
                WifiAutoEvent::CaptivePortalReady => {
                    led8x12_ref.write_text("JO\nIN", COLORS).await?
                 }
                WifiAutoEvent::Connecting { .. } => 
                    show_animated_dots(led8x12_ref).await?,
                WifiAutoEvent::ConnectionFailed => 
                    led8x12_ref.write_text("FA\nIL", COLORS).await?,
            }
            Ok(())
        })
        .await?;

    // Show initial state with dashes until DNS is fetched.
    led8x12.write_text("--\n--", COLORS).await?;

Notice what is missing from the application code: there are no reconnection loops, DHCP retries, access-point state machines, or credential management logic. All of that lives inside the device abstraction.

A button can optionally reset stored credentials. That makes it possible to move the device to a new network without reflashing firmware.

This example shows that device abstractions are not only about hardware output. The LED panel controlled visible output. WifiAuto manages connectivity and long-running background coordination.

    Aside: API documentation for WifiAuto is available on docs.rs.

Once connected to the Internet, the Pico can retrieve data and consume or serve network services.

Before we conclude, we will look at two final demos without showing additional code. The next demo uses the Wi-Fi connection to synchronize the clock.
Clocks Demo: Three Picos, Three Displays

This demo shows three separate Raspberry Pi Picos, each running a clock application composed from device abstractions but rendering time differently:

    a 4-digit, 7-segment LED display
    an 8×12 LED panel
    two servos acting as clock “hands”

Each Pico connects to Wi-Fi, synchronizes time over the network, and then displays that time using its own device abstraction. The high-level structure is similar. The specific rendering device changes.

On boot, each device connects automatically using its stored credentials. Once connected, it retrieves the current time and begins updating once per minute. A button toggles the display mode. By default, the clocks show hours and minutes. Pressing the button switches to minutes and seconds.
Three separate Picos running similar WiFi-synchronized clock applications, each rendering time differently: 4-digit 7-segment, 8×12 LED panel, and a two-servo clock.

The servo version deserves one clarification. The servos rotate only 180 degrees and this build does not use gears, so it behaves more like an elevator indicator than a traditional analog clock. The “hands” simply point within that limited range.

The hardware form is not the point. Each device abstraction encapsulates hardware ownership and async coordination behind a focused API. The application composes them. There is no operating system and no hand-written scheduling loop micromanaging timing across devices.

The final demo shifts from timekeeping to something more playful: Conway’s Game of Life on LEDs.
Conway’s Game of Life: Computation on Display

Conway’s Game of Life is a simple cellular automaton played on a grid. Each cell is either alive or dead. On each step, the state of every cell updates based on the number of live neighbors it has:

    A live cell with two or three neighbors survives.
    A dead cell with exactly three neighbors becomes alive.
    All other cells die or remain dead.

Those rules are simple, but the resulting patterns can be surprisingly complex. Some configurations stabilize. Some oscillate. Some move across the grid.

In this demo, the 16×16 LED panel acts as the grid. Each pixel represents one cell. On each generation, the program computes the next board state and writes a full 2D frame to the panel.
Conway’s Game of Life running on a 16×16 LED panel. An IR remote controls speed, stepping, and pattern selection.

The demo includes several predefined patterns. It begins with a Glider, which moves diagonally across the panel. It then shows several oscillators that repeat in place, including the Blinker and the Beacon. The Beacon alternates between two shapes, while the Blinker switches between a horizontal and vertical line. The demo concludes with a random seed, which evolves into whatever stable or moving patterns emerge.

An IR remote allows interactive control:

    Increase or decrease the simulation speed
    Start and pause the simulation
    Step forward one generation at a time
    Change colors
    Switch between preset patterns

The IR receiver runs as its own device abstraction, decoding signals in the background. The LED panel renders each generation. The simulation logic computes the next state. The application code coordinates them without an operating system.

Nothing here requires manual polling loops or carefully timed delays. Each device abstraction owns its hardware and its asynchronous coordination. The Game of Life computation becomes just another device abstraction.

This demo highlights the central idea of the library. Device abstractions let application code focus on behavior — whether that behavior is displaying time or evolving a cellular automaton — while the complexity of hardware ownership and concurrency remains encapsulated.
Conclusion and Call to Action
What device-envoy Gives You

The clearest way to summarize what device-envoy provides is to look at one concrete example: a 4-digit, 7-segment LED clock.
Press enter or click to view image in full size
4-digit, 7-segment LED clock

Even a small clock quickly requires a program with many moving parts.

    LED multiplexing, blinking, and display updates: the display must be refreshed continuously, and it often needs independent blinking or animation for user feedback.
    Button timing and debouncing: the program must detect short presses versus long presses reliably.
    A ticking clock: timekeeping introduces its own periodic updates and state.
    Network time synchronization: when connected, the device periodically reaches out to synchronize time.

At the lowest level, you can build this kind of firmware directly with timers, interrupts, and shared mutable state. Rust helps immediately. It improves safety and resource ownership, so you are less likely to reuse a hardware resource accidentally. But even with Rust, that style of firmware is still painful to write and easy to make fragile. Small changes can ripple through timing and shared-state code.

Embassy is a major step up. It brings Rust’s async building blocks to bare metal, especially tasks, channels, and signals. With Embassy, you can write structured concurrent programs on a tiny microcontroller without an operating system.

device-envoy adds one more layer. It encapsulates Embassy mechanisms into device abstractions. The goal is not to expose every low-level detail of every peripheral, but to cover most common use cases with focused APIs and make it straightforward to add new device abstractions when something is missing.

That is what enables application-level programming on bare metal. The application describes behavior. The device abstractions own the hardware resources and the async coordination. The result can feel closer to the simplicity of GUI or web development, while still running directly on a OS-free microcontroller.
Call to Action

If this approach resonates with you, there are several ways to get involved.

    Use device-envoy to build your own cool Pico and ESP32 projects.

    Feedback: If you try this crate, I’d love to hear how it goes, whether it works well, fails to build, needs clearer docs, or does not fit your hardware. Please send feedback to carlk AT msn.com.

    Create new device abstractions for Pico and beyond.
    Add new peripherals, such as joystick input. Add new use cases for existing peripherals. Today the audio support is a clip player; a synthesizer would be a natural next step. Or take the same ideas to new hardware such as the ESP32-C6 or an STM32 board. For background on the architectural approach, see: How Rust & Embassy Shine on Embedded Devices.
    Join the discussion on application-level Embassy development.
    The discussion is brand new and currently just a welcome message. It is not limited to device-envoy. If you are building applications with Rust and Embassy, or exploring device abstractions on any hardware, I would love to hear from you. The conversation lives here:
    https://github.com/CarlKCarlK/device-envoy/discussions

Thanks for following along on this tour of device-envoy and application-level programming on bare metal. I hope some of these ideas are useful as you build your own embedded projects with Rust and Embassy.

Aside: If you’re interested in future articles, please follow me on Medium. I write on scientific programming in Rust and Python, machine learning, and statistics. I tend to write about one article per month.
