
ich nutze rust embassy fuer pi pico 2. ich mag aber nicht mehr die knoepfe druecken, kann ich von meiner firmware direkt in bootsel modus rebooten?

Ja, das ist mit dem Raspberry Pi Pico 2 (RP2350) und embassy-rp problemlos möglich! Du musst dafür keine Knöpfe mehr drücken oder Kabel ziehen. 
Raspberry Pi Forums +1
Der RP2350 besitzt im Bootrom eine eingebaute Funktion namens reset_to_usb_boot, die du direkt aus deinem Rust-Code aufrufen kannst. Sobald dieser Code ausgeführt wird, startet der Pico 2 sofort im BOOTSEL-Modus neu und meldet sich am PC als Massenspeicher (oder für picotool) an. 
Mongoose web server +1
Der Rust-Code für Embassy
Du kannst die Funktion an einer beliebigen Stelle in deiner Applikation aufrufen (z. B. wenn ein bestimmter GPIO-Pin auf Masse gezogen wird, oder wenn ein spezieller Befehl über USB-Serial/UART eingeht)


embassy_rp0.10.0
In embassy_rp::rom_data
embassy_rp::rom_data
Function reset_to_usb_boot Copy item path
Search
Settings
Help
SummarySource

pub extern "C" fn reset_to_usb_boot(
    gpio_activity_pin_mask: u32,
    disable_interface_mask: u32,
)

Expand description

Resets the RP2040 and uses the watchdog facility to re-start in BOOTSEL mode:

    gpio_activity_pin_mask is provided to enable an ‘activity light’ via GPIO attached LED for the USB Mass Storage Device:
        0 No pins are used as per cold boot.
        Otherwise a single bit set indicating which GPIO pin should be set to output and raised whenever there is mass storage activity from the host.
    disable_interface_mask may be used to control the exposed USB interfaces:
        0 To enable both interfaces (as per cold boot).
        1 To disable the USB Mass Storage Interface.
        2 to Disable the USB PICOBOOT Interface.

