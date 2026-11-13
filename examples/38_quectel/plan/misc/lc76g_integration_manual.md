# Technical Specification & Integration Manual: Waveshare LC76G GNSS Module

This technical reference document provides comprehensive hardware specifications, protocol definitions, configuration commands, hardware integration schematics, and troubleshooting procedures for the **Waveshare LC76G Multi-GNSS Module** and its variants (L76K, L76B).

---

## 1. Product Overview

The **Waveshare LC76G GNSS Module** is a high-performance Multi-GNSS (Global Navigation Satellite System) receiver module based on the **Airoha AG3352Q** chipset. It supports concurrent reception of GPS, BDS (BeiDou Navigation Satellite System), GLONASS, Galileo, and QZSS.

### Core Capabilities
- **Multi-Constellation Support**: Supports concurrent multi-system joint positioning or single-system independent positioning across GPS, GLONASS, Galileo, BeiDou (BDS), and QZSS.
- **Assisted GNSS (A-GNSS)**: Supports **EPO™** (Extended Predictive Orbit) and **EASY™** (Embedded Assist System) autonomous prediction to accelerate Time-To-First-Fix (TTFF).
- **RF Front-End**: Integrated Low-Noise Amplifier (LNA) and SAW filter for high sensitivity (-166 dBm tracking) and active anti-jamming/interference detection.
- **Interfaces**: Hardware UART (3.3V/5V compatible on module board), I2C interface (NMEA data streaming), Reset pin, and 1PPS (Pulse-Per-Second) output.
- **Reference Code / Host Demos**: C and Python drivers for Raspberry Pi, Arduino (Mega 2560/Uno), Raspberry Pi Pico (MicroPython), ESP32, and NVIDIA Jetson Nano.

---

## 2. Technical Specifications & Comparative Matrix

| Parameter / Feature | LC76G (AB) | L76K | L76B |
| :--- | :--- | :--- | :--- |
| **Core Chipset** | **Airoha AG3352Q** | AT6558R | MT3333 |
| **Supported GNSS Constellations** | GPS, GLONASS, Galileo, BDS, QZSS | GPS, GLONASS, BDS, QZSS | GPS, BDS, QZSS |
| **Form Factor** | LCC | LCC | LCC |
| **Dimensions (mm)** | 9.7 × 10.1 × 2.4 mm | 9.7 × 10.1 × 2.4 mm | 9.7 × 10.1 × 2.4 mm |
| **Weight** | 0.6 g | 0.4 g | 0.5 g |
| **Operating Temperature** | -40℃ to +85℃ | -40℃ to +85℃ | -40℃ to +85℃ |
| **Storage Temperature** | -40℃ to +90℃ | -40℃ to +90℃ | -40℃ to +90℃ |
| **L1 Receiver Channels** | 47 tracking channels | 32 tracking / 72 acquisition | 22 tracking / 66 acquisition |
| **SBAS Support** | Supported | Not Supported (N/A) | Supported |
| **A-GNSS Support** | Supported (EPO™, EASY™) | Supported (AGNSS) | Supported (EASY™) |
| **Auto-Acquisition Sensitivity** | -147 dBm | -148 dBm | -148 dBm |
| **Re-Acquisition Sensitivity** | -159 dBm | -160 dBm | -160 dBm |
| **Tracking Sensitivity** | **-166 dBm** | -162 dBm | -163 dBm |
| **TTFF: Cold Start (Autonomous)** | 26 s | 30 s | 35 s |
| **TTFF: Cold Start (EASY™)** | 12 s | N/A | 15 s |
| **TTFF: Cold Start (EPO™ / AGNSS)**| **5 s (EPO™)** | 5.5 s (AGNSS) | N/A |
| **TTFF: Warm Start (Autonomous)** | 20 s | N/A | 30 s |
| **TTFF: Warm Start (EASY™)** | 2 s | N/A | 5 s |
| **TTFF: Hot Start** | **1 s** | 2 s | 1 s |
| **Horizontal Position Accuracy** | **1.5 m CEP** | 2.0 m CEP | 2.5 m CEP |
| **Velocity Accuracy** | 0.1 m/s | 0.1 m/s | 0.1 m/s |
| **Max Acceleration Accuracy** | 0.1 m/s² | 0.1 m/s² | 0.1 m/s² |
| **Timing Accuracy (PPS)** | 100 ns | 30 ns | 10 ns |
| **Max Update Frequency** | **10 Hz** | 5 Hz | 10 Hz |
| **Default UART Baud Rate** | **115200 bps** | 9600 bps | 9600 bps |
| **Geo-fencing** | Supported | Not Supported | Supported |
| **Interference Detection** | Supported | Not Supported | Supported |
| **Active Anti-Jamming** | Supported | Not Supported | Supported |
| **Internal LNA** | Supported | Supported | Supported |
| **Core Supply Voltage (VDD)** | 2.55 V to 3.6 V | 2.7 V to 3.4 V | 2.8 V to 4.3 V |
| **Breakout Input Voltage (VCC)**| **3.3 V to 5.0 V** | 3.3 V to 5.0 V | 3.3 V to 5.0 V |
| **Power Draw: Acquisition** | 36 mA (GPS+GLO+GAL+BDS) | 29 mA (GPS+BDS) | 29 mA (GPS+BDS) |
| **Power Draw: Tracking** | 36 mA (GPS+GLO+GAL+BDS) | 29 mA (GPS+BDS) | 18 mA (GPS+BDS) |
| **Power Draw: Backup/RTC** | 13 µA | 8 µA | 7 µA |
| **Antenna Compatibility** | Active, Passive | Active, Passive | Active, Passive |
| **Antenna Protection** | Short & Open circuit detect| Short & Open circuit detect| Short & Open circuit detect|

---

## 3. Hardware Interfaces & Pinout

The module breakout board routes the IC connections to standard 2.54mm pitch headers.

```
       +---------------------------------------------+
       |               WAVESHARE LC76G               |
       |                                             |
       |  [Active/Passive Antenna Connector: IPEX]   |
       |                                             |
       |  [PWR LED]                  [PPS LED]       |
       |                                             |
       |  (1) (2) (3) (4) (5) (6) (7) (8)            |
       +---|---|---|---|---|---|---|---|-------------+
           |   |   |   |   |   |   |   |
          VCC GND  TX  RX PPS RST SDA SCL
```

### Pin Function Definitions

| Pin Name | Direction | Signal Type | Voltage Range | Functional Description |
| :--- | :--- | :--- | :--- | :--- |
| **VCC** | Power Input | DC Power | 3.3 V to 5.0 V | Main system power input. Onboard LDO regulates to core operating level. |
| **GND** | Ground | Ground | 0 V | Common ground reference. |
| **TX** | Output | Digital (UART) | 3.3 V TTL | UART serial data output (NMEA sentence stream from module to host). |
| **RX** | Input | Digital (UART) | 3.3 V TTL (5V tol.) | UART serial command input (PAIR/configuration packets from host). |
| **PPS** | Output | Digital Pulse | 3.3 V TTL | Pulse-Per-Second output; accurate time reference. Also indicates fix state. |
| **RST** | Input | Active-Low | 3.3 V TTL | Hardware system reset. Pull low to force hardware chip reset. |
| **SDA** | Bi-directional| Digital (I2C) | 3.3 V TTL | I2C Serial Data line for NMEA streaming over I2C bus. |
| **SCL** | Input | Digital (I2C) | 3.3 V TTL | I2C Serial Clock line for NMEA streaming over I2C bus. |

---

## 4. Antenna Requirements & Physical Deployment

1. **RF Antenna Orientation**:
   - Ceramic patch antennas have a directional reception radiation pattern.
   - The flat ceramic surface without silkscreen/text must face **skyward** toward the satellites.
   - The metal ground plane / silkscreen labeled side must face **downward**.
2. **Environmental Conditions**:
   - GNSS signals operate in the microwave L-band (1575.42 MHz / 1561.098 MHz / 1602 MHz) and cannot penetrate thick concrete, earth, or metal enclosures.
   - Initial cold-start positioning must be performed outdoors or adjacent to an open window with line-of-sight to at least 4 satellites.
   - Heavy rain and ambient atmospheric moisture will induce path attenuation, increasing initial TTFF.

---

## 5. NMEA 0183 Communication Protocol

The host communicates with the LC76G via serial stream using the standard **NMEA 0183** format at 8 data bits, 1 stop bit, no parity, and no hardware flow control.

### Standard Sentence Identifiers
The first two letters following `$` identify the satellite system (Talker ID):
- `$GP` — GPS (United States)
- `$BD` / `$GB` — BeiDou Navigation Satellite System (China)
- `$GL` — GLONASS (Russia)
- `$GA` — Galileo (European Union)
- `$GN` — Multi-GNSS combined / joint navigation solution

Supported standard sentence types include:
- `RMC` — Recommended Minimum Specific GNSS Data
- `GGA` — Global Positioning System Fix Data (includes altitude and fix status)
- `GSA` — GNSS DOP and Active Satellites
- `GSV` — GNSS Satellites in View (azimuth, elevation, C/N0 SNR)
- `VTG` — Course Over Ground and Ground Speed
- `GLL` — Geographic Position - Latitude/Longitude
- `ZDA` — Time & Date

### Checksum Calculation
The checksum is required at the end of each NMEA sentence, marked by `*hh`. It represents the 2-digit hexadecimal representation of the bitwise XOR calculation of all ASCII characters between `$` and `*` (exclusive of `$` and `*`).

---

### Detailed Protocol Frame: $GPRMC / $GNRMC

Format:
```
$GNRMC,<1>,<2>,<3>,<4>,<5>,<6>,<7>,<8>,<9>,<10>,<11>,<12>*hh<CR><LF>
```

Example:
```
$GNRMC,010555.000,A,2232.4682,N,11404.6748,E,0.00,125.29,230822,,,D*71
```

| Field # | Example Data | Field Name | Format / Range | Description |
| :--- | :--- | :--- | :--- | :--- |
| `<1>` | `010555.000` | UTC Time | `hhmmss.sss` | 01 hours, 05 minutes, 55.000 seconds |
| `<2>` | `A` | Fix Status | `A` or `V` | `A` = Valid Autonomous/Differential positioning; `V` = Receiver warning (Invalid fix) |
| `<3>` | `2232.4682` | Latitude | `ddmm.mmmm` | 22 degrees, 32.4682 minutes |
| `<4>` | `N` | Latitude Hemisphere | `N` or `S` | `N` = North, `S` = South |
| `<5>` | `11404.6748` | Longitude | `dddmm.mmmm`| 114 degrees, 04.6748 minutes |
| `<6>` | `E` | Longitude Hemisphere | `E` or `W` | `E` = East, `W` = West |
| `<7>` | `0.00` | Speed Over Ground | `000.0` to `999.9` | Measured in knots |
| `<8>` | `125.29` | Course Over Ground | `000.0` to `359.9` | Degrees referenced to True North |
| `<9>` | `230822` | UTC Date | `ddmmyy` | Day: 23, Month: 08, Year: 2022 |
| `<10>`| *(empty)* | Magnetic Declination | `000.0` to `180.0` | Degrees of magnetic variation |
| `<11>`| *(empty)* | Declination Direction | `E` or `W` | Direction of variation |
| `<12>`| `D` | Mode Indicator | Single Character | `A` = Autonomous, `D` = Differential (DGPS), `E` = Estimated/DR, `N` = Data invalid |
| `*hh` | `*71` | Checksum | 2 Hex Characters | Bitwise XOR validation value |

---

### Detailed Protocol Frame: $GPGGA / $GNGGA

The GGA frame provides 3D positioning fix data and antenna altitude relative to mean sea level.

Format:
```
$GNGGA,<1>,<2>,<3>,<4>,<5>,<6>,<7>,<8>,<9>,<10>,<11>,<12>,<13>,<14>*hh<CR><LF>
```

Example:
```
$GNGGA,085509.000,2232.467506,N,11404.694718,E,2,14,0.95,24.239,M,-1.934,M,,*57
```

| Field # | Example Data | Field Name | Description |
| :--- | :--- | :--- | :--- |
| `<1>` | `085509.000` | UTC Time | Fix taken at 08:55:09.000 UTC |
| `<2>` | `2232.467506` | Latitude | 22 degrees, 32.467506 minutes |
| `<3>` | `N` | Latitude Direction | North hemisphere |
| `<4>` | `11404.694718`| Longitude | 114 degrees, 04.694718 minutes |
| `<5>` | `E` | Longitude Direction | East hemisphere |
| `<6>` | `2` | Fix Quality Indicator | `0` = Invalid; `1` = GPS fix (SPS); `2` = DGPS fix; `6` = Dead reckoning |
| `<7>` | `14` | Number of Satellites | 14 satellites actively used for navigation solution |
| `<8>` | `0.95` | HDOP | Horizontal Dilution of Precision (0.95 indicates high accuracy) |
| `<9>` | `24.239` | Antenna Altitude | 24.239 meters above mean sea level |
| `<10>`| `M` | Altitude Units | Meters (`M`) |
| `<11>`| `-1.934` | Geoidal Separation | -1.934 meters (Difference between WGS-84 earth ellipsoid and geoid) |
| `<12>`| `M` | Geoidal Units | Meters (`M`) |
| `<13>`| *(empty)* | Age of DGPS Data | Elapsed time in seconds since last SC104 type update |
| `<14>`| *(empty)* | DGPS Station ID | Differential reference station ID |
| `*hh` | `*57` | Checksum | Data integrity XOR checksum |

---

## 6. Proprietary PAIR Configuration Command Protocol

The LC76G uses Airoha/Mediatek proprietary commands prefixed by `$PAIR`. Every command must end with an asterisk `*`, the two-digit hexadecimal checksum, and `<CR><LF>`.

### Common PAIR Command Register Set

| Command String | Operational Function | Parameter Arguments & Description |
| :--- | :--- | :--- |
| `$PAIR864,<Port>,<Idx>,<Baud>*<CS>` | Configure UART Baud Rate | `<Port>`: 0 = UART.<br>`<Idx>`: 0 = Port index 0.<br>`<Baud>`: Supported: `9600`, `115200`, `230400`, `460800`, `921600`, `3000000`.<br>Default: `115200`. |
| `$PAIR050,<Interval>*<CS>` | Configure Positioning Fix Rate | `<Interval>`: Fix cycle in milliseconds (`100` to `1000`).<br>`1000` = 1 Hz (Default).<br>`100` = 10 Hz.<br>*Note: For fix rates > 1 Hz, only RMC and GGA update at high rate; other frames output at 1 Hz.* |
| `$PAIR062,<Type>,<Rate>*<CS>` | Configure NMEA Sentence Output Rate | `<Type>`: Sentence type (0 = GGA, 1 = GLL, 2 = GSA, 3 = GSV, 4 = RMC, 5 = VTG, 6 = ZDA).<br>`<Rate>`: Output step (e.g., `3` outputs once every 3 fix cycles). |
| `$PAIR066,<GPS>,<GLO>,<GAL>,<BDS>,<QZSS>,0*<CS>` | Configure Active GNSS Constellations | Enable/Disable bits (`1` = Enable, `0` = Disable):<br>`<GPS>`: GPS L1<br>`<GLO>`: GLONASS G1<br>`<GAL>`: Galileo E1<br>`<BDS>`: BeiDou B1<br>`<QZSS>`: QZSS L1 |
| `$PAIR004*3E` | Hot Start | Restarts GNSS software using all ephemeris, almanac, and RTC time data in memory. |
| `$PAIR005*3F` | Warm Start | Restarts GNSS clearing ephemeris; retains almanac, position, and time. |
| `$PAIR006*3C` | Cold Start | Restarts GNSS clearing ephemeris, position, and time; retains almanac. |
| `$PAIR007*3D` | Full Factory Cold Start | Completely clears NVRAM/system configuration and performs cold restart. |

### Ready-To-Use Command Examples

```text
# Set UART Baud Rate to 115200 bps:
$PAIR864,0,0,115200*1B<CR><LF>

# Set UART Baud Rate to 9600 bps:
$PAIR864,0,0,9600*13<CR><LF>

# Set Positioning Frequency to 1 Hz (1000 ms):
$PAIR050,1000*12<CR><LF>

# Set Positioning Frequency to 10 Hz (100 ms):
$PAIR050,100*22<CR><LF>

# Set GGA Sentence Output to once every 3 position cycles:
$PAIR062,0,3*3D<CR><LF>

# Enable GPS + GLONASS + Galileo + BDS (QZSS disabled):
$PAIR066,1,1,1,1,0,0*3A<CR><LF>

# Execute Cold Start:
$PAIR006*3C<CR><LF>
```

*Note on Configuration Volatility: The LC76G does not feature internal non-volatile Flash storage for user configuration. All modifications will revert to chip factory defaults upon full power down unless continuous backup power is maintained on the RTC supply.*

---

## 7. Platform Interfacing & Integration Guides

### 7.1 Windows Host (USB-to-UART Adapter)

#### Wiring Diagram
```
  [USB to UART Adapter]           [Waveshare LC76G]
       +----------+                 +----------+
       |   5V/3V3 |---------------->| VCC      |
       |      GND |---------------->| GND      |
       |      TXD |---------------->| RX       |
       |      RXD |<----------------| TX       |
       +----------+                 | PPS (NC) |
                                    +----------+
```

#### Verification Steps
1. Connect hardware via USB-to-UART converter to Windows PC. Verify `PWR` LED lights up solid red.
2. Open **SSCOM** serial debugging utility:
   - Select COM port.
   - Configure Baud Rate: `115200` (or `9600` depending on current module setting).
   - Data Bits: `8`, Stop Bits: `1`, Parity: `None`, Flow Control: `None`.
3. Open **u-blox u-center** evaluation software:
   - Go to `Receiver` -> `Port` -> Choose assigned COM Port.
   - Go to `Receiver` -> `Baudrate` -> Select module baud rate.
   - Real-time satellite reception bars, deviation maps, and positioning status will populate.

---

### 7.2 Raspberry Pi Integration

#### Hardware Pin Mapping (40-Pin Header)
```
  Raspberry Pi (Physical Pins)        LC76G Module Pin
  ----------------------------        ----------------
  Pin 02 / 04 (5V Power)      ------> VCC
  Pin 06 / 09 / 14 (Ground)   ------> GND
  Pin 08 (GPIO 14 / TXD0)     ------> RX
  Pin 10 (GPIO 15 / RXD0)     <------ TX
  Pin NC                      ------  PPS (Unconnected)
```

#### OS Configuration & Serial Port Setup
1. Launch configuration utility:
   ```bash
   sudo raspi-config
   ```
2. Navigate to: `Interface Options` -> `Serial Port`.
3. Set **"Would you like a login shell to be accessible over serial?"** -> Select **NO**.
4. Set **"Would you like the serial port hardware to be enabled?"** -> Select **YES**.
5. Save and reboot the Raspberry Pi.

#### Serial Device Mapping Check
Identify which physical device maps to the primary serial port (`serial0`):
```bash
ls -l /dev/serial*
```
- If `serial0 -> ttyS0`: The system uses miniUART (`/dev/ttyS0`).
- If `serial0 -> ttyAMA0`: The system uses hardware PL011 UART (`/dev/ttyAMA0`).

*Note for Raspberry Pi 5: Add the following line to `/boot/config.txt` (or `/boot/firmware/config.txt`) to disable Bluetooth and map hardware UART to ttyS0:*
```ini
dtoverlay=disable-bt
```

#### Testing Raw NMEA Stream via Minicom
```bash
# Install minicom
sudo apt-get install minicom -y

# Launch serial console (adjust port and baud rate accordingly)
sudo minicom -D /dev/ttyS0 -b 115200
# To exit Minicom: Press Ctrl+A, then press X, then select YES.
```

#### Python Environment Setup
```bash
# Install system build dependencies and packages
sudo apt-get update
sudo apt-get install python3-pip python3-serial -y

# Install micropyGPS NMEA parser library
cd ~
wget https://files.waveshare.com/upload/d/dc/LC76G_GPS_Module_code.zip
unzip LC76G_GPS_Module_code.zip -d ./LC76G_GPS_Module_code
cd LC76G_GPS_Module_code/RaspberryPi/python3/micropyGPS-master
sudo python3 setup.py install

# Run Python 3 integration test
cd ../
sudo python3 main.py
```

---

### 7.3 Arduino (Mega 2560 / Uno)

#### Hardware Pin Mapping
```
  Arduino Mega 2560 Pin           LC76G Module Pin
  ---------------------           ----------------
  5V Power Supply         ------> VCC
  GND (Ground)            ------> GND
  Pin 16 (TX2)            ------> RX
  Pin 17 (RX2)            <------ TX
```

*Note on Arduino Uno: The Arduino Uno has only one hardware serial interface (shared with USB programming). If integrating with an Uno, the SoftwareSerial library must be used on digital pins (e.g., Pin 10 to TX, Pin 11 to RX). Uno 8-bit RAM constraints make high-frequency NMEA buffering difficult.*

#### Firmware Execution
1. Open `L76X.ino` within the Arduino IDE.
2. Under `Tools` -> `Board`, select **Arduino Mega 2560**.
3. Select the active COM port and click **Upload**.
4. Open the Serial Monitor at `9600` or `115200` baud. The program parses latitude, longitude, and converted map coordinates.

---

### 7.4 Raspberry Pi Pico (MicroPython)

#### Hardware Pin Mapping
```
  Raspberry Pi Pico Pin           LC76G Module Pin
  ---------------------           ----------------
  Pin 40 (VBUS 5V)        ------> VCC
  Pin 38 (GND)            ------> GND
  Pin 01 (GP0 / UART0 TX) ------> RX
  Pin 02 (GP1 / UART0 RX) <------ TX
```

#### Execution Procedure (Thonny IDE)
1. Open Thonny IDE; set Interpreter to **MicroPython (Raspberry Pi Pico)**.
2. Upload the demo directory containing `coordinate_convert.py` and supporting modules to the root of the Pico.
3. Open and run `coordinate_convert.py`.
4. The script initialises `UART(0, baudrate=115200, tx=Pin(0), rx=Pin(1))` and prints parsed coordinates to the Thonny terminal.

---

### 7.5 ESP32 Integration

#### Hardware Pin Mapping
```
  ESP32 Development Board         LC76G Module Pin
  -----------------------         ----------------
  5V / VBUS               ------> VCC
  GND                     ------> GND
  GPIO 17 (UART2 TX2)     ------> RX
  GPIO 16 (UART2 RX2)     <------ TX
```

#### Arduino IDE Board Manager Configuration
1. Open Arduino IDE -> `File` -> `Preferences`.
2. Add ESP32 board manager index URL:
   ```
   https://dl.espressif.com/dl/package_esp32_index.json
   ```
3. Open `Tools` -> `Board` -> `Boards Manager`, search for `esp32` by Espressif Systems, and click install.
4. Select `ESP32 Dev Module` under `Tools` -> `Board`.
5. Open `L76X.ino` from the ESP32 demo folder and click **Upload**.

---

### 7.6 NVIDIA Jetson Nano Dev Kit

#### Hardware Pin Mapping (40-Pin Expansion Header)
```
  Jetson Nano Header Pin          LC76G Module Pin
  ----------------------          ----------------
  Pin 02 / 04 (5V Power)  ------> VCC
  Pin 06 / 09 / 14 (GND)  ------> GND
  Pin 08 (UART1 TXD)      ------> RX
  Pin 10 (UART1 RXD)      <------ TX
```

#### Environment Configuration
```bash
# Install core Python 3 package managers
sudo apt-get update
sudo apt-get install python3-pip -y
sudo pip3 install pyserial setuptools

# Install micropyGPS driver
cd jetson/python3/micropyGPS-master
sudo python3 setup.py install

# Execute monitoring demo
cd ../
sudo python3 main.py
```

---

## 8. Coordinate Representations & Mathematical Conversion

The LC76G outputs raw geodetic coordinates using the **WGS-84** (World Geodetic System 1984) datum in **Degrees-Minutes** format (`ddmm.mmmmmm`). 

### Coordinate Formatting Steps

1. **Raw NMEA Format**:
   - Latitude: `2232.4682, N` -> $22^\circ\ 32.4682'$
   - Longitude: `11404.6748, E` -> $114^\circ\ 04.6748'$

2. **Conversion to Decimal Degrees (DD)**:
   $$\text{Decimal Degrees} = \text{Degrees} + \left(\frac{\text{Minutes}}{60}\right)$$
   $$\text{Latitude} = 22 + \left(\frac{32.4682}{60}\right) = 22.541136^\circ\text{ N}$$
   $$\text{Longitude} = 114 + \left(\frac{04.6748}{60}\right) = 114.077913^\circ\text{ E}$$

3. **Baidu Map Datum Conversion (BD-09)**:
   - Waveshare sample scripts automatically compute non-linear coordinate offsets to transform WGS-84 coordinates through the Chinese GCJ-02 datum into the Baidu BD-09 coordinate frame for local map visualization.

---

## 9. Engineering Troubleshooting & FAQ

### Q1: Why does the module fail to acquire a valid position fix?
- **Root Cause 1 (Antenna Orientation)**: The ceramic patch antenna must be deployed with the bare ceramic square facing upward to the open sky. If the printed text/silkscreen side faces up, satellite signal acquisition will fail.
- **Root Cause 2 (Signal Blockage & Attenuation)**: Testing indoors, under concrete ceilings, underground parking structures, or between dense high-rise urban canyons results in multipath interference and high attenuation. Testing must be conducted with open line-of-sight to the horizon.
- **Root Cause 3 (Atmospheric / Meteorological Conditions)**: Intense rainstorms and high atmospheric water vapor content induce signal degradation, increasing cold start TTFF from 26 seconds to several minutes.

### Q2: Why does the module's positioning differ from a smartphone?
- Smartphones use multi-frequency GNSS combined with **AGPS** (cellular base-station triangulation / LBS), Wi-Fi BSSID proximity maps, and Bluetooth beacons.
- The standalone LC76G module relies entirely on pure RF spaceborne satellite constellation signals.

### Q3: Why does the module reject a baud rate modification command?
- Sending `$PAIR864,0,0,9600*13` will fail if the module is actively streaming a full set of NMEA sentences ($GGA, $GLL, $GSA, $GSV, $RMC, $VTG) at high frequency. At 9600 bps, the physical UART bandwidth is saturated, resulting in internal serial FIFO buffer overruns.
- **Mitigation Procedure**: Disable or lower the frequency of verbose sentences (specifically `$GSV` and `$GSA`) using `$PAIR062` before issuing the baud rate reduction command.

### Q4: Why are custom PAIR configurations lost after power loss?
- The LC76G does not write runtime configuration changes to internal non-volatile Flash memory.
- If main system power (`VCC`) is disconnected and no backup battery is supplied to the internal RTC, the module executes a cold reset upon power-up and restores factory default settings (115200 baud, 1 Hz fix rate, all constellations active).

### Q5: How is altitude extracted from the LC76G?
- Altitude is not present in the `$GPRMC`/`$GNRMC` sentence.
- Altitude must be parsed from field 9 of the `$GPGGA`/`$GNGGA` sentence, which provides antenna elevation above mean sea level in meters. Field 11 provides the geoidal separation value.

### Q6: Raspberry Pi minicom opens but outputs no data or garbled text.
1. Verify the Linux primary serial assignment via `ls -l /dev/serial*`. If using `/dev/ttyS0`, ensure the terminal baud rate matches the module hardware default:
   ```bash
   sudo minicom -D /dev/ttyS0 -b 115200
   ```
2. On Raspberry Pi 3/4/5, hardware UART is routed by default to the onboard Bluetooth modem. Ensure `dtoverlay=disable-bt` is configured in `/boot/config.txt`.
3. Check TX/RX crossover: Host RX must connect to Module TX; Host TX must connect to Module RX.
