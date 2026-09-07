Here is the pinout mapping converted into structured tables based on the board's layout. 

*(Note: Typos in the original diagram such as `12C` have been corrected to `I2C`, and `UARTO`/`UARTI` to `UART0`/`UART1`)*.

---

### 1. Outer Edge Pins (Main Headers)

#### **Left Side (Top to Bottom)**
| Pin Label | GPIO / Power | ADC | SPI | I2C | UART |
| :--- | :--- | :---: | :---: | :---: | :---: |
| **5V** | 5V Power | — | — | — | — |
| **GND** | Ground | — | — | — | — |
| **3V3** | 3.3V Power | — | — | — | — |
| **29** | GPIO29 | ADC3 | — | — | — |
| **28** | GPIO28 | ADC2 | — | — | — |
| **27** | GPIO27 | ADC1 | — | I2C1 SCL | — |
| **26** | GPIO26 | ADC0 | — | I2C1 SDA | — |
| **15** | GPIO15 | — | SPI1 TX | I2C1 SCL | — |
| **14** | GPIO14 | — | SPI1 SCK | I2C1 SDA | — |

#### **Right Side (Top to Bottom)**
| Pin Label | GPIO | ADC | SPI | I2C | UART |
| :--- | :--- | :---: | :---: | :---: | :---: |
| **0** | GPIO00 | — | SPI0 RX | I2C0 SDA | UART0 TX |
| **1** | GPIO01 | — | SPI0 CSn | I2C0 SCL | UART0 RX |
| **2** | GPIO02 | — | SPI0 SCK | I2C1 SDA | — |
| **3** | GPIO03 | — | SPI0 TX | I2C1 SCL | — |
| **4** | GPIO04 | — | SPI0 RX | I2C0 SDA | UART1 TX |
| **5** | GPIO05 | — | SPI1 CSn | I2C0 SCL | UART1 RX |
| **6** | GPIO06 | — | SPI1 SCK | I2C1 SDA | — |
| **7** | GPIO07 | — | SPI1 TX | I2C1 SCL | — |
| **8** | GPIO08 | — | SPI1 RX | I2C0 SDA | UART1 TX |

---

### 2. Underside Surface Pads (Bottom View)

#### **Left Column (Pads 25 to 20)**
| Pad # | GPIO | SPI | I2C | UART |
| :---: | :--- | :---: | :---: | :---: |
| **25** | GPIO25 | — | — | — |
| **24** | GPIO24 | — | — | — |
| **23** | GPIO23 | — | — | — |
| **22** | GPIO22 | — | I2C0 SCL | — |
| **21** | GPIO21 | — | I2C0 SCL | — |
| **20** | GPIO20 | — | — | — |

#### **Right Column (Inner Pads)**
| Pad # | GPIO | SPI | I2C | UART |
| :---: | :--- | :---: | :---: | :---: |
| **12** | GPIO12 | SPI1 RX | I2C0 SDA | UART0 TX |
| **11** | GPIO11 | SPI1 TX | I2C1 SCL | — |
| **10** | GPIO10 | SPI1 SCK | I2C1 SDA | — |
| **9** | GPIO09 | SPI1 CSn | I2C0 SCL | UART0 RX |
| **13** | GPIO13 | SPI1 CSn | I2C0 SCL | UART0 RX |
| **14** | GPIO14 | SPI1 SCK | I2C1 SDA | — |
| **15** | GPIO15 | SPI1 TX | I2C1 SCL *(SDL)* | — |
