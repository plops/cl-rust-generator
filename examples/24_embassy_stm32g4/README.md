run:

```
24_embassy_stm32g4/fw $ cargo run --release --bin g474-tui /dev/ttyACM0
```


◆ Cable connections needed for the hardware loopback tests (all on the upper header inner row, all 3.3V — no level shifting; use ~1 kΩ series
  resistors since these are non-5V-tolerant TT_a pins, max 3.6V):ct

  ┌──────────────────────────┬─────────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────┐
  │ Test                     │ Wire                                                    │ Why                                                      │
  ├──────────────────────────┼─────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────┤
  │ E edge-proof (counter)   │ PA5 → PA7                                               │ 1 kHz tone from the AWG task into the COMP2 counter      │
  │                          │                                                         │ input; FreqStart must read ~1000 Hz                      │
  │ A loopback (scope)       │ PA5 → PA0                                               │ Same tone into the ADC1 scope input; ScopeRead must show │
  │                          │                                                         │ the square wave                                          │
  │ B VNA cal + DUT          │ PA5 → PA0 (through), then PA5 → RC → PA0                │ Through = flat 0 dB reference; RC low-pass = falling     │
  │                          │                                                         │ magnitude curve                                          │
  │ E external (alternative) │ Generator → PA7 via 1 kΩ, ≤3.3 V                        │ If you have a function generator, no AWG needed          │
  │ D capacitance            │ Test cap (e.g. 100 nF) between PA1 (or PB1/PB0) and GND │ Discharge-time measurement; floating pin alone reads ~0  │
  └──────────────────────────┴─────────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────┘

  One wire PA5→PA7 unlocks the counter proof; moving it to PA5→PA0 unlocks scope and VNA-through. No cable needed for anything committed so far
  (all green without wires).
