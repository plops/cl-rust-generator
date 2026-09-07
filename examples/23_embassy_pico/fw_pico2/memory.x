/* Pico 2 (RP2350A) memory layout: 4 MB external QSPI flash, 520 KB SRAM.
   FLASH starts at 0x200 (not 0x100): the RP2350 vector table is 276 bytes,
   so cortex-m-rt requires 512-byte alignment. The BOOT2 slot holds
   IMAGE_DEF (.start_block, placed by link-rp235x.x); RP235x links no BOOT2. */
MEMORY {
    BOOT2 : ORIGIN = 0x10000000, LENGTH = 0x100
    FLASH : ORIGIN = 0x10000200, LENGTH = 4096K - 0x200
    RAM   : ORIGIN = 0x20000000, LENGTH = 520K
}
