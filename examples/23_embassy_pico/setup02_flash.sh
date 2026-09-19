
# cargo install elf2flash --locked
# elf2flash deploy --board rp2350 fw_pico2/target/thumbv8m.main-none-eabihf/release/pico2-fw
elf2flash convert --board rp2350 fw_pico2/target/thumbv8m.main-none-eabihf/release/pico2-fw pico2-fw.uf2
