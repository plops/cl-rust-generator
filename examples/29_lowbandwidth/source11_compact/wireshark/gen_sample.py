#!/usr/bin/env python3
"""Erzeugt sample.pcap: eine LBW-v5-Sitzung (alle 16 Nachrichtenvarianten).

Nur stdlib. Body-Bytes sind per `encode_msg` verifizierte Vektoren
(common/examples/vecdump.rs); bei Protokolländerung hier + im Dissector
nachziehen. Ausnahme: die Kachel-Nutzdaten sind AV1 (TD + echte
rav1e-SEQ-Bytes + synthetische Frame-Bytes — nur Header werden gelesen,
s. S_TILE). Kachel absichtlich über zwei TCP-Segmente gesplittet
(Reassembly-Test). Deterministisch (feste Zeitstempel), echte
IP/TCP-Prüfsummen.
Aufruf: ./gen_sample.py [ausgabe.pcap]
"""
import struct
import sys

# Verifiziert per vecdump (encode_msg, bincode-2.0-Standard):
V = {
    # Server -> Client
    # Challenge version=5 nonce="0123456789abcdef"
    "S_CHALLENGE": "000530313233343536373839616263646566",
    "S_HELLO": "01",
    "S_CLEAR": "02",
    # NewText id=7 rect=1,2,300,16 fg=000000 bg=fffffa text="n€u"
    "S_NEW": "03070102fb2c0110000000fffffa056ee282ac75",
    # MoveText id=7 rect=10,200,100,16
    "S_MOVE": "04070ac86410",
    "S_RM": "0509",
    # Tile x=10 y=20 w=64 h=32 data=AV1 (20B): TD + SEQ + Frame.
    # SEQ-Payload 1fd53ff94250 sind echte rav1e-0.8.1-Still-Bytes
    # (Profil 0, still, reduziert, Level 31, 64x32 — s. packet-lbw.c);
    # Frame-Payload aa55.. ist synthetisch (Dissector liest nur Header).
    "S_TILE": "060a1440201412000a061fd53ff942503208aa55aa55aa55aa55",
    # Unbekannte Variante (Robustheit: als lbw.raw, kein Abort)
    "S_UNKNOWN": "09ff",
    # Hello mit Restbyte (Robustheit: Rest als lbw.raw)
    "S_TRAIL": "01aa",
    # Client -> Server
    # Auth mac=00..1f
    "C_AUTH": "00000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
    "C_HELLO": "0105",
    "C_MOVE": "0264c8",
    "C_BTN": "030101",
    "C_TEXT": "04026869",
    "C_KEY": "0505456e74657200",
    "C_RESYNC": "06",
    "C_QUALITY": "07",
}

CLI_PORT, SRV_PORT = 54321, 7878
CLI_MAC = bytes.fromhex("020000000001")
SRV_MAC = bytes.fromhex("020000000002")
CLI_IP = bytes([10, 0, 0, 1])
SRV_IP = bytes([10, 0, 0, 2])
BASE_TS = 1_700_000_000


def cksum(b: bytes) -> int:
    if len(b) % 2:
        b += b"\x00"
    s = sum(struct.unpack(f"!{len(b) // 2}H", b))
    while s >> 16:
        s = (s & 0xFFFF) + (s >> 16)
    return (~s) & 0xFFFF


def frame(body_hex: str) -> bytes:
    body = bytes.fromhex(body_hex)
    return struct.pack("<I", len(body)) + body


def packet(src_mac, dst_mac, src_ip, dst_ip, sport, dport, seq, ack, payload):
    iph = struct.pack(
        "!BBHHHBBH4s4s", 0x45, 0, 20 + 20 + len(payload), 0, 0x4000, 64, 6, 0,
        src_ip, dst_ip)
    iph = iph[:10] + struct.pack("!H", cksum(iph)) + iph[12:]
    tcph = struct.pack("!HHIIBBHHH", sport, dport, seq, ack, 5 << 4, 0x10,
                       65535, 0, 0)
    pseudo = src_ip + dst_ip + struct.pack("!BBH", 0, 6, 20 + len(payload))
    tcph = (tcph[:16] + struct.pack("!H", cksum(pseudo + tcph + payload))
            + tcph[18:])
    return (dst_mac + src_mac + b"\x08\x00" + iph + tcph + payload)


def main(path: str) -> None:
    tile = frame(V["S_TILE"])
    # (Richtung Server->Client?, Nutzdaten); Kachel in 5 + Rest gesplittet.
    msgs = [
        (False, frame(V["C_AUTH"])),
        (False, frame(V["C_HELLO"])),
        (True, frame(V["S_CHALLENGE"])),
        (True, frame(V["S_HELLO"])),
        (True, frame(V["S_CLEAR"])),
        (True, frame(V["S_NEW"])),
        (True, frame(V["S_MOVE"])),
        (True, tile[:5]),
        (True, tile[5:]),
        (True, frame(V["S_RM"])),
        (True, frame(V["S_UNKNOWN"])),
        (True, frame(V["S_TRAIL"])),
        (False, frame(V["C_MOVE"])),
        (False, frame(V["C_BTN"])),
        (False, frame(V["C_TEXT"])),
        (False, frame(V["C_KEY"])),
        (False, frame(V["C_RESYNC"])),
        (False, frame(V["C_QUALITY"])),
    ]
    out = [struct.pack("<IHHIIII", 0xA1B2C3D4, 2, 4, 0, 0, 65535, 1)]
    seq = {True: 1000, False: 5000}
    for i, (srv, data) in enumerate(msgs):
        if srv:
            pkt = packet(SRV_MAC, CLI_MAC, SRV_IP, CLI_IP, SRV_PORT, CLI_PORT,
                         seq[True], seq[False], data)
        else:
            pkt = packet(CLI_MAC, SRV_MAC, CLI_IP, SRV_IP, CLI_PORT, SRV_PORT,
                         seq[False], seq[True], data)
        seq[srv] += len(data)
        out.append(struct.pack("<IIII", BASE_TS + i, 0, len(pkt), len(pkt)))
        out.append(pkt)
    with open(path, "wb") as f:
        f.write(b"".join(out))
    print(f"{path}: {len(msgs)} Pakete")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "sample.pcap")
