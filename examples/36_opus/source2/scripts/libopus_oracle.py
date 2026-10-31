#!/usr/bin/env python3
"""libopus-Gegenprobe: final_range des C-Decoders gegen opus-vis-JSON.

Unabhaengig von opus-pure: Ogg wird hier selbst zerlegt, decodiert wird
mit der System-libopus (ctypes, libopus.so.0). opus-vis liefert je Paket
`final_range` (JSON-Schema 3). Leere Frames (DTX/PLC) meldet libopus als
0 — wie opus-vis.

Aufruf:  scripts/libopus_oracle.py [--bin PFAD] datei.opus ...
Exit 0 = alle Pakete aller Dateien gleich.
"""
import ctypes
import ctypes.util
import json
import subprocess
import sys

OPUS_GET_FINAL_RANGE = 4031


def ogg_packets(data: bytes):
    """Pakete aus Ogg-Seiten zusammensetzen (RFC 3533, Lacing)."""
    pos, buf, out = 0, b"", []
    while pos + 27 <= len(data):
        assert data[pos:pos + 4] == b"OggS", "kein Ogg"
        nseg = data[pos + 26]
        lace = data[pos + 27:pos + 27 + nseg]
        body = pos + 27 + nseg
        for n in lace:
            buf += data[body:body + n]
            body += n
            if n < 255:
                out.append(buf)
                buf = b""
        pos = body
    return out[2:]  # OpusHead + OpusTags ueberspringen


def libopus_ranges(path, lib):
    data = open(path, "rb").read()
    pkts = ogg_packets(data)
    channels = ogg_packets_head_channels(data)
    err = ctypes.c_int()
    dec = lib.opus_decoder_create(48000, channels, ctypes.byref(err))
    assert err.value == 0, f"opus_decoder_create: {err.value}"
    pcm = (ctypes.c_float * (5760 * channels))()
    rng = ctypes.c_uint32()
    out = []
    for p in pkts:
        n = lib.opus_decode_float(dec, p, len(p), pcm, 5760, 0)
        assert n >= 0, f"opus_decode_float: {n}"
        lib.opus_decoder_ctl(dec, OPUS_GET_FINAL_RANGE, ctypes.byref(rng))
        out.append(rng.value)
    lib.opus_decoder_destroy(dec)
    return out


def ogg_packets_head_channels(data: bytes) -> int:
    nseg = data[26]
    head = data[27 + nseg:]
    assert head[:8] == b"OpusHead"
    return head[9]


def main(argv):
    binary = "target/release/opus-vis"
    if len(argv) > 2 and argv[1] == "--bin":
        binary, argv = argv[2], argv[2:]
    lib = ctypes.CDLL(ctypes.util.find_library("opus") or "libopus.so.0")
    lib.opus_decoder_create.restype = ctypes.c_void_p
    lib.opus_decoder_create.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
    lib.opus_decode_float.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_int,
                                      ctypes.POINTER(ctypes.c_float), ctypes.c_int, ctypes.c_int]
    lib.opus_decoder_ctl.argtypes = [ctypes.c_void_p, ctypes.c_int, ctypes.POINTER(ctypes.c_uint32)]
    lib.opus_decoder_destroy.argtypes = [ctypes.c_void_p]
    lib.opus_get_version_string.restype = ctypes.c_char_p
    print("libopus:", lib.opus_get_version_string().decode())
    bad = 0
    total = 0
    for path in argv[1:]:
        js = subprocess.run([binary, "--quiet", "-o", "/dev/null", "--json", "-", path],
                            check=True, capture_output=True).stdout
        ours = [f["final_range"] for f in json.loads(js)["frames"]]
        theirs = libopus_ranges(path, lib)
        same = sum(a == b for a, b in zip(ours, theirs))
        ok = same == len(ours) == len(theirs)
        bad += 0 if ok else 1
        total += same
        print(f"{'OK ' if ok else 'BAD'} {path}: {same}/{len(theirs)} Pakete gleich")
    print(f"gesamt {total} Pakete, {bad} Dateien mit Abweichung")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
