/* packet-lbw.c — Wireshark-Dissector für das LBW-Protokoll (v5, source11).
 *
 * Rahmen: [u32 LE Länge][bincode-Body] über TCP (Default-Port 7878, als
 * Präferenz änderbar; alternativ „Decode As → TCP → lbw"). Die Daten sind
 * Klartext *innerhalb* des SSH-Tunnels — Mitschnitt auf `lo` am
 * Tunnel-Endpunkt oder Client-`--tap`. Richtung: Quellport == Server-Port
 * → Server→Client, sonst Client→Server (Fallback: niedrigerer Port ist
 * der Server).
 *
 * bincode 2.0 (Standard-Config): Varints (<251 direkt, sonst 251+u16LE /
 * 252+u32LE / 253+u64LE), Enum-Variante als u32-Varint, String/Vec als
 * u64-Varint-Länge + Bytes, u8/bool als Einzelbytes, feste Arrays roh.
 * Alle Body-Bytes in gen_sample.py sind per `encode_msg` verifiziert
 * (common/examples/vecdump.rs).
 *
 * v5-Varianten S→C: 0 Challenge{version, nonce[16]}, 1 Hello, 2 ClearText,
 * 3 NewText, 4 MoveText, 5 RemoveText, 6 Tile{x, y, w, h, data} (w/h =
 * Fußabdruck, Pixel ggf. kleiner kodiert; data = AV1-OBUs TD+SEQ+Frame,
 * Dissector zeigt OBU-Typen + SEQ-Maße als lbw.av1.*). C→S: 0 Auth{mac[32]},
 * 1 Hello{version}, 2 MouseMove, 3 Button, 4 Text, 5 Key, 6 ResyncText,
 * 7 QualityTile.
 * Vom Auth-Token zeigt der Dissector nur Längen, nie Geheimnisse.
 *
 * Bauen: s. CMakeLists.txt (braucht libwireshark-dev derselben Version).
 */

#include <string.h>

#include <glib.h>
#include <epan/expert.h>
#include <epan/packet.h>
#include <epan/prefs.h>
#include <epan/dissectors/packet-tcp.h>
#include <wsutil/plugins.h>

/* Vom Build gesetzt (laufende Wireshark-Version, s. CMakeLists.txt). */
#ifndef LBW_PLUGIN_VERSION
#define LBW_PLUGIN_VERSION "0.0.0"
#endif
#ifndef LBW_WANT_MAJOR
#define LBW_WANT_MAJOR 0
#endif
#ifndef LBW_WANT_MINOR
#define LBW_WANT_MINOR 0
#endif

const gchar plugin_version[] = LBW_PLUGIN_VERSION;
const int plugin_want_major = LBW_WANT_MAJOR;
const int plugin_want_minor = LBW_WANT_MINOR;

static void proto_register_lbw(void);
static void proto_reg_handoff_lbw(void);

uint32_t
plugin_describe(void)
{
    return WS_PLUGIN_DESC_DISSECTOR;
}

void
plugin_register(void)
{
    static proto_plugin plug;
    plug.register_protoinfo = proto_register_lbw;
    plug.register_handoff = proto_reg_handoff_lbw;
    proto_register_plugin(&plug);
}

#define LBW_DEFAULT_PORT 7878
#define LBW_MAX_BODY (8u * 1024u * 1024u) /* = MAX_MSG in common */

static int proto_lbw = -1;
static dissector_handle_t lbw_handle;
static guint g_port = LBW_DEFAULT_PORT;

static int hf_len, hf_msg, hf_raw;
static int hf_tid, hf_tx, hf_ty, hf_tw, hf_th, hf_fg, hf_bg, hf_str;
static int hf_tilx, hf_tily, hf_tilw, hf_tilh, hf_tillen, hf_tildata;
static int hf_av1_obus, hf_av1_obu, hf_av1_profile, hf_av1_still;
static int hf_av1_level, hf_av1_w, hf_av1_h;
static int hf_rmid;
static int hf_ver, hf_mx, hf_my, hf_btn, hf_down, hf_itext, hf_key;
static int hf_nonce_len, hf_mac_len;
static int ett_lbw, ett_av1, ett_obu;
static expert_field ei_trunc, ei_variant, ei_av1;

/* Lesecursor mit Ende-Marke; jede Entnahme prüft Bounds (trunc-Flag). */
typedef struct {
    tvbuff_t *tvb;
    wmem_allocator_t *pool;
    int off;
    int end;
    bool trunc;
} cur_t;

static uint64_t
cur_varint(cur_t *c)
{
    uint8_t b;
    unsigned n, take, i;
    uint64_t v = 0;
    if (c->off >= c->end) {
        c->trunc = true;
        return 0;
    }
    b = tvb_get_uint8(c->tvb, c->off++);
    if (b < 251)
        return b;
    n = (b == 251) ? 2 : (b == 252) ? 4 : 8;
    if (b == 254)
        n = 16;
    if (c->off + (int)n > c->end) {
        c->trunc = true;
        return 0;
    }
    take = n > 8 ? 8 : n;
    for (i = 0; i < take; i++)
        v |= (uint64_t)tvb_get_uint8(c->tvb, c->off + i) << (8 * i);
    c->off += n;
    return v;
}

static uint8_t
cur_u8(cur_t *c)
{
    if (c->off >= c->end) {
        c->trunc = true;
        return 0;
    }
    return tvb_get_uint8(c->tvb, c->off++);
}

/* n Bytes als Feld; liefert Start-Offset (trunc-Flag bei Überlauf). */
static int
cur_bytes(cur_t *c, unsigned n)
{
    int at = c->off;
    if (n > (unsigned)(c->end - c->off)) {
        c->trunc = true;
        return c->off;
    }
    c->off += n;
    return at;
}

static void
add_rect(cur_t *c, proto_tree *t)
{
    int at = c->off;
    uint64_t x = cur_varint(c), y = cur_varint(c);
    uint64_t w = cur_varint(c), h = cur_varint(c);
    if (c->trunc)
        return;
    proto_tree_add_uint(t, hf_tx, c->tvb, at, c->off - at, (uint32_t)x);
    proto_tree_add_uint(t, hf_ty, c->tvb, at, c->off - at, (uint32_t)y);
    proto_tree_add_uint(t, hf_tw, c->tvb, at, c->off - at, (uint32_t)w);
    proto_tree_add_uint(t, hf_th, c->tvb, at, c->off - at, (uint32_t)h);
}

static const char *
add_string(cur_t *c, proto_tree *t, int hf)
{
    uint64_t n = cur_varint(c);
    int at;
    uint8_t *s;
    if (c->trunc || n > (uint64_t)(c->end - c->off)) {
        c->trunc = true;
        return "";
    }
    at = cur_bytes(c, (unsigned)n);
    s = tvb_get_string_enc(c->pool, c->tvb, at, (int)n, ENC_UTF_8);
    proto_tree_add_string(t, hf, c->tvb, at, (int)n, (const char *)s);
    return (const char *)s;
}

/* --- AV1-Nutzdaten: OBU-Walk, nur Metadaten (kein Dekodieren) ---
 * Unser Encoder (rav1e 0.8.1, still_picture) schreibt pro Kachel:
 * Temporal Delimiter + Sequence Header + Frame. Der Sequence Header
 * trägt Profil, still_picture, reduced_still_picture_header, Level
 * (5 Bit), dann Breite/Höhe je als (Bitbreite-1 in 4 Bit + Wert).
 * Achtung: rav1e 0.8.1 schreibt das Profil in 3 Bit (statt 2 laut
 * AV1-Spezifikation §5.5) — gegen echte Encoder-Frames verifiziert
 * (ergibt exakt die kodierte Kachelgröße). Quelle: rav1e-0.8.1
 * src/header.rs (write_sequence_header_obu, write_max_frame_size).
 * Der Dissector zeigt OBU-Typen + SEQ-Maße; Frame-Payload nur Größe
 * (Bilder zeigt Wireshark prinzipbedingt nicht). Volle (nicht
 * reduzierte) Sequence Header werden als OBU gezeigt, Maße dann leer.
 */

/* Eigener Lookup statt val_to_str (dessen Signatur je nach
 * Wireshark-Version einen Scope-Allocator verlangt). */
static const char *
obu_name(unsigned t)
{
    switch (t) {
    case 0: return "Reserved";
    case 1: return "Sequence Header";
    case 2: return "Temporal Delimiter";
    case 3: return "Frame Header";
    case 4: return "Tile Group";
    case 5: return "Metadata";
    case 6: return "Frame";
    case 7: return "Redundant Frame Header";
    case 8: return "Tile List";
    case 15: return "Padding";
    default: return "Unbekannt";
    }
}

#define LBW_AV1_MAX_OBU 64

typedef struct {
    unsigned type;
    int at;   /* OBU-Start (Kopf) */
    int len;  /* Kopf + Payload */
    int pay;  /* Payload-Start */
    int plen; /* Payload-Länge */
    uint8_t ext;
    bool has_ext;
} obu_t;

typedef struct {
    uint32_t profile, level, w, h;
    bool still, have;
} seq_t;

/* Bitleser (MSB zuerst), für den reduzierten Sequence Header. */
typedef struct {
    tvbuff_t *tvb;
    int off;
    int end;
    unsigned bit;
    bool trunc;
} bit_t;

static uint32_t
bit_read(bit_t *b, unsigned n)
{
    uint32_t v = 0;
    while (n-- > 0) {
        if (b->off >= b->end) {
            b->trunc = true;
            return 0;
        }
        v = (v << 1) | ((tvb_get_uint8(b->tvb, b->off) >> (7 - b->bit)) & 1);
        if (++b->bit == 8) {
            b->bit = 0;
            b->off++;
        }
    }
    return v;
}

/* AV1-LEB128 (max. 8 Byte); zu lang/abgeschnitten → trunc. */
static uint64_t
tvb_leb128(tvbuff_t *tvb, int *off, int end, bool *trunc)
{
    uint64_t v = 0;
    unsigned shift = 0, i;
    for (i = 0; i < 8; i++) {
        uint8_t by;
        if (*off >= end) {
            *trunc = true;
            return 0;
        }
        by = tvb_get_uint8(tvb, (*off)++);
        if (shift < 64)
            v |= (uint64_t)(by & 0x7f) << shift;
        if (!(by & 0x80))
            return v;
        shift += 7;
    }
    *trunc = true;
    return 0;
}

/* Reduzierter Sequence Header (rav1e-Still-Picture): Maße lesen.
 * Kein Still/reduziert → have=false (nur OBU zeigen, nicht raten). */
static void
parse_seq(tvbuff_t *tvb, int at, int len, seq_t *s)
{
    bit_t b = { tvb, at, at + len, 0, false };
    uint32_t wb, hb;
    s->have = false;
    s->profile = bit_read(&b, 3);
    s->still = bit_read(&b, 1) != 0;
    if (bit_read(&b, 1) == 0)
        return; /* kein reduced_still_picture_header */
    if (!s->still)
        return;
    s->level = bit_read(&b, 5);
    wb = bit_read(&b, 4) + 1;
    hb = bit_read(&b, 4) + 1;
    if (wb > 16 || hb > 16)
        return;
    s->w = bit_read(&b, wb) + 1;
    s->h = bit_read(&b, hb) + 1;
    if (!b.trunc)
        s->have = true;
}

/* OBU-Walk über Kachel-Bytes [at, at+len). Erste OBU ungültig →
 * still zurück (kein AV1?), sonst Baum + Maße (out: have/w/h).
 * Abbruch später → Expert-Info (echte Korruption, nicht Raten). */
static void
add_av1(proto_tree *t, packet_info *pinfo, tvbuff_t *tvb, int at, int len,
    bool *have, uint32_t *w, uint32_t *h)
{
    obu_t obus[LBW_AV1_MAX_OBU];
    seq_t seq = { 0, 0, 0, 0, false, false };
    int off = at, end = at + len, n = 0, i;
    bool cut = false;
    proto_item *ai;
    proto_tree *a;

    *have = false;
    if (len <= 0 || (tvb_get_uint8(tvb, at) & 0x80) != 0)
        return; /* verbotenes Bit gesetzt: kein OBU-Strom */

    while (off < end && !cut) {
        obu_t *o;
        uint8_t hdr = tvb_get_uint8(tvb, off);
        uint64_t sz;
        bool t = false;
        if ((hdr & 0x80) != 0 || n >= LBW_AV1_MAX_OBU) {
            cut = true;
            break;
        }
        o = &obus[n];
        o->type = (unsigned)((hdr >> 3) & 15);
        o->at = off;
        o->has_ext = ((hdr >> 2) & 1) != 0;
        off++;
        if (o->has_ext) {
            if (off >= end) {
                cut = true;
                break;
            }
            o->ext = tvb_get_uint8(tvb, off++);
        } else {
            o->ext = 0;
        }
        if ((hdr >> 1) & 1) {
            sz = tvb_leb128(tvb, &off, end, &t);
            if (t) {
                cut = true;
                break;
            }
        } else {
            sz = (uint64_t)(end - off); /* nur letzte OBU: Rest */
        }
        if (sz > (uint64_t)(end - off)) {
            cut = true;
            break;
        }
        o->pay = off;
        o->plen = (int)sz;
        o->len = off + (int)sz - o->at;
        if (o->type == 1 && !seq.have)
            parse_seq(tvb, o->pay, o->plen, &seq);
        n++;
        off += (int)sz;
    }
    if (n == 0)
        return;

    if (seq.have) {
        *have = true;
        *w = seq.w;
        *h = seq.h;
        ai = proto_tree_add_uint_format(t, hf_av1_obus, tvb, at, len,
            (uint32_t)n, "AV1: %d OBU%s, %" PRIu32 "x%" PRIu32, n,
            n == 1 ? "" : "s", seq.w, seq.h);
    } else {
        ai = proto_tree_add_uint_format(t, hf_av1_obus, tvb, at, len,
            (uint32_t)n, "AV1: %d OBU%s", n, n == 1 ? "" : "s");
    }
    a = proto_item_add_subtree(ai, ett_av1);
    for (i = 0; i < n; i++) {
        obu_t *o = &obus[i];
        const char *name = obu_name(o->type);
        proto_item *oi;
        proto_tree *os;
        if (o->has_ext)
            oi = proto_tree_add_string_format(a, hf_av1_obu, tvb, o->at,
                o->len, name, "%s (%d B, ext 0x%02x)", name, o->plen, o->ext);
        else
            oi = proto_tree_add_string_format(a, hf_av1_obu, tvb, o->at,
                o->len, name, "%s (%d B)", name, o->plen);
        if (o->type == 1 && seq.have) {
            /* Nur reduzierter Still-Header: volles Layout unbekannt. */
            os = proto_item_add_subtree(oi, ett_obu);
            proto_tree_add_uint(os, hf_av1_profile, tvb, o->pay, o->plen,
                seq.profile);
            proto_tree_add_boolean(os, hf_av1_still, tvb, o->pay, o->plen,
                seq.still);
            proto_tree_add_uint(os, hf_av1_level, tvb, o->pay, o->plen,
                seq.level);
            proto_tree_add_uint(os, hf_av1_w, tvb, o->pay, o->plen, seq.w);
            proto_tree_add_uint(os, hf_av1_h, tvb, o->pay, o->plen, seq.h);
        }
    }
    if (cut || off != end)
        expert_add_info_format(pinfo, ai, &ei_av1,
            "AV1-OBUs abgebrochen (%d OBU(s) gelesen)", n);
}

static unsigned
get_lbw_len(packet_info *pinfo, tvbuff_t *tvb, int offset, void *data)
{
    uint32_t n;
    (void)pinfo;
    (void)data;
    if (!tvb_bytes_exist(tvb, offset, 4))
        return 0;
    n = tvb_get_letohl(tvb, offset);
    if (n == 0 || n > LBW_MAX_BODY)
        return 4; /* Müll: nur Header schlucken, Resync versuchen */
    return 4 + n;
}

static int
dissect_msg(tvbuff_t *tvb, packet_info *pinfo, proto_tree *tree, void *data)
{
    cur_t c = { tvb, pinfo->pool, 0, tvb_captured_length(tvb), false };
    proto_item *ti;
    proto_tree *t;
    uint32_t len;
    uint64_t v;
    bool from_srv;
    const char *dir;
    (void)data;

    col_set_str(pinfo->cinfo, COL_PROTOCOL, "LBW");
    col_clear(pinfo->cinfo, COL_INFO);

    if (c.end < 4) {
        col_add_str(pinfo->cinfo, COL_INFO, "Fragment");
        return c.end;
    }
    len = tvb_get_letohl(tvb, 0);
    c.off = 4;

    if (pinfo->srcport == g_port)
        from_srv = true;
    else if (pinfo->destport == g_port)
        from_srv = false;
    else
        from_srv = pinfo->srcport < pinfo->destport; /* Fallback-Heuristik */
    dir = from_srv ? "S→C" : "C→S";

    ti = proto_tree_add_item(tree, proto_lbw, tvb, 0, c.end, ENC_NA);
    t = proto_item_add_subtree(ti, ett_lbw);
    proto_tree_add_uint(t, hf_len, tvb, 0, 4, len);

    v = cur_varint(&c);
    if (c.trunc)
        goto short_frame;

    if (from_srv) {
        switch (v) {
        case 0: {
            int at = 4;
            uint64_t ver = cur_varint(&c);
            int n;
            if (c.trunc)
                goto short_frame;
            n = cur_bytes(&c, 16); /* Nonce: nur Länge zeigen */
            if (c.trunc)
                goto short_frame;
            (void)n;
            proto_tree_add_uint(t, hf_ver, tvb, at, c.off - at, (uint32_t)ver);
            proto_tree_add_uint(t, hf_nonce_len, tvb, at, c.off - at, 16);
            proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                "Challenge", "Challenge v%" PRIu64 " (nonce 16B)", ver);
            col_add_fstr(pinfo->cinfo, COL_INFO, "S→C Challenge v%" PRIu64, ver);
            break;
        }
        case 1:
            proto_tree_add_string(t, hf_msg, tvb, 4, 1, "Hello");
            col_add_str(pinfo->cinfo, COL_INFO, "S→C Hello");
            break;
        case 2:
            proto_tree_add_string(t, hf_msg, tvb, 4, 1, "ClearText");
            col_add_str(pinfo->cinfo, COL_INFO, "S→C ClearText");
            break;
        case 3: {
            int at = 4;
            uint64_t id = cur_varint(&c);
            const char *s;
            int fg, bg;
            if (c.trunc)
                goto short_frame;
            proto_tree_add_uint64(t, hf_tid, tvb, at, c.off - at, id);
            add_rect(&c, t);
            fg = cur_bytes(&c, 3);
            bg = cur_bytes(&c, 3);
            s = add_string(&c, t, hf_str);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_item(t, hf_fg, tvb, fg, 3, ENC_NA);
            proto_tree_add_item(t, hf_bg, tvb, bg, 3, ENC_NA);
            proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                "NewText", "NewText id=%" PRIu64 " \"%s\"", id, s);
            col_add_fstr(pinfo->cinfo, COL_INFO, "S→C NewText id=%" PRIu64 " \"%s\"", id, s);
            break;
        }
        case 4: {
            int at = 4;
            uint64_t id = cur_varint(&c);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_uint64(t, hf_tid, tvb, at, c.off - at, id);
            add_rect(&c, t);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                "MoveText", "MoveText id=%" PRIu64, id);
            col_add_fstr(pinfo->cinfo, COL_INFO, "S→C MoveText id=%" PRIu64, id);
            break;
        }
        case 5: {
            int at = 4;
            uint64_t id = cur_varint(&c);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_uint64(t, hf_rmid, tvb, at, c.off - at, id);
            proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                "RemoveText", "RemoveText id=%" PRIu64, id);
            col_add_fstr(pinfo->cinfo, COL_INFO, "S→C RemoveText id=%" PRIu64, id);
            break;
        }
        case 6: {
            int at = 4;
            uint64_t x = cur_varint(&c), y = cur_varint(&c);
            uint64_t w = cur_varint(&c), h = cur_varint(&c), n = cur_varint(&c);
            int dat;
            if (c.trunc)
                goto short_frame;
            dat = cur_bytes(&c, (unsigned)n);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_uint(t, hf_tilx, tvb, at, c.off - at, (uint32_t)x);
            proto_tree_add_uint(t, hf_tily, tvb, at, c.off - at, (uint32_t)y);
            proto_tree_add_uint(t, hf_tilw, tvb, at, c.off - at, (uint32_t)w);
            proto_tree_add_uint(t, hf_tilh, tvb, at, c.off - at, (uint32_t)h);
            proto_tree_add_uint(t, hf_tillen, tvb, at, c.off - at, (uint32_t)n);
            proto_tree_add_item(t, hf_tildata, tvb, dat, (int)n, ENC_NA);
            {
                bool have = false;
                uint32_t aw = 0, ah = 0;
                add_av1(t, pinfo, tvb, dat, (int)n, &have, &aw, &ah);
                proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                    "Tile", "Tile @%" PRIu64 ",%" PRIu64 " %" PRIu64 "x%" PRIu64 " %" PRIu64 "B",
                    x, y, w, h, n);
                col_add_fstr(pinfo->cinfo, COL_INFO,
                    "S→C Tile @%" PRIu64 ",%" PRIu64 " %" PRIu64 "x%" PRIu64 " %" PRIu64 "B",
                    x, y, w, h, n);
                if (have)
                    col_append_fstr(pinfo->cinfo, COL_INFO, " AV1 %" PRIu32 "x%" PRIu32, aw, ah);
            }
            break;
        }
        default:
            goto unknown;
        }
    } else {
        switch (v) {
        case 0: {
            int at = 4;
            int m = cur_bytes(&c, 32); /* MAC: nur Länge zeigen */
            if (c.trunc)
                goto short_frame;
            (void)m;
            proto_tree_add_uint(t, hf_mac_len, tvb, at, c.off - at, 32);
            proto_tree_add_string(t, hf_msg, tvb, at, c.off - at, "Auth");
            col_add_str(pinfo->cinfo, COL_INFO, "C→S Auth (mac 32B)");
            break;
        }
        case 1: {
            int at = 4;
            uint64_t ver = cur_varint(&c);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_uint(t, hf_ver, tvb, at, c.off - at, (uint32_t)ver);
            proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                "Hello", "Hello v%" PRIu64, ver);
            col_add_fstr(pinfo->cinfo, COL_INFO, "C→S Hello v%" PRIu64, ver);
            break;
        }
        case 2: {
            int at = 4;
            uint64_t x = cur_varint(&c), y = cur_varint(&c);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_uint(t, hf_mx, tvb, at, c.off - at, (uint32_t)x);
            proto_tree_add_uint(t, hf_my, tvb, at, c.off - at, (uint32_t)y);
            proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                "MouseMove", "MouseMove %" PRIu64 ",%" PRIu64, x, y);
            col_add_fstr(pinfo->cinfo, COL_INFO, "C→S MouseMove %" PRIu64 ",%" PRIu64, x, y);
            break;
        }
        case 3: {
            int at = 4;
            uint8_t b = cur_u8(&c), d = cur_u8(&c);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_uint(t, hf_btn, tvb, at, 1, b);
            proto_tree_add_boolean(t, hf_down, tvb, at + 1, 1, d != 0);
            proto_tree_add_string_format(t, hf_msg, tvb, at, 2,
                "Button", "Button %u %s", b, d ? "down" : "up");
            col_add_fstr(pinfo->cinfo, COL_INFO, "C→S Button %u %s", b, d ? "down" : "up");
            break;
        }
        case 4: {
            int at = 4;
            const char *s = add_string(&c, t, hf_itext);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                "Text", "Text \"%s\"", s);
            col_add_fstr(pinfo->cinfo, COL_INFO, "C→S Text \"%s\"", s);
            break;
        }
        case 5: {
            int at = 4;
            const char *s = add_string(&c, t, hf_key);
            uint8_t d = cur_u8(&c);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_boolean(t, hf_down, tvb, c.off - 1, 1, d != 0);
            proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                "Key", "Key \"%s\" %s", s, d ? "down" : "up");
            col_add_fstr(pinfo->cinfo, COL_INFO, "C→S Key \"%s\" %s", s, d ? "down" : "up");
            break;
        }
        case 6:
            proto_tree_add_string(t, hf_msg, tvb, 4, 1, "ResyncText");
            col_add_str(pinfo->cinfo, COL_INFO, "C→S ResyncText");
            break;
        case 7:
            proto_tree_add_string(t, hf_msg, tvb, 4, 1, "QualityTile");
            col_add_str(pinfo->cinfo, COL_INFO, "C→S QualityTile");
            break;
        default:
            goto unknown;
        }
    }

    if (!c.trunc && c.off < c.end) {
        /* Restbytes (neue Felder?) als Rohdaten zeigen statt zu raten. */
        proto_tree_add_item(t, hf_raw, tvb, c.off, c.end - c.off, ENC_NA);
    }
    return c.end;

unknown: {
    expert_add_info_format(pinfo, ti, &ei_variant,
        "Unbekannte %s-Variante %" PRIu64, dir, v);
    proto_tree_add_item(t, hf_raw, tvb, 4, c.end - 4, ENC_NA);
    col_add_fstr(pinfo->cinfo, COL_INFO, "%s ?%" PRIu64 " (%dB)", dir, v, c.end - 4);
    return c.end;
}

short_frame:
    expert_add_info(pinfo, ti, &ei_trunc);
    col_add_fstr(pinfo->cinfo, COL_INFO, "%s (Fragment, %dB)", dir, c.end);
    return c.end;
}

static int
dissect_lbw(tvbuff_t *tvb, packet_info *pinfo, proto_tree *tree, void *data)
{
    tcp_dissect_pdus(tvb, pinfo, tree, true, 4, get_lbw_len, dissect_msg, data);
    return tvb_captured_length(tvb);
}

static void
proto_register_lbw(void)
{
    static hf_register_info hf[] = {
        { &hf_len, { "Rahmenlänge", "lbw.len", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_msg, { "Nachricht", "lbw.msg", FT_STRING, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_raw, { "Rohdaten", "lbw.raw", FT_BYTES, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_tid, { "Text-ID", "lbw.text.id", FT_UINT64, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_tx, { "Text-X", "lbw.text.x", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_ty, { "Text-Y", "lbw.text.y", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_tw, { "Text-Breite", "lbw.text.w", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_th, { "Text-Höhe", "lbw.text.h", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_fg, { "Vordergrund", "lbw.text.fg", FT_BYTES, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_bg, { "Hintergrund", "lbw.text.bg", FT_BYTES, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_str, { "Text", "lbw.text.str", FT_STRING, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_tilx, { "Kachel-X", "lbw.tile.x", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_tily, { "Kachel-Y", "lbw.tile.y", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_tilw, { "Kachel-Breite", "lbw.tile.w", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_tilh, { "Kachel-Höhe", "lbw.tile.h", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_tillen, { "Kachel-Bytes", "lbw.tile.len", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_tildata, { "AV1-Daten", "lbw.tile.data", FT_BYTES, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_av1_obus, { "AV1-OBUs", "lbw.av1.obus", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_av1_obu, { "AV1-OBU", "lbw.av1.obu", FT_STRING, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_av1_profile, { "AV1-Profil", "lbw.av1.profile", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_av1_still, { "AV1-Still-Picture", "lbw.av1.still", FT_BOOLEAN, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_av1_level, { "AV1-Level", "lbw.av1.level", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_av1_w, { "AV1-Breite", "lbw.av1.width", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_av1_h, { "AV1-Höhe", "lbw.av1.height", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_rmid, { "Entferne-ID", "lbw.remove.id", FT_UINT64, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_ver, { "Protokollversion", "lbw.version", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_mx, { "Maus-X", "lbw.mouse.x", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_my, { "Maus-Y", "lbw.mouse.y", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_btn, { "Taste", "lbw.button", FT_UINT8, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_down, { "Gedrückt", "lbw.down", FT_BOOLEAN, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_itext, { "Eingabetext", "lbw.input.text", FT_STRING, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_key, { "Sondertaste", "lbw.key", FT_STRING, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_nonce_len, { "Nonce-Länge", "lbw.auth.nonce_len", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_mac_len, { "MAC-Länge", "lbw.auth.mac_len", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
    };
    static int *ett[] = { &ett_lbw, &ett_av1, &ett_obu };
    static ei_register_info ei[] = {
        { &ei_trunc, { "lbw.truncated", PI_MALFORMED, PI_ERROR, "Nachricht unvollständig", EXPFILL } },
        { &ei_variant, { "lbw.unknown_variant", PI_PROTOCOL, PI_WARN, "Unbekannte Variante", EXPFILL } },
        { &ei_av1, { "lbw.av1.truncated", PI_MALFORMED, PI_WARN, "AV1-OBUs abgebrochen", EXPFILL } },
    };
    module_t *mod;
    expert_module_t *exp;

    proto_lbw = proto_register_protocol("Low-Bandwidth-Desktop", "LBW", "lbw");
    proto_register_field_array(proto_lbw, hf, array_length(hf));
    proto_register_subtree_array(ett, array_length(ett));
    exp = expert_register_protocol(proto_lbw);
    expert_register_field_array(exp, ei, array_length(ei));

    lbw_handle = register_dissector("lbw", dissect_lbw, proto_lbw);
    mod = prefs_register_protocol(proto_lbw, proto_reg_handoff_lbw);
    prefs_register_uint_preference(mod, "port", "Server-Port",
        "TCP-Port des LBW-Servers (Richtung + Zerlegung)", 10, &g_port);
}

static void
proto_reg_handoff_lbw(void)
{
    static guint last_port = 0;
    static bool init = false;
    if (!init) {
        init = true;
    } else if (last_port != 0) {
        dissector_delete_uint("tcp.port", last_port, lbw_handle);
    }
    last_port = g_port;
    dissector_add_uint("tcp.port", g_port, lbw_handle);
}
