/* packet-lbw.c — Wireshark-Dissector für das LBW-Protokoll (v3).
 *
 * Rahmen: [u32 LE Länge][bincode-Body] über TCP (Default-Port 7878, als
 * Präferenz änderbar; alternativ „Decode As → TCP → lbw"). Richtung:
 * Quellport == Server-Port → Server→Client, sonst Client→Server (Fallback:
 * niedrigerer Port ist der Server).
 *
 * bincode 2.0 (Standard-Config): Varints (<251 direkt, sonst 251+u16LE /
 * 252+u32LE / 253+u64LE), Enum-Variante als u32-Varint, String/Vec als
 * u64-Varint-Länge + Bytes, u8/bool als Einzelbytes. Byte-Vektoren s.
 * gen_sample.py (aus `encode_msg` verifiziert).
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
static int hf_tilx, hf_tily, hf_tillen, hf_tildata;
static int hf_rmid;
static int hf_ver, hf_mx, hf_my, hf_btn, hf_down, hf_itext, hf_key;
static int ett_lbw;
static expert_field ei_trunc, ei_variant;

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
        case 0:
            proto_tree_add_string(t, hf_msg, tvb, 4, 1, "Hello");
            col_add_str(pinfo->cinfo, COL_INFO, "S→C Hello");
            break;
        case 1:
            proto_tree_add_string(t, hf_msg, tvb, 4, 1, "ClearText");
            col_add_str(pinfo->cinfo, COL_INFO, "S→C ClearText");
            break;
        case 2: {
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
                "AddText", "AddText id=%" PRIu64 " \"%s\"", id, s);
            col_add_fstr(pinfo->cinfo, COL_INFO, "S→C AddText id=%" PRIu64 " \"%s\"", id, s);
            break;
        }
        case 3: {
            int at = 4;
            uint64_t x = cur_varint(&c), y = cur_varint(&c), n = cur_varint(&c);
            int dat;
            if (c.trunc)
                goto short_frame;
            dat = cur_bytes(&c, (unsigned)n);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_uint(t, hf_tilx, tvb, at, c.off - at, (uint32_t)x);
            proto_tree_add_uint(t, hf_tily, tvb, at, c.off - at, (uint32_t)y);
            proto_tree_add_uint(t, hf_tillen, tvb, at, c.off - at, (uint32_t)n);
            proto_tree_add_item(t, hf_tildata, tvb, dat, (int)n, ENC_NA);
            proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                "Tile", "Tile @%" PRIu64 ",%" PRIu64 " %" PRIu64 "B", x, y, n);
            col_add_fstr(pinfo->cinfo, COL_INFO,
                "S→C Tile @%" PRIu64 ",%" PRIu64 " %" PRIu64 "B", x, y, n);
            break;
        }
        case 4: {
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
        default:
            goto unknown;
        }
    } else {
        switch (v) {
        case 0: {
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
        case 1: {
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
        case 2: {
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
        case 3: {
            int at = 4;
            const char *s = add_string(&c, t, hf_itext);
            if (c.trunc)
                goto short_frame;
            proto_tree_add_string_format(t, hf_msg, tvb, at, c.off - at,
                "Text", "Text \"%s\"", s);
            col_add_fstr(pinfo->cinfo, COL_INFO, "C→S Text \"%s\"", s);
            break;
        }
        case 4: {
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
        { &hf_tillen, { "Kachel-Bytes", "lbw.tile.len", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_tildata, { "AV1-Daten", "lbw.tile.data", FT_BYTES, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_rmid, { "Entferne-ID", "lbw.remove.id", FT_UINT64, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_ver, { "Protokollversion", "lbw.version", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_mx, { "Maus-X", "lbw.mouse.x", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_my, { "Maus-Y", "lbw.mouse.y", FT_UINT32, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_btn, { "Taste", "lbw.button", FT_UINT8, BASE_DEC, NULL, 0, NULL, HFILL } },
        { &hf_down, { "Gedrückt", "lbw.down", FT_BOOLEAN, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_itext, { "Eingabetext", "lbw.input.text", FT_STRING, BASE_NONE, NULL, 0, NULL, HFILL } },
        { &hf_key, { "Sondertaste", "lbw.key", FT_STRING, BASE_NONE, NULL, 0, NULL, HFILL } },
    };
    static int *ett[] = { &ett_lbw };
    static ei_register_info ei[] = {
        { &ei_trunc, { "lbw.truncated", PI_MALFORMED, PI_ERROR, "Nachricht unvollständig", EXPFILL } },
        { &ei_variant, { "lbw.unknown_variant", PI_PROTOCOL, PI_WARN, "Unbekannte Variante", EXPFILL } },
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
