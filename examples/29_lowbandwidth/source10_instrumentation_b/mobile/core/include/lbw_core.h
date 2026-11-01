/*
 * lbw_core.h — C-ABI des gemeinsamen Mobil-Kerns (mobile/core/src/04_ffi.rs).
 * Genutzt von der iOS-App (Swift-Bridging-Header) und vom C-Header-Test
 * (mobile/core/tests/c/header_check.c). Alle Strings: nullterminiertes UTF-8.
 * Ausgabefunktionen liefern die BENÖTIGTE Länge; ist sie größer als `cap`,
 * mit größerem Puffer erneut aufrufen (out = NULL, cap = 0 fragt nur).
 */
#ifndef LBW_CORE_H
#define LBW_CORE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Rückgabe-Bits von lbw_poll. */
#define LBW_FRAME 1 /* Canvas in den Puffer kopiert */
#define LBW_TEXT 2  /* Texte geändert → lbw_texts */
#define LBW_UP 8    /* Verbindung steht */

/* Canvas: immer 1280×720 RGBA8 (Protokoll v3). */
#define LBW_WIDTH 1280
#define LBW_HEIGHT 720

typedef struct LbwEngine LbwEngine;

/* json: {"hops":["user@host:port",…],"identities":["-----BEGIN OPENSSH…",…],
 *        "passphrases":[…],"pins":["SHA256:…" | "",…],
 *        "target_sshd_host":"127.0.0.1","target_sshd_port":22,"lbw_port":7878}
 * oder ohne SSH: {"direct":"host:port"}. NULL bei ungültiger Konfiguration. */
LbwEngine *lbw_new(const char *json);
void lbw_free(LbwEngine *h);

int32_t lbw_poll(const LbwEngine *h, uint8_t *rgba, size_t len);
/* Blob: u32 n, je Text: u64 id | u16 x y w h | u8 fg[3] bg[3] | u16 len | UTF-8 (LE) */
size_t lbw_texts(const LbwEngine *h, uint8_t *out, size_t cap);
size_t lbw_status(const LbwEngine *h, uint8_t *out, size_t cap);
/* Hostkey-Fingerprints pro Hop, durch '\n' getrennt (TOFU speichern). */
size_t lbw_pins(const LbwEngine *h, uint8_t *out, size_t cap);

void lbw_key(const LbwEngine *h, const char *name); /* "Enter","Backspace",… */
void lbw_text(const LbwEngine *h, const char *utf8);
void lbw_mouse(const LbwEngine *h, int32_t x, int32_t y);
void lbw_button(const LbwEngine *h, int32_t button, bool down); /* 1 L, 2 M, 3 R */
void lbw_wheel(const LbwEngine *h, int32_t dy);

#ifdef __cplusplus
}
#endif
#endif
