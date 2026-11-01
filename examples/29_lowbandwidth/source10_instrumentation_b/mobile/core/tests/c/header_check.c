/* header_check.c — prüft, dass include/lbw_core.h zur Rust-C-ABI passt:
 * wird gegen liblbw_mobile_core (Host-Build) gelinkt und spricht einen
 * echten lbw-server direkt an (Port als Argument). Von
 * tests/c_header.rs gebaut und gestartet. Exit 0 = OK. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "../../include/lbw_core.h"

int main(int argc, char **argv) {
    if (argc < 2) return 2;
    if (lbw_new("kein json") != NULL) { puts("FEHLER: kaputte Konfig akzeptiert"); return 1; }
    char cfg[128];
    snprintf(cfg, sizeof cfg, "{\"direct\":\"127.0.0.1:%s\"}", argv[1]);
    LbwEngine *h = lbw_new(cfg);
    if (!h) { puts("FEHLER: lbw_new"); return 1; }
    static uint8_t canvas[LBW_WIDTH * LBW_HEIGHT * 4];
    int up = 0, frame = 0, text = 0;
    for (int i = 0; i < 500 && !(up && frame && text); i++) {
        int32_t f = lbw_poll(h, canvas, sizeof canvas);
        up |= (f & LBW_UP) != 0;
        frame |= (f & LBW_FRAME) != 0 && canvas[0] > 150;
        size_t n = lbw_texts(h, NULL, 0);
        uint8_t *b = malloc(n);
        lbw_texts(h, b, n);
        text |= n >= 4 && b[0] > 0;
        free(b);
        usleep(20000);
    }
    char st[256] = {0};
    size_t n = lbw_status(h, (uint8_t *)st, sizeof st - 1);
    st[n < sizeof st - 1 ? n : 0] = 0;
    lbw_text(h, "c");
    lbw_key(h, "Enter");
    lbw_mouse(h, 1, 2);
    lbw_button(h, 1, true);
    lbw_wheel(h, -1);
    usleep(300000);
    lbw_free(h);
    printf("c-header: up=%d frame=%d text=%d status=%s\n", up, frame, text, st);
    return up && frame && text ? 0 : 1;
}
