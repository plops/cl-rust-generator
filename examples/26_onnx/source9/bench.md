# Benchmark-Zahlen (T9-Nachweis, Release)

Befehle (aus `source9/`, Modell- und Korpus-Assets per `scripts/` geladen):

```bash
cargo build --release
for g in pangram words markov chars; do
  ./target/release/unicode_ocr bench --gen $g --lang all \
    --samples 5 --lines 4 --seed 1
done
```

Je Zelle: 5 Samples × 4 Zeilen, 32 px, Modell `auto`. CER = Fehler/Zeichen
(Korpus), Exakt = Anteil fehlerfreier Zeilen, Recall = gefundene GT-Zeilen.

## pangram

| Lang | Modell | CER | Exakt | Recall | FP | ms/Sample |
|---|---|---|---|---|---|---|
| ar | arabic_PP-OCRv5_mobile_rec_onnx | 0.748 | 0.0% | 100.0% | 0 | 118 |
| de | PP-OCRv6_small_rec_onnx | 0.033 | 40.0% | 100.0% | 0 | 140 |
| el | el_PP-OCRv5_mobile_rec_onnx | 0.020 | 50.0% | 100.0% | 0 | 143 |
| en | PP-OCRv6_small_rec_onnx | 0.014 | 75.0% | 100.0% | 0 | 94 |
| es | PP-OCRv6_small_rec_onnx | 0.021 | 75.0% | 100.0% | 0 | 98 |
| fr | PP-OCRv6_small_rec_onnx | 0.054 | 35.0% | 100.0% | 0 | 107 |
| hi | devanagari_PP-OCRv5_mobile_rec_onnx | 0.193 | 5.0% | 100.0% | 0 | 108 |
| ja | PP-OCRv6_small_rec_onnx | 0.000 | 100.0% | 100.0% | 0 | 115 |
| ko | korean_PP-OCRv5_mobile_rec_onnx | 0.101 | 10.0% | 100.0% | 0 | 153 |
| pl | latin_PP-OCRv5_mobile_rec_onnx | 0.092 | 30.0% | 100.0% | 0 | 126 |
| ru | eslav_PP-OCRv5_mobile_rec_onnx | 0.000 | 100.0% | 100.0% | 0 | 148 |
| ta | ta_PP-OCRv5_mobile_rec_onnx | 0.146 | 45.0% | 100.0% | 0 | 134 |
| th | th_PP-OCRv5_mobile_rec_onnx | 0.123 | 5.0% | 100.0% | 0 | 121 |
| uk | eslav_PP-OCRv5_mobile_rec_onnx | 0.066 | 0.0% | 100.0% | 0 | 115 |
| zh | PP-OCRv6_small_rec_onnx | 0.007 | 90.0% | 100.0% | 0 | 106 |

## words

| Lang | Modell | CER | Exakt | Recall | FP | ms/Sample |
|---|---|---|---|---|---|---|
| ar | arabic_PP-OCRv5_mobile_rec_onnx | 0.322 | 0.0% | 100.0% | 0 | 151 |
| de | PP-OCRv6_small_rec_onnx | 0.000 | 100.0% | 100.0% | 0 | 131 |
| el | el_PP-OCRv5_mobile_rec_onnx | 0.009 | 75.0% | 100.0% | 0 | 151 |
| en | PP-OCRv6_small_rec_onnx | 0.000 | 100.0% | 100.0% | 0 | 112 |
| es | PP-OCRv6_small_rec_onnx | 0.011 | 75.0% | 100.0% | 0 | 111 |
| fr | PP-OCRv6_small_rec_onnx | 0.002 | 95.0% | 100.0% | 0 | 107 |
| hi | devanagari_PP-OCRv5_mobile_rec_onnx | 0.176 | 10.0% | 100.0% | 0 | 138 |
| ja | PP-OCRv6_small_rec_onnx | 0.000 | 100.0% | 100.0% | 0 | 106 |
| ko | korean_PP-OCRv5_mobile_rec_onnx | 0.040 | 50.0% | 100.0% | 0 | 158 |
| pl | latin_PP-OCRv5_mobile_rec_onnx | 0.019 | 60.0% | 100.0% | 0 | 150 |
| ru | eslav_PP-OCRv5_mobile_rec_onnx | 0.013 | 75.0% | 100.0% | 0 | 149 |
| ta | ta_PP-OCRv5_mobile_rec_onnx | 0.145 | 0.0% | 100.0% | 0 | 145 |
| th | th_PP-OCRv5_mobile_rec_onnx | 0.079 | 0.0% | 100.0% | 0 | 145 |
| uk | eslav_PP-OCRv5_mobile_rec_onnx | 0.026 | 60.0% | 100.0% | 0 | 144 |
| zh | PP-OCRv6_small_rec_onnx | 0.000 | 100.0% | 100.0% | 0 | 119 |

## markov

| Lang | Modell | CER | Exakt | Recall | FP | ms/Sample |
|---|---|---|---|---|---|---|
| ar | arabic_PP-OCRv5_mobile_rec_onnx | 0.265 | 15.0% | 100.0% | 0 | 152 |
| de | PP-OCRv6_small_rec_onnx | 0.002 | 95.0% | 100.0% | 0 | 123 |
| el | el_PP-OCRv5_mobile_rec_onnx | 0.007 | 85.0% | 100.0% | 0 | 149 |
| en | PP-OCRv6_small_rec_onnx | 0.005 | 85.0% | 100.0% | 0 | 114 |
| es | PP-OCRv6_small_rec_onnx | 0.005 | 85.0% | 100.0% | 0 | 114 |
| fr | PP-OCRv6_small_rec_onnx | 0.006 | 85.0% | 100.0% | 0 | 114 |
| hi | devanagari_PP-OCRv5_mobile_rec_onnx | 0.194 | 0.0% | 100.0% | 0 | 141 |
| ja | PP-OCRv6_small_rec_onnx | 0.011 | 85.0% | 95.0% | 0 | 115 |
| ko | korean_PP-OCRv5_mobile_rec_onnx | 0.071 | 20.0% | 100.0% | 0 | 167 |
| pl | latin_PP-OCRv5_mobile_rec_onnx | 0.035 | 40.0% | 100.0% | 0 | 151 |
| ru | eslav_PP-OCRv5_mobile_rec_onnx | 0.029 | 75.0% | 100.0% | 0 | 144 |
| ta | ta_PP-OCRv5_mobile_rec_onnx | 0.113 | 5.0% | 100.0% | 0 | 144 |
| th | th_PP-OCRv5_mobile_rec_onnx | 0.091 | 20.0% | 100.0% | 0 | 127 |
| uk | eslav_PP-OCRv5_mobile_rec_onnx | 0.026 | 40.0% | 100.0% | 0 | 137 |
| zh | PP-OCRv6_small_rec_onnx | 0.003 | 95.0% | 100.0% | 0 | 117 |

## chars

| Lang | Modell | CER | Exakt | Recall | FP | ms/Sample |
|---|---|---|---|---|---|---|
| ar | arabic_PP-OCRv5_mobile_rec_onnx | 0.684 | 0.0% | 100.0% | 0 | 159 |
| de | PP-OCRv6_small_rec_onnx | 0.651 | 0.0% | 100.0% | 0 | 126 |
| el | el_PP-OCRv5_mobile_rec_onnx | 0.553 | 0.0% | 100.0% | 0 | 151 |
| en | PP-OCRv6_small_rec_onnx | 0.663 | 0.0% | 100.0% | 0 | 107 |
| es | PP-OCRv6_small_rec_onnx | 0.654 | 0.0% | 100.0% | 0 | 107 |
| fr | PP-OCRv6_small_rec_onnx | 0.640 | 0.0% | 100.0% | 0 | 107 |
| hi | devanagari_PP-OCRv5_mobile_rec_onnx | 0.617 | 0.0% | 100.0% | 0 | 146 |
| ja | PP-OCRv6_small_rec_onnx | 0.460 | 0.0% | 100.0% | 0 | 111 |
| ko | korean_PP-OCRv5_mobile_rec_onnx | 1.084 | 0.0% | 100.0% | 0 | 175 |
| pl | latin_PP-OCRv5_mobile_rec_onnx | 0.624 | 0.0% | 100.0% | 0 | 149 |
| ru | eslav_PP-OCRv5_mobile_rec_onnx | 0.111 | 5.0% | 100.0% | 0 | 156 |
| ta | ta_PP-OCRv5_mobile_rec_onnx | 0.634 | 0.0% | 100.0% | 0 | 152 |
| th | th_PP-OCRv5_mobile_rec_onnx | 0.482 | 0.0% | 100.0% | 0 | 144 |
| uk | eslav_PP-OCRv5_mobile_rec_onnx | 0.141 | 0.0% | 100.0% | 0 | 142 |
| zh | PP-OCRv6_small_rec_onnx | 0.415 | 0.0% | 100.0% | 0 | 110 |

## Erste Lesung (Details im Walkthrough, T13)

- Latein/CJK auf Realtext (pangram/words/markov): CER 0–5 %, Detektion
  Recall 100 %, keine FP. `ja markov` verfehlt eine Zeile (95 %).
- Arabisch ohne Shaping: 27–75 % (Pangramm am schwersten).
- Hindi/Tamil/Thai/Koreanisch: 4–20 % auf Realtext.
- `chars` (gleichverteiltes Kauderwelsch): überall 40–100 % — seltene
  Diakritika (Ą→A, §→s, Ĝ→G) kennt das Modell nur vom Hörensagen.
  Kyrillisch (kleiner, distinkter Zeichensatz) bleibt bei 11–14 %.
- Tempo: ~100–175 ms/Sample (4 Zeilen), Detektion dominiert.
