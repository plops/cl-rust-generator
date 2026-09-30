#!/bin/bash
# fetch_models.sh — lädt PaddleOCR-ONNX-Modelle (Detektion + Erkennung je
# Schrift) von HuggingFace (Apache-2.0) mit gepinntem Commit + SHA256.
#
# Aufruf (aus source9/):  ./scripts/fetch_models.sh [ZIEL-VERZEICHNIS]
# Ohne Argument: source9/models/<repo>/inference.{onnx,yml}.
# Bereits vorhandene Dateien mit korrekter Prüfsumme werden übersprungen.
set -euo pipefail

SRC_DIR="$(cd "$(dirname "$0")/.." && pwd)"
OUT="${1:-$SRC_DIR/models}"

# repo  commit  sha256(inference.onnx)  sha256(inference.yml)
MODELS=(
  "PP-OCRv6_small_det_onnx 28fe5895c24fd108c19eb3e8479f4ab385fbfc62 d73e0058b7a8086bbd57f3d10b8bcd4ff95363f67e06e2762b5e814fe9c9410e 193f435274bf9f0b5f71a929bbfbcf148282df7e633b34e7c373e8f44741b516"
  "PP-OCRv6_small_rec_onnx b8f84f0b80c529de40b4fbb3544b84fa7233a513 5435fd747c9e0efe15a96d0b378d5bd157e9492ed8fd80edf08f30d02fa24634 ab078671bb49f06228eadccd34f1bb501e157f7a047095ffb943ba81512c77d1"
  "latin_PP-OCRv5_mobile_rec_onnx 89d3a50e2c27e2e7cceeab0e944c25c807d5db4f 7888113072263cb471b93f66dd5e2ad70548dc526fa1ace760d0d973dd121498 0bbe984570f597af3638e50bdf2e8276f3ab26a61966096538b3b0d1849f5c84"
  "eslav_PP-OCRv5_mobile_rec_onnx 9a32171fc5718746875e1a261818884517975013 b3018ef2b09a0250b6e0c8e871c927098363e5fd4df890cc68e8358eb0aaf1bd 025039bac23eb4a308efcefa4d58eab3af440767815c6ba6938468bf6353ee5a"
  "el_PP-OCRv5_mobile_rec_onnx 8152b89d2ee0e1d4c92decab75fe75e5e3d836b6 2acf17fcaea2bc81b878e311e6263b8885f48bb03796f75f9f30ed3242bbaa6d 17d85b2fe2d2f24cd4ab07bcbc33e0c126859b956ced36e281dc65e2d0c1f0bf"
  "korean_PP-OCRv5_mobile_rec_onnx 5c6f574b8e2230adf4287b33e736d71b9fabd28e 92f0b7785e64fc9090106a241cf4c1eb97472824558272751b88a2a4476d3a08 f757fa1c40e99edcf27e9cce879b93eb2a51fa46f5ef39095689b8c37dd75998"
  "th_PP-OCRv5_mobile_rec_onnx 1d4adbbafb1034a2fd6618498575b81ea7b69f69 27618be66018f8598ac0a526a593f9f1cebf794e7eded93428e8fb016e537f5f f6ba7fefc38ca1ff398ddafa75d67d16e0b3757c4e6c833adffee98a981766c9"
  "arabic_PP-OCRv5_mobile_rec_onnx 14aaedcd75825982689ecf5cd64ab33ee083215a 799113ebf267fbe742deb99eb36e8d42c9ddc5291ceacf92add41b4d52a59110 21368419e6c016c31db55d316d59e11c128e1913e6e6fe10287084710043d3a6"
  "devanagari_PP-OCRv5_mobile_rec_onnx 251aec19e36739540d35e2cc943f6aa7503b98e5 cb789212ce96c69d3e74728ae4309d179281d68cb3945d0616b67cafab41c986 9bd172dd26440c8ce94d1cde5d5baea6aefdc7cf3c5c8492e0beedef656d4e54"
  "ta_PP-OCRv5_mobile_rec_onnx 0db89798af0d5218fad9ea7ace0557526e9077f1 c6d2b682d2a0ea4cb1fccdba295976f93fd439964d16cdc666cadef531accbee 88a28f5a1bb30cabe38a0985cb5e6619fa4f0c7c78e57a08274674228c5219a6"
)

fetch() { # url sha ziel
  local url="$1" sha="$2" dest="$3"
  if [ -f "$dest" ] && echo "$sha  $dest" | sha256sum -c --status -; then
    echo "ok (cached): $dest"
    return 0
  fi
  echo "download: $url"
  curl -sSL --fail --retry 3 -o "$dest.tmp" "$url"
  if ! echo "$sha  $dest.tmp" | sha256sum -c --status -; then
    echo "FEHLER: Prüfsumme falsch für $dest" >&2
    rm -f "$dest.tmp"
    return 1
  fi
  mv "$dest.tmp" "$dest"
  echo "ok (verified): $dest"
}

for row in "${MODELS[@]}"; do
  read -r repo commit onnx_sha yml_sha <<<"$row"
  mkdir -p "$OUT/$repo"
  base="https://huggingface.co/PaddlePaddle/$repo/resolve/$commit"
  fetch "$base/inference.onnx" "$onnx_sha" "$OUT/$repo/inference.onnx"
  fetch "$base/inference.yml" "$yml_sha" "$OUT/$repo/inference.yml"
done

# Schrift (Laufzeit-Dep): Suchliste wie in src/06_render.rs.
for p in /usr/share/fonts/opentype/unifont/unifont.otf \
         /usr/share/fonts/unifont/unifont.otf \
         /usr/share/fonts/truetype/unifont/unifont.ttf; do
  if [ -f "$p" ]; then
    echo "ok (font): $p"
    exit 0
  fi
done
echo "font missing: GNU Unifont nicht gefunden." >&2
if command -v apt-get >/dev/null && [ "$(id -u)" = "0" ]; then
  apt-get install -y fonts-unifont
else
  echo "bitte installieren: apt-get install fonts-unifont" >&2
  exit 1
fi
