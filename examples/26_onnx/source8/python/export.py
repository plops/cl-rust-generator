"""Export von Salesforce/GPA-GUI-Detector (YOLO11m) nach ONNX-Varianten.

Aufruf (aus source8/python/):  uv run python export.py [--models ../models]

Erzeugt in models/:
  gpa_{640,384x640}_{fp32,fp16,int8}.onnx   Modellvarianten
  example_input.ppm                         Test-Fixture für Rust
  reference_{640,384x640}.tsv               Ultralytics-Boxen (Parität)
  export_report.tsv                         Größe + Übereinstimmung vs. fp32
"""

import argparse
import glob
import shutil
from pathlib import Path

import numpy as np
import onnx
import onnxruntime as ort
import torch
import torchvision
from onnxruntime.quantization import CalibrationDataReader, QuantFormat, QuantType, quantize_static
from onnxruntime.quantization.shape_inference import quant_pre_process
from onnxruntime.transformers import float16
from PIL import Image
from ultralytics import YOLO
from ultralytics.data.augment import LetterBox

SHAPES = {"640": (640, 640), "384x640": (384, 640)}  # tag -> (H, W)
CONF, IOU = 0.05, 0.7  # Model-Card-Parameter
CAL_CROPS = 6  # Zusatz-Ausschnitte pro Kalibrierbild


def load_rgb(path: str) -> np.ndarray:
    return np.asarray(Image.open(path).convert("RGB"))


def to_tensor(rgb: np.ndarray, hw: tuple[int, int]) -> np.ndarray:
    """Ultralytics-LetterBox (auto=False) → NCHW float32 /255."""
    bgr = rgb[:, :, ::-1]
    img = LetterBox(new_shape=hw, auto=False)(image=bgr)[:, :, ::-1]
    return np.ascontiguousarray(img.transpose(2, 0, 1)[None], dtype=np.float32) / 255.0


def calibration_images(models: Path) -> list[np.ndarray]:
    """Screens + Beispielbild, plus zufällige Ausschnitte (andere Skalen)."""
    rng = np.random.default_rng(0)
    paths = sorted(glob.glob(str(models / "screens" / "*.ppm"))) + [str(models / "example_input.png")]
    out = []
    for p in paths:
        img = load_rgb(p)
        out.append(img)
        h, w = img.shape[:2]
        for _ in range(CAL_CROPS):
            cw = int(w * rng.uniform(0.3, 0.8))
            ch = int(cw * h / w)
            x, y = rng.integers(0, w - cw + 1), rng.integers(0, h - ch + 1)
            out.append(img[y : y + ch, x : x + cw])
    return out


class Reader(CalibrationDataReader):
    def __init__(self, imgs: list[np.ndarray], hw: tuple[int, int]):
        self.it = iter([{"images": to_tensor(i, hw)} for i in imgs])

    def get_next(self):
        return next(self.it, None)


def export_fp32(pt: Path, hw: tuple[int, int], dst: Path) -> None:
    f = YOLO(str(pt)).export(format="onnx", imgsz=list(hw), simplify=True, nms=False, dynamic=False, device="cpu")
    shutil.move(f, dst)


def export_fp16(src: Path, dst: Path) -> None:
    m = float16.convert_float_to_float16(onnx.load(str(src)), keep_io_types=True)
    onnx.save(m, str(dst))


def export_int8(src: Path, dst: Path, imgs: list[np.ndarray], hw: tuple[int, int]) -> None:
    pre = dst.with_suffix(".pre.onnx")
    quant_pre_process(str(src), str(pre))
    # Nur gewichtete Ops quantisieren: Der Decode im Detect-Head mischt
    # Box-Pixel (0..640) und Scores (0..1) — eine INT8-Skala dafür rundet
    # alle Scores auf 0 (gleiche Wahl wie Ultralytics onnx_int8_quantize).
    graph = onnx.load(str(pre)).graph
    exclude = [n.name for n in graph.node if n.op_type not in {"Conv", "MatMul"}]
    quantize_static(
        str(pre),
        str(dst),
        Reader(imgs, hw),
        quant_format=QuantFormat.QDQ,
        per_channel=True,
        activation_type=QuantType.QUInt8,
        weight_type=QuantType.QInt8,
        nodes_to_exclude=exclude,
    )
    pre.unlink()


def detect(sess: ort.InferenceSession, x: np.ndarray) -> np.ndarray:
    """Roh-Output [1,5,N] → NMS-Boxen (x1,y1,x2,y2,score) im Input-Raum."""
    p = sess.run(None, {"images": x})[0][0].T
    p = p[p[:, 4] >= CONF]
    xyxy = np.concatenate([p[:, :2] - p[:, 2:4] / 2, p[:, :2] + p[:, 2:4] / 2], 1)
    keep = torchvision.ops.nms(torch.from_numpy(xyxy), torch.from_numpy(p[:, 4]), IOU).numpy()[:300]
    return np.concatenate([xyxy[keep], p[keep, 4:5]], 1)


def iou(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    return torchvision.ops.box_iou(torch.from_numpy(a[:, :4]), torch.from_numpy(b[:, :4])).numpy()


def agreement(ref: np.ndarray, got: np.ndarray, thr: float = 0.25) -> tuple[float, float]:
    """Recall/Präzision bei conf≥thr, IoU≥0.5 (fp32 = Referenz)."""
    r, g = ref[ref[:, 4] >= thr], got[got[:, 4] >= thr]
    if len(r) == 0 or len(g) == 0:
        return float(len(r) == len(g)), float(len(r) == len(g))
    m = iou(r, g) >= 0.5
    return m.any(1).mean(), m.any(0).mean()


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--models", default="../models")
    models = Path(ap.parse_args().models).resolve()
    pt = models / "model.pt"
    example = load_rgb(str(models / "example_input.png"))
    Image.fromarray(example).save(models / "example_input.ppm")

    cal = calibration_images(models)
    evals = [load_rgb(p) for p in sorted(glob.glob(str(models / "screens" / "*.ppm")))] + [example]
    print(f"kalibrierung: {len(cal)} bilder, evaluierung: {len(evals)} bilder")

    report = ["variant\tbytes\trecall_vs_fp32\tprecision_vs_fp32\tboxes_example"]
    for tag, hw in SHAPES.items():
        f32, f16, i8 = (models / f"gpa_{tag}_{p}.onnx" for p in ("fp32", "fp16", "int8"))
        export_fp32(pt, hw, f32)
        export_fp16(f32, f16)
        export_int8(f32, i8, cal, hw)

        # Parität-Referenz: Ultralytics-Predictor auf dem fp32-ONNX.
        res = YOLO(str(f32), task="detect").predict(
            str(models / "example_input.png"), conf=CONF, iou=IOU, imgsz=list(hw), verbose=False
        )[0]
        ref = np.concatenate([res.boxes.xyxy.numpy(), res.boxes.conf.numpy()[:, None]], 1)
        np.savetxt(models / f"reference_{tag}.tsv", ref, fmt="%.2f", delimiter="\t")

        xs = [to_tensor(i, hw) for i in evals]
        base = ort.InferenceSession(str(f32), providers=["CPUExecutionProvider"])
        base_dets = [detect(base, x) for x in xs]
        for f in (f32, f16, i8):
            s = ort.InferenceSession(str(f), providers=["CPUExecutionProvider"])
            out = s.get_outputs()[0].shape
            assert out[:2] == [1, 5], out
            dets = [detect(s, x) for x in xs]
            rp = np.array([agreement(b, d) for b, d in zip(base_dets, dets)]).mean(0)
            row = f"{f.stem}\t{f.stat().st_size}\t{rp[0]:.3f}\t{rp[1]:.3f}\t{len(dets[-1])}"
            report.append(row)
            print(row)
    (models / "export_report.tsv").write_text("\n".join(report) + "\n")


if __name__ == "__main__":
    main()
