(load (merge-pathnames "00_util.lisp" *load-pathname*))

(in-package :cl-rust-generator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case *readtable*) :invert))

;;;; common.lisp --- lbw-common: types, framing, yuv, lib.rs, manifest.

(defun common-cargo-toml ()
  "[package]
name = \"lbw-common\"
version.workspace = true
edition.workspace = true
authors.workspace = true
license.workspace = true

[dependencies]
serde = { version = \"1.0\", features = [\"derive\"] }
bincode = { version = \"2.0\", features = [\"serde\"] }
")

(defun common-types-rs ()
  `(do0
    ,(doc "`01_types` — alle Nachrichtentypen des MVP-Protokolls plus Konstanten."
          ""
          "Serialisierung per `serde` + `bincode` (siehe `02_framing`); deshalb hier"
          "keine einzige Zeile Hand-Codec. Koordinaten sind `u16` im Capture-Raum"
          "(MVP: `size`×`size`, Default 640). Farben sind RGB8.")
    ,*blank*
    (use (serde (curly Deserialize Serialize)))
    ,*blank*
    "/// Protokollversion (Client-`Hello`; Server lehnt Abweichungen ab)."
    (space "pub const PROTO_VERSION: u16 =" "1;")
    "/// Default-TCP-Port."
    (space "pub const DEFAULT_PORT: u16 =" "7878;")
    "/// Feste Kantenlänge des quadratischen Bildes (MVP: immer 640×640)."
    (space "pub const SIZE: u32 =" "640;")
    "/// Max. Nachrichtengröße in Byte (Schutz vor OOM bei korrupten Längen)."
    (space "pub const MAX_MSG: usize =" (* 8 1024 1024) ";")
    ,*blank*
    "/// Achsenparalleles Rechteck."
    (attr "derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)"
      ,(pub_ '(defstruct0 Rect
                ("pub x" u16)
                ("pub y" u16)
                ("pub w" u16)
                ("pub h" u16))))
    ,*blank*
    (impl Rect
      (attr "must_use"
        (space "pub const fn new(x: u16, y: u16, w: u16, h: u16) -> Self"
          (progn (make-instance Self x y w h))))
      ,*blank*
      "/// Rechte Kante (exklusiv)."
      (attr "must_use"
        ,(pub_ '(defun x2 ("&self")
                  (declare (values u16))
                  (+ (dot self x) (dot self w)))))
      ,*blank*
      "/// Untere Kante (exklusiv)."
      (attr "must_use"
        ,(pub_ '(defun y2 ("&self")
                  (declare (values u16))
                  (+ (dot self y) (dot self h)))))
      ,*blank*
      (attr "must_use"
        ,(pub_ '(defun area ("&self")
                  (declare (values u32))
                  (* (u32--from (dot self w)) (u32--from (dot self h)))))))
    ,*blank*
    "/// Ein erkanntes Textelement (Box, Farben, String). Keine ID: der Server"
    "/// sendet bei jeder Textänderung `ClearText` + alle `AddText` neu."
    (attr "derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)"
      (space "pub"
        "struct TextItem {
    pub rect: Rect,
    /// Vordergrund (Schrift).
    pub fg: [u8; 3],
    /// Hintergrund unter der Schrift.
    pub bg: [u8; 3],
    pub text: String,
}"))
    ,*blank*
    "/// Maustaste (X11-Nummerierung: 1 links, 2 mitte, 3 rechts)."
    (space "pub type Button =" "u8;")
    ,*blank*
    "/// Nachrichten Server → Client. Das Bild ist immer [`SIZE`]×[`SIZE`];"
    "/// die AV1-Box hat variable Größe (steht im Bitstrom, nicht im Protokoll)."
    (attr "derive(Clone, Debug, PartialEq, Serialize, Deserialize)"
      (space "pub"
        "enum ServerMsg {
    Hello,
    /// Alle bisherigen Texte verwerfen.
    ClearText,
    AddText(TextItem),
    /// AV1-Box (Still-Picture, rohe OBUs) an Position (`x`, `y`).
    Tile {
        x: u16,
        y: u16,
        data: Vec<u8>,
    },
}"))
    ,*blank*
    "/// Nachrichten Client → Server. Positionen im Capture-Raum."
    (attr "derive(Clone, Debug, PartialEq, Serialize, Deserialize)"
      (space "pub"
        "enum ClientMsg {
    Hello {
        version: u16,
    },
    MouseMove {
        x: u16,
        y: u16,
    },
    Button {
        button: Button,
        down: bool,
    },
    /// Getippter Text (Zeichen oder Paste).
    Text(String),
    /// Sondertaste als Name („Enter“, „Esc“, „Tab“, „Left“, …).
    Key {
        key: String,
        down: bool,
    },
}"))
    ,*blank*
    ,(testmod
      "use super::*;"
      *blank*
      '(attr "test"
         (defun rect_geometry ()
           (let ((a (Rect--new 0 0 10 10)))
             (assert_eq! (paren (dot a (x2)) (dot a (y2)) (dot a (area)))
                         (paren 10 10 100))
             (assert_eq! a (make-instance Rect :x 0 :y 0 :w 10 :h 10))))))))

(defun common-framing-rs ()
  `(do0
    ,(doc "`02_framing` — Nachrichtenrahmen über beliebige `Read`/`Write`-Streams."
          ""
          "Frame = `[u32 LE Länge][bincode-Body]`. Der Leser puffert Teil-Frames,"
          "damit Socket-Timeouts (`WouldBlock`/`TimedOut`) keine Daten verlieren —"
          "so kann ein Thread regelmäßig aufwachen ohne Nebenläufigkeit.")
    ,*blank*
    (use (std io (curly self ErrorKind Read Write)))
    ,*blank*
    (use (serde Serialize))
    (use (serde de DeserializeOwned))
    ,*blank*
    (use (crate types MAX_MSG))
    ,*blank*
    "/// Header-Größe eines Frames."
    (space "pub const HEADER: usize =" "4;")
    ,*blank*
    "/// Kodiert eine Nachricht (nur Body, ohne Rahmen)."
    ,(pub_ '(defun "encode_msg<T: Serialize>" (m)
              (declare (type "&T" m)
                       (values "Result<Vec<u8>, String>"))
              (dot (bincode--serde--encode_to_vec m (bincode--config--standard))
                   (map_err (lambda (e) (dot e (to_string)))))))
    ,*blank*
    "/// Dekodiert einen Body."
    ,(pub_ '(defun "decode_msg<T: DeserializeOwned>" (b)
              (declare (type "&[u8]" b)
                       (values "Result<T, String>"))
              (let (("(m, _): (T, usize)"
                     (? (dot (bincode--serde--decode_from_slice b (bincode--config--standard))
                             (map_err (lambda (e) (dot e (to_string))))))))
                (Ok m))))
    ,*blank*
    "/// Schreibt eine Nachricht als Frame; liefert die Bytes auf der Leitung."
    ,(pub_ '(defun write_msg ("w: &mut impl Write" "m: &impl Serialize")
              (declare (values "io::Result<usize>"))
              (let ((body (? (dot (encode_msg m)
                                   (map_err (lambda (e)
                                              (io--Error--new (scope ErrorKind InvalidInput) e)))))))
                (when (> (dot body (len)) MAX_MSG)
                  (return (Err (io--Error--new (scope ErrorKind InvalidInput)
                                               (string "Nachricht zu groß")))))
                (? (dot w (write_all (ref (dot (coerce (dot body (len)) u32)
                                                   (to_le_bytes))))))
                (? (dot w (write_all (ref body))))
                (Ok (+ HEADER (dot body (len)))))))
    ,*blank*
    "/// Ergebnis eines Leseversuchs."
    (attr "derive(Debug, PartialEq, Eq)"
      (space "pub"
        "enum Read1 {
    /// Vollständiger Body.
    Frame(Vec<u8>),
    /// Timeout ohne vollständigen Frame (Teildaten bleiben gepuffert).
    Idle,
}"))
    ,*blank*
    "/// Puffernder Frame-Leser."
    (attr "derive(Default)"
      ,(pub_ '(defstruct0 FrameReader (buf "Vec<u8>"))))
    ,*blank*
    (impl FrameReader
      (attr "must_use"
        ,(pub_ '(defun new ()
                  (declare (values Self))
                  (Self--default))))
      ,*blank*
      "/// Nächster Frame aus dem Puffer, falls vollständig."
      (defun pop ("&mut self")
        (declare (values "io::Result<Option<Vec<u8>>>"))
        (when (< (dot (dot self buf) (len)) HEADER)
          (return (Ok None)))
        (let ((n (coerce (u32--from_le_bytes
                           (dot (aref (dot self buf) (space ".." HEADER))
                                (try_into) (unwrap)))
                         usize)))
          (when (> n MAX_MSG)
            (return (Err (io--Error--new
                           (scope ErrorKind InvalidData)
                           (string "Frame-Länge zu groß")))))
          (when (< (dot (dot self buf) (len)) (+ HEADER n))
            (return (Ok None)))
          (let ((body (dot (aref (dot self buf) (space HEADER ".." (+ HEADER n)))
                            (to_vec))))
            (dot (dot self buf) (drain (space ".." (+ HEADER n))))
            (Ok (Some body)))))
      ,*blank*
      "/// Liest bis ein Frame komplett ist, EOF (Fehler) oder Timeout (`Idle`)."
      ,(pub_ '(defun read ("&mut self" "r: &mut impl Read")
                (declare (values "io::Result<Read1>"))
                (loop
                  (if-let ((Some f) (? (dot self (pop))))
                    (return (Ok (Read1--Frame f))))
                  (let* ((tmp (array-repeat "0u8" 4096)))
                    (case (dot r (read (ref-mut tmp)))
                      ((Ok 0) (return (Err (dot (scope ErrorKind UnexpectedEof) (into)))))
                      ((Ok n) (dot (dot self buf)
                                    (extend_from_slice (ref (aref tmp (space ".." n))))))
                      ((Err e)
                       (when (matches! (dot e (kind))
                                       (logior (scope ErrorKind WouldBlock)
                                               (scope ErrorKind TimedOut)))
                         (return (Ok (scope Read1 Idle))))
                       (when (!= (dot e (kind)) (scope ErrorKind Interrupted))
                         (return (Err e)))))))))
      ,*blank*
      "/// Liest genau eine Nachricht (`Idle` bei Timeout ohne Daten)."
      ,(pub_ '(defun "read_msg<T: DeserializeOwned>" ("&mut self" "r: &mut impl Read")
                (declare (values "io::Result<Option<T>>"))
                (case (? (dot self (read r)))
                  ((scope Read1 Idle) (Ok None))
                  ("Read1::Frame(b)"
                   (dot (decode_msg (ref b))
                        (map Some)
                        (map_err (lambda (e)
                                   (io--Error--new (scope ErrorKind InvalidData) e)))))))))
    ,*blank*
    ,(testmod
      "use super::*;"
      "use crate::types::{ClientMsg, Rect, ServerMsg, TextItem};"
      "use std::io::Cursor;"
      *blank*
      '(defun item (text)
         (declare (type "&str" text)
                  (values TextItem))
         (make-instance TextItem
           :rect (Rect--new 1 2 300 16)
           :fg (bracket 0 0 0)
           :bg (bracket 255 255 250)
           :text (dot text (into))))
      *blank*
      '(defun server_samples ()
         (declare (values "Vec<ServerMsg>"))
         (vec! (scope ServerMsg Hello)
               (scope ServerMsg ClearText)
               (ServerMsg--AddText (item (string "Hallo Welt ä€𝄞")))
               "ServerMsg::Tile { x: 0, y: 64, data: vec![1, 2, 3, 255] }"))
      *blank*
      '(defun client_samples ()
         (declare (values "Vec<ClientMsg>"))
         (vec! "ClientMsg::Hello { version: 1 }"
               "ClientMsg::MouseMove { x: 639, y: 0 }"
               "ClientMsg::Button { button: 3, down: true }"
               (ClientMsg--Text (dot (string "zeile1\\nzeile2") (into)))
               "ClientMsg::Key { key: \"Enter\".into(), down: false }"))
      *blank*
      '(attr "test"
         (defun roundtrip_all_variants ()
           (for (m (server_samples))
             (assert_eq! (dot ("decode_msg::<ServerMsg>"
                               (ref (dot (encode_msg (ref m)) (unwrap))))
                              (unwrap))
                         m))
           (for (m (client_samples))
             (assert_eq! (dot ("decode_msg::<ClientMsg>"
                               (ref (dot (encode_msg (ref m)) (unwrap))))
                              (unwrap))
                         m))))
      *blank*
      '(attr "test"
         (defun truncated_body_is_an_error ()
           (for (m (server_samples))
             (let ((b (dot (encode_msg (ref m)) (unwrap))))
               (for (n (range 0 (dot b (len))))
                 (assert! (dot ("decode_msg::<ServerMsg>"
                                (ref (aref b (space ".." n))))
                               (is_err))
                          (string "{m:?} bei {n}")))))))
      *blank*
      "/// Liefert je zweitem Aufruf ein Byte, sonst Timeout-Fehler."
      "struct Trickle(Vec<u8>, usize, bool);"
      *blank*
      '(impl (space Read for Trickle)
         (defun read ("&mut self" "out: &mut [u8]")
           (declare (values "io::Result<usize>"))
           (= (dot self 2) (not (dot self 2)))
           (when (dot self 2)
             (return (Err (dot (scope ErrorKind WouldBlock) (into)))))
           (when (>= (dot self 1) (dot (dot self 0) (len)))
             (return (Ok 0)))
           (= (aref out 0) (aref (dot self 0) (dot self 1)))
           (incf (dot self 1))
           (Ok 1)))
      *blank*
      '(attr "test"
         (defun framed_roundtrip_several_messages ()
           (let* ((wire ("Vec::new")))
             (for (m (server_samples))
               (stmt (dot (write_msg (ref-mut wire) (ref m)) (unwrap))))
             (let* ((c (Cursor--new wire))
                    (fr (FrameReader--new)))
               (for (m (server_samples))
                 (let (("got: Option<ServerMsg>"
                        (dot (dot fr (read_msg (ref-mut c))) (unwrap))))
                   (assert_eq! (dot got (as_ref)) (Some (ref m)))))
               (assert_eq! (dot (dot fr (read (ref-mut c)))
                                 (unwrap_err) (kind))
                            (scope ErrorKind UnexpectedEof))))))
      *blank*
      '(attr "test"
         (defun partial_reads_with_timeouts_lose_nothing ()
           (let* ((wire ("Vec::new")))
             (dot (write_msg (ref-mut wire) (ref (scope ServerMsg Hello))) (unwrap))
             (dot (write_msg (ref-mut wire)
                             (ref (ClientMsg--Text (dot (string "hi") (into)))))
                  (unwrap))
             (let* ((t (Trickle wire 0 false))
                    (fr (FrameReader--new))
                    (got ("Vec::new"))
                    (idles 0))
               (while (< (dot got (len)) 2)
                 (case (dot (dot fr (read (ref-mut t))) (unwrap))
                   ("Read1::Frame(f)" (dot got (push f)))
                   ((scope Read1 Idle) (incf idles))))
               (assert! (> idles 5))
               (assert_eq! (dot ("decode_msg::<ServerMsg>" (ref (aref got 0))) (unwrap))
                           (scope ServerMsg Hello))))))
      *blank*
      '(attr "test"
         (defun oversized_length_is_rejected ()
           (let* ((fr (FrameReader--new))
                  (c (Cursor--new (dot (paren (+ (coerce MAX_MSG u32) 1))
                                             (to_le_bytes)))))
             (assert_eq! (dot (dot fr (read (ref-mut c))) (unwrap_err) (kind))
                         (scope ErrorKind InvalidData))))))))

(defun common-yuv-rs ()
  `(do0
    ,(doc "`03_yuv` — Farbraum-Umrechnung RGB ↔ YUV 4:2:0 (BT.601, Full Range)."
          ""
          "Aus `source6/common/05_yuv.rs` übernommen: Server (Encoder) und Client"
          "(Decoder) nutzen exakt dieselben Formeln, damit keine Farbverschiebung"
          "entsteht. Integer-Arithmetik (Q8), keine Abhängigkeiten.")
    ,*blank*
    "/// Drei YUV-Ebenen; `u`/`v` haben halbe Auflösung (aufgerundet)."
    (attr "derive(Clone, Debug, PartialEq, Eq)"
      ,(pub_ '(defstruct0 Yuv420
                ("pub w" usize)
                ("pub h" usize)
                ("pub y" "Vec<u8>")
                ("pub u" "Vec<u8>")
                ("pub v" "Vec<u8>"))))
    ,*blank*
    (impl Yuv420
      "/// Breite der Chroma-Ebenen."
      (attr "must_use"
        ,(pub_ '(defun cw ("&self")
                  (declare (values usize))
                  (dot (dot self w) (div_ceil 2))))))
    ,*blank*
    (defun clamp8 (v)
      (declare (type i32 v)
               (values u8))
      (coerce (dot v (clamp 0 255)) u8))
    ,*blank*
    "/// Ein RGB-Pixel → (Y, U, V), BT.601 Full Range."
    (attr "must_use"
      ,(pub_ '(defun rgb_to_yuv (r g b)
                (declare (type u8 r g b)
                         (values u8 u8 u8))
                (let (((paren r g b)
                       (paren (i32--from r) (i32--from g) (i32--from b))))
                  (let ((y (>> (paren (+ (* 77 r) (* 150 g) (* 29 b) 128)) 8))
                        (u (+ (>> (paren (+ (* -43 r) (* -85 g) (* 128 b) 128)) 8) 128))
                        (v (+ (>> (paren (+ (* 128 r) (* -107 g) (* -21 b) 128)) 8) 128)))
                    (paren (clamp8 y) (clamp8 u) (clamp8 v)))))))
    ,*blank*
    "/// (Y, U, V) → RGB, Umkehrung von [`rgb_to_yuv`]."
    (attr "must_use"
      ,(pub_ '(defun yuv_to_rgb (y u v)
                (declare (type u8 y u v)
                         (values "[u8; 3]"))
                (let (((paren y u v)
                       (paren (i32--from y) (- (i32--from u) 128) (- (i32--from v) 128))))
                  (let ((r (+ y (>> (paren (+ (* 359 v) 128)) 8)))
                        (g (- y (>> (paren (+ (* 88 u) (* 183 v) 128)) 8)))
                        (b (+ y (>> (paren (+ (* 454 u) 128)) 8))))
                    (bracket (clamp8 r) (clamp8 g) (clamp8 b)))))))
    ,*blank*
    "/// Interleaved RGB8 (`w*h*3`) → YUV 4:2:0; Chroma = Mittel über 2×2."
    (attr "must_use"
      ,(pub_ '(defun rgb_to_yuv420 (rgb w h)
                (declare (type "&[u8]" rgb)
                         (type usize w h)
                         (values Yuv420))
                (assert! (>= (dot rgb (len)) (* (* w h) 3)))
                (let (((paren cw ch)
                       (paren (dot w (div_ceil 2)) (dot h (div_ceil 2)))))
                  (let* ((out (make-instance Yuv420
                                w h
                                :y "vec![0; w * h]"
                                :u "vec![0; cw * ch]"
                                :v "vec![0; cw * ch]"))
                         (usum "vec![0u32; cw * ch]")
                         (vsum "vec![0u32; cw * ch]")
                         (cnt "vec![0u32; cw * ch]"))
                    (for (yy (range 0 h))
                      (for (xx (range 0 w))
                        (let ((i (* (+ (* yy w) xx) 3)))
                          (let (((paren y u v)
                                 (rgb_to_yuv (aref rgb i) (aref rgb (+ i 1)) (aref rgb (+ i 2)))))
                            (= (aref (dot out y) (+ (* yy w) xx)) y)
                            (let ((c (+ (* (/ yy 2) cw) (/ xx 2))))
                              (incf (aref usum c) (u32--from u))
                              (incf (aref vsum c) (u32--from v))
                              (incf (aref cnt c)))))))
                    (for (c (range 0 (* cw ch)))
                      (= (aref (dot out u) c)
                         (coerce (/ (+ (aref usum c) (/ (aref cnt c) 2)) (aref cnt c)) u8))
                      (= (aref (dot out v) c)
                         (coerce (/ (+ (aref vsum c) (/ (aref cnt c) 2)) (aref cnt c)) u8)))
                    out)))))
    ,*blank*
    "/// YUV-Ebenen mit beliebigen Strides → RGBA8 (`w*h*4`, Alpha 255)."
    (attr "allow(clippy::too_many_arguments)"
      ,(pub_ '(defun yuv420_to_rgba (y ys u v cs w h rgba)
                (declare (type "&[u8]" y u v)
                         (type usize ys cs w h)
                         (type "&mut [u8]" rgba))
                (for (yy (range 0 h))
                  (for (xx (range 0 w))
                    (let ((c (+ (* (/ yy 2) cs) (/ xx 2))))
                      (let (((bracket r g b)
                             (yuv_to_rgb (aref y (+ (* yy ys) xx))
                                         (aref u c)
                                         (aref v c))))
                        (let ((o (* (+ (* yy w) xx) 4)))
                          (dot (aref rgba (space o ".." (+ o 4)))
                               (copy_from_slice (ref (bracket r g b 255))))))))))))
    ,*blank*
    ,(testmod
      "use super::*;"
      *blank*
      '(attr "test"
         (defun roundtrip_error_is_small ()
           (progn "// Grobes Raster über den RGB-Würfel: Hin-/Rückweg max. ±3."
             (for (r (dot (range-inclusive 0 255) (step_by 15)))
             (for (g (dot (range-inclusive 0 255) (step_by 15)))
               (for (b (dot (range-inclusive 0 255) (step_by 15)))
                 (let (((paren y u v)
                        (rgb_to_yuv (coerce r u8) (coerce g u8) (coerce b u8))))
                   (let ((back (yuv_to_rgb y u v)))
                     (for ((tuple a o) (dot (bracket r g b) (iter) (zip back)))
                       (assert! (<= (dot (paren (- a (i32--from o))) (abs)) 3)
                                (string "{r},{g},{b} -> {back:?}")))))))))))
      *blank*
      '(attr "test"
         (defun grey_has_neutral_chroma ()
           (assert_eq! (rgb_to_yuv 0 0 0) (paren 0 128 128))
           (assert_eq! (rgb_to_yuv 255 255 255) (paren 255 128 128))))
      *blank*
      '(attr "test"
         (defun odd_size_planes ()
           (let ((rgb "vec![200u8; 3 * 3 * 3]")
                 (yuv (rgb_to_yuv420 (ref rgb) 3 3)))
             (assert_eq! (paren (dot (dot yuv y) (len))
                                (dot (dot yuv u) (len))
                                (dot yuv (cw)))
                         (paren 9 4 2))
             (let* ((rgba "vec![0u8; 3 * 3 * 4]"))
               (yuv420_to_rgba (ref (dot yuv y)) 3
                               (ref (dot yuv u)) (ref (dot yuv v)) 2
                               3 3 (ref-mut rgba))
               (assert! (dot rgba
                             (chunks 4)
                             (all (lambda (p)
                                    (and (<= (dot (aref p 0) (abs_diff 200)) 2)
                                         (== (aref p 3) 255)))))))))))))

(defun common-lib-rs ()
  `(do0
    ,(doc "`lbw-common` — geteiltes MVP-Protokoll: Typen, Framing, YUV."
          "Nur Modul-Deklarationen.")
    ,*blank*
    (attr "path = \"01_types.rs\""
      (space "pub" "mod types;"))
    ,*blank*
    (attr "path = \"02_framing.rs\""
      (space "pub" "mod framing;"))
    ,*blank*
    (attr "path = \"03_yuv.rs\""
      (space "pub" "mod yuv;"))
    ,*blank*
    (space "pub"
      (use (types (curly ClientMsg DEFAULT_PORT MAX_MSG PROTO_VERSION Rect SIZE ServerMsg TextItem))))))
