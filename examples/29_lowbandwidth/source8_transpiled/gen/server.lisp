(load (merge-pathnames "00_util.lisp" *load-pathname*))

(in-package :cl-rust-generator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case *readtable*) :invert))

;;;; server.lisp --- lbw-server: manifest, config, capture, av1, input, tiles, session, lib, main.

;;; Splice-Helfer (Tabellen statt Wiederholung; vgl. `00_util.lisp`).

(defun argv-expr (args)
  "Argv-Array für `try_parse_from`-Tests: `[\"lbw-server\", ARGS...]`."
  `(bracket (string "lbw-server")
            ,@(loop for a in args collect (list 'string a))))

(defun config-reject-assert (args)
  "`try_parse_from([\"lbw-server\", ARGS...])` muss scheitern."
  `(assert! (dot (Config--try_parse_from ,(argv-expr args)) (is_err))))

(defun minmax-assign (var op expr)
  "`VAR = VAR.OP(EXPR)` (Bbox-Akkumulation: min/max-Faltung)."
  (list '= var (list 'dot var (list op expr))))

(defun rgb565-expr ()
  "12-bit-Farb-Schlüssel `(r4<<8)|(g4<<4)|b4` aus den Nibbles von `p`."
  (reduce (lambda (a b) (list 'logior a b))
          (loop for (i sh) in '((0 8) (1 4) (2 0))
                for x = `(u16--from (>> (aref p ,i) 4))
                collect (if (zerop sh) x `(<< ,x ,sh)))))

(defun unclip-bindings ()
  "fx0/fy0/fx1/fy1 aus der Unclip-Geometrie (x/y-symmetrisch)."
  (append (loop for (v c d) in '((fx0 x0 dist) (fy0 y0 dist_y))
                collect `(,v (dot (- (coerce ,c f32) ,d) (max 0.0))))
          (loop for (v c d lim) in '((fx1 x1 dist w) (fy1 y1 dist_y h))
                collect `(,v (dot (+ (+ (coerce ,c f32) 1.0) ,d)
                                  (min (coerce ,lim f32)))))))

(defun ceil-extent (v base)
  "`(V.ceil() as u16) - BASE` (Kachel-Ausdehnung in einer Achse)."
  `(- (coerce (dot ,v (ceil)) u16) ,base))

(defun luma-term (i weight)
  "Gewichteter Helligkeits-Term aus `bg[I]` (Gewicht 1: ohne Faktor)."
  (let ((x `(u32--from (aref bg ,i))))
    (if (= weight 1) x `(* ,x ,weight))))

(defun server-cargo-toml ()
  "[package]
name = \"lbw-server\"
version.workspace = true
edition.workspace = true
authors.workspace = true
license.workspace = true

[dependencies]
clap = { version = \"4.6.7\", features = [\"derive\"] }
enigo = \"0.6.1\"
image = { version = \"0.24\", default-features = false }
lbw-common = { path = \"../common\" }
ort = { version = \"2.0.0-rc.13\", default-features = false, features = [\"download-binaries\", \"copy-dylibs\", \"tls-native\", \"std\"] }
rav1e = { version = \"0.8.1\", default-features = false, features = [\"threading\"] }
scrap = \"0.5.0\"
serde = { version = \"1.0.229\", features = [\"derive\"] }
serde_yaml = \"0.9.34\"
x11rb = \"0.13\"

[dev-dependencies]
# Nur Tests lesen/schreiben Dateien (PPM-Testbild öffnen, PNG-Capture
# speichern); Produktion nutzt nur Rgb/RgbImage ohne jedes Format.
image = { version = \"0.24\", default-features = false, features = [\"png\", \"pnm\"] }
")

(defun server-config-rs ()
  `(do0
    ,(doc "`01_config` — Kommandozeile des MVP-Servers (clap-derive).")
    ,*blank*
    (use (clap Parser))
    ,*blank*
    ,(clap-struct
      '("`lbw-server` — Minimal Low-Bandwidth Remote Desktop Server (MVP)."
        ""
        "Ohne Authentifizierung: nur an localhost binden oder per `ssh -L`/`-R`"
        "zugreifen! Display-Auswahl per `$DISPLAY`.")
      '("derive(Clone, Debug, Parser)"
        "command(name = \"lbw-server\", version)")
      "Config"
      '(("Adresse (Default nur localhost)."
         "arg(long, default_value = \"127.0.0.1:7878\")"
         "listen" "String")
        ("Linke obere Ecke des Ausschnitts."
         "arg(long, default_value_t = 0)"
         "x" "u32")
        ("Linke obere Ecke des Ausschnitts."
         "arg(long, default_value_t = 0)"
         "y" "u32")
        ("AV1-Quantizer 0..=255 (höher = kleiner/schlechter)."
         "arg(long, default_value_t = 180)"
         "quantizer" "usize")
        ("Modellverzeichnis (PP-OCRv6). Fehlt es, startet der Server nicht."
         "arg(long, default_value = \"models\")"
         "models" "String")
        ("Pipeline-Log."
         "arg(short, long)"
         "verbose" "bool")))
    ,*blank*
    (impl Config
      "/// Prüft Wertebereiche (clap parst nur Typen)."
      ,(pub_ '(defun validate ("&self")
                (declare (values "Result<(), String>"))
                (when (> (dot self quantizer) 255)
                  (return (Err (dot (string "--quantizer muss zwischen 0 und 255 liegen") (into)))))
                (Ok (paren))))
      ,*blank*
      "/// Bindet die Adresse nicht an localhost?"
      (attr "must_use"
        ,(pub_ '(defun is_public ("&self")
                  (declare (values bool))
                  (not (or (dot (dot self listen) (starts_with (string "127.")))
                           (dot (dot self listen) (starts_with (string "[::1]")))
                           (dot (dot self listen) (starts_with (string "localhost")))))))))
    ,*blank*
    ,(testmod
      "use super::*;"
      *blank*
      '(attr "test"
         (defun defaults_are_local ()
           (let ((c (dot (Config--try_parse_from (bracket (string "lbw-server"))) (unwrap))))
             (assert_eq! (paren (dot (dot c listen) (as_str)) (dot c quantizer))
                         (paren (string "127.0.0.1:7878") 180))
             (assert! (and (not (dot c (is_public))) (not (dot c verbose))))
             (dot (dot c (validate)) (unwrap)))))
      *blank*
      `(attr "test"
         (defun options_are_parsed ()
           (let ((c (dot (Config--try_parse_from
                          ,(argv-expr '("--listen" "0.0.0.0:9" "--x" "10" "--y" "20"
                                        "--quantizer" "99" "--models" "/m" "-v")))
                         (unwrap))))
             (assert! (dot c (is_public)))
             (assert_eq! (paren ,@(loop for f in '(x y quantizer)
                                        collect (list 'dot 'c f)))
                         (paren 10 20 99))
             (assert_eq! (dot c models) (string "/m"))
             (assert! (dot c verbose))
             (dot (dot c (validate)) (unwrap)))))
      *blank*
      `(attr "test"
         (defun bad_values_are_rejected ()
           (progn "// Unbekannte Option scheitert schon beim Parsen."
             ,@(loop for args in '(("--bogus") ("--no-ocr") ("--dump")
                                    ("--no-input") ("--size") ("--quantizer" "x"))
                     collect (config-reject-assert args)))
           "// Falsche Wertebereiche scheitern bei validate()."
           (let ((c (dot (Config--try_parse_from
                          (bracket (string "lbw-server")
                                   (string "--quantizer") (string "256")))
                         (unwrap))))
             (assert! (dot (dot c (validate)) (is_err)))))))))

(defun server-capture-rs ()
  `(do0
    ,(doc "`02_capture` — Bildquellen: Scrap-Ausschnitt oder synthetisch (Tests)."
          "Die Session kennt nur den Trait [`FrameSource`].")
    ,*blank*
    (use (std sync (curly Arc Mutex)))
    ,*blank*
    (use (image RgbImage))
    ,*blank*
    "/// Liefert RGB-Frames fester Größe. Läuft im Session-Thread (kein `Send`:"
    "/// `scrap::Capturer` ist `!Send`)."
    (space "pub"
      "trait FrameSource {
    /// Aktuelles Bild.
    fn grab(&mut self) -> Result<RgbImage, String>;
}")
    ,*blank*
    "/// Rechteckiger Ausschnitt der primären Anzeige (via `scrap`, X11/MIT-SHM)."
    "/// Das Display kommt aus `$DISPLAY`."
    ,(pub_ '(defstruct0 ScrapSource
              (cap "scrap::Capturer")
              (fw u32)
              (x u32)
              (y u32)
              (w u32)
              (h u32)))
    ,*blank*
    (impl ScrapSource
      "/// Öffnet die primäre Anzeige und prüft den Ausschnitt."
      ,(pub_ '(defun open (x y w h)
                (declare (type u32 x y w h)
                         (values "Result<Self, String>"))
                (let ((d (? (dot (scrap--Display--primary)
                                    (map_err (lambda (e) (format! (string "scrap: {e}"))))))))
                  (let (((paren fw fh)
                         (paren (coerce (dot d (width)) u32)
                                 (coerce (dot d (height)) u32))))
                    (when (or (> (+ x w) fw) (> (+ y h) fh))
                      (return (Err (format! (string "Ausschnitt {w}x{h}@{x},{y} außerhalb {fw}x{fh}")))))
                    (let ((cap (? (dot (scrap--Capturer--new d)
                                         (map_err (lambda (e) (format! (string "scrap: {e}"))))))))
                      (Ok (make-instance Self cap fw x y w h)))))))
      ,*blank*
      "/// Wartet auf einen frischen Frame (scrap liefert anfangs `WouldBlock`)."
      (defun frame ("&mut self")
        (declare (values "Result<Vec<u8>, String>"))
        (for (_ (range 0 300))
          (case (dot (dot self cap) (frame))
            ((Ok f) (return (Ok (dot f (to_vec)))))
            ((Err e)
             (when (!= (dot e (kind)) (scope std--io--ErrorKind WouldBlock))
               (return (Err (format! (string "capture: {e}")))))
             (std--thread--sleep (std--time--Duration--from_millis 10)))))
        (Err (dot (string "capture: kein Frame nach 3 s") (into)))))
    ,*blank*
    (impl (space FrameSource for ScrapSource)
      (defun grab ("&mut self")
        (declare (values "Result<RgbImage, String>"))
        (let ((f (? (dot self (frame)))))
          (Ok (crop_bgrx_to_rgb (ref f)
                                (dot self fw) (dot self x) (dot self y)
                                (dot self w) (dot self h))))))
    ,*blank*
    "/// Schneidet `(x, y, w, h)` aus einem BGRX-Vollbild (`fw*4` Stride) und"
    "/// wandelt nach RGB."
    (defun crop_bgrx_to_rgb (f fw x y w h)
      (declare (type "&[u8]" f)
               (type u32 fw x y w h)
               (values RgbImage))
      (let* ((out (RgbImage--new w h)))
        (for (dy (range 0 h))
          (for (dx (range 0 w))
            (let ((s (coerce (* (+ (* (+ y dy) fw) (paren (+ x dx))) 4) usize)))
              (dot out (put_pixel dx dy (image--Rgb (bracket ,@(channel-arefs 'f 's '(2 1 0)))))))))
        out))
    ,*blank*
    "/// Synthetische Quelle: liefert das Bild, das zuletzt per Handle gesetzt wurde."
    (attr "derive(Clone)"
      ,(pub_ '(defstruct0 SharedSource
                (img "Arc<Mutex<RgbImage>>")
                (w u32)
                (h u32))))
    ,*blank*
    (impl SharedSource
      (attr "must_use"
        ,(pub_ '(defun new (img)
                  (declare (type RgbImage img)
                           (values Self))
                  (let (((paren w h) (paren (dot img (width)) (dot img (height)))))
                    (make-instance Self
                      :img (Arc--new (Mutex--new img)) w h)))))
      ,*blank*
      "/// Ersetzt das Bild (Test simuliert Bildschirmänderung)."
      ,(pub_ '(defun set ("&self" img)
                (declare (type RgbImage img))
                (assert_eq! (paren (dot img (width)) (dot img (height)))
                            (paren (dot self w) (dot self h)))
                (= (deref (dot (dot self img) (lock) (unwrap))) img))))
    ,*blank*
    (impl (space FrameSource for SharedSource)
      (defun grab ("&mut self")
        (declare (values "Result<RgbImage, String>"))
        (Ok (dot (dot (dot self img) (lock) (unwrap)) (clone)))))
    ,*blank*
    "/// Einfarbiges Testbild."
    (attr "must_use"
      ,(pub_ '(defun solid (w h c)
                (declare (type u32 w h)
                         (type "[u8; 3]" c)
                         (values RgbImage))
                (RgbImage--from_fn w h (lambda (_ _) (image--Rgb c))))))
    ,*blank*
    ,(testmod
      "use super::*;"
      *blank*
      '(attr "test"
         (defun shared_source_follows_updates ()
           (let ((s (SharedSource--new (solid 4 4 (bracket "0; 3")))))
             (let* ((reader (dot s (clone))))
               (assert_eq! (dot (dot reader (grab) (unwrap)) (get_pixel 0 0) 0)
                           (bracket "0; 3"))
               (dot s (set (solid 4 4 (bracket "5; 3"))))
               (assert_eq! (dot (dot reader (grab) (unwrap)) (get_pixel 3 3) 0)
                           (bracket "5; 3"))))))
      *blank*
      '(attr "test"
         (defun bgrx_crop_swaps_channels ()
           (progn "// 2×2 BGRX: Pixel (1,0) = R1 G2 B3."
             (let ((f "vec![
        0, 0, 0, 0, 3, 2, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
    ]"))
               (let ((img (crop_bgrx_to_rgb (ref f) 2 1 0 1 1)))
                 (assert_eq! (dot (dot img (get_pixel 0 0)) 0)
                             (bracket 1 2 3))))))))))

(defun server-av1-rs ()
  `(do0
    ,(doc "`05_av1` — AV1-Still-Picture-Encoder (rav1e) für einzelne Bildkacheln."
          ""
          "Jede Kachel ist ein eigenständiges Intra-Bild (wie in AVIF, nur ohne"
          "Container — spart ~300 Byte je Kachel). Ausgabe: rohe OBUs, direkt"
          "dekodierbar mit dav1d/rav1d. Aus `source6/server/09_av1.rs` übernommen,"
          "ohne `asm`-Feature (kein `nasm` nötig).")
    ,*blank*
    (use (lbw_common yuv rgb_to_yuv420))
    (use (rav1e color (curly ChromaSampling PixelRange)))
    (use (rav1e prelude *))
    ,*blank*
    "/// Kleinste Boxkante (AV1 arbeitet in 8×8-Blöcken)."
    (space "pub const MIN_TILE: usize =" "16;")
    ,*blank*
    "/// Kodiert ein RGB8-Bild (`w*h*3`) als AV1-Still-Picture."
    "/// `w`,`h` müssen gerade und ≥ [`MIN_TILE`] sein; `quantizer` 0..=255"
    "/// (höher = kleiner/schlechter). Speed-Preset 10 und 4 Threads sind fest"
    "/// verdrahtet (MVP: nie per CLI erreichbar gewesen)."
    ,(pub_ `(defun encode_rgb (rgb w h quantizer)
              (declare (type "&[u8]" rgb)
                       (type usize w h quantizer)
                       (values "Result<Vec<u8>, String>"))
              (when (or (< w MIN_TILE) (< h MIN_TILE)
                        (not (dot w (is_multiple_of 2)))
                        (not (dot h (is_multiple_of 2))))
                (return (Err (format! (string "ungültige Boxgröße {w}x{h}")))))
              (let ((yuv (rgb_to_yuv420 rgb w h)))
                (let* ((enc (EncoderConfig--with_speed_preset 10)))
                  ,@(loop for (field value) in '((width w)
                                                 (height h)
                                                 (bit_depth 8)
                                                 (chroma_sampling (scope ChromaSampling Cs420))
                                                 (pixel_range (scope PixelRange Full))
                                                 (still_picture true)
                                                 (low_latency true)
                                                 (quantizer (dot quantizer (min 255)))
                                                 (min_quantizer (coerce (dot quantizer (min 255)) u8))
                                                 (max_key_frame_interval 1))
                          collect (list '= (list 'dot 'enc field) value))
                  (let ((cfg (dot (Config--new)
                                   (with_encoder_config enc)
                                   (with_threads 4)))
                        (ctx (? (dot (dot cfg (new_context))
                                     (map_err (lambda (e) (format! (string "rav1e: {e:?}"))))))))
                    (declare (type "Context<u8>" ctx)
                             (mutable ctx))
                      " "
                      (let* ((frame (dot ctx (new_frame))))
                        (dot (aref (dot frame planes) 0)
                             (copy_from_raw_u8 (ref (dot yuv y)) w 1))
                        (dot (aref (dot frame planes) 1)
                             (copy_from_raw_u8 (ref (dot yuv u)) (dot yuv (cw)) 1))
                        (dot (aref (dot frame planes) 2)
                             (copy_from_raw_u8 (ref (dot yuv v)) (dot yuv (cw)) 1))
                        (? (dot (dot ctx (send_frame frame))
                                (map_err (lambda (e) (format! (string "send_frame: {e:?}"))))))
                        (dot ctx (flush))
                        " "
                        (let* ((out ("Vec::new")))
                          (loop
                            (case (dot ctx (receive_packet))
                              ((Ok pkt) (dot out (extend_from_slice (ref (dot pkt data)))))
                              ("Err(EncoderStatus::Encoded)" (progn))
                              ("Err(EncoderStatus::LimitReached)" (break))
                              ((Err e) (return (Err (format! (string "receive_packet: {e:?}")))))))
                          (when (dot out (is_empty))
                            (return (Err (dot (string "rav1e lieferte kein Paket") (into)))))
                          (Ok out))))))))
    ,*blank*
    ,(testmod
      "use super::*;"
      *blank*
      '(attr "test"
         (defun flat_tile_is_tiny ()
           (let ((rgb (dot "[40u8, 80, 160]" (repeat (* 64 64))))
                 (bytes (dot (encode_rgb (ref rgb) 64 64 180) (unwrap))))
             (assert! (and (not (dot bytes (is_empty)))
                           (< (dot bytes (len)) 200))
                      (string "{} Byte")
                      (dot bytes (len))))))
      *blank*
      '(attr "test"
         (defun rejects_odd_or_tiny_sizes ()
           (let ((rgb "vec![0u8; 15 * 16 * 3]"))
             (assert! (dot (encode_rgb (ref rgb) 15 16 180) (is_err)))
             (assert! (dot (encode_rgb (ref rgb) 8 8 180) (is_err))))))
      *blank*
      '(attr "test"
         (defun higher_quantizer_is_smaller ()
           (let ((rgb (dot (range 0 (* (* 128 128) 3))
                            (map (lambda (i)
                                   (declare (type usize i))
                                   (coerce (>> (dot i (wrapping_mul "2_654_435_761")) 13) u8)))
                            (collect))))
             (declare (type "Vec<u8>" rgb))
             (let ((lo (encode_rgb (ref rgb) 128 128 60))
                   (hi (encode_rgb (ref rgb) 128 128 240)))
               (assert! (< (dot (dot hi (unwrap)) (len))
                           (dot (dot lo (unwrap)) (len)))))))))))

(defun key-code-fn ()
  `(defun key_code (name)
     (declare (type "&str" name)
              (values "Option<Key>"))
     (Some (case name
             ,@(server-key-arms)
             (_ (return None))))))

(defun known-keys-test ()
  `(attr "test"
     (defun known_keys_resolve ()
       (for (k (bracket ,@(known-keys-test-list)))
         (assert! (dot (key_code k) (is_some)) (string "{k}")))
       (assert! (dot (key_code (string "F13")) (is_none))))))

(defun server-input-rs ()
  `(do0
    ,(doc "`06_input` — Eingabe-Injektion per `enigo` (Maus + Tastatur)."
          "Client-Koordinaten sind relativ zum Capture-Ausschnitt; Ausschnitt-Offset"
          "und Ursprung des primären Monitors (RandR) werden auf absolute"
          "Bildschirm-Koordinaten addiert.")
    ,*blank*
    (use (enigo (curly Button Coordinate Direction Enigo Key Keyboard Mouse Settings)))
    (use (x11rb connection "Connection as _"))
    (use (x11rb protocol randr))
    ,*blank*
    (use (lbw_common ClientMsg))
    ,*blank*
    "/// Injiziert Client-Eingaben ins lokale Display (`$DISPLAY`)."
    ,(pub_ '(defstruct0 Injector
              (enigo Enigo)
              (ox i32)
              (oy i32)))
    ,*blank*
    (impl Injector
      "/// `offset`: linke obere Ecke des Capture-Ausschnitts im primären"
      "/// Monitor; dessen RandR-Ursprung kommt dazu (Mehrmonitor-Layouts)."
      ,(pub_ '(defun open (offset)
                (declare (type "(u32, u32)" offset)
                         (values "Result<Self, String>"))
                (let ((enigo (? (dot (Enigo--new (ref (Settings--default)))
                                      (map_err (lambda (e) (format! (string "enigo: {e}"))))))))
                  (let (((paren mx my) (primary_origin)))
                    (Ok (make-instance Self enigo
                          :ox (+ mx (coerce (dot offset 0) i32))
                          :oy (+ my (coerce (dot offset 1) i32))))))))
      ,*blank*
      "/// Führt eine Client-Nachricht aus (Hello wird ignoriert)."
      ,(pub_ '(defun handle ("&mut self" m)
                (declare (type "&ClientMsg" m)
                         (values "Result<(), String>"))
                (case m
                  ("ClientMsg::Hello { .. }" (Ok (paren)))
                  ("ClientMsg::MouseMove { x, y }"
                   (let (((paren ax ay) (inject_pos (dot self ox) (dot self oy)
                                                    (deref x) (deref y))))
                     (dot (dot (dot self enigo)
                                    (move_mouse ax ay (scope Coordinate Abs)))
                          (map_err (lambda (e) (format! (string "mouse: {e}")))))))
                  ("ClientMsg::Button { button, down }"
                   (let ((b (case button
                              (2 (scope Button Middle))
                              (3 (scope Button Right))
                              (_ (scope Button Left)))))
                     (let ((d (if (deref down)
                                  (scope Direction Press)
                                  (scope Direction Release))))
                       (dot (dot (dot self enigo) (button b d))
                            (map_err (lambda (e) (format! (string "button: {e}"))))))))
                  ("ClientMsg::Text(s)"
                   (dot (dot (dot self enigo) (text s))
                        (map_err (lambda (e) (format! (string "text: {e}"))))))
                  ("ClientMsg::Key { key, down }"
                   (let ((d (if (deref down)
                                (scope Direction Press)
                                (scope Direction Release))))
                     (case (key_code key)
                       ((Some k) (dot (dot (dot self enigo) (key k d))
                                       (map_err (lambda (e) (format! (string "key: {e}"))))))
                       (None (Err (format! (string "unbekannte Taste {key:?}")))))))))))
    ,*blank*
    "/// Ursprung des primären Monitors im globalen Bildschirm (RandR"
    "/// `GetMonitors` — dieselbe Quelle, aus der `scrap` den Capture-Monitor"
    "/// wählt). Bei Fehler +0+0 mit Warnung (Ein-Monitor-Verhalten)."
    (defun primary_origin ()
      (declare (values "(i32, i32)"))
      (case (query_monitors)
        ((Ok ms) (let (((paren ox oy) (pick_origin (ref ms))))
                   (eprintln! (string "[input] Monitor-Ursprung {:+}{:+}")
                              ox oy)
                   (paren ox oy)))
        ((Err e) (eprintln! (string "[input] Monitore nicht abfragbar ({e}) — Ursprung +0+0"))
         (paren 0 0))))
    ,*blank*
    "/// Alle RandR-Monitore als (x, y, primär?) — Display aus `$DISPLAY`."
    (defun query_monitors ()
      (declare (values "Result<Vec<(i32, i32, bool)>, String>"))
      (let (((paren conn screen)
             (? (dot (x11rb--connect None)
                     (map_err (lambda (e) (dot e (to_string))))))))
        (let ((root (dot (? (dot (dot (dot (dot conn (setup)) roots)
                                           (get screen))
                                      (ok_or_else (lambda ()
                                                    (format! (string "Bildschirm {screen} fehlt"))))))
                         root)))
          (let ((reply (? (dot (? (dot (randr--get_monitors (ref conn) root true)
                                        (map_err (lambda (e) (dot e (to_string))))))
                                 (reply)
                                 (map_err (lambda (e) (dot e (to_string))))))))
            (Ok (dot (dot (dot reply monitors) (iter))
                     (map (lambda (m)
                            (paren (i32--from (dot m x))
                                    (i32--from (dot m y))
                                    (dot m primary))))
                     (collect)))))))
    ,*blank*
    "/// Wählt den Injektions-Ursprung: primärer Monitor gewinnt, sonst der erste"
    "/// (wie `scrap::Display::primary`), sonst +0+0."
    (attr "must_use"
      (defun pick_origin (monitors)
        (declare (type "&[(i32, i32, bool)]" monitors)
                 (values "(i32, i32)"))
        (dot (dot (dot (dot (dot monitors (iter))
                                 (find (lambda (m) (dot m 2))))
                            (or_else (lambda () (dot monitors (first)))))
                       (map (lambda (m) (paren (dot m 0) (dot m 1)))))
                 (unwrap_or (paren 0 0)))))
    ,*blank*
    "/// Client-Punkt → absoluter Bildschirmpunkt (Ursprung + Ausschnitt + Punkt)."
    (attr "must_use"
      (defun inject_pos (ox oy x y)
        (declare (type i32 ox oy)
                 (type u16 x y)
                 (values "(i32, i32)"))
        (paren (+ ox (i32--from x)) (+ oy (i32--from y)))))
    ,*blank*
    "/// Sonder-Tastenname → enigo-Taste (vgl. Client `05_app`)."
    ,(key-code-fn)
    ,*blank*
    ,(testmod
      "use super::*;"
      *blank*
      (known-keys-test)
      *blank*
      `(attr "test"
         (defun primary_monitor_origin_wins ()
           (progn "// Vier-Monitor-Layout: primärer Monitor rechts, hochkant."
             (let ((ms (bracket ,@(loop for m in '((4200 0 true) (0 0 false)
                                                   (3120 0 false) (1920 0 false))
                                        collect (cons 'paren m)))))
               (assert_eq! (pick_origin (ref ms)) (paren 4200 0))))))
      *blank*
      '(attr "test"
         (defun origin_falls_back_to_first_then_zero ()
           (assert_eq! (pick_origin (ref (bracket (paren 100 50 false)
                                                  (paren 0 0 false))))
                       (paren 100 50))
           (assert_eq! (pick_origin (ref (bracket))) (paren 0 0))))
      *blank*
      '(attr "test"
         (defun injection_adds_origin_and_region ()
           (progn "// Monitor +4200+0, Ausschnitt @10,10, Client (550,639)."
             (assert_eq! (inject_pos (+ 4200 10) 10 550 639)
                         (paren 4760 649))))))))

(defun server-tiles-rs ()
  `(do0
    ,(doc "`04_tiles` — Änderungserkennung als einzelne Bounding Box plus Text-Maskierung."
          ""
          "Statt eines Kachelrasters: Die kleinste Box über allen geänderten Pixeln"
          "geht als genau ein AV1-Still-Picture raus — ein Header-Overhead pro Frame"
          "(~50 B) statt N×, den unveränderten Rest in der Box komprimiert AV1 als"
          "Skip-Blöcke. Der Vergleich läuft zeilenweise über ganze Slices (vom"
          "Compiler vektorisiert); nur geänderte Zeilen werden pixelweise vermessen.")
    ,*blank*
    (use (image RgbImage))
    ,*blank*
    (use (lbw_common Rect))
    ,*blank*
    (use (crate av1 MIN_TILE))
    ,*blank*
    "/// Kleinste Box über allen Änderungen; `prev = None` liefert Vollbild."
    "/// Kanten werden gerade und mindestens [`MIN_TILE`] (rav1e-Bedingung);"
    "/// `None` heißt Standbild (Funkstille). Beide Bilder müssen gleich groß sein"
    "/// und gerade Kanten ≥ [`MIN_TILE`] haben (640×640 tut das)."
    (attr "must_use"
      ,(pub_ `(defun dirty_bbox (prev cur)
                (declare (type "Option<&RgbImage>" prev)
                         (type "&RgbImage" cur)
                         (values "Option<Rect>"))
                (let (((paren w h) (dot cur (dimensions)))
                      (prev (case prev
                              ((Some p) p)
                              (None (return (Some (Rect--new 0 0
                                                             (coerce w u16)
                                                             (coerce h u16))))))))
                    (debug_assert_eq! (paren (dot prev (width)) (dot prev (height)))
                                      (paren w h))
                    (debug_assert! (and (>= w (coerce MIN_TILE u32))
                                        (>= h (coerce MIN_TILE u32))
                                        (== (% w 2) 0)
                                        (== (% h 2) 0)))
                    (let ((stride (* (coerce w usize) 3))
                          ((paren a b) (paren (dot prev (as_raw))
                                              (dot cur (as_raw)))))
                        "let (mut x0, mut x1, mut y0, mut y1) = (w, 0, h, 0);"
                          (for (y (range 0 h))
                            (let ((s (* (coerce y usize) stride))
                                  ((paren ra rb)
                                   (paren (ref (aref a (space s ".." (+ s stride))))
                                          (ref (aref b (space s ".." (+ s stride)))))))
                                (when (== ra rb)
                                  (continue))
                                (= y0 (dot y0 (min y)))
                                (= y1 (dot y1 (max y)))
                                (for (x (range 0 w))
                                  (let ((o (* (coerce x usize) 3)))
                                    (when (!= (aref ra (space o ".." (+ o 3)))
                                              (aref rb (space o ".." (+ o 3))))
                                      (= x0 (dot x0 (min x)))
                                      (break))))
                                (for (x (dot (range 0 w) (rev)))
                                  (let ((o (* (coerce x usize) 3)))
                                    (when (!= (aref ra (space o ".." (+ o 3)))
                                              (aref rb (space o ".." (+ o 3))))
                                      (= x1 (dot x1 (max x)))
                                      (break))))))
                          (when (> x0 x1)
                            (return None))
                          "// Auf Mindestgröße und gerade Kanten erweitern, im Bild halten."
                          (let ((t (coerce MIN_TILE u32))
                                (bw (dot (dot (paren (+ (- x1 x0) 1)) (max t)) (min w)))
                                (bh (dot (dot (paren (+ (- y1 y0) 1)) (max t)) (min h))))
                            (declare (mutable bw bh))
                              (incf bw (logand bw 1))
                              (incf bh (logand bh 1))
                              (let (((paren bw bh) (paren (dot bw (min w))
                                                          (dot bh (min h))))
                                    (bx (dot x0 (min (- w bw))))
                                    (by (dot y0 (min (- h bh)))))
                                (Some (Rect--new ,@(loop for v in '(bx by bw bh)
                                                         collect (list 'coerce v 'u16)))))))))))
    ,*blank*
    "/// Maskierungs-Zuschlag je Seite (zusätzlich zum Erkennungs-Padding im"
    "/// `TextItem`-Rechteck): löscht Glyphen-Fransen, die sonst als AV1-Reste"
    "/// Bandbreite kosten. Per Xvfb/xterm-Sweep bestimmt (vgl. `tests/padding.rs`):"
    "/// 6 entfernt die Fransensäume beider Testfonts; größere Werte sparen nur noch"
    "/// dadurch, dass sie den benachbarten Textcursor verschlucken — das bleibt"
    "/// sichtbar, darum ist hier Schluss."
    (space "pub const MASK_PAD: u16 =" "6;")
    ,*blank*
    "/// Weitet `r` um `pad` Pixel je Seite auf (im `w`×`h`-Bild gehalten)."
    "/// Detektions-Boxen schneiden Glyphen haarscharf ab — ohne Rand leidet die"
    "/// Erkennung und Fransensäume bleiben als AV1-Reste stehen."
    (attr "must_use"
      ,(pub_ `(defun pad_rect (r pad w h)
                (declare (type Rect r)
                         (type u16 pad)
                         (type u32 w h)
                         (values Rect))
                (let ((x0 (dot (u32--from (dot r x))
                                (saturating_sub (u32--from pad)))))
                  (let ((y0 (dot (u32--from (dot r y))
                                  (saturating_sub (u32--from pad)))))
                    (let ((x1 (dot (+ (+ (u32--from (dot r x))
                                         (u32--from (dot r w)))
                                      (u32--from pad))
                                   (min w))))
                      (let ((y1 (dot (+ (+ (u32--from (dot r y))
                                           (u32--from (dot r h)))
                                        (u32--from pad))
                                     (min h))))
                        (Rect--new ,@(loop for e in '(x0 y0 (- x1 x0) (- y1 y0))
                                           collect (list 'coerce e 'u16))))))))))
    ,*blank*
    "/// Füllt `r` (aufs Bild begrenzt) mit `c` — für die Text-Maskierung."
    ,(pub_ '(defun fill_rect (img r c)
              (declare (type "&mut RgbImage" img)
                       (type Rect r)
                       (type "[u8; 3]" c))
              (let (((paren w h) (dot img (dimensions)))
                    (x0 (dot (u32--from (dot r x)) (min w)))
                    (y0 (dot (u32--from (dot r y)) (min h)))
                    (x1 (dot (+ x0 (u32--from (dot r w))) (min w)))
                    (y1 (dot (+ y0 (u32--from (dot r h))) (min h)))
                    (px (image--Rgb c)))
                          (for (y (range y0 y1))
                            (for (x (range x0 x1))
                              (dot img (put_pixel x y px)))))))
    ,*blank*
    "/// Rechteck als RGB-Bytes (Zeile für Zeile, ohne Padding)."
    (attr "must_use"
      ,(pub_ '(defun crop_rgb (img r)
                (declare (type "&RgbImage" img)
                         (type Rect r)
                         (values "Vec<u8>"))
                (let ((stride (* (dot img (width)) 3))
                      (raw (dot img (as_raw)))
                      (out (Vec--with_capacity
                            (* (coerce (dot r (area)) usize) 3))))
                  (declare (mutable out))
                      (for (row (range 0 (u32--from (dot r h))))
                        (let ((s (coerce (+ (* (+ (u32--from (dot r y)) row) stride)
                                            (* (u32--from (dot r x)) 3))
                                         usize)))
                          (dot out (extend_from_slice
                                    (ref (aref raw (space s ".."
                                                             (+ s (coerce (* (u32--from (dot r w)) 3)
                                                                          usize)))))))))
                      out))))
    ,*blank*
    ,(testmod
      "use super::*;"
      "use crate::capture::solid;"
      *blank*
      '(attr "test"
         (defun first_frame_is_full_screen ()
           (let ((cur (solid 128 128 (bracket "1; 3"))))
             (assert_eq! (dirty_bbox None (ref cur))
                         (Some (Rect--new 0 0 128 128))))))
      *blank*
      '(attr "test"
         (defun identical_frames_are_silent ()
           (let ((a (solid 128 128 (bracket "7; 3")))
                 (b (solid 128 128 (bracket "7; 3"))))
             (assert_eq! (dirty_bbox (Some (ref a)) (ref b)) None))))
      *blank*
      '(attr "test"
         (defun scattered_pixels_yield_single_box ()
           (let ((a (solid 128 128 (bracket "0; 3")))
                 (b (dot a (clone))))
             (declare (mutable b))
               (dot b (put_pixel 10 10 (image--Rgb (bracket "9; 3"))))
               (dot b (put_pixel 100 100 (image--Rgb (bracket "9; 3"))))
               (progn "// 10..=100 → 91 px, auf gerade Kanten erweitert."
                 (assert_eq! (dirty_bbox (Some (ref a)) (ref b))
                             (Some (Rect--new 10 10 92 92)))))))
      *blank*
      '(attr "test"
         (defun single_pixel_is_padded_to_minimum ()
           (let ((a (solid 128 128 (bracket "0; 3")))
                 (b (dot a (clone))))
             (declare (mutable b))
               (dot b (put_pixel 5 5 (image--Rgb (bracket "9; 3"))))
               (assert_eq! (dirty_bbox (Some (ref a)) (ref b))
                           (Some (Rect--new 5 5 16 16))))))
      *blank*
      '(attr "test"
         (defun box_clamps_at_image_edge ()
           (let ((a (solid 128 128 (bracket "0; 3")))
                 (b (dot a (clone))))
             (declare (mutable b))
               (dot b (put_pixel 127 127 (image--Rgb (bracket "9; 3"))))
               (assert_eq! (dirty_bbox (Some (ref a)) (ref b))
                           (Some (Rect--new 112 112 16 16))))))
      *blank*
      '(attr "test"
         (defun pad_expands_and_clamps ()
           (assert_eq! (pad_rect (Rect--new 10 10 20 8) 4 128 128)
                       (Rect--new 6 6 28 16))
           (assert_eq! (pad_rect (Rect--new 10 10 20 8) 0 128 128)
                       (Rect--new 10 10 20 8))
           (progn "// Am Rand klemmen statt überlaufen."
             (assert_eq! (pad_rect (Rect--new 0 0 10 10) 4 128 128)
                         (Rect--new 0 0 14 14)))
           (assert_eq! (pad_rect (Rect--new 120 120 8 8) 4 128 128)
                       (Rect--new 116 116 12 12))))
      *blank*
      '(attr "test"
         (defun fill_and_crop_roundtrip ()
           (let ((img (solid 8 8 (bracket "0; 3"))))
             (declare (mutable img))
             (fill_rect (ref-mut img) (Rect--new 2 1 3 2) (bracket 9 8 7))
             (assert_eq! (dot (dot img (get_pixel 2 1)) 0)
                         (bracket 9 8 7))
             (assert_eq! (dot (dot img (get_pixel 5 1)) 0)
                         (bracket "0; 3"))
             (assert_eq! (crop_rgb (ref img) (Rect--new 2 1 3 2))
                         (dot (bracket 9 8 7) (repeat 6)))
             (progn "// Außerhalb clippen statt panicken."
               (fill_rect (ref-mut img) (Rect--new 100 100 5 5) (bracket "1; 3")))
             (assert_eq! (dot (dot img (get_pixel 7 7)) 0)
                         (bracket "0; 3"))))))))

(defun server-session-rs ()
  `(do0
    ,(doc "`07_session` — Client-Bedienung: Handshake, Input-Thread und"
          "Capture → OCR → Maske → Bounding-Box mit direktem TCP."
          "Kein Scheduler, kein Client-State: Reconnect beginnt bei Vollbild.")
    ,*blank*
    (use (std net TcpStream))
    (use (std sync Arc))
    (use (std sync atomic (curly AtomicBool Ordering)))
    (use (std time Duration))
    ,*blank*
    (use (image RgbImage))
    ,*blank*
    (use (lbw_common framing (curly FrameReader write_msg)))
    (use (lbw_common (curly ClientMsg PROTO_VERSION ServerMsg TextItem)))
    ,*blank*
    (use (crate av1 encode_rgb))
    (use (crate capture FrameSource))
    (use (crate config Config))
    (use (crate input Injector))
    (use (crate ocr Ocr))
    (use (crate tiles (curly MASK_PAD crop_rgb dirty_bbox fill_rect pad_rect)))
    ,*blank*
    "/// Was die Session zum Erkennen braucht (Tests nutzen Attrappen)."
    (space "pub"
      "trait Recognize {
    fn text(&mut self, img: &RgbImage) -> Result<Vec<TextItem>, String>;
}")
    ,*blank*
    (impl (space Recognize for Ocr)
      (defun text ("&mut self" img)
        (declare (type "&RgbImage" img)
                 (values "Result<Vec<TextItem>, String>"))
        (Ocr--text self img)))
    ,*blank*
    "/// Abstand zweier Frames (10 fps genügen fürs MVP)."
    (space "const FRAME_GAP: Duration =" "Duration::from_millis(100);")
    "/// Zeit für das Client-`Hello`."
    (space "const HELLO_TIMEOUT: Duration =" "Duration::from_secs(10);")
    ,*blank*
    "/// Ergebnis des Kachelversands: nichts Neues, Bytes gesendet, Abriss."
    (space "enum TileOut {" "Same," "Sent(usize)," "Gone" "}")
    ,*blank*
    "/// Bedient genau einen Client bis zum Abriss. `max_frames` begrenzt die"
    "/// Schleife (Tests); `None` läuft für immer. Flach gehalten: Handshake,"
    "/// Textversand, Maskierung und Kachelversand sind eigene Funktionen."
    "/// Verbindungsabbrüche sind `Ok(())`, nur lokale Fehler sind `Err`."
    ,(pub_ '(defun "serve_client<S: FrameSource, R: Recognize>"
              (stream cfg src ocr max_frames)
              (declare (type TcpStream stream)
                       (type "&Config" cfg)
                       (type "&mut S" src)
                       (type "&mut R" ocr)
                       (type "Option<u64>" max_frames)
                       (values "Result<(), String>"))
              (? (dot (dot stream (set_read_timeout (Some HELLO_TIMEOUT)))
                      (map_err (lambda (e) (dot e (to_string))))))
              (let ((rd (? (dot (dot stream (try_clone))
                                 (map_err (lambda (e) (dot e (to_string)))))))
                    (wr stream)
                    (fr (FrameReader--new)))
                (declare (mutable rd wr fr))
                (? (handshake (ref-mut rd) (ref-mut fr) (ref-mut wr)))
                    "// Eingaben laufen in eigenem Thread, damit Tippen nie auf AV1 wartet."
                    (? (dot (dot rd (set_read_timeout
                                     (Some (Duration--from_millis 200))))
                            (map_err (lambda (e) (dot e (to_string))))))
                    (let ((stop (Arc--new (AtomicBool--new false)))
                          (input (case (Injector--open (paren (dot cfg x) (dot cfg y)))
                                   ((Ok inj) (Some (spawn_input rd fr inj
                                                               (dot stop (clone))
                                                               (dot cfg verbose))))
                                   ((Err e)
                                    (eprintln! (string "[input] {e} — laufe ohne Eingabe"))
                                    None)))
                          (prev None)
                          (last_texts (Vec--new))
                          (frames 0))
                      (declare (type "Option<RgbImage>" prev)
                               (mutable prev)
                               (type "Vec<TextItem>" last_texts)
                               (mutable last_texts)
                               (type u64 frames)
                               (mutable frames))
                      (let ((result
                             (loop
                                       (when (dot max_frames
                                                  (is_some_and (lambda (n) (>= frames n))))
                                         (break (Ok (paren))))
                                       (incf frames)
                                       (let ((img (case (dot src (grab))
                                                    ((Ok i) i)
                                                    ((Err e) (break (Err e))))))
                                         (let ((texts (case (dot ocr (text (ref img)))
                                                        ((Ok t) t)
                                                        ((Err e) (break (Err e))))))
                                           "// Text nur bei Änderung senden (sonst Dauerlast bei Standbild)."
                                           (when (!= texts last_texts)
                                             (when (dot (push_texts (ref-mut wr) (ref texts))
                                                        (is_err))
                                               (break (Ok (paren))))
                                             (= last_texts (dot texts (clone))))
                                           (let ((masked (dot img (clone))))
                                             (declare (mutable masked))
                                             (mask_text (ref-mut masked) (ref texts))
                                             (let ((sent
                                                    (case (push_tile (ref-mut wr)
                                                                     (dot prev (as_ref))
                                                                     (ref masked)
                                                                     (dot cfg quantizer))
                                                      ("Ok(TileOut::Sent(n))" n)
                                                      ("Ok(TileOut::Same)" 0)
                                                      ("Ok(TileOut::Gone)" (break (Ok (paren))))
                                                      ((Err e) (break (Err e))))))
                                               (when (dot cfg verbose)
                                                 (eprintln! (string "[frame {frames}] {} Texte, {sent} B")
                                                            (dot texts (len))))
                                               (= prev (Some masked))
                                               (std--thread--sleep FRAME_GAP))))))))
                              (dot stop (store true (scope Ordering Relaxed)))
                              (if-let ((Some h) input)
                                (let ((_ (dot h (join))))))
                              result)))))
    ,*blank*
    "/// Hello lesen, Version prüfen, Hello antworten."
    (defun handshake (rd fr wr)
      (declare (type "&mut TcpStream" rd)
               (type "&mut FrameReader" fr)
               (type "&mut TcpStream" wr)
               (values "Result<(), String>"))
      "// Handshake: erstes Client-`Hello` prüfen (Timeout → Abbruch)."
      (let ((hello (case (? (dot (dot fr ("read_msg::<ClientMsg>" rd))
                                     (map_err (lambda (e) (dot e (to_string))))))
                     ((Some m) m)
                     (None (return (Err (dot (string "kein Hello vom Client") (into))))))))
        (let-else ("ClientMsg::Hello { version }" hello)
          (return (Err (dot (string "erste Nachricht war kein Hello") (into)))))
        (when (!= version PROTO_VERSION)
          (return (Err (format! (string "Protokoll {version}, erwartet {PROTO_VERSION}")))))
        (? (dot (write_msg wr (ref (scope ServerMsg Hello)))
                (map_err (lambda (e) (dot e (to_string))))))
        (Ok (paren))))
    ,*blank*
    "/// Texte als `ClearText` plus je ein `AddText` senden. `Err(())` = Abriss."
    (defun push_texts (wr texts)
      (declare (type "&mut TcpStream" wr)
               (type "&[TextItem]" texts)
               (values "Result<(), ()>"))
      (when (dot (write_msg wr (ref (scope ServerMsg ClearText)))
                 (is_err))
        (return (Err (paren))))
      (for (t texts)
        (when (dot (write_msg wr (ref (ServerMsg--AddText (dot t (clone)))))
                   (is_err))
          (return (Err (paren)))))
      (Ok (paren)))
    ,*blank*
    "/// Übermalt erkannte Textstellen mit ihrer Hintergrundfarbe."
    (defun mask_text (masked texts)
      (declare (type "&mut RgbImage" masked)
               (type "&[TextItem]" texts))
      (for (t texts)
        (let ((m (pad_rect (dot t rect)
                           MASK_PAD
                           (dot masked (width))
                           (dot masked (height)))))
          (fill_rect masked m (dot t bg)))))
    ,*blank*
    "/// Genau ein AV1-Bild pro Frame (`Same` = nichts geändert)."
    (defun push_tile (wr prev masked quantizer)
      (declare (type "&mut TcpStream" wr)
               (type "Option<&RgbImage>" prev)
               (type "&RgbImage" masked)
               (type usize quantizer)
               (values "Result<TileOut, String>"))
      (let ((r (case (dirty_bbox prev masked)
                    ((Some r) r)
                    (None (return (Ok (scope TileOut Same))))))
            (rgb (crop_rgb masked r))
            (data (? (encode_rgb (ref rgb)
                                 (coerce (dot r w) usize)
                                 (coerce (dot r h) usize)
                                 quantizer)))
            (msg (make-instance ServerMsg--Tile
                                :x (dot r x)
                                :y (dot r y)
                                data)))
              (case (write_msg wr (ref msg))
                ((Ok n) (Ok (TileOut--Sent n)))
                ((Err _) (Ok (scope TileOut Gone))))))
    ,*blank*
    "/// Liest Client-Nachrichten bis EOF/Stop und ruft `on_msg` je Nachricht."
    "/// Eigenständig (und `pub`), damit Tests die Eingabe-Anlieferung ohne"
    "/// Display prüfen können; die Produktion übergibt den enigo-Injector."
    ,(pub_ '(defun input_loop (rd fr stop verbose on_msg)
              (declare (type TcpStream rd)
                       (type FrameReader fr)
                       (type "&AtomicBool" stop)
                       (type bool verbose)
                       (type "impl FnMut(ClientMsg)" on_msg)
                       (mutable rd fr on_msg))
              (loop
                (when (dot stop (load (scope Ordering Relaxed)))
                  (break))
                (case (dot fr ("read_msg::<ClientMsg>" (ref-mut rd)))
                  ("Ok(Some(m))"
                   (when verbose
                     (eprintln! (string "[input] {m:?}")))
                   (on_msg m))
                  ("Ok(None)" (progn))
                  ((Err _) (break))))))
    ,*blank*
    (defun spawn_input (rd fr inj stop verbose)
      (declare (type TcpStream rd)
               (type FrameReader fr)
               (type Injector inj)
               (type "Arc<AtomicBool>" stop)
               (type bool verbose)
               (mutable inj)
               (values "std::thread::JoinHandle<()>"))
      (when verbose
        (eprintln! (string "[input] bereit")))
      (space "std::thread::spawn(move ||"
        (progn
          (input_loop rd fr (ref stop) verbose
            (lambda (m)
              (if-let ((Err e) (dot inj (handle (ref m))))
                (eprintln! (string "[input] {e}"))))))
        ")"))))

;;;; OCR pieces, batches A-C (T4). Each returns emitter forms; assembled
;;;; by server-ocr-rs below. Verified piece by piece via emit-probes.

(defun ocr-consts ()
  (loop for (name ty val . docs) in
        '(("REC_PAD" "u16" "4"
           "Erkennungs-Rand je Seite: Detektions-Boxen schneiden Glyphen (z. B."
           "Umlaut-Punkte) haarscharf ab — mit Weißraum liest das Netz deutlich besser"
           "(Befund aus `26_onnx`: CER 69→4 %). Per Xvfb/xterm-Sweep bestimmt.")
          ("DET_THRESH" "f32" "0.3"
           "Pixel-Schwelle für Textkandidaten.")
          ("BOX_THRESH" "f32" "0.6"
           "Mindest-Mittelscore einer Komponente.")
          ("UNCLIP_RATIO" "f32" "1.5"
           "Aufweitungsfaktor der Boxen.")
          ("MIN_TEXT_CONF" "f32" "0.5"
           "Mindest-Konfidenz, sonst bleibt die Zeile Bildinhalt.")
          ("MAX_LINES" "usize" "160"
           "Max. erkannte Zeilen je Frame (Rechenzeit-Deckel).")
          ("REC_H" "usize" "48"
           "Eingabehöhe des Erkenners.")
          ("MAX_W" "usize" "960"
           "Max. Eingabebreite des Erkenners."))
        append (append (mapcar (lambda (d) (format nil "/// ~a" d)) docs)
                       (list (list 'space
                                   (format nil "const ~a: ~a =" name ty)
                                   (format nil "~a;" val))))))

(defun ocr-session-fn ()
  '(defun session (path threads)
     (declare (type "&str" path)
              (type usize threads)
              (values "Result<Session, String>"))
     (let ((b (? (dot (dot (Session--builder)
                               (map_err (lambda (e) (format! (string "{path}: {e}")))))))))
       (declare (mutable b))
       (= b (? (dot (dot b (with_optimization_level (scope GraphOptimizationLevel Level3)))
                       (map_err (lambda (e) (format! (string "{path}: {e}")))))))
       (when (> threads 0)
         (= b (? (dot (dot b (with_intra_threads threads))
                         (map_err (lambda (e) (format! (string "{path}: {e}"))))))))
       (dot (dot b (commit_from_file path))
            (map_err (lambda (e) (format! (string "{path}: {e}"))))))))

(defun ocr-px-fn ()
  '(defun px (img x y)
     (declare (type "&RgbImage" img)
              (type usize x)
              (type usize y)
              (values "[u8; 3]"))
     (let ((i (* (+ (* y (coerce (dot img (width)) usize)) x) 3)))
       (dot (dot (aref (dot img (as_raw)) (space i ".." (+ i 3)))
                 (try_into))
            (unwrap)))))

(defun ocr-detector-struct ()
  '(space pub
     (defstruct0 Detector
       (session Session)
       (input "Vec<f32>")
       (visited "Vec<u32>")
       (tag u32)
       (queue "Vec<(usize, usize)>"))))

(defun ocr-detector-impl ()
  '(impl Detector
     (space pub
       (defun new (path threads)
         (declare (type "&str" path)
                  (type usize threads)
                  (values "Result<Self, String>"))
         (Ok (make-instance Self
               :session (? (session path threads))
               :input (Vec--new)
               :visited (Vec--new)
               :tag 0
               :queue (Vec--with_capacity 512)))))
     "/// Textzeilen-Boxen im Bild (Breite/Höhe Vielfache von 32)."
     (space pub
       (defun detect ("&mut self" img)
         (declare (type "&RgbImage" img)
                  (values "Result<Vec<Rect>, String>"))
         (let (((paren w h) (paren (coerce (dot img (width)) usize)
                                   (coerce (dot img (height)) usize))))
           (when (or (not (dot w (is_multiple_of 32)))
                     (not (dot h (is_multiple_of 32))))
             (return (Err (format! (string "DBNet braucht Vielfache von 32, nicht {w}x{h}")))))
           (normalize_imagenet img (ref-mut (dot self input)))
           (when (!= (dot (dot self visited) (len)) (* w h))
             (dot (dot self visited) (clear))
             (dot (dot self visited) (resize (* w h) 0)))
           (let ((out (? (dot (dot (dot self session)
                                       (run (space "ort::inputs!"
                                             (bracket
                                              (? (dot (TensorRef--from_array_view
                                                       (paren (bracket 1 3 h w)
                                                              (ref (aref (dot self input)
                                                                         (space "..")))))
                                                      (map_err (lambda (e)
                                                                 (dot e (to_string))))))))))
                                  (map_err (lambda (e)
                                             (format! (string "det: {e}"))))))))
             (let (((paren _ prob) (? (dot (dot (aref out 0)
                                                    ("try_extract_tensor::<f32>"))
                                               (map_err (lambda (e)
                                                          (dot e (to_string))))))))
               (= (dot self tag) (dot (dot self tag) (wrapping_add 1)))
               (when (== (dot self tag) 0)
                 (dot (dot self visited) (fill 0))
                 (= (dot self tag) 1))
               (Ok (postprocess prob
                                w
                                h
                                (ref-mut (dot self visited))
                                (dot self tag)
                                (ref-mut (dot self queue)))))))))))

(defun ocr-normalize-fn ()
  '(defun normalize_imagenet (img out)
     (declare (type "&RgbImage" img)
              (type "&mut Vec<f32>" out))
     (space "const MEAN: [f32; 3] =" "[0.485, 0.456, 0.406];")
     (space "const STD: [f32; 3] =" "[0.229, 0.224, 0.225];")
     (let ((plane (* (coerce (dot img (width)) usize)
                     (coerce (dot img (height)) usize))))
       (dot out (resize (* 3 plane) 0.0))
       (let ((raw (dot img (as_raw)))
             (chunks (dot (dot raw ("as_chunks::<3>")) 0)))
           (for ((paren i p) (dot (dot chunks (iter)) (enumerate)))
             (for ((paren c (ref v)) (dot (dot p (iter)) (enumerate)))
               (= (aref out (+ (* c plane) i))
                  (/ (- (/ (f32--from v) 255.0) (aref MEAN c))
                     (aref STD c)))))))))

(defun ocr-flood-fn ()
  `(defun flood_component (prob w h visited tag queue x y)
     (declare (type "&[f32]" prob)
              (type usize w)
              (type usize h)
              (type "&mut [u32]" visited)
              (type u32 tag)
              (type "&mut Vec<(usize, usize)>" queue)
              (type usize x)
              (type usize y)
              (values "Option<Rect>"))
     (= (aref visited (+ (* y w) x)) tag)
     (dot queue (clear))
     (dot queue (push (paren x y)))
     (let (((paren "mut x0" "mut x1" "mut y0" "mut y1") (paren x x y y))
           ((paren "mut sum" "mut head") (paren "0.0f32" 0)))
         (while (< head (dot queue (len)))
           (let (((paren cx cy) (aref queue head)))
             (incf head)
             ,@(loop for (v op c) in '((x0 min cx) (x1 max cx)
                                       (y0 min cy) (y1 max cy))
                     collect (minmax-assign v op c))
             (incf sum (aref prob (+ (* cy w) cx)))
             (for ((paren dx dy) (bracket ,@(loop for (dx dy) in '(("-1isize" "0isize") (1 0)
                                                                                 (0 "-1isize") (0 1))
                                                               collect (list 'paren dx dy))))
               (let (((paren nx ny) (paren (+ (coerce cx isize) dx)
                                           (+ (coerce cy isize) dy))))
                 (when (and ,@(loop for (op v lim) in '((>= nx 0) (>= ny 0)
                                                                    (< nx (coerce w isize))
                                                                    (< ny (coerce h isize)))
                                    collect (list op v lim)))
                   (let ((n (+ (* (coerce ny usize) w) (coerce nx usize))))
                     (when (and (!= (aref visited n) tag)
                                (>= (aref prob n) DET_THRESH))
                       (= (aref visited n) tag)
                       (dot queue (push (paren (coerce nx usize)
                                                      (coerce ny usize)))))))))))
         (let ((bw (coerce (+ (- x1 x0) 1) f32))
               (bh (coerce (+ (- y1 y0) 1) f32))
               (avg (/ sum (coerce (dot queue (len)) f32))))
               (when (or ,@(loop for (a b) in '(((dot queue (len)) 16)
                                                 (avg BOX_THRESH)
                                                 (bw 8.0)
                                                 (bh 6.0))
                                 collect (list '< a b)))
                 (return None))
               (let ((dist (/ (* (* bw bh) UNCLIP_RATIO) (* 2.0 (+ bw bh))))
                     (dist_y (dot (dot (* dist 0.4) (min (* bh 0.15))) (max 1.0)))
                     ,@(unclip-bindings)
                     ((paren rx ry) (paren (coerce (dot fx0 (floor)) u16)
                                           (coerce (dot fy0 (floor)) u16))))
                 (Some (Rect--new rx
                                  ry
                                  ,@(loop for (v base) in '((fx1 rx) (fy1 ry))
                                          collect (ceil-extent v base)))))))))

(defun ocr-postprocess-fn ()
  '(defun postprocess (prob w h visited tag queue)
     (declare (type "&[f32]" prob)
              (type usize w)
              (type usize h)
              (type "&mut [u32]" visited)
              (type u32 tag)
              (type "&mut Vec<(usize, usize)>" queue)
              (values "Vec<Rect>"))
     (let ((boxes (Vec--new)))
       (declare (mutable boxes))
       (for (y (range 0 h))
         (for (x (range 0 w))
           (let ((idx (+ (* y w) x)))
             (when (or (< (aref prob idx) DET_THRESH)
                       (== (aref visited idx) tag))
               (continue))
             (if-let ((Some r) (flood_component prob
                                                w
                                                h
                                                (ref-mut (deref visited))
                                                tag
                                                (ref-mut (deref queue))
                                                x
                                                y))
               (dot boxes (push r))))))
       (dot boxes (sort_by_key (lambda (b) (paren (/ (dot b y) 16) (dot b x)))))
       boxes)))

;;;; OCR pieces, batches D-F (T4): dict/recognizer, ctc/colors, Ocr, tests.

(defun ocr-load-dict-fn ()
  '(defun load_dict (yaml)
     (declare (type "&str" yaml)
              (values "Result<Vec<String>, String>"))
     (let ((y (? (dot (serde_yaml--from_str yaml)
                          (map_err (lambda (e) (dot e (to_string))))))))
       (declare (type InferenceYml y))
       (Ok (dot (dot y post_process) character_dict)))))

(defun ocr-recognizer-struct ()
  '(space pub
     (defstruct0 Recognizer
       (session Session)
       (dict "Vec<String>")
       (input "Vec<f32>"))))

(defun ocr-rec-method-new ()
  '(space pub
     (defun new (path dict_path threads)
       (declare (type "&str" path)
                (type "&str" dict_path)
                (type usize threads)
                (values "Result<Self, String>"))
       (let ((yaml (? (dot (std--fs--read_to_string dict_path)
                               (map_err (lambda (e) (format! (string "{dict_path}: {e}"))))))))
         (let ((dict (? (load_dict (ref yaml)))))
           (when (dot dict (is_empty))
             (return (Err (format! (string "{dict_path}: kein character_dict")))))
           (Ok (make-instance Self
                 :session (? (session path threads))
                 dict
                 :input (Vec--new))))))))

(defun ocr-rec-method-recognize ()
  '(space pub
     (defun recognize ("&mut self" img r)
       (declare (type "&RgbImage" img)
                (type Rect r)
                (values "Result<(String, f32), String>"))
       (let ((tw (dot self (preprocess img r)))
             (out (? (dot (dot (dot self session)
                                     (run (space "ort::inputs!"
                                           (bracket
                                            (? (dot (TensorRef--from_array_view
                                                     (paren (bracket 1 3 REC_H tw)
                                                            (ref (aref (dot self input)
                                                                       (space ".." (* (* 3 REC_H) tw))))))
                                                    (map_err (lambda (e)
                                                               (dot e (to_string))))))))))
                                (map_err (lambda (e)
                                           (format! (string "rec: {e}"))))))))
           (let (((paren shape preds) (? (dot (dot (aref out 0)
                                                       ("try_extract_tensor::<f32>"))
                                                  (map_err (lambda (e)
                                                             (dot e (to_string))))))))
             (Ok (ctc_decode preds shape (ref (dot self dict)))))))))

(defun ocr-rec-method-preprocess ()
  '(defun preprocess ("&mut self" img r)
     (declare (type "&RgbImage" img)
              (type Rect r)
              (values usize))
     (let (((paren cw ch) (paren (f32--from (dot (dot r w) (max 1)))
                                 (f32--from (dot (dot r h) (max 1)))))
           (raw_w (coerce (dot (/ (* (coerce REC_H f32) cw) ch) (round)) usize))
           (tw (dot (* (dot raw_w (div_ceil 32)) 32) (clamp 32 MAX_W)))
           (rw (dot raw_w (clamp 1 tw)))
           (plane (* REC_H tw)))
               (dot (dot self input) (clear))
               (dot (dot self input) (resize (* 3 plane) 0.0))
               (let (((paren iw ih) (paren (coerce (dot img (width)) usize)
                                           (coerce (dot img (height)) usize))))
                 (for (dy (range 0 REC_H))
                   (let ((sy (dot (coerce (dot (dot (- (+ (f32--from (dot r y))
                                                           (/ (* (+ (coerce dy f32) 0.5) ch)
                                                              (coerce REC_H f32)))
                                                        0.5)
                                                     (round))
                                                  (max 0.0))
                                             usize)
                                       (min (- ih 1)))))
                     (for (dx (range 0 rw))
                       (let ((sx (dot (coerce (dot (dot (- (+ (f32--from (dot r x))
                                                               (/ (* (+ (coerce dx f32) 0.5) cw)
                                                                  (coerce rw f32)))
                                                            0.5)
                                                         (round))
                                                      (max 0.0))
                                                 usize)
                                           (min (- iw 1)))))
                         (for ((paren c (ref v)) (dot (dot (px img sx sy) (iter)) (enumerate)))
                           (= (aref (dot self input) (+ (* c plane) (+ (* dy tw) dx)))
                              (- (/ (f32--from v) 127.5) 1.0))))))))
                 tw)))

(defun ocr-recognizer-impl ()
  `(impl Recognizer
     "/// `dict_path`: `inference.yml` mit `PostProcess.character_dict`."
     ,(ocr-rec-method-new)
     "/// Erkennt den Text in `r`; liefert (Text, Konfidenz 0..1)."
     ,(ocr-rec-method-recognize)
     "/// Crop mit Nearest-Resize auf `REC_H × tw`, Werte in [-1, 1]."
     ,(ocr-rec-method-preprocess)))

(defun ocr-ctc-fn ()
  '(defun ctc_decode (data shape dict)
     (declare (type "&[f32]" data)
              (type "&[i64]" shape)
              (type "&[String]" dict)
              (values "(String, f32)"))
     (let ((last (dot (dot (dot shape (last)) (copied)) (unwrap_or 0)))
           (n (coerce (dot last (max 0)) usize)))
         (when (== n 0)
           (return (paren (String--new) 0.0)))
         (let (((paren "mut text" "mut prev" "mut conf" "mut cnt")
                (paren (String--new) "0usize" "0.0f32" "0usize")))
           (for (row (dot data (chunks_exact n)))
             (let ((it (dot (dot (dot row (iter)) (copied)) (enumerate)))
                   ((paren idx p) (dot (dot it (max_by (lambda ((paren _ pa) (paren _ pb))
                                                        (dot pa (total_cmp pb)))))
                                      (unwrap_or (paren 0 0.0)))))
                 (when (and (!= idx 0) (!= idx prev))
                   (case (dot dict (get (- idx 1)))
                     ((Some s) (dot text (push_str s)))
                     (None (when (== (- idx 1) (dot dict (len)))
                             (dot text (push (char " "))))))
                   (incf conf p)
                   (incf cnt))
                 (= prev idx)))
           (paren text (if (> cnt 0) (/ conf (coerce cnt f32)) 0.0))))))

(defun ocr-dist2-fn ()
  '(defun dist2 (a b)
     (declare (type "[u8; 3]" a)
              (type "[u8; 3]" b)
              (values u32))
     (let ((it (dot (dot a (iter)) (zip b))))
       (dot (dot it (map (lambda ((paren x y))
                           (dot (u32--from (dot x (abs_diff y))) (pow 2)))))
            (sum)))))

(defun ocr-acc-fn ()
  '(defun acc (s p)
     (declare (type "&mut [u32; 3]" s)
              (type "[u8; 3]" p))
     (for ((paren a v) (dot (dot s (iter_mut)) (zip p)))
       (incf (deref a) (u32--from v)))))

(defun ocr-sample-fn ()
  `(defun sample_colors (img r)
     (declare (type "&RgbImage" img)
              (type Rect r)
              (values "([u8; 3], [u8; 3])"))
     (let (((paren w h) (paren (coerce (dot img (width)) usize)
                               (coerce (dot img (height)) usize))))
       (let (((paren x0 y0) (paren (dot (coerce (dot r x) usize)
                                         (min (dot w (saturating_sub 1))))
                                    (dot (coerce (dot r y) usize)
                                         (min (dot h (saturating_sub 1)))))))
         (let (((paren x1 y1) (paren (dot (dot (dot (+ x0 (coerce (dot r w) usize))
                                                     (min w))
                                                  (saturating_sub 1))
                                               (max x0))
                                      (dot (dot (dot (+ y0 (coerce (dot r h) usize))
                                                     (min h))
                                                  (saturating_sub 1))
                                               (max y0)))))
           (let ((bins (Default--default)))
             (declare (type "std::collections::HashMap<u16, ([u32; 3], u32)>" bins)
                      (mutable bins))
             (let ((add (lambda ("p: [u8; 3]")
                          (let ((k ,(rgb565-expr))
                                (e (dot (dot bins (entry k)) (or_default))))
                            (acc (ref-mut (dot e 0)) p)
                            (incf (dot e 1))))))
               (declare (mutable add))
               (for (x (range-inclusive x0 x1))
                 (add (px img x y0))
                 (add (px img x y1)))
               (for (y (range-inclusive y0 y1))
                 (add (px img x0 y))
                 (add (px img x1 y)))
               (let ((best (dot (dot bins ("values"))
                                (max_by_key (lambda ((paren _ n)) (deref n)))))
                     ((paren sum n) (dot (dot best (copied)) (unwrap)))
                     (bg (dot sum (map (lambda (s)
                                         (coerce (/ (+ s (/ n 2)) n) u8)))))
                     (maxd 0))
                 (declare (mutable maxd))
                       (for (y (range-inclusive y0 y1))
                         (for (x (range-inclusive x0 x1))
                           (= maxd (dot maxd (max (dist2 (px img x y) bg))))))
                       (when (< maxd (* 30 30))
                         "// Kaum Kontrast: Schrift in Schwarz/Weiß je nach Helligkeit."
                         (let ((luma (+ ,@(loop for (i w) in '((0 3) (1 6) (2 1))
                                               collect (luma-term i w)))))
                           (return (paren (if (> luma 1280)
                                              (array-repeat 0 3)
                                              (array-repeat 255 3))
                                          bg))))
                       (let (((paren "mut s" "mut cnt")
                              (paren (array-repeat "0u32" 3) "0u32")))
                         (for (y (range-inclusive y0 y1))
                           (for (x (range-inclusive x0 x1))
                             (let ((p (px img x y)))
                               (when (>= (* (dist2 p bg) 4) maxd)
                                 (acc (ref-mut s) p)
                                 (incf cnt)))))
                         (paren (dot s (map (lambda (v)
                                               (coerce (/ (+ v (/ cnt 2)) cnt) u8))))
                                bg))))))))))

(defun ocr-ocr-struct ()
  '(space pub
     (defstruct0 Ocr
       (det Detector)
       (rec Recognizer))))

(defun ocr-ocr-impl ()
  '(impl Ocr
     "/// Lädt `PP-OCRv6_small_{det,rec}.onnx` + `inference.yml` aus `dir`."
     "/// Fehlt eine Datei, ist das ein harter Fehler (kein Fallback: ohne"
     "/// Textmaskierung sprengt AV1-Text das Bandbreiten-Budget)."
     (space pub
       (defun load (dir threads)
         (declare (type "&str" dir)
                  (type usize threads)
                  (values "Result<Self, String>"))
         (let (((paren det rec dict)
                (paren (format! (string "{dir}/PP-OCRv6_small_det.onnx"))
                       (format! (string "{dir}/PP-OCRv6_small_rec.onnx"))
                       (format! (string "{dir}/inference.yml")))))
           (for (p (bracket (ref det) (ref rec) (ref dict)))
             (when (not (dot (dot (std--path--Path--new p)) (exists)))
               (return (Err (format! (string "Modell fehlt: {p}"))))))
           (Ok (make-instance Self
                 :det (? (Detector--new (ref det) threads))
                 :rec (? (Recognizer--new (ref rec) (ref dict) threads)))))))
     "/// Textzeilen mit Farben; unsichere/leere Erkennungen fallen weg"
     "/// (die bleiben dann Bildinhalt). Das Rechteck ist bereits um [`REC_PAD`]"
     "/// erweitert — Erkennung, Farben, Maske und Client malen dasselbe."
     (space pub
       (defun text ("&mut self" img)
         (declare (type "&RgbImage" img)
                  (values "Result<Vec<TextItem>, String>"))
         (let ((out (Vec--new)))
           (declare (mutable out))
           (let ((boxes (? (dot (dot self det) (detect img)))))
             (for (r (dot (dot boxes (into_iter)) (take MAX_LINES)))
               (let ((r (pad_rect r REC_PAD (dot img (width)) (dot img (height))))
                     ((paren text conf) (? (dot (dot self rec) (recognize img r))))
                     (text (dot (dot text (trim)) (to_owned))))
                     (when (or (dot text (is_empty)) (< conf MIN_TEXT_CONF))
                       (continue))
                     (let (((paren fg bg) (sample_colors img r)))
                       (dot out (push (make-instance TextItem :rect r fg bg text)))))))
           (Ok out))))))

(defun ocr-tests-fn ()
  (testmod "use super::*;"
           *blank*
           "use crate::capture::solid;"
           *blank*
           '(defun d (v)
              (declare (type "&[&str]" v)
                       (values "Vec<String>"))
              (dot (dot (dot v (iter))
                        (map (lambda (s) (dot (deref s) (to_owned)))))
                   (collect)))
           *blank*
           '(attr "test"
              (defun dict_parses_yaml_structure ()
                (let ((dict (dot (load_dict (string "PostProcess:\\n  name: CTCLabelDecode\\n  character_dict:\\n  - 'a'\\n  - b\\n  - \\\"c\\\"\\n  - ''''\\n")) (unwrap))))
                  (assert_eq! dict (d (ref (bracket (string "a") (string "b") (string "c") (string "'"))))))))
           *blank*
           '(attr "test"
              (defun dict_rejects_garbage ()
                (assert! (dot (load_dict (string "kein yaml: [")) (is_err)))
                (assert! (dot (load_dict (string "PostProcess:\\n  name: x\\n")) (is_err)))))
           *blank*
           '(attr "test"
              (defun ctc_collapses_duplicates_blanks_and_reports_confidence ()
                (let ((dict (d (ref (bracket (string "a") (string "b")))))
                      (data (bracket 0.1 0.9 0.0 0.0
                                     0.1 0.8 0.1 0.0
                                     0.9 0.05 0.05 0.0
                                     0.1 0.1 0.7 0.1
                                     0.0 0.0 0.0 1.0))
                      ((paren t c) (ctc_decode (ref data) (ref (bracket 5 4)) (ref dict))))
                      (assert_eq! t (string "ab "))
                      (assert! (< (dot (- c (/ (+ 0.9 0.7 1.0) 3.0)) (abs)) 1e-6)))))
           *blank*
           '(attr "test"
              (defun ctc_empty_inputs ()
                (assert_eq! (dot (ctc_decode (ref (bracket 0.9 0.1)) (ref (bracket 1 2)) (ref (d (ref (bracket (string "a")))))) 0) (string ""))
                (assert_eq! (ctc_decode (ref (bracket)) (ref (bracket 4 0)) (ref (d (ref (bracket (string "a")))))) (paren (String--new) 0.0))))
           *blank*
           '(attr "test"
              (defun missing_models_are_an_error ()
                (let-else ((Err e) (Ocr--load (string "/pfad/den/es/nicht/gibt") 1))
                  (panic! (string "muss scheitern")))
                (assert! (dot e (contains (string "Modell fehlt"))) (string "{e}"))))
           *blank*
           '(attr "test"
              (defun colors_follow_contrast ()
                (let ((img (solid 16 16 (array-repeat 255 3))))
                  (declare (mutable img))
                  (for (x (range 4 12))
                    (dot img (put_pixel x 8 (image--Rgb (array-repeat 0 3)))))
                  (let (((paren fg bg) (sample_colors (ref img) (Rect--new 0 0 16 16))))
                    (assert_eq! bg (array-repeat 255 3))
                    (assert! (< (aref fg 0) 128) (string "{fg:?}"))))
                (let (((paren fg _) (sample_colors (ref (solid 8 8 (array-repeat 250 3))) (Rect--new 0 0 8 8))))
                  (assert_eq! fg (array-repeat 0 3)))))
           *blank*
           '(attr "test"
              (defun postprocess_finds_solid_block ()
                (let (((paren w h) (paren 96 64))
                      (prob (space "vec!" (bracket (space 0.0 ";" (* w h))))))
                  (declare (mutable prob))
                    (for (y (range 20 32))
                      (for (x (range 10 60))
                        (= (aref prob (+ (* y w) x)) 0.9)))
                    (let ((visited (space "vec!" (bracket (space 0 ";" (* w h))))))
                      (declare (mutable visited))
                      (let ((b (postprocess (ref prob) w h (ref-mut visited) 1 (ref-mut (Vec--new)))))
                        (assert_eq! (dot b (len)) 1)
                        (assert! (and (<= (dot (aref b 0) x) 10) (>= (dot (aref b 0) (x2)) 60))))))))))

(defun ocr-yml-structs ()
  "inference.yml section structs as one string (field attributes fit no
defstruct0 slot — same reason clap-struct assembles a string)."
  "/// `PostProcess`-Abschnitt aus `inference.yml`.
#[derive(serde::Deserialize)]
struct InferenceYml {
    #[serde(rename = \"PostProcess\")]
    post_process: PostProcess,
}

#[derive(serde::Deserialize)]
struct PostProcess {
    character_dict: Vec<String>,
}")

(defun server-ocr-rs ()
  `(do0
    ,(doc "`03_ocr` — PP-OCRv6-Texterkennung: DBNet-Detektion + SVTR/CTC-Erkennung."
          ""
          "Aus `source6` (`04_ocr_detect`, `05_ocr_recognize`, Farb-Sampling aus"
          "`07_layout`) übernommen, aber auf `image::RgbImage` umgestellt, das"
          "Wörterbuch per `serde_yaml` gelesen und ohne Erkennungs-Cache (MVP)."
          "OCR ist Pflicht: ohne Modelle startet der Server nicht (reines AV1-Textbild"
          "würde das 6-kB/s-Budget sprengen).")
    ,*blank*
    (use (image RgbImage))
    (use (ort session Session))
    (use (ort session builder GraphOptimizationLevel))
    (use (ort value TensorRef))
    ,*blank*
    (use (lbw_common (curly Rect TextItem)))
    ,*blank*
    (use (crate tiles pad_rect))
    ,*blank*
    ,@(ocr-consts)
    ,*blank*
    "/// Lädt ein ONNX-Modell (CPU, Level 3, `threads` Intra-Op-Threads; 0 = Default)."
    ,(ocr-session-fn)
    ,*blank*
    ,(ocr-px-fn)
    ,*blank*
    "/// DBNet-Detektor mit wiederverwendbaren Puffern."
    ,(ocr-detector-struct)
    ,*blank*
    ,(ocr-detector-impl)
    ,*blank*
    "/// RGB8 → planar, ImageNet-normalisiert."
    ,(ocr-normalize-fn)
    ,*blank*
    "/// Flutet die Schwellen-Komponente ab (`x`, `y`) und liefert die aufgeweitete"
    "/// Box — oder `None` bei zu klein/schwach. (In `source7` in `postprocess`"
    "/// eingelagert; als eigene Funktion kürzer und für sich lesbar.)"
    "/// (8 Parameter sind hier ehrlich — ein Bündel-Struct wäre mehr Code.)"
    (attr "allow(clippy::too_many_arguments)"
      ,(ocr-flood-fn))
    ,*blank*
    "/// DBNet-Nachverarbeitung: verbundene Schwellen-Pixel → aufgeweitete Boxen,"
    "/// sortiert nach Zeile (16-px-Bänder), dann x."
    ,(ocr-postprocess-fn)
    ,*blank*
    ,(ocr-yml-structs)
    ,*blank*
    "/// Liest `character_dict` aus dem YAML (per `serde_yaml`)."
    ,(pub_ (ocr-load-dict-fn))
    ,*blank*
    "/// CTC-Erkenner mit Wörterbuch."
    ,(ocr-recognizer-struct)
    ,*blank*
    ,(ocr-recognizer-impl)
    ,*blank*
    "/// CTC-Greedy: Argmax je Zeitschritt, Blank (0) und Wiederholungen raus."
    "/// Klasse `dict.len()+1` ist das Leerzeichen."
    (attr "must_use"
      ,(pub_ (ocr-ctc-fn)))
    ,*blank*
    ,(ocr-dist2-fn)
    ,*blank*
    ,(ocr-acc-fn)
    ,*blank*
    "/// Dominante Hintergrund- und Schriftfarbe einer Textbox."
    "/// `bg` = Mittel der häufigsten (auf 4 bit quantisierten) Randfarbe;"
    "/// `fg` = Mittel der Innenpixel mit ≥ 50 % der maximalen Distanz zu `bg`."
    (attr "must_use"
      ,(pub_ (ocr-sample-fn)))
    ,*blank*
    "/// Geladene Texterkennung (Pflicht: ohne Modelle kein Serverstart)."
    ,(ocr-ocr-struct)
    ,*blank*
    ,(ocr-ocr-impl)
    ,*blank*
    ,(ocr-tests-fn)))

;;;; Integration tests (T4): loopback, models, padding. Every statement
;;;; shape below was validated with an emit-probe + rustfmt before assembly.

(defun test-models-dir-fn ()
  "models_dir helper shared by the models and padding tests."
  '(defun models_dir ()
     (declare (values String))
     (format! (string "{}/../../source6/models")
              (env! (string "CARGO_MANIFEST_DIR")))))

(defun server-test-loopback-rs ()
  `(do0
    ,(doc "Loopback: echte Session (`SharedSource` + Stub-OCR) über echtes TCP."
          "Läuft ohne X11 und ohne Modelle.")
    ,*blank*
    (use (std net (curly TcpListener TcpStream)))
    (use (std sync atomic AtomicBool))
    (use (std time (curly Duration Instant)))
    ,*blank*
    (use (clap Parser))
    (use (image RgbImage))
    (use (lbw_common framing (curly FrameReader write_msg)))
    (use (lbw_common (curly ClientMsg PROTO_VERSION Rect ServerMsg TextItem)))
    (use (lbw_server capture (curly SharedSource solid)))
    (use (lbw_server config Config))
    (use (lbw_server session (curly Recognize input_loop serve_client)))
    ,*blank*
    (space "struct StubOcr" (paren "Vec<TextItem>") ";")
    ,*blank*
    (impl (space Recognize for StubOcr)
      (defun text ("&mut self" _img)
        (declare (type "&RgbImage" _img)
                 (values "Result<Vec<TextItem>, String>"))
        (Ok (dot (dot self 0) (clone)))))
    ,*blank*
    (defun test_cfg ()
      (declare (values Config))
      "// Ohne Display: Injector::open scheitert, die Session läuft ohne Eingabe."
      (dot (Config--try_parse_from (bracket (string "lbw-server")))
           (unwrap)))
    ,*blank*
    (defun item ()
      (declare (values TextItem))
      (make-instance TextItem
        :rect (Rect--new 8 8 32 16)
        :fg (array-repeat 0 3)
        :bg (array-repeat 255 3)
        :text (dot (string "hi") (into))))
    ,*blank*
    "/// Liest bis zur Deadline; `want` zählt relevante Nachrichten."
    ; `source7` nutzt eine Let-Kette (`if let ... && ...`); der Emitter kennt
    ; keine Ketten, daher die äquivalente Schachtelung (exakter Desugar).
    (attr "allow(clippy::collapsible_if)"
      (defun read_until (fr s until want)
        (declare (type "&mut FrameReader" fr)
                 (type "&mut TcpStream" s)
                 (type Instant until)
                 (type "&mut dyn FnMut(&ServerMsg) -> bool" want))
        (dot (dot s (set_read_timeout (Some (Duration--from_millis 200))))
             (unwrap))
        (while (< (Instant--now) until)
          (if-let ((Some m) (dot (dot fr (read_msg s)) (unwrap)))
            (when (want (ref m))
              (return (paren)))))))
    ,*blank*
    (attr "test"
      (defun full_frame_then_single_dirty_tile ()
        (let ((listener (dot (TcpListener--bind (string "127.0.0.1:0"))
                             (unwrap)))
              (addr (dot (dot listener (local_addr))
                         (unwrap)))
              (shared (SharedSource--new (solid 128 128 (array-repeat 40 3))))
              (worker_src (dot shared (clone)))
              (server (std--thread--spawn
                       (space "move"
                         (lambda ()
                           (let (((paren stream _)
                                  (dot (dot listener (accept))
                                       (unwrap)))
                                 (src worker_src)
                                 (ocr (StubOcr (vec! (item)))))
                             (declare (mutable src ocr))
                             (serve_client stream
                                           (ref (test_cfg))
                                           (ref-mut src)
                                           (ref-mut ocr)
                                           (Some 30)))))))
              (s (dot (TcpStream--connect addr)
                      (unwrap)))
              (fr (FrameReader--new)))
          (declare (mutable s fr))
          (dot (write_msg (ref-mut s)
                          (ref (space "ClientMsg::Hello"
                                      (curly "version: PROTO_VERSION"))))
               (unwrap))
          "// Hello + Vollbild: ClearText, AddText, 1 Box (128×128)."
          (let ((hello false))
            (declare (mutable hello))
            (read_until (ref-mut fr)
                        (ref-mut s)
                        (+ (Instant--now) (Duration--from_secs 10))
                        (ref-mut (lambda (m)
                                   (when (matches! m (scope ServerMsg Hello))
                                     (= hello true)
                                     (return true))
                                   false)))
            (assert! hello)
            (let (((paren "mut clear" "mut texts" "mut tiles") (paren 0 0 0)))
              (read_until (ref-mut fr)
                          (ref-mut s)
                          (+ (Instant--now) (Duration--from_secs 10))
                          (ref-mut (lambda (m)
                                     (case m
                                       ("ServerMsg::ClearText" (incf clear))
                                       ("ServerMsg::AddText(t)"
                                        (assert_eq! (dot t text) (string "hi"))
                                        (incf texts))
                                       ("ServerMsg::Tile { .. }" (incf tiles))
                                       (_ (progn)))
                                     (and (>= clear 1)
                                          (>= texts 1)
                                          (>= tiles 1)))))
              (assert_eq! (paren clear texts tiles) (paren 1 1 1))
              "// Ein Pixel ändern → genau eine 16×16-Box um den Pixel kommt neu."
              (let ((img (solid 128 128 (array-repeat 40 3))))
                (declare (mutable img))
                (dot img (put_pixel 100 10 (image--Rgb (array-repeat 9 3))))
                (dot shared (set img))
                (let ((found None))
                  (declare (mutable found))
                  (read_until (ref-mut fr)
                              (ref-mut s)
                              (+ (Instant--now) (Duration--from_secs 10))
                              (ref-mut (lambda (m)
                                         (if-let ("ServerMsg::Tile { x, y, data }" m)
                                           (progn
                                             (assert! (not (dot data (is_empty))))
                                             (= found (Some (paren (deref x) (deref y))))
                                             (return true)))
                                         false)))
                  (assert_eq! found (Some (paren 100 10)))
                  (dot (dot (dot server (join)) (unwrap)) (unwrap)))))))))
    ,*blank*
    (attr "test"
      (defun wrong_version_is_rejected ()
        (let ((listener (dot (TcpListener--bind (string "127.0.0.1:0"))
                             (unwrap)))
              (addr (dot (dot listener (local_addr))
                         (unwrap)))
              (server (std--thread--spawn
                       (space "move"
                         (lambda ()
                           (let (((paren stream _)
                                  (dot (dot listener (accept))
                                       (unwrap)))
                                 (src (SharedSource--new (solid 128 128 (array-repeat 0 3))))
                                 (ocr (StubOcr (vec!))))
                             (declare (mutable src ocr))
                             (serve_client stream
                                           (ref (test_cfg))
                                           (ref-mut src)
                                           (ref-mut ocr)
                                           (Some 1))))))))
          (let ((s (dot (TcpStream--connect addr)
                        (unwrap)))
                (fr (FrameReader--new)))
            (declare (mutable s fr))
            (dot (write_msg (ref-mut s)
                            (ref (space "ClientMsg::Hello"
                                        (curly "version: 999"))))
                 (unwrap))
            "// Server schließt: kein Hello, EOF beim Lesen."
            (dot (dot s (set_read_timeout (Some (Duration--from_secs 5))))
                 (unwrap))
            (loop
              (case (dot fr (read (ref-mut s)))
                ("Ok(lbw_common::framing::Read1::Frame(_))"
                 (panic! (string "unerwarteter Frame")))
                ("Ok(lbw_common::framing::Read1::Idle)" (progn))
                ("Err(_)" (progn (break) "// EOF: Abweisung bestätigt"))))
            (let ((r (dot (dot server (join))
                          (unwrap))))
              (assert! (dot r (is_err)) (string "falsche Version muss Fehler sein")))))))
    ,*blank*
    (attr "test"
      (defun input_messages_reach_handler_in_order ()
        (let ((listener (dot (TcpListener--bind (string "127.0.0.1:0"))
                             (unwrap)))
              (addr (dot (dot listener (local_addr))
                         (unwrap)))
              (sent (vec! (space "ClientMsg::Hello" (curly "version: PROTO_VERSION"))
                          (space "ClientMsg::MouseMove" (curly "x: 100" "y: 200"))
                          (space "ClientMsg::Button" (curly "button: 1" "down: true"))
                          (space "ClientMsg::Button" (curly "button: 1" "down: false"))
                          (space "ClientMsg::Text" (paren (dot (string "hi") (into))))
                          (space "ClientMsg::Key" (curly "key: \"Enter\".into()" "down: true"))))
              (writer (dot sent (clone)))
              (client (std--thread--spawn
                       (space "move"
                         (lambda ()
                           (let ((s (dot (TcpStream--connect addr)
                                         (unwrap))))
                             (declare (mutable s))
                             (for (m (ref writer))
                               (stmt (dot (write_msg (ref-mut s) m)
                                          (unwrap))))
                             "// Socket fällt hier: input_loop sieht EOF und endet."))))))
          (let (((paren rd _) (dot (dot listener (accept))
                                   (unwrap))))
            (dot (dot rd (set_read_timeout (Some (Duration--from_millis 200))))
                 (unwrap))
            (let (((paren tx rx) (std--sync--mpsc--channel))
                  (stop (AtomicBool--new false)))
              (input_loop rd
                          (FrameReader--new)
                          (ref stop)
                          false
                          (lambda (m)
                            (dot (dot tx (send m))
                                 (unwrap))))
              (dot (dot client (join))
                   (unwrap))
              (assert_eq! (dot (dot rx (try_iter))
                               ("collect::<Vec<_>>"))
                          sent))))))))

(defun server-test-models-rs ()
  `(do0
    ,(doc "Modell-Test (ignored): echte PP-OCRv6-Modelle aus `source6/models`."
          "Laufen lassen mit:"
          "`cargo test --release -p lbw-server --test models -- --ignored`")
    ,*blank*
    (use (lbw_server ocr Ocr))
    ,*blank*
    ,(test-models-dir-fn)
    ,*blank*
    (attr "test" "ignore"
      (defun detects_text_on_real_screenshot ()
        (let ((dir (models_dir))
              (ppm (format! (string "{dir}/test_screen.ppm"))))
          (assert! (dot (dot (std--path--Path--new (ref ppm))
                             (exists)))
                   (string "Testbild fehlt: {ppm}"))
          "// Wie der Server: festen 640×640-Ausschnitt verwenden."
          (let ((full (dot (dot (image--open (ref ppm))
                                (unwrap))
                           (to_rgb8)))
                (img (dot (image--imageops--crop_imm (ref full) 0 0 640 640)
                          (to_image))))
            (assert_eq! (paren (dot img (width)) (dot img (height)))
                        (paren 640 640))
            (let ((ocr (dot (Ocr--load (ref dir) 8)
                            (unwrap)))
                  (texts (dot (dot ocr (text (ref img)))
                              (unwrap)))
                  (chars (dot (dot (dot texts (iter))
                                   (map (lambda (t)
                                          (dot (dot (dot t text)
                                                    (chars))
                                               (count)))))
                              (sum))))
              (declare (mutable ocr)
                       (type usize chars))
              (assert! (not (dot texts (is_empty)))
                       (string "kein Text auf dem Testbild erkannt"))
              (assert! (> chars 20)
                       (string "zu wenig Text: {texts:?}"))
              (eprintln! (string "[models] {} Zeilen, {} Zeichen")
                         (dot texts (len))
                         chars)
              (for (t (dot (dot texts (iter)) (take 5)))
                (eprintln! (string "[models] {:?} {:?}")
                           (dot t rect)
                           (dot t text))))))))))

(defun test-at-init (vec)
  "`|p: u16| ...` lookup closure over apad/value table (padding sweep)."
  `(lambda ("p: u16")
     (dot (dot (dot (dot ,vec (iter))
                        (find (lambda ((paren q _))
                                (== (deref q) p))))
               (unwrap))
          1)))

(defun server-test-padding-rs ()
  `(do0
    ,(doc "Padding-Sweep (ignored): misst gute REC_PAD-/MASK_PAD-Werte auf einem"
          "echten xterm-Screenshot. Startet ein eigenes Xvfb + xterm, braucht echte"
          "Modelle und läuft nur im Release-Modus sinnvoll schnell:"
          "`cargo test --release -p lbw-server --test padding -- --ignored --nocapture`"
          ""
          "Der Test druckt zwei Tabellen (Erkennung je `pad`, AV1-Bytes je"
          "Masken-`pad`) und pinnt das Produktverhalten: Der ASCII-Marker muss über"
          "den echten [`Ocr`]-Pfad gefunden werden, und die Default-Paddings dürfen"
          "nicht schlechter sein als `0`.")
    ,*blank*
    (use (std process (curly Child Command)))
    (use (std time Duration))
    ,*blank*
    (use (clap Parser))
    (use (image RgbImage))
    (use (lbw_server av1 encode_rgb))
    (use (lbw_server capture (curly FrameSource ScrapSource)))
    (use (lbw_server config Config))
    (use (lbw_server ocr (curly Detector Ocr Recognizer sample_colors)))
    (use (lbw_server tiles (curly MASK_PAD crop_rgb fill_rect pad_rect)))
    ,*blank*
    "/// Was das xterm anzeigt (ASCII-Anteil wird assertiert, Umlaute nur berichtet)."
    (space "const MARKER_ASCII: &str =" "\"PADDING-SWEEP-640\";")
    "/// Display für das eigene Xvfb (muss frei sein)."
    (space "const DISPLAY: &str =" "\":97\";")
    "/// Wie `Ocr::text` (dort privat): Mindest-Konfidenz je Zeile."
    (space "const MIN_CONF: f32 =" "0.5;")
    ,*blank*
    ,(test-models-dir-fn)
    ,*blank*
    "/// Eigenes Xvfb + xterm; räumt beim Drop auf."
    (defstruct0 Xterm
      (xvfb Child)
      (xterm Child))
    ,*blank*
    (impl Xterm
      (defun start ()
        (declare (values Self))
        "// PADDING_XTERM_EXTRA=\"...\" hängt weitere xterm-Argumente an (z. B."
        "// \"-fa Monospace -fs 14\" für einen zweiten Messpunkt mit Skalierfont)."
        (let ((xvfb (dot (dot (dot (Command--new (string "Xvfb"))
                                       (args (bracket DISPLAY
                                                      ,@(loop for a in '("-screen" "0" "1280x1024x24")
                                                              collect (list 'string a)))))
                                  (spawn))
                             (expect (string "Xvfb fehlt (apt install xvfb) oder Display belegt"))))
              (line (format! (string "echo '{MARKER_ASCII} ÄÖÜäöüß'; exec sleep 120")))
              (extra (dot (std--env--var (string "PADDING_XTERM_EXTRA"))
                          (unwrap_or_default)))
              (args (vec! ,@(loop for a in '("-u8" "-geometry" "80x24+10+10")
                                                collect (list 'string a)))))
          (declare (mutable xvfb args))
          (std--thread--sleep (Duration--from_secs 1))
          (dot args (extend (dot extra (split_whitespace))))
          (dot args (extend (bracket ,@(loop for a in '("-e" "sh" "-c")
                                                            collect (list 'string a))
                                                   (ref line))))
          (let ((xterm (dot (dot (dot (dot (dot (Command--new (string "xterm"))
                                                (args (ref args)))
                                           (env (string "DISPLAY") DISPLAY))
                                      (env (string "LANG") (string "C.UTF-8")))
                                 (spawn))
                            (expect (string "xterm fehlt (apt install xterm)")))))
            (std--thread--sleep (Duration--from_secs 2))
            (when (dot (dot (dot xvfb (try_wait))
                            (expect (string "Xvfb-Status")))
                       (is_some))
              (panic! (string "Xvfb startete nicht (Display {DISPLAY} belegt?)")))
            (make-instance Self xvfb xterm))))
      ,*blank*
      (defun capture ("&self")
        (declare (values RgbImage))
        (unsafe (std--env--set_var (string "DISPLAY") DISPLAY))
        (let ((src (dot (ScrapSource--open 0 0 640 640)
                        (expect (string "Capture öffnen")))))
          (declare (mutable src))
          (dot (dot src (grab))
               (expect (string "Frame lesen"))))))
    ,*blank*
    (impl (space Drop for Xterm)
      (defun drop ("&mut self")
        (let ((_ (dot (dot self xterm) (kill)))
              (_ (dot (dot self xvfb) (kill)))))))
    ,*blank*
    (attr "test" "ignore"
      (defun sweep_padding_on_xterm ()
        (let ((dir (models_dir)))
          (for (f (bracket (string "PP-OCRv6_small_det.onnx")
                           (string "PP-OCRv6_small_rec.onnx")
                           (string "inference.yml")))
            (assert! (dot (std--path--Path--new (ref (format! (string "{dir}/{f}"))))
                           (exists))
                     (string "Modell fehlt: {dir}/{f}")))
          (let ((xt (Xterm--start))
                (img (dot xt (capture))))
            (dot (dot img (save (string "/tmp/padding_frame.png")))
                 (unwrap))
            (eprintln! (string "[padding] Capture nach /tmp/padding_frame.png geschrieben"))
            "// Produktpfad (mit REC_PAD-Default): ASCII-Marker muss gefunden werden."
            (let ((ocr (dot (Ocr--load (ref dir) 4)
                            (unwrap)))
                  (items (dot (dot ocr (text (ref img)))
                              (unwrap)))
                  (joined (dot (dot (dot (dot items (iter))
                                             (map (lambda (t)
                                                    (dot (dot t text)
                                                         (as_str)))))
                                        ("collect::<Vec<_>>"))
                                   (join (string " ")))))
              (declare (mutable ocr))
              (eprintln! (string "[padding] Produkt: {} Zeilen: {joined:?}")
                         (dot items (len)))
              (assert! (dot joined (contains MARKER_ASCII))
                       (string "Marker {MARKER_ASCII:?} nicht erkannt in {joined:?}"))
              "// Erkennungs-Sweep: Detektion einmal, Recognition je pad."
              (let ((det (dot (Detector--new (ref (format! (string "{dir}/PP-OCRv6_small_det.onnx"))) 4)
                              (unwrap)))
                    (rec (dot (Recognizer--new (ref (format! (string "{dir}/PP-OCRv6_small_rec.onnx")))
                                               (ref (format! (string "{dir}/inference.yml")))
                                               4)
                              (unwrap)))
                    (boxes (dot (dot det (detect (ref img)))
                                (unwrap)))
                    (rec_chars (Vec--new)))
                (declare (mutable det rec rec_chars))
                (assert! (not (dot boxes (is_empty)))
                         (string "keine Box detektiert"))
                (eprintln! (string "[padding] {} Boxen detektiert")
                           (dot boxes (len)))
                (for (pad (bracket "0u16" 2 4 6 8))
                  (let ((n 0)
                        (umlauts 0))
                    (declare (mutable n umlauts))
                    (for (b (ref boxes))
                      (let ((r (pad_rect (deref b) pad 640 640))
                            ((paren t c) (dot (dot rec (recognize (ref img) r))
                                              (unwrap))))
                        (when (and (not (dot (dot t (trim)) (is_empty)))
                                   (>= c MIN_CONF))
                          (incf n (dot (dot t (chars)) (count)))
                          (incf umlauts
                                (dot (dot (dot t (chars))
                                          (filter (lambda (c)
                                                    (dot (string "ÄÖÜäöüß")
                                                         (contains (deref c))))))
                                     (count))))))
                    (dot rec_chars (push (paren pad n)))
                    (eprintln! (string "[padding] rec_pad={pad}: {n} Zeichen, davon {umlauts} Umlaute/ß"))))
                (let ((at ,(test-at-init 'rec_chars)))
                  (assert! (>= (at 4) (at 0))
                           (string "REC_PAD=4 schlechter als 0: {rec_chars:?}"))
                  "// Masken-Sweep: Textregion maskieren, Rest als AV1 messen."
                  "// Echter Server-Default statt hartkodierter Zahl."
                  (let ((quantizer (dot (dot (Config--try_parse_from (bracket (string "lbw-server")))
                                             (unwrap))
                                        quantizer))
                        (mask_bytes (Vec--new)))
                    (declare (mutable mask_bytes))
                    (for (mpad (bracket "0u16" 2 4 6 8 10 12))
                      (let ((masked (dot img (clone))))
                        (declare (mutable masked))
                        (for (b (ref boxes))
                          (let ((r (pad_rect (deref b) 4 640 640))
                                ((paren _ bg) (sample_colors (ref img) r)))
                            (fill_rect (ref-mut masked)
                                       (pad_rect r mpad 640 640)
                                       bg)))
                        "// Feste Vergleichsregion: Union aller Boxen (mit Max-Pad), gerade."
                        (let ((x0 "640u16")
                              (y0 "640u16")
                              (x1 "0u16")
                              (y1 "0u16"))
                          (declare (mutable x0 y0 x1 y1))
                          (for (b (ref boxes))
                            (let ((r (pad_rect (deref b) 16 640 640)))
                              ,@(loop for (v op e) in '((x0 min (dot r x))
                                                        (y0 min (dot r y))
                                                        (x1 max (+ (dot r x) (dot r w)))
                                                        (y1 max (+ (dot r y) (dot r h))))
                                      collect (minmax-assign v op e))))
                          (let ((w (dot (coerce (+ (- x1 x0) 1) usize)
                                        (max 16)))
                                (h (dot (coerce (+ (- y1 y0) 1) usize)
                                        (max 16))))
                            (let (((paren w h) (paren (+ w (% w 2)) (+ h (% h 2))))
                                  (r (lbw_common--Rect--new (dot x0 (min (- 640 (coerce w u16))))
                                                            (dot y0 (min (- 640 (coerce h u16))))
                                                            (coerce w u16)
                                                            (coerce h u16)))
                                  (rgb (crop_rgb (ref masked) r))
                                  (bytes (dot (dot (encode_rgb (ref rgb) w h quantizer)
                                                   (unwrap))
                                              (len))))
                                (dot mask_bytes (push (paren mpad bytes)))
                                (eprintln! (string "[padding] mask_pad={mpad}: Region {w}x{h} = {bytes} B AV1")))))))
                    (let ((at ,(test-at-init 'mask_bytes)))
                      (assert! (<= (at MASK_PAD) (at 0))
                               (string "MASK_PAD schlechter als 0: {mask_bytes:?}")))))))))))))

(defun server-lib-rs ()
  `(do0
    ,(doc "`lbw-server` — Bildschirm-Capture, OCR-Text, AV1-Kacheln, direktes TCP."
          "Nur Modul-Deklarationen.")
    ,*blank*
    ,@(loop for (file name) in '(("01_config.rs" "config")
                                 ("02_capture.rs" "capture")
                                 ("03_ocr.rs" "ocr")
                                 ("04_tiles.rs" "tiles")
                                 ("05_av1.rs" "av1")
                                 ("06_input.rs" "input")
                                 ("07_session.rs" "session"))
            append `((attr ,(format nil "path = ~s" file)
                        (space "pub" ,(format nil "mod ~a;" name)))
                     ,*blank*))))

(defun server-main-rs ()
  `(do0
    ,(doc "`lbw-server` — nur Verdrahtung: Konfiguration → Quelle, OCR-Modelle,"
          "TCP-Accept, Session-Schleife (ein Client nach dem anderen).")
    ,*blank*
    (use (std net TcpListener))
    ,*blank*
    (use (clap Parser))
    (use (lbw_common SIZE))
    ,*blank*
    (use (lbw_server capture ScrapSource))
    (use (lbw_server config Config))
    (use (lbw_server ocr Ocr))
    (use (lbw_server session serve_client))
    ,*blank*
    "/// ONNX-Threads (fest: MVP ohne `--threads`)."
    (space "const OCR_THREADS: usize =" "8;")
    ,*blank*
    (defun main ()
      (let ((cfg (Config--parse)))
        (if-let ((Err e) (dot cfg (validate)))
          (progn
            (eprintln! (string "{e}"))
            (std--process--exit 2)))
        (if-let ((Err e) (run cfg))
          (progn
            (eprintln! (string "lbw-server: {e}"))
            (std--process--exit 1)))))
    ,*blank*
    (defun run (cfg)
      (declare (type Config cfg)
               (values "Result<(), String>"))
      (when (dot cfg (is_public))
        (eprintln! (string "WARNUNG: {} ist nicht localhost — das Protokoll hat keine Authentifizierung!")
                   (dot cfg listen)))
      (when (or (dot (std--env--var (string "WAYLAND_DISPLAY")) (is_ok))
                (dot (std--env--var (string "XDG_SESSION_TYPE"))
                     (is_ok_and (lambda (t) (== t (string "wayland"))))))
        (eprintln! (string "WARNUNG: Wayland-Sitzung erkannt — X11-Capture sieht dort nur Schwarz. Für echte Bildschirminhalte eine Xorg-Sitzung verwenden!")))
      (let ((src (? (ScrapSource--open (dot cfg x) (dot cfg y) SIZE SIZE))))
        (declare (mutable src))
        (let ((ocr (? (Ocr--load (ref (dot cfg models)) OCR_THREADS))))
          (declare (mutable ocr))
          (let ((listener (? (dot (TcpListener--bind (ref (dot cfg listen)))
                                   (map_err (lambda (e)
                                              (format! (string "{}: {e}")
                                                       (dot cfg listen))))))))
            (eprintln! (string "[server] lauscht auf {} ({}x{}@{},{}, q={})")
                       (dot cfg listen) SIZE SIZE (dot cfg x) (dot cfg y)
                       (dot cfg quantizer))
            (for (stream (dot listener (incoming)))
              (case stream
                ((Ok s)
                 (eprintln! (string "[server] Client verbunden"))
                 (if-let ((Err e) (serve_client s (ref cfg) "&mut src" "&mut ocr" None))
                   (eprintln! (string "[server] Session-Fehler: {e}")))
                 (eprintln! (string "[server] Client getrennt")))
                ((Err e) (eprintln! (string "[server] Accept: {e}")))))
            (Ok (paren))))))))
