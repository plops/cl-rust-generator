(load (merge-pathnames "00_util.lisp" *load-pathname*))

(in-package :cl-rust-generator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case *readtable*) :invert))

;;;; server.lisp --- lbw-server: manifest, config, capture, av1, input, tiles, session, lib, main.

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
      '(attr "test"
         (defun options_are_parsed ()
           (let ((c (dot (Config--try_parse_from
                          (bracket (string "lbw-server")
                                   (string "--listen") (string "0.0.0.0:9")
                                   (string "--x") (string "10")
                                   (string "--y") (string "20")
                                   (string "--quantizer") (string "99")
                                   (string "--models") (string "/m")
                                   (string "-v")))
                         (unwrap))))
             (assert! (dot c (is_public)))
             (assert_eq! (paren (dot c x) (dot c y) (dot c quantizer))
                         (paren 10 20 99))
             (assert_eq! (dot c models) (string "/m"))
             (assert! (dot c verbose))
             (dot (dot c (validate)) (unwrap)))))
      *blank*
      '(attr "test"
         (defun bad_values_are_rejected ()
           (progn "// Unbekannte Option scheitert schon beim Parsen."
             (assert! (dot (Config--try_parse_from (bracket (string "lbw-server") (string "--bogus"))) (is_err))))
           (assert! (dot (Config--try_parse_from (bracket (string "lbw-server") (string "--no-ocr"))) (is_err)))
           (assert! (dot (Config--try_parse_from (bracket (string "lbw-server") (string "--dump"))) (is_err)))
           (assert! (dot (Config--try_parse_from (bracket (string "lbw-server") (string "--no-input"))) (is_err)))
           (assert! (dot (Config--try_parse_from (bracket (string "lbw-server") (string "--size"))) (is_err)))
           (assert! (dot (Config--try_parse_from (bracket (string "lbw-server") (string "--quantizer") (string "x"))) (is_err)))
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
              (dot out (put_pixel dx dy (image--Rgb (bracket (aref f (+ s 2))
                                                                     (aref f (+ s 1))
                                                                     (aref f s))))))))
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
    ,(pub_ '(defun encode_rgb (rgb w h quantizer)
              (declare (type "&[u8]" rgb)
                       (type usize w h quantizer)
                       (values "Result<Vec<u8>, String>"))
              (when (or (< w MIN_TILE) (< h MIN_TILE)
                        (not (dot w (is_multiple_of 2)))
                        (not (dot h (is_multiple_of 2))))
                (return (Err (format! (string "ungültige Boxgröße {w}x{h}")))))
              (let ((yuv (rgb_to_yuv420 rgb w h)))
                (let* ((enc (EncoderConfig--with_speed_preset 10)))
                  (= (dot enc width) w)
                  (= (dot enc height) h)
                  (= (dot enc bit_depth) 8)
                  (= (dot enc chroma_sampling) (scope ChromaSampling Cs420))
                  (= (dot enc pixel_range) (scope PixelRange Full))
                  (= (dot enc still_picture) true)
                  (= (dot enc low_latency) true)
                  (= (dot enc quantizer) (dot quantizer (min 255)))
                  (= (dot enc min_quantizer) (coerce (dot quantizer (min 255)) u8))
                  (= (dot enc max_key_frame_interval) 1)
                  (let ((cfg (dot (Config--new)
                                   (with_encoder_config enc)
                                   (with_threads 4))))
                    (let ((ctx (? (dot (dot cfg (new_context))
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
                          (Ok out)))))))))
    ,*blank*
    ,(testmod
      "use super::*;"
      *blank*
      '(attr "test"
         (defun flat_tile_is_tiny ()
           (let ((rgb (dot "[40u8, 80, 160]" (repeat (* 64 64)))))
             (let ((bytes (dot (encode_rgb (ref rgb) 64 64 180) (unwrap))))
               (assert! (and (not (dot bytes (is_empty)))
                             (< (dot bytes (len)) 200))
                        (string "{} Byte")
                        (dot bytes (len)))))))
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
        ((Ok ms) (let ((o (pick_origin (ref ms))))
                   (eprintln! (string "[input] Monitor-Ursprung {:+}{:+}")
                              (dot o 0) (dot o 1))
                   o))
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
      '(attr "test"
         (defun primary_monitor_origin_wins ()
           (progn "// Vier-Monitor-Layout: primärer Monitor rechts, hochkant."
             (let ((ms (bracket (paren 4200 0 true)
                                (paren 0 0 false)
                                (paren 3120 0 false)
                                (paren 1920 0 false))))
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
      ,(pub_ '(defun dirty_bbox (prev cur)
                (declare (type "Option<&RgbImage>" prev)
                         (type "&RgbImage" cur)
                         (values "Option<Rect>"))
                (let (((paren w h) (dot cur (dimensions))))
                  (let ((prev (case prev
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
                    (let ((stride (* (coerce w usize) 3)))
                      (let (((paren a b) (paren (dot prev (as_raw))
                                                (dot cur (as_raw)))))
                        "let (mut x0, mut x1, mut y0, mut y1) = (w, 0, h, 0);"
                          (for (y (range 0 h))
                            (let ((s (* (coerce y usize) stride)))
                              (let (((paren ra rb)
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
                                      (break)))))))
                          (when (> x0 x1)
                            (return None))
                          "// Auf Mindestgröße und gerade Kanten erweitern, im Bild halten."
                          (let ((t (coerce MIN_TILE u32)))
                            (let ((bw (dot (dot (paren (+ (- x1 x0) 1)) (max t)) (min w)))
                                  (bh (dot (dot (paren (+ (- y1 y0) 1)) (max t)) (min h))))
                              (declare (mutable bw bh))
                              (incf bw (logand bw 1))
                              (incf bh (logand bh 1))
                              (let (((paren bw bh) (paren (dot bw (min w))
                                                          (dot bh (min h)))))
                                (let ((bx (dot x0 (min (- w bw))))
                                      (by (dot y0 (min (- h bh)))))
                                  (Some (Rect--new (coerce bx u16)
                                                   (coerce by u16)
                                                   (coerce bw u16)
                                                   (coerce bh u16))))))))))))))
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
      ,(pub_ '(defun pad_rect (r pad w h)
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
                        (Rect--new (coerce x0 u16)
                                   (coerce y0 u16)
                                   (coerce (- x1 x0) u16)
                                   (coerce (- y1 y0) u16)))))))))
    ,*blank*
    "/// Füllt `r` (aufs Bild begrenzt) mit `c` — für die Text-Maskierung."
    ,(pub_ '(defun fill_rect (img r c)
              (declare (type "&mut RgbImage" img)
                       (type Rect r)
                       (type "[u8; 3]" c))
              (let (((paren w h) (dot img (dimensions))))
                (let ((x0 (dot (u32--from (dot r x)) (min w))))
                  (let ((y0 (dot (u32--from (dot r y)) (min h))))
                    (let ((x1 (dot (+ x0 (u32--from (dot r w))) (min w))))
                      (let ((y1 (dot (+ y0 (u32--from (dot r h))) (min h))))
                        (let ((px (image--Rgb c)))
                          (for (y (range y0 y1))
                            (for (x (range x0 x1))
                              (dot img (put_pixel x y px))))))))))))
    ,*blank*
    "/// Rechteck als RGB-Bytes (Zeile für Zeile, ohne Padding)."
    (attr "must_use"
      ,(pub_ '(defun crop_rgb (img r)
                (declare (type "&RgbImage" img)
                         (type Rect r)
                         (values "Vec<u8>"))
                (let ((stride (* (dot img (width)) 3)))
                  (let ((raw (dot img (as_raw))))
                    (let ((out (Vec--with_capacity
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
                      out))))))
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
           (let ((a (solid 128 128 (bracket "7; 3"))))
             (let ((b (solid 128 128 (bracket "7; 3"))))
               (assert_eq! (dirty_bbox (Some (ref a)) (ref b)) None)))))
      *blank*
      '(attr "test"
         (defun scattered_pixels_yield_single_box ()
           (let ((a (solid 128 128 (bracket "0; 3"))))
             (let ((b (dot a (clone))))
               (declare (mutable b))
               (dot b (put_pixel 10 10 (image--Rgb (bracket "9; 3"))))
               (dot b (put_pixel 100 100 (image--Rgb (bracket "9; 3"))))
               (progn "// 10..=100 → 91 px, auf gerade Kanten erweitert."
                 (assert_eq! (dirty_bbox (Some (ref a)) (ref b))
                             (Some (Rect--new 10 10 92 92))))))))
      *blank*
      '(attr "test"
         (defun single_pixel_is_padded_to_minimum ()
           (let ((a (solid 128 128 (bracket "0; 3"))))
             (let ((b (dot a (clone))))
               (declare (mutable b))
               (dot b (put_pixel 5 5 (image--Rgb (bracket "9; 3"))))
               (assert_eq! (dirty_bbox (Some (ref a)) (ref b))
                           (Some (Rect--new 5 5 16 16)))))))
      *blank*
      '(attr "test"
         (defun box_clamps_at_image_edge ()
           (let ((a (solid 128 128 (bracket "0; 3"))))
             (let ((b (dot a (clone))))
               (declare (mutable b))
               (dot b (put_pixel 127 127 (image--Rgb (bracket "9; 3"))))
               (assert_eq! (dirty_bbox (Some (ref a)) (ref b))
                           (Some (Rect--new 112 112 16 16)))))))
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
                                 (map_err (lambda (e) (dot e (to_string))))))))
                (declare (mutable rd))
                (let ((wr stream))
                  (declare (mutable wr))
                  (let ((fr (FrameReader--new)))
                    (declare (mutable fr))
                    (? (handshake (ref-mut rd) (ref-mut fr) (ref-mut wr)))
                    "// Eingaben laufen in eigenem Thread, damit Tippen nie auf AV1 wartet."
                    (? (dot (dot rd (set_read_timeout
                                     (Some (Duration--from_millis 200))))
                            (map_err (lambda (e) (dot e (to_string))))))
                    (let ((stop (Arc--new (AtomicBool--new false))))
                      (let ((input (case (Injector--open (paren (dot cfg x) (dot cfg y)))
                                     ((Ok inj) (Some (spawn_input rd fr inj
                                                                 (dot stop (clone))
                                                                 (dot cfg verbose))))
                                     ((Err e)
                                      (eprintln! (string "[input] {e} — laufe ohne Eingabe"))
                                      None))))
                        (let ((prev None))
                          (declare (type "Option<RgbImage>" prev)
                                   (mutable prev))
                          (let ((last_texts (Vec--new)))
                            (declare (type "Vec<TextItem>" last_texts)
                                     (mutable last_texts))
                            (let ((frames 0))
                              (declare (type u64 frames)
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
                              result)))))))))))
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
                 (None (return (Ok (scope TileOut Same)))))))
        (let ((rgb (crop_rgb masked r)))
          (let ((data (? (encode_rgb (ref rgb)
                                     (coerce (dot r w) usize)
                                     (coerce (dot r h) usize)
                                     quantizer))))
            (let ((msg (make-instance ServerMsg--Tile
                                      :x (dot r x)
                                      :y (dot r y)
                                      data)))
              (case (write_msg wr (ref msg))
                ((Ok n) (Ok (TileOut--Sent n)))
                ((Err _) (Ok (scope TileOut Gone)))))))))
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
