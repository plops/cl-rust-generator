(load (merge-pathnames "00_util.lisp" *load-pathname*))

(in-package :cl-rust-generator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case *readtable*) :invert))

;;;; server_a.lisp --- lbw-server part 1: config, capture, av1, input.

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

