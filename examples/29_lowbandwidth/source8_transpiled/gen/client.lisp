(load (merge-pathnames "00_util.lisp" *load-pathname*))

(in-package :cl-rust-generator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case *readtable*) :invert))

;;;; client.lisp --- lbw-client: manifest, config, av1, net, scene, lib
;;;; (T6 adds app, main, probe, loopback test).

(defun client-cargo-toml ()
  "[package]
name = \"lbw-client\"
version.workspace = true
edition.workspace = true
authors.workspace = true
license.workspace = true

[dependencies]
clap = { version = \"4.6.7\", features = [\"derive\"] }
lbw-common = { path = \"../common\" }
macroquad = \"0.4.16\"
rav1d = { version = \"1.1.0\", default-features = false, features = [\"bitdepth_8\"] }

[dev-dependencies]
lbw-server = { path = \"../server\" }
")

(defun client-config-rs ()
  `(do0
    ,(doc "`01_config` — Kommandozeile des MVP-Clients (clap-derive).")
    ,*blank*
    (use (clap Parser))
    ,*blank*
    ,(clap-struct
      '("`lbw-client` — Minimal Low-Bandwidth Remote Desktop Client (MVP)."
        "Das Bild ist immer 640×640 (F1: HUD an/aus).")
      '("derive(Clone, Debug, Parser)"
        "command(name = \"lbw-client\", version)")
      "Config"
      '(("Server (typ. Ende von `ssh -L`)."
         "arg(long, default_value = \"127.0.0.1:7878\")"
         "connect" "String")))
    ,*blank*
    ,(testmod
      "use super::*;"
      *blank*
      '(attr "test"
         (defun defaults_and_options ()
           (let ((c (dot (Config--try_parse_from (bracket (string "lbw-client"))) (unwrap))))
             (assert_eq! (dot c connect) (string "127.0.0.1:7878"))
             (let ((c (dot (Config--try_parse_from
                            (bracket (string "lbw-client") (string "--connect") (string "h:1")))
                           (unwrap))))
               (assert_eq! (dot c connect) (string "h:1"))
               (assert! (dot (Config--try_parse_from
                              (bracket (string "lbw-client") (string "--bogus")))
                             (is_err))))))))))

(defun client-av1-rs ()
  `(do0
    ,(doc "`02_av1` — AV1-Decoder (rav1d, reines Rust) für Still-Picture-Kacheln."
          ""
          "Aus `source6/client/02_av1.rs` übernommen: rav1d exportiert die"
          "dav1d-C-API als Rust-Funktionen; dieser Wrapper kapselt das `unsafe`"
          "an einer Stelle und liefert RGBA8.")
    ,*blank*
    (use (std ptr NonNull))
    ,*blank*
    (use (lbw_common yuv yuv420_to_rgba))
    (use (rav1d include dav1d data Dav1dData))
    (use (rav1d include dav1d dav1d (curly Dav1dContext Dav1dSettings)))
    (use (rav1d include dav1d headers DAV1D_PIXEL_LAYOUT_I420))
    (use (rav1d include dav1d picture Dav1dPicture))
    (use (rav1d src lib (curly dav1d_close dav1d_data_create dav1d_data_unref dav1d_default_settings dav1d_get_picture dav1d_open dav1d_picture_unref dav1d_send_data)))
    ,*blank*
    "/// Dekodiertes Bild."
    (attr "derive(Debug)"
      ,(pub_ '(defstruct0 Rgba
                ("pub w" usize)
                ("pub h" usize)
                ("pub data" "Vec<u8>"))))
    ,*blank*
    "/// `-EAGAIN` der dav1d-API (Linux: EAGAIN = 11)."
    (space "const EAGAIN: i32 =" "-11;")
    ,*blank*
    "/// Einmal geöffneter Decoder; wird für jede Kachel wiederverwendet."
    ,(pub_ '(defstruct0 Decoder
              (ctx "Option<Dav1dContext>")))
    ,*blank*
    "// SAFETY: Der Kontext wird nur über `&mut self` benutzt (kein geteilter Zugriff)."
    (space unsafe (impl (space Send for Decoder)))
    ,*blank*
    (impl Decoder
      "/// Öffnet rav1d (1 Thread genügt für 640²)."
      ,(pub_ '(defun new ()
                (declare (values "Result<Self, String>"))
                (let ((s ("std::mem::MaybeUninit::<Dav1dSettings>::uninit")))
                  (declare (mutable s))
                  "// SAFETY: `s` ist gültig beschreibbar; danach initialisiert."
                  (let ((s (unsafe
                            (dav1d_default_settings
                             (dot (NonNull--new (dot s (as_mut_ptr))) (unwrap)))
                            (dot s (assume_init)))))
                    (declare (mutable s))
                    (setf (dot s n_threads) 1)
                    (setf (dot s max_frame_delay) 1)
                    (let ((ctx None))
                      (declare (mutable ctx))
                      "// SAFETY: Zeiger auf lokale, gültige Werte."
                      (let ((r (unsafe (dav1d_open (Some (NonNull--from (ref-mut ctx)))
                                                   (Some (NonNull--from (ref-mut s)))))))
                        (when (or (!= (dot r 0) 0) (dot ctx (is_none)))
                          (return (Err (format! (string "dav1d_open: {}") (dot r 0)))))
                        (Ok (make-instance Self ctx))))))))
      ,*blank*
      "/// Dekodiert eine Kachel (rohe OBUs eines Still-Pictures) zu RGBA8."
      ,(pub_ '(defun decode ("&mut self" "obu: &[u8]")
                (declare (values "Result<Rgba, String>"))
                (when (dot obu (is_empty))
                  (return (Err (dot (string "leere Kachel") (into)))))
                "// `Dav1dContext` ist ein `Copy`-Handle (roher Arc-Zeiger)."
                (let ((ctx (dot self ctx)))
                  (let ((data (Dav1dData--default)))
                    (declare (mutable data))
                    "// SAFETY: `data` ist gültig beschreibbar; Puffer hat `obu.len()` Byte."
                    (unsafe
                     (let ((p (dav1d_data_create (Some (NonNull--from (ref-mut data)))
                                                 (dot obu (len)))))
                       (when (dot p (is_null))
                         (return (Err (dot (string "dav1d_data_create") (into)))))
                       (std--ptr--copy_nonoverlapping (dot obu (as_ptr)) p (dot obu (len)))))
                    (let ((pic (Dav1dPicture--default)))
                      (declare (mutable pic))
                      (let ((got false))
                        (declare (mutable got))
                        "// Senden bis alles verbraucht ist; dazwischen Bilder abholen."
                        (for (_ (range 0 16))
                          (when (> (dot data sz) 0)
                            "// SAFETY: `ctx` stammt aus `dav1d_open`; `data` ist gültig."
                            (let ((r (unsafe (dav1d_send_data ctx (Some (NonNull--from (ref-mut data)))))))
                              (when (and (!= (dot r 0) 0) (!= (dot r 0) EAGAIN))
                                "// SAFETY: `data` ist gültig."
                                (unsafe (dav1d_data_unref (Some (NonNull--from (ref-mut data)))))
                                (return (Err (format! (string "dav1d_send_data: {}") (dot r 0)))))))
                          "// SAFETY: `ctx` gültig, `pic` beschreibbar."
                          (let ((r (unsafe (dav1d_get_picture ctx (Some (NonNull--from (ref-mut pic)))))))
                            (when (== (dot r 0) 0)
                              (setf got true)
                              (break))
                            (when (!= (dot r 0) EAGAIN)
                              "// SAFETY: `data` ist gültig."
                              (unsafe (dav1d_data_unref (Some (NonNull--from (ref-mut data)))))
                              (return (Err (format! (string "dav1d_get_picture: {}") (dot r 0)))))
                            (when (== (dot data sz) 0)
                              (break))))
                        "// SAFETY: `data` ist gültig (evtl. schon leer)."
                        (unsafe (dav1d_data_unref (Some (NonNull--from (ref-mut data)))))
                        (when (not got)
                          (return (Err (dot (string "kein Bild dekodiert") (into)))))
                        (let ((out (picture_to_rgba (ref pic))))
                          "// SAFETY: `pic` wurde von `dav1d_get_picture` gefüllt."
                          (unsafe (dav1d_picture_unref (Some (NonNull--from (ref-mut pic)))))
                          out))))))))
    ,*blank*
    (defun picture_to_rgba ("pic: &Dav1dPicture")
      (declare (values "Result<Rgba, String>"))
      (let ((w (coerce (dot pic p w) usize)))
        (let ((h (coerce (dot pic p h) usize)))
          (when (or (!= (dot pic p bpc) 8)
                    (!= (dot pic p layout) DAV1D_PIXEL_LAYOUT_I420))
            (return (Err (format! (string "nicht unterstützt: bpc {} layout {}")
                                  (dot pic p bpc) (dot pic p layout)))))
          (let ((ys (coerce (aref (dot pic stride) 0) usize)))
            (let ((cs (coerce (aref (dot pic stride) 1) usize)))
              (let ((ch (dot h (div_ceil 2))))
                (let ((cw (dot w (div_ceil 2))))
                  (let ((plane (lambda (i len)
                                 (declare (type usize i) (type usize len)
                                          (values "Result<&[u8], String>"))
                                 (let ((p (? (dot (aref (dot pic data) i)
                                                   (ok_or (string "fehlende Ebene"))))))
                                   "// SAFETY: dav1d garantiert `stride * Zeilen` gültige Bytes je Ebene."
                                   (Ok (unsafe (std--slice--from_raw_parts
                                                (coerce (dot p (as_ptr)) "*const u8")
                                                len)))))))
                    (let ((y (? (plane 0 (+ (* ys (- h 1)) w)))))
                      (let ((u (? (plane 1 (+ (* cs (- ch 1)) cw)))))
                        (let ((v (? (plane 2 (+ (* cs (- ch 1)) cw)))))
                          (let ((data (space "vec!" (bracket (space "0u8" ";" (* w h 4))))))
                            (declare (mutable data))
                            (yuv420_to_rgba y ys u v cs w h (ref-mut data))
                            (Ok (make-instance Rgba w h data))))))))))))))
    ,*blank*
    (impl (space Drop for Decoder)
      (defun drop ("&mut self")
        (progn
          "// SAFETY: `ctx` stammt aus `dav1d_open` und wird hier genau einmal geschlossen."
          (unsafe (dav1d_close (Some (NonNull--from (ref-mut (dot self ctx)))))))))
    ,*blank*
    ,(testmod
      "use super::*;"
      *blank*
      '(attr "test"
         (defun garbage_is_an_error_not_a_crash ()
           (let ((d (dot (Decoder--new) (unwrap))))
             (declare (mutable d))
             (assert! (dot (dot d (decode (ref (bracket)))) (is_err)))
             (assert! (dot (dot d (decode (ref (bracket "0x12" "0x00" "0xff" "0xff" "0x01"))))
                           (is_err)))))))))

(defun client-net-rs ()
  `(do0
    ,(doc "`03_net` — Verbindung zum Server mit automatischem Reconnect."
          ""
          "Ein Netz-Thread verbindet (Backoff 0,5 → 5 s), sendet `Hello`, liest"
          "Frames, dekodiert AV1-Kacheln und reicht fertige Ereignisse an die UI."
          "Ausgehende Eingaben fließen über einen Kanal (max. ~50 ms Verzögerung)."
          "`drop(Net)` beendet Thread und Verbindung. Kein Heartbeat im MVP:"
          "Neuaufbau nur bei TCP-Fehler (Stille ist bei statischem Bild normal).")
    ,*blank*
    (use (std net TcpStream))
    (use (std sync Arc))
    (use (std sync atomic (curly AtomicBool Ordering)))
    (use (std sync mpsc (curly Receiver Sender channel)))
    (use (std time Duration))
    ,*blank*
    (use (lbw_common framing (curly FrameReader Read1 decode_msg write_msg)))
    (use (lbw_common (curly ClientMsg PROTO_VERSION ServerMsg TextItem)))
    ,*blank*
    (use (crate av1 Decoder))
    ,*blank*
    "/// Ereignisse an die UI (`Tile`: dekodierte AV1-Box, RGBA8)."
    (attr "derive(Debug)"
      ,(pub_ '(defenum Event
                Connected
                (Disconnected String)
                ClearText
                (AddText TextItem)
                (space Tile (curly "x: u16" "y: u16" "w: usize" "h: usize" "rgba: Vec<u8>" "bytes: usize")))))
    ,*blank*
    "/// Griff der UI auf den Netz-Thread."
    ,(pub_ '(defstruct0 Net
              ("pub events" "Receiver<Event>")
              ("out" "Sender<ClientMsg>")
              ("stop" "Arc<AtomicBool>")))
    ,*blank*
    (impl (space Drop for Net)
      (defun drop ("&mut self")
        (dot (dot self stop) (store true (scope Ordering Relaxed)))))
    ,*blank*
    (impl Net
      "/// Verbindet mit `addr` (Reconnect läuft im Hintergrund)."
      ,(pub_ '(defun connect ("addr: &str")
                (declare (values Self))
                ;; Der Emitter kennt keine Tupel-`let`s: Kanal-Enden
                ;; werden über `.0`/`.1` entpackt (statt `let (a, b)`).
                (let ((ev_ch (channel)))
                  (let ((ev_tx (dot ev_ch 0)))
                    (let ((events (dot ev_ch 1)))
                      (let ((out_ch (channel)))
                        (let ((out (dot out_ch 0)))
                          (let ((out_rx (dot out_ch 1)))
                            (let ((stop (Arc--new (AtomicBool--new false))))
                              (std--thread--spawn
                               (progn
                                 (let ((addr (dot addr (to_owned))))
                                   (let ((stop (dot stop (clone))))
                                     (space "move" (lambda () (run (ref addr) ev_tx out_rx (ref stop))))))))
                              (make-instance Self events out stop))))))))))
      ,*blank*
      "/// Sendet eine Nachricht (geht verloren, wenn gerade keine Verbindung besteht)."
      ,(pub_ '(defun send ("&self" "m: ClientMsg")
                (let ((_ (dot (dot self out) (send m))))))))
    ,*blank*
    (defun run ("addr: &str" "ev: Sender<Event>" "out: Receiver<ClientMsg>" "stop: &AtomicBool")
      (let ((decoder (case (Decoder--new)
                       ((Ok d) d)
                       ((Err e)
                        (let ((_ (dot ev (send (scope Event (Disconnected (format! (string "rav1d: {e}"))))))))
                          (return))))))
        (declare (mutable decoder))
        (let ((backoff (Duration--from_millis 500)))
          (declare (mutable backoff))
          (while (not (dot stop (load (scope Ordering Relaxed))))
            (case (TcpStream--connect addr)
              ((Ok s)
               (progn
                 (setf backoff (Duration--from_millis 500))
                 (session s (ref ev) (ref out) stop (ref-mut decoder))
                 (when (dot stop (load (scope Ordering Relaxed)))
                   (break))
                 (let ((_ (dot ev (send (scope Event (Disconnected (dot (string "getrennt") (into)))))))))))
              ((Err e)
               (let ((_ (dot ev (send (scope Event (Disconnected (format! (string "kein Server ({e})")))))))))))
            "// Backoff in Scheiben, damit `drop` schnell wirkt."
            (let ((steps (dot (dot backoff (as_millis)) (div_ceil 100))))
              (for (_ (range 0 steps))
                (when (dot stop (load (scope Ordering Relaxed)))
                  (return))
                (std--thread--sleep (Duration--from_millis 100)))
              (setf backoff (dot (paren (* backoff 2)) (min (Duration--from_secs 5)))))))))
    ,*blank*
    (defun session ("s: TcpStream" "ev: &Sender<Event>" "out: &Receiver<ClientMsg>" "stop: &AtomicBool" "dec: &mut Decoder")
      (when (dot (dot s (set_read_timeout (Some (Duration--from_millis 50)))) (is_err))
        (return))
      (let ((rd (case (dot s (try_clone))
                  ((Ok r) r)
                  ((Err _) (return)))))
        (declare (mutable rd))
        (let ((wr s))
          (declare (mutable wr))
          (when (dot (write_msg (ref-mut wr)
                                (ref (make-instance (scope ClientMsg Hello) :version PROTO_VERSION)))
                     (is_err))
            (return))
          (let ((fr (FrameReader--new)))
            (declare (mutable fr))
            (loop
              (when (dot stop (load (scope Ordering Relaxed)))
                (return))
              (while-let ((Ok m) (dot out (try_recv)))
                (when (dot (write_msg (ref-mut wr) (ref m)) (is_err))
                  (return)))
              (case (dot fr (read (ref-mut rd)))
                ((Ok (scope Read1 (Frame b)))
                 (case ("decode_msg::<ServerMsg>" (ref b))
                   ((Ok (scope ServerMsg Hello))
                    (let ((_ (dot ev (send (scope Event Connected)))))))
                   ((Ok (scope ServerMsg ClearText))
                    (let ((_ (dot ev (send (scope Event ClearText)))))))
                   ((Ok (scope ServerMsg (AddText t)))
                    (let ((_ (dot ev (send (scope Event (AddText t))))))))
                   ("Ok(ServerMsg::Tile { x, y, data })"
                    (case (dot dec (decode (ref data)))
                      ((Ok rgba)
                       (let ((_ (dot ev (send (make-instance (scope Event Tile)
                                                              x y
                                                              :w (dot rgba w)
                                                              :h (dot rgba h)
                                                              :rgba (dot rgba data)
                                                              :bytes (dot data (len)))))))))
                      ((Err e)
                       (eprintln! (string "[net] AV1: {e}")))))
                   ((Err e)
                    (progn
                      (eprintln! (string "[net] Protokoll: {e}"))
                      (return)))))
                ((Ok (scope Read1 Idle)))
                ((Err _) "// EOF/Verbindung weg → Reconnect."
                 (return))))))))))

(defun client-scene-rs ()
  `(do0
    ,(doc "`04_scene` — Client-Modell des entfernten Bildschirms: RGBA-Canvas"
          "(aus AV1-Kacheln) plus Textelemente (aus Clear/Add-Nachrichten)."
          "Fest 640×640, ohne Skalierungscode. Rein, ohne Grafik-Kontext"
          "testbar; `05_app` zeichnet daraus.")
    ,*blank*
    (use (lbw_common (curly SIZE TextItem)))
    ,*blank*
    (use (crate net Event))
    ,*blank*
    "/// Verbindungsstatus fürs HUD."
    (attr "derive(Clone, Debug, PartialEq, Eq)"
      ,(pub_ '(defenum Link
                Connecting
                Up
                (Down String))))
    ,*blank*
    "/// Zustand der Anzeige (immer 640×640 RGBA)."
    "// `dirty`: Canvas seit dem letzten Upload verändert."
    ,(pub_ '(defstruct0 Scene
              ("pub canvas" "Vec<u8>")
              ("pub texts" "Vec<TextItem>")
              ("pub dirty" bool)
              ("pub link" Link)
              ("pub tiles" u32)
              ("pub tile_bytes" u64)))
    ,*blank*
    (space "const N: usize =" "SIZE as usize;")
    ,*blank*
    (impl Scene
      (attr "must_use"
        ,(pub_ '(defun new ()
                  (declare (values Self))
                  (make-instance Self
                                 :canvas (dot (bracket 24 24 32 255) (repeat (* N N)))
                                 :texts (Vec--new)
                                 :dirty true
                                 :link (scope Link Connecting)
                                 :tiles 0
                                 :tile_bytes 0))))
      ,*blank*
      "/// Kopiert eine `w`×`h`-RGBA-Box an (`x`, `y`). Kaputte Boxen"
      "/// werden ignoriert statt den Client abstürzen zu lassen."
      ,(pub_ '(defun blit ("&mut self" "x: u16" "y: u16" "w: usize" "h: usize" "rgba: &[u8]")
                ;; Kein Tupel-`let` im Emitter: zwei `let`s statt `let (x0, y0)`.
                (let ((x0 (coerce x usize)))
                  (let ((y0 (coerce y usize)))
                    (when (or (> (+ x0 w) N)
                              (> (+ y0 h) N)
                              (< (dot rgba (len)) (* w h 4)))
                      (return))
                    (for (row (range 0 h))
                      (let ((s (* row w 4)))
                        (let ((d (* (+ (* (+ y0 row) N) x0) 4)))
                          (dot (aref (dot self canvas) (space d ".." (+ d (* w 4))))
                               (copy_from_slice (ref (aref rgba (space s ".." (+ s (* w 4))))))))))
                    (setf (dot self dirty) true)))))
      ,*blank*
      "/// Wendet ein Netz-Ereignis an."
      ,(pub_ '(defun apply ("&mut self" "e: Event")
                (case e
                  ((scope Event Connected)
                   (setf (dot self link) (scope Link Up)))
                  ((scope Event (Disconnected why))
                   (when (not (matches! (dot self link) (scope Link (Down _))))
                     (setf (dot self link) (scope Link (Down why)))))
                  ((scope Event ClearText)
                   (dot (dot self texts) (clear)))
                  ((scope Event (AddText t))
                   (dot (dot self texts) (push t)))
                  ("Event::Tile { x, y, w, h, rgba, bytes }"
                   (progn
                     (dot self (blit x y w h (ref rgba)))
                     (incf (dot self tiles))
                     (incf (dot self tile_bytes) (coerce bytes u64)))))))
      ,*blank*
      "/// Farbe des Canvas an `(x, y)` (Tests/Debug)."
      (attr "must_use"
        ,(pub_ '(defun pixel ("&self" "x: usize" "y: usize")
                  (declare (values "[u8; 3]"))
                  (let ((i (* (+ (* y N) x) 4)))
                    (bracket (aref (dot self canvas) i)
                             (aref (dot self canvas) (+ i 1))
                             (aref (dot self canvas) (+ i 2))))))))
    ,*blank*
    (impl (space Default for Scene)
      (defun default ()
        (declare (values Self))
        (Self--new)))
    ,*blank*
    ,(testmod
      "use super::*;"
      "use lbw_common::Rect;"
      *blank*
      '(defun item ("text: &str")
         (declare (values TextItem))
         (make-instance TextItem
                        :rect (Rect--new 0 0 10 10)
                        :fg (array-repeat 0 3)
                        :bg (array-repeat 255 3)
                        :text (dot text (into))))
      *blank*
      '(attr "test"
         (defun clear_and_add_texts ()
           (let ((s (Scene--new)))
             (declare (mutable s))
             (dot s (apply (scope Event (AddText (item (string "a"))))))
             (dot s (apply (scope Event (AddText (item (string "b"))))))
             (assert_eq! (dot (dot s texts) (len)) 2)
             (dot s (apply (scope Event ClearText)))
             (assert! (dot (dot s texts) (is_empty)))
             (dot s (apply (scope Event (AddText (item (string "c"))))))
             (assert_eq! (dot (aref (dot s texts) 0) text) (string "c")))))
      *blank*
      '(attr "test"
         (defun blit_places_tile_and_ignores_garbage ()
           (let ((s (Scene--new)))
             (declare (mutable s))
             (setf (dot s dirty) false)
             (dot s (blit 64 0 64 64 (ref (dot (bracket 200 100 50 255) (repeat (* 64 64))))))
             (assert! (dot s dirty))
             (assert_eq! (dot s (pixel 64 0)) (bracket 200 100 50))
             (assert_eq! (dot s (pixel 127 63)) (bracket 200 100 50))
             (assert_eq! (dot s (pixel 63 0)) (bracket 24 24 32))
             "// Außerhalb und zu kurz: ignoriert."
             (dot s (blit 640 0 64 64 (ref (array-repeat 0 (* 64 64 4)))))
             (dot s (blit 0 0 64 64 (ref (array-repeat 0 10))))
             (assert_eq! (dot s (pixel 0 0)) (bracket 24 24 32)))))
      *blank*
      '(attr "test"
         (defun blit_handles_arbitrary_box_sizes ()
           (let ((s (Scene--new)))
             (declare (mutable s))
             (dot s (blit 100 100 16 92 (ref (dot (bracket 10 20 30 255) (repeat (* 16 92))))))
             (assert_eq! (dot s (pixel 100 100)) (bracket 10 20 30))
             (assert_eq! (dot s (pixel 115 191)) (bracket 10 20 30))
             (assert_eq! (dot s (pixel 116 100)) (bracket 24 24 32))
             (assert_eq! (dot s (pixel 100 192)) (bracket 24 24 32)))))
      *blank*
      '(attr "test"
         (defun link_state_follows_events ()
           (let ((s (Scene--new)))
             (declare (mutable s))
             (dot s (apply (scope Event Connected)))
             (assert_eq! (dot s link) (scope Link Up))
             (dot s (apply (scope Event (Disconnected (dot (string "x") (into))))))
             (assert_eq! (dot s link) (scope Link (Down (dot (string "x") (into)))))
             (dot s (apply (scope Event (Disconnected (dot (string "y") (into))))))
             (assert_eq! (dot s link) (scope Link (Down (dot (string "x") (into))))
                         (string "erste Trennung zählt"))))))))

(defun client-lib-rs ()
  `(do0
    ,(doc "`lbw-client` — schlanker MVP-Client: Empfang, AV1-Dekodierung,"
          "Szenen-Zusammenbau, Eingabe-Weiterleitung. Nur Modul-Deklarationen.")
    ,*blank*
    ,@(loop for (file name) in '(("01_config.rs" "config")
                                 ("02_av1.rs" "av1")
                                 ("03_net.rs" "net")
                                 ("04_scene.rs" "scene"))
            append `((attr ,(format nil "path = ~s" file)
                        (space "pub" ,(format nil "mod ~a;" name)))
                     ,*blank*))))
