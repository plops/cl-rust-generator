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

(defun app-image-expr ()
  "Shared `&Image { bytes, width, height }` expr for the app texture
init and update (macroquad `Image` literal, same shape twice)."
  '(ref (make-instance Image
                       :bytes (dot (dot scene canvas) (clone))
                       :width (coerce SIZE u16)
                       :height (coerce SIZE u16))))

(defun send-input-defun ()
  "Private `send_input` as backquoted data (`,@` splices `+key-table+`)."
  `(defun send_input ("net: &Net" "last_mouse: &mut (u16, u16)" "show_hud: &mut bool")
       ;; Tupel-`let` → `.0`/`.1` (Emitter-Limit, siehe T5).
       ;; Tasten-Tabelle per `,@`-Splice aus `+key-table+` (eine Quelle
       ;; für Client- und Server-Tasten, siehe `00_util.lisp`).
       (let ((mp (mouse_position)))
         (let ((mx (dot mp 0)))
           (let ((my (dot mp 1)))
             (let ((pos (paren (coerce (dot mx (clamp "0.0" (coerce (paren (- SIZE 1)) f32))) u16)
                                (coerce (dot my (clamp "0.0" (coerce (paren (- SIZE 1)) f32))) u16))))
               (when (!= pos (deref last_mouse))
                 (dot net (send (make-instance (scope ClientMsg MouseMove)
                                               :x (dot pos 0) :y (dot pos 1))))
                 (setf (deref last_mouse) pos))
               (for ((paren btn code)
                      (bracket (paren (scope MouseButton Left) 1)
                               (paren (scope MouseButton Middle) 2)
                               (paren (scope MouseButton Right) 3)))
                 (when (is_mouse_button_pressed btn)
                   (dot net (send (make-instance (scope ClientMsg Button)
                                                 :button code :down true))))
                 (when (is_mouse_button_released btn)
                   (dot net (send (make-instance (scope ClientMsg Button)
                                                 :button code :down false)))))
               (while-let ((Some c) (get_char_pressed))
                 (when (not (dot c (is_control)))
                   (dot net (send (scope ClientMsg (Text (dot c (to_string))))))))
               (for ((paren key name) (bracket ,@(client-key-pairs)))
                 (when (is_key_pressed key)
                   (dot net (send (make-instance (scope ClientMsg Key)
                                                 :key (dot name (into)) :down true))))
                 (when (is_key_released key)
                   (dot net (send (make-instance (scope ClientMsg Key)
                                                 :key (dot name (into)) :down false)))))
               (when (is_key_pressed (scope KeyCode F1))
                 (setf (deref show_hud) (not (deref show_hud))))))))))

(defun client-app-rs ()
  `(do0
    ,(doc "`05_app` — macroquad-Schleife: Textur + Text + HUD rendern,"
          "Maus/Tastatur an den Server schicken. Fest 640×640, ohne Skalierung.")
    ,*blank*
    (use (lbw_common (curly ClientMsg SIZE)))
    (use (macroquad prelude "*"))
    ,*blank*
    (use (crate config Config))
    (use (crate net Net))
    (use (crate scene (curly Link Scene)))
    ,*blank*
    "/// Startet den Client (läuft bis zum Fensterschluss)."
    ,(pub_ `(defun-async run ("cfg: Config")
              (let ((net (Net--connect (ref (dot cfg connect)))))
                (let ((scene (Scene--new)))
                  (declare (mutable scene))
                  (let ((texture (Texture2D--from_image ,(app-image-expr))))
                    (setf (dot scene dirty) false)
                    (let ((show_hud true))
                      (declare (mutable show_hud))
                      (let ((last_mouse (paren (scope u16 MAX) (scope u16 MAX))))
                        (declare (mutable last_mouse))
                        (loop
                          (while-let ((Ok e) (dot (dot net events) (try_recv)))
                            (dot scene (apply e)))
                          (when (dot scene dirty)
                            (dot texture (update ,(app-image-expr)))
                            (setf (dot scene dirty) false))
                          (clear_background BLACK)
                          (draw_texture (ref texture) "0.0" "0.0" WHITE)
                          (for (t (ref (dot scene texts)))
                            (let ((r (dot t rect)))
                              (draw_rectangle (coerce (dot r x) f32)
                                              (coerce (dot r y) f32)
                                              (coerce (dot r w) f32)
                                              (coerce (dot r h) f32)
                                              (Color--from_rgba (aref (dot t bg) 0)
                                                                (aref (dot t bg) 1)
                                                                (aref (dot t bg) 2)
                                                                255))
                              (stmt (draw_text (ref (dot t text))
                                                (coerce (dot r x) f32)
                                                (coerce (dot r y) f32)
                                                (coerce (dot r h) f32)
                                                (Color--from_rgba (aref (dot t fg) 0)
                                                                  (aref (dot t fg) 1)
                                                                  (aref (dot t fg) 2)
                                                                  255)))))
                          (when show_hud
                            (stmt (draw_text (dot (hud (ref scene)) (as_str)) "8.0" "16.0" "16.0" YELLOW)))
                          (send_input (ref net) (ref-mut last_mouse) (ref-mut show_hud))
                          (await (next_frame))))))))))
    ,*blank*
    (defun hud ("s: &Scene")
      (declare (values String))
      (let ((link (case (ref (dot s link))
                    ((scope Link Connecting)
                     (dot (string "verbinde…") (to_owned)))
                    ((scope Link Up)
                     (dot (string "online") (to_owned)))
                    ((scope Link (Down why))
                     (format! (string "offline ({why})"))))))
        (format! (string "{link} | {} Texte | {} Kacheln ({} B) | F1 HUD")
                 (dot (dot s texts) (len))
                 (dot s tiles)
                 (dot s tile_bytes))))
    ,*blank*
    "/// Liest macroquad-Eingaben und schickt Deltas an den Server."
    ,(send-input-defun)))

(defun client-main-rs ()
  `(do0
    ,(doc "`lbw-client` — nur Verdrahtung: Konfiguration → Fenster → App-Schleife.")
    ,*blank*
    (use (clap Parser))
    (use (lbw_common SIZE))
    (use (macroquad window Conf))
    ,*blank*
    (use (lbw_client app run))
    (use (lbw_client config Config))
    ,*blank*
    (defun window_conf ()
      (declare (values Conf))
      ;; `..Default::default()` passt in kein `make-instance`: Struct-Literal
      ;; als `space`/`curly` (eine Stelle, rustfmt expandiert).
      (space Conf (curly "window_title: \"lbw-client\".into()"
                         "window_width: SIZE as i32"
                         "window_height: SIZE as i32"
                         "window_resizable: false"
                         "..Default::default()")))
    ,*blank*
    (attr "macroquad::main(window_conf)"
      (defun-async main ()
        (await (run (Config--parse)))))
    ,*blank*
    ,(testmod
      "use super::*;"
      *blank*
      '(attr "test"
         (defun window_is_fixed_640 ()
           (let ((c (window_conf)))
             (assert_eq! (paren (dot c window_width) (dot c window_height))
                         (paren 640 640))
             (assert! (not (dot c window_resizable)))))))))

(defun client-probe-rs ()
  `(do0
    ,(doc "Headless-Smoke-Client: verbindet, wartet auf Text + Kachel, schickt"
          "Eingaben und meldet Erfolg. Mit zweitem Argument bleibt er noch N"
          "Sekunden verbunden und meldet Summen (Durchsatz-Messung). Nutzung:"
          "`cargo run --release -p lbw-client --example probe -- 127.0.0.1:7878 [N]`")
    ,*blank*
    (use (std time (curly Duration Instant)))
    ,*blank*
    (use (lbw_client net (curly Event Net)))
    (use (lbw_client scene Scene))
    (use (lbw_common ClientMsg))
    ,*blank*
    (defun main ()
      (let ((args (dot (std--env--args) (skip 1))))
        (declare (mutable args))
        (let ((addr (dot (dot args (next))
                         (unwrap_or_else (lambda ()
                                           (dot (string "127.0.0.1:7878") (into)))))))
          (let ((stay (dot (dot (dot args (next))
                                    (and_then (lambda (s)
                                                (dot (dot s (parse)) (ok)))))
                               (unwrap_or 0))))
            (declare (type u64 stay))
            (let ((net (Net--connect (ref addr))))
              (let ((scene (Scene--new)))
                (declare (mutable scene))
                (let ((deadline (+ (Instant--now) (Duration--from_secs 60))))
                  (let ((sent_input false))
                    (declare (mutable sent_input))
                    (let ((connected false))
                      (declare (mutable connected))
                      (while (< (Instant--now) deadline)
                        (case (dot (dot net events) (recv_timeout (Duration--from_millis 500)))
                          ((Ok (scope Event Connected))
                           (progn
                             (setf connected true)
                             (println! (string "probe: verbunden"))))
                          ((Ok e)
                           (dot scene (apply e)))
                          ((Err _)))
                        (let ((got_text (not (dot (dot scene texts) (is_empty)))))
                          (let ((got_tile (> (dot scene tiles) 0)))
                            (when (and connected got_tile (not sent_input))
                              (dot net (send (make-instance (scope ClientMsg MouseMove) :x 100 :y 100)))
                              (dot net (send (make-instance (scope ClientMsg Button) :button 1 :down true)))
                              (dot net (send (make-instance (scope ClientMsg Button) :button 1 :down false)))
                              (dot net (send (scope ClientMsg (Text (dot (string "hi") (into))))))
                              (dot net (send (make-instance (scope ClientMsg Key)
                                                            :key (dot (string "Enter") (into)) :down true)))
                              (dot net (send (make-instance (scope ClientMsg Key)
                                                            :key (dot (string "Enter") (into)) :down false)))
                              (setf sent_input true)
                              (println! (string "probe: Eingaben geschickt")))
                            (when (and connected got_text got_tile sent_input)
                              "// Netz-Thread braucht einen Schleifendurchlauf (≤50 ms), um die"
                              "// eben geschickten Eingaben zu flushen — sonst sterben sie mit"
                              "// dem Prozess, bevor der Server sie sieht."
                              (std--thread--sleep (Duration--from_secs 1))
                              (println! (string "probe: OK ({} Texte, {} Kacheln, {} B)")
                                        (dot (dot scene texts) (len))
                                        (dot scene tiles)
                                        (dot scene tile_bytes))
                              (for (t (dot (dot (dot scene texts) (iter)) (take 5)))
                                (println! (string "probe: Text {:?} {:?}") (dot t rect) (dot t text)))
                              (when (== stay 0)
                                (return))
                              (let ((end (+ (Instant--now) (Duration--from_secs stay))))
                                (while (< (Instant--now) end)
                                  (if-let ((Ok e) (dot (dot net events) (recv_timeout (Duration--from_millis 500))))
                                    (dot scene (apply e))))
                                (println! (string "probe: nach {stay}s: {} Texte, {} Kacheln, {} B ({} B/s)")
                                          (dot (dot scene texts) (len))
                                          (dot scene tiles)
                                          (dot scene tile_bytes)
                                          (/ (dot scene tile_bytes) (dot stay (max 1))))
                                (return))))))
                      (eprintln! (string "probe: TIMEOUT (connected={connected} texte={} kacheln={})")
                                 (dot (dot scene texts) (len))
                                 (dot scene tiles))
                      (std--process--exit 1))))))))))))

(defun client-test-loopback-rs ()
  `(do0
    ,(doc "Loopback: `Net` gegen einen Stub-Server (Hello, Text, echte AV1-Kachel,"
          "Abriss + Reconnect). Läuft ohne Display und ohne Modelle.")
    ,*blank*
    (use (std net TcpListener))
    (use (std time (curly Duration Instant)))
    ,*blank*
    (use (lbw_client net (curly Event Net)))
    (use (lbw_common framing (curly FrameReader write_msg)))
    (use (lbw_common (curly ClientMsg Rect ServerMsg TextItem)))
    ,*blank*
    (defun item ()
      (declare (values TextItem))
      (make-instance TextItem
                     :rect (Rect--new 8 8 32 16)
                     :fg (array-repeat 0 3)
                     :bg (array-repeat 255 3)
                     :text (dot (string "hi") (into))))
    ,*blank*
    "/// Stub: Hello lesen, Hello + Text + Kachel schicken, dann `expect`"
    "/// Client-Nachrichten lesen und zurückgeben. Schließen → Client sieht EOF."
    (defun stub ("listener: TcpListener" "tile: Vec<u8>" "expect: usize")
      (declare (values "std::thread::JoinHandle<Vec<ClientMsg>>"))
      (std--thread--spawn
       (space "move" (lambda ()
                       ;; Tupel-`let` → `.0` (Emitter-Limit, siehe T5).
                       (let ((acc (dot (dot listener (accept)) (unwrap))))
                         (let ((s (dot acc 0)))
                           (declare (mutable s))
                           (let ((fr (FrameReader--new)))
                             (declare (mutable fr))
                             (dot (dot s (set_read_timeout (Some (Duration--from_secs 10)))) (unwrap))
                             (assert! (matches! (dot (dot fr ("read_msg::<ClientMsg>" (ref-mut s))) (unwrap))
                                                "Some(ClientMsg::Hello { version: 1 })"))
                             (dot (write_msg (ref-mut s) (ref (scope ServerMsg Hello))) (unwrap))
                             (dot (write_msg (ref-mut s) (ref (scope ServerMsg ClearText))) (unwrap))
                             (dot (write_msg (ref-mut s) (ref (scope ServerMsg (AddText (item))))) (unwrap))
                             (dot (write_msg (ref-mut s)
                                             (ref (make-instance (scope ServerMsg Tile)
                                                                 :x 0 :y 0 :data tile)))
                                  (unwrap))
                             (let ((got (Vec--new)))
                               (declare (mutable got))
                               (while (< (dot got (len)) expect)
                                 (case (dot (dot fr ("read_msg::<ClientMsg>" (ref-mut s))) (unwrap))
                                   ((Some m)
                                    (dot got (push m)))
                                   (None
                                    (panic! (string "Timeout beim Warten auf Client-Nachrichten")))))
                               got))))))))
    ,*blank*
    (defun recv_until ("net: &Net" "until: Instant" "want: &mut dyn FnMut(Event) -> bool")
      (while (< (Instant--now) until)
        ;; Let-Chain (`if let ... && ...`) desugared (Emitter-Limit, vgl. T4).
        (attr "allow(clippy::collapsible_if)"
          (if-let ((Ok e) (dot (dot net events) (recv_timeout (Duration--from_millis 200))))
            (when (want e)
              (return))))))
    ,*blank*
    (attr "test"
      (defun hello_text_tile_and_reconnect ()
        (let ((rgb (dot (bracket "40u8" 80 160) (repeat (* 64 64)))))
          (let ((tile (dot (lbw_server--av1--encode_rgb (ref rgb) 64 64 180) (unwrap))))
            (let ((listener (dot (TcpListener--bind (string "127.0.0.1:0")) (unwrap))))
              (let ((port (dot (dot (dot listener (local_addr)) (unwrap)) (port))))
                (let ((addr (format! (string "127.0.0.1:{port}"))))
                  (let ((sent (vec! (make-instance (scope ClientMsg MouseMove) :x 10 :y 20)
                                    (make-instance (scope ClientMsg Button) :button 1 :down true)
                                    (make-instance (scope ClientMsg Button) :button 1 :down false)
                                    (scope ClientMsg (Text (dot (string "ab") (into)))))))
                    (let ((stub1 (stub listener tile (dot sent (len)))))
                      (let ((net (Net--connect (ref addr))))
                        "// Erste Verbindung: Hello → Clear → Text → Kachel."
                        ;; Tupel-`let` → vier `let`s (Emitter-Limit, siehe T5).
                        (let ((connected 0))
                          (declare (mutable connected))
                          (let ((clear 0))
                            (declare (mutable clear))
                            (let ((texts 0))
                              (declare (mutable texts))
                              (let ((tiles 0))
                                (declare (mutable tiles))
                                (recv_until (ref net) (+ (Instant--now) (Duration--from_secs 10))
                                            (ref-mut (lambda (e)
                                                       (case e
                                                         ((scope Event Connected)
                                                          (incf connected))
                                                         ((scope Event (Disconnected _)))
                                                         ((scope Event ClearText)
                                                          (incf clear))
                                                         ((scope Event (AddText t))
                                                          (progn
                                                            (assert_eq! (dot t text) (string "hi"))
                                                            (incf texts)))
                                                         ("Event::Tile { x, y, w, h, rgba, bytes }"
                                                          (progn
                                                            (assert_eq! (paren x y) (paren 0 0))
                                                            (assert_eq! (paren w h) (paren 64 64))
                                                            (assert_eq! (dot rgba (len)) (* 64 64 4))
                                                            (assert! (> bytes 0))
                                                            "// Flache Kachel: überall fast die Quellfarbe, Alpha 255."
                                                            (assert! (dot (dot rgba (chunks 4))
                                                                          (all (lambda (p) (== (aref p 3) 255)))))
                                                            (for ((paren got want)
                                                                   (dot (dot (aref rgba (space 0 ".." 3)) (iter))
                                                                        (zip (bracket 40 80 160))))
                                                              (assert! (<= (dot got (abs_diff want)) 3) (string "{rgba:?}")))
                                                            (incf tiles))))
                                                       (and (>= connected 1) (>= clear 1) (>= texts 1) (>= tiles 1)))))
                                (assert_eq! (paren connected clear texts tiles) (paren 1 1 1 1))
                                "// Gegenrichtung: Net::send muss vollständig beim Server ankommen."
                                (for (m (ref sent))
                                  (dot net (send (dot m (clone)))))
                                (assert_eq! (dot (dot stub1 (join)) (unwrap)) sent)
                                "// Abriss bemerken, neu verbinden."
                                (let ((down false))
                                  (declare (mutable down))
                                  (recv_until (ref net) (+ (Instant--now) (Duration--from_secs 5))
                                              (ref-mut (lambda (e)
                                                         (when (matches! e (scope Event (Disconnected _)))
                                                           (setf down true)
                                                           (return true))
                                                         false)))
                                  (assert! down (string "Abriss muss als Event kommen"))
                                  (let ((listener2 (dot (TcpListener--bind (ref addr)) (unwrap))))
                                    (let ((tile2 (dot (lbw_server--av1--encode_rgb (ref rgb) 64 64 180) (unwrap))))
                                      (let ((stub2 (stub listener2 tile2 0)))
                                        (let ((reconnected false))
                                          (declare (mutable reconnected))
                                          (recv_until (ref net) (+ (Instant--now) (Duration--from_secs 10))
                                                      (ref-mut (lambda (e)
                                                                 (when (matches! e (scope Event Connected))
                                                                   (setf reconnected true)
                                                                   (return true))
                                                                 false)))
                                          (assert! reconnected (string "Client muss neu verbinden"))
                                          (dot (dot stub2 (join)) (unwrap))
                                          (drop net))))))))))))))))))))))

(defun client-lib-rs ()
  `(do0
    ,(doc "`lbw-client` — schlanker MVP-Client: Empfang, AV1-Dekodierung,"
          "Szenen-Zusammenbau, Eingabe-Weiterleitung. Nur Modul-Deklarationen.")
    ,*blank*
    ,@(loop for (file name) in '(("01_config.rs" "config")
                                 ("02_av1.rs" "av1")
                                 ("03_net.rs" "net")
                                 ("04_scene.rs" "scene")
                                 ("05_app.rs" "app"))
            append `((attr ,(format nil "path = ~s" file)
                        (space "pub" ,(format nil "mod ~a;" name)))
                     ,*blank*))))
