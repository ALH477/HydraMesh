;;;; ============================================================================
;;;; hydramesh-hotfix.lisp — runtime redefinitions for HydraMesh v2.2.0
;;;; DeMoD LLC · DCF mono repo · LGPL-3.0
;;;;
;;;; LOAD ORDER (required):
;;;;   1. Edit src/hydramesh.lisp FIRST — two one-line source changes, because a
;;;;      load failure there cannot be hotfixed afterward:
;;;;        a) remove :cl-json-schema from the (ql:quickload '(...)) list
;;;;        b) remove :cl-json-schema from the (defpackage :d-lisp (:use ...)) list
;;;;      cl-json-schema is not in the Quicklisp dist; the bare (ql:quickload ...)
;;;;      aborts the load before any of this file could run. This hotfix replaces
;;;;      the schema validation it provided (see validate-streamdb-data below).
;;;;   2. (load "src/hydramesh.lisp")
;;;;   3. (load "hydramesh-hotfix.lisp")   ; this file — prints CERTIFIED on success
;;;;
;;;; Every redefinition below is late-bound through the function cell, so callers
;;;; compiled in step 2 (encode-dcf-frame, decode-dcf-frame, dcf-init, ...) pick
;;;; up the fixes automatically. For permanence, fold these bodies back into
;;;; src/hydramesh.lisp; the forms are drop-in replacements.
;;;;
;;;; Fixes (verified under SBCL 2.x; anchors machine-checked against the Python
;;;; reference codec and the Haskell FrameSpec exampleFrame):
;;;;   F1  crc16-ccitt: inner LET* shadowed CRC -> function returned #xFFFF for
;;;;       ALL input. Wire validity was vacuous; cross-language frames rejected.
;;;;       Fixed form returns 0x29B1 for "123456789", 0xA963 for exampleFrame body.
;;;;   F2  send-udp-pong/handle-udp-message: pong stamped the RESPONDER's clock;
;;;;       RTT across processes was garbage. Pong now echoes the ping timestamp.
;;;;   F3  stop-udp-endpoint: joined a receiver parked in socket-receive (no
;;;;       timeout) BEFORE closing the socket -> shutdown hang. Close first.
;;;;   F4  dcf-stop: freed StreamDB, then called save-state on it (use-after-
;;;;       free). Order is now save -> stop endpoint -> flush+free -> clear.
;;;;   F5  load-config: used GETF on the ALIST cl-json returns -> every config
;;;;       key silently ignored, defaults always used. Alist accessor + manual
;;;;       validation (replaces the cl-json-schema dependency).
;;;;   F6  save-state/restore-state: passed a Lisp LIST to dcf-db-insert (which
;;;;       AREFs it) and stored a bare array against an object schema. Peers are
;;;;       now persisted as the JSON object {"peers":[...]}.
;;;;   F7  dcf-db-insert: hardened to accept string | (unsigned-byte 8) vector |
;;;;       anything-else (JSON-encoded), so no caller can hit the AREF trap.
;;;;   F8  collect-streamdb-results: walked a pointer ARRAY, but
;;;;       streamdb_prefix_search returns a Result* LINKED LIST (streamdb.h).
;;;;       Proper defcstruct + list walk; was reading garbage / crashing.
;;;;   F9  dcf-benchmark: (when (network-stats-last-rtt stats)) — 0 is truthy in
;;;;       CL, so zeros were pushed before any pong arrived and min-rtt was
;;;;       always 0. Now PLUSP-guarded.
;;;; ============================================================================

(in-package :d-lisp)

;;; ---------------------------------------------------------------------------
;;; F1 — CRC-16/CCITT-FALSE (poly #x1021, init #xFFFF), the DeModFrame integrity
;;; function over bytes [0..14]. SBCL-verified: broken version returns #xFFFF
;;; for every input; this version matches the Python/Haskell/Lua references.
;;; ---------------------------------------------------------------------------
(defun crc16-ccitt (vec &optional (start 0) (end (length vec)))
  "CRC-CCITT over VEC[START..END). Returns uint16."
  (let ((crc #xFFFF))
    (loop for i from start below end do
      (setf crc (logand #xFFFF (logxor crc (ash (aref vec i) 8))))
      (loop repeat 8 do
        (setf crc (if (logbitp 15 crc)
                      (logand (logxor (ash crc 1) #x1021) #xFFFF)
                      (logand (ash crc 1) #xFFFF)))))
    crc))

;;; ---------------------------------------------------------------------------
;;; F2 — RTT measurement. A pong must ECHO the ping's timestamp; the sender
;;; then subtracts on its own clock. (get-internal-real-time has a per-process
;;; epoch in SBCL, so cross-stamping clocks can never work.)
;;; ---------------------------------------------------------------------------
(defun send-udp-pong (endpoint seq echo-ts remote-host remote-port)
  (let ((msg (make-proto-message
              :type +msg-type-pong+
              :sequence seq
              :timestamp echo-ts          ; echo, do not restamp
              :payload #())))
    (send-udp-raw endpoint (serialize-proto-message msg) remote-host remote-port)))

(defun handle-udp-message (endpoint msg-bytes remote-host remote-port)
  (handler-case
      (let ((msg (deserialize-proto-message msg-bytes)))
        (case (proto-message-type msg)
          (#.+msg-type-ack+
           (multiple-value-bind (seq offset) (read-u32 (proto-message-payload msg) 0)
             (declare (ignore offset))
             (bt:with-lock-held ((udp-endpoint-reliable-lock endpoint))
               (setf (gethash seq (udp-endpoint-ack-received endpoint)) t)
               (remhash seq (udp-endpoint-reliable-packets endpoint)))))
          (#.+msg-type-ping+
           (send-udp-pong endpoint
                          (proto-message-sequence msg)
                          (proto-message-timestamp msg)   ; F2: echo sender's ts
                          remote-host remote-port))
          (#.+msg-type-pong+
           (let* ((now (get-internal-real-time))
                  (sent-time (proto-message-timestamp msg))  ; our own clock, echoed
                  (rtt (/ (- now sent-time) internal-time-units-per-second 0.001)))
             (when (plusp rtt)
               (update-rtt-stats (udp-endpoint-stats endpoint) rtt))))
          (#.+msg-type-reliable+
           (send-ack endpoint (proto-message-sequence msg) remote-host remote-port)
           (when (udp-endpoint-receive-callback endpoint)
             (funcall (udp-endpoint-receive-callback endpoint)
                      msg remote-host remote-port)))
          (otherwise
           (when (udp-endpoint-receive-callback endpoint)
             (funcall (udp-endpoint-receive-callback endpoint)
                      msg remote-host remote-port)))))
    (error (e)
      (log:warn *dcf-logger* "Error handling UDP message: ~A" e))))

;;; ---------------------------------------------------------------------------
;;; F3 — shutdown must close the socket BEFORE joining the receiver, because
;;; socket-receive blocks with no timeout. Closing unblocks it; the receiver's
;;; handler-case eats the closed-socket error and the loop exits on RUNNING=nil.
;;; ---------------------------------------------------------------------------
(defun stop-udp-endpoint (endpoint)
  (setf (udp-endpoint-running endpoint) nil)
  (ignore-errors (usocket:socket-close (udp-endpoint-socket endpoint)))
  (when (udp-endpoint-thread endpoint)
    (ignore-errors (bt:join-thread (udp-endpoint-thread endpoint))))
  t)

;;; ---------------------------------------------------------------------------
;;; F4 — stop order: persist state while the DB is alive, then tear down.
;;; ---------------------------------------------------------------------------
(defun dcf-stop ()
  (when *node*
    (handler-case (save-state *node*)
      (error (e) (log:warn *dcf-logger* "save-state failed during stop: ~A" e)))
    (when (dcf-node-udp-endpoint *node*)
      (stop-udp-endpoint (dcf-node-udp-endpoint *node*)))
    (when (and (dcf-node-streamdb *node*)
               (not (cffi:null-pointer-p (dcf-node-streamdb *node*))))
      (streamdb_flush (dcf-node-streamdb *node*))
      (streamdb_free (dcf-node-streamdb *node*))
      (setf (dcf-node-streamdb *node*) nil))
    (setf *node* nil)
    `(:status "stopped")))

;;; ---------------------------------------------------------------------------
;;; F5 — cl-json:decode-json returns an ALIST with mangled keyword keys, not a
;;; plist; GETF on it always returns the default. JREF is a tolerant accessor
;;; (case- and punctuation-insensitive), immune to cl-json's key conversion.
;;; ---------------------------------------------------------------------------
(defun jkey-norm (s)
  (remove-if-not #'alphanumericp (string-upcase (string s))))

(defun jref (alist key &optional default)
  "Tolerant alist accessor for cl-json output. KEY is a keyword or string."
  (let ((hit (assoc key alist
                    :test (lambda (want have)
                            (string= (jkey-norm want) (jkey-norm have))))))
    (if hit (cdr hit) default)))

(defun load-config (file)
  (handler-case
      (with-open-file (stream file :direction :input :if-does-not-exist :error)
        (let* ((config (cl-json:decode-json stream))
               (missing (remove-if (lambda (k) (jref config k))
                                   '(:transport :host :port :mode))))
          (when missing
            (signal-dcf-error :config-validation
                              (format nil "Config missing required keys: ~{~A~^, ~}" missing)))
          (let ((port (jref config :port))
                (mode (jref config :mode))
                (transport (jref config :transport)))
            (unless (and (integerp port) (<= 0 port 65535))
              (signal-dcf-error :config-validation
                                (format nil "port must be an integer in [0,65535], got ~S" port)))
            (unless (member mode '("client" "server" "p2p" "auto" "master") :test #'string=)
              (signal-dcf-error :config-validation
                                (format nil "mode must be one of client|server|p2p|auto|master, got ~S" mode)))
            (unless (string-equal transport "UDP")
              (log:warn *dcf-logger* "transport ~S declared; v2.2.0 runtime implements UDP" transport))
            (make-dcf-config
             :transport transport
             :host (jref config :host "0.0.0.0")
             :port port
             :udp-port (jref config :udp-port 7777)
             :mode mode
             :node-id (jref config :node-id (format nil "node-~A" (random 10000)))
             :peers (jref config :peers '())
             :group-rtt-threshold (jref config :group-rtt-threshold 50)
             :storage (jref config :storage "in-memory")
             :streamdb-path (jref config :streamdb-path)
             :optimization-level (jref config :optimization-level 2)
             :retry-max (jref config :retry-max 3)
             :udp-mtu (jref config :udp-mtu 1400)
             :udp-reliable-timeout (jref config :udp-reliable-timeout 500)
             :audio-priority (jref config :audio-priority t)))))
    (file-error (e)
      (signal-dcf-error :file-error (format nil "Failed to read config: ~A" e)))))

;;; Replaces the cl-json-schema dependency: assert the payload is well-formed
;;; JSON. (The old sexp "schemas" were never valid cl-json-schema input anyway.)
(defun validate-streamdb-data (data schema)
  (declare (ignore schema))
  (handler-case (progn (cl-json:decode-json-from-string data) t)
    (error (e)
      (signal-dcf-error :schema-validation
                        (format nil "Stored value is not well-formed JSON: ~A" e)))))

;;; ---------------------------------------------------------------------------
;;; F6 — peers persisted as a JSON object string, restored symmetrically.
;;; First boot (no key yet) is not an error.
;;; ---------------------------------------------------------------------------
(defun save-state (node)
  (when (dcf-node-streamdb node)
    (let ((json (cl-json:encode-json-to-string
                 (list (cons :peers (or (dcf-node-peers node) '()))))))
      (dcf-db-insert node "/state/peers" json)
      (dcf-db-flush node))))

(defun restore-state (node)
  (when (dcf-node-streamdb node)
    (handler-case
        (let ((peers-json (dcf-db-query node "/state/peers")))
          (when (stringp peers-json)
            (setf (dcf-node-peers node)
                  (jref (cl-json:decode-json-from-string peers-json) :peers '()))))
      (dcf-error (e)
        (unless (eq (dcf-error-code e) :not-found)
          (log:warn *dcf-logger* "restore-state: ~A" e))))))

;;; ---------------------------------------------------------------------------
;;; F7 — dcf-db-insert accepts string | octet vector | any Lisp object (JSON-
;;; encoded). The old code AREF'd whatever it was given.
;;; ---------------------------------------------------------------------------
(defun dcf-db-insert (node path data &key (schema nil))
  (unless (dcf-node-streamdb node)
    (return-from dcf-db-insert nil))
  (let* ((value-octets
           (cond ((stringp data)
                  (flexi-streams:string-to-octets data :external-format :utf-8))
                 ((and (vectorp data) (every (lambda (x) (typep x '(unsigned-byte 8))) data))
                  (coerce data '(vector (unsigned-byte 8))))
                 (t
                  (flexi-streams:string-to-octets
                   (cl-json:encode-json-to-string data) :external-format :utf-8))))
         (cache-form (if (stringp data)
                         data
                         (handler-case
                             (flexi-streams:octets-to-string value-octets :external-format :utf-8)
                           (error () data)))))
    (when (and schema (stringp cache-form))
      (validate-streamdb-data cache-form schema))
    (multiple-value-bind (key-ptr key-len) (string-to-byte-array path)
      (unwind-protect
          (let* ((value-len (length value-octets))
                 (value-ptr (cffi:foreign-alloc :uint8 :count value-len)))
            (unwind-protect
                (progn
                  (loop for i from 0 below value-len
                        do (setf (cffi:mem-aref value-ptr :uint8 i) (aref value-octets i)))
                  (let ((result (streamdb_insert (dcf-node-streamdb node)
                                                 key-ptr key-len value-ptr value-len)))
                    (if (= result +success+)
                        (progn (lru-put node path cache-form) t)
                        (map-streamdb-error result))))
              (cffi:foreign-free value-ptr)))
        (cffi:foreign-free key-ptr)))))

;;; ---------------------------------------------------------------------------
;;; F8 — Result is a LINKED LIST (see streamdb.h):
;;;   struct Result { unsigned char* key; size_t key_len;
;;;                   void* value; size_t value_size; struct Result* next; };
;;; ---------------------------------------------------------------------------
(cffi:defcstruct streamdb-result
  (key        :pointer)
  (key-len    :size)
  (value      :pointer)
  (value-size :size)
  (next       :pointer))

(defun collect-streamdb-results (results-ptr)
  "Walk the Result* linked list from streamdb_prefix_search; collect keys."
  (loop for ptr = results-ptr
          then (cffi:foreign-slot-value ptr '(:struct streamdb-result) 'next)
        until (cffi:null-pointer-p ptr)
        collect (byte-array-to-string
                 (cffi:foreign-slot-value ptr '(:struct streamdb-result) 'key)
                 (cffi:foreign-slot-value ptr '(:struct streamdb-result) 'key-len))))

;;; ---------------------------------------------------------------------------
;;; F9 — benchmark: only record strictly positive RTT samples.
;;; ---------------------------------------------------------------------------
(defun dcf-benchmark (peer-id &key (count 100))
  (unless *node* (signal-dcf-error :not-initialized "Node not initialized"))
  (let ((peer-info (gethash peer-id (dcf-node-peer-map *node*)))
        (rtts '()))
    (unless peer-info
      (signal-dcf-error :peer-not-found (format nil "Peer ~A not found" peer-id)))
    (dotimes (i count)
      (let ((msg (make-proto-message
                  :type +msg-type-ping+
                  :sequence (next-sequence)
                  :timestamp (get-internal-real-time)
                  :payload #())))
        (send-udp-message (dcf-node-udp-endpoint *node*) msg
                          (car peer-info) (cdr peer-info) :reliable nil)
        (sleep 0.01))
      (let ((stats (udp-endpoint-stats (dcf-node-udp-endpoint *node*))))
        (when (plusp (network-stats-last-rtt stats))
          (push (network-stats-last-rtt stats) rtts))))
    (let ((avg (if rtts (/ (reduce #'+ rtts) (length rtts)) 0))
          (min-rtt (if rtts (reduce #'min rtts) 0))
          (max-rtt (if rtts (reduce #'max rtts) 0)))
      `(:peer ,peer-id :count ,count :avg-rtt ,avg
        :min-rtt ,min-rtt :max-rtt ,max-rtt :samples ,(length rtts)))))

;;; ---------------------------------------------------------------------------
;;; Self-certification against the cross-language anchors (same constants as
;;; wirelab_core.py / wirelab.lua / golden_vectors.json / Haskell FrameSpec):
;;;   crc16("123456789")        = #x29B1
;;;   crc16(exampleFrame body)  = #xA963
;;;   exampleFrame (17 bytes)   = D31012340001FFFFDEADBEEFAB12CDA963
;;; Loading this file FAILS LOUDLY if the wire codec does not match.
;;; ---------------------------------------------------------------------------
(defun certify-wire-codec ()
  (let ((anchor (map '(vector (unsigned-byte 8)) #'char-code "123456789"))
        (ex (coerce #(#xD3 #x10 #x12 #x34 #x00 #x01 #xFF #xFF
                      #xDE #xAD #xBE #xEF #xAB #x12 #xCD #xA9 #x63)
                    '(vector (unsigned-byte 8)))))
    (assert (= (crc16-ccitt anchor) #x29B1) ()
            "crc16-ccitt anchor failed: got #x~4,'0X, want #x29B1" (crc16-ccitt anchor))
    (assert (= (crc16-ccitt ex 0 15) #xA963) ()
            "exampleFrame body CRC failed: got #x~4,'0X, want #xA963" (crc16-ccitt ex 0 15))
    (let ((msg (decode-dcf-frame ex)))
      (assert msg () "exampleFrame failed to decode")
      (assert (= (proto-message-sequence msg) #x1234) () "exampleFrame seq mismatch")
      (assert (equalp (proto-message-payload msg) #(#xDE #xAD #xBE #xEF)) ()
              "exampleFrame payload mismatch"))
    (let ((bad (copy-seq ex)))
      (setf (aref bad 9) (logxor (aref bad 9) #x01))
      (assert (null (decode-dcf-frame bad)) () "corrupted frame was ACCEPTED"))
    (values :certified #x29B1 #xA963)))

(multiple-value-bind (status a b) (certify-wire-codec)
  (format t "~&;; hydramesh-hotfix: wire codec ~A (anchors #x~4,'0X / #x~4,'0X)~%"
          status a b))
