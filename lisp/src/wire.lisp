;;;; SPDX-License-Identifier: LGPL-3.0-only
;;;; lisp/src/wire.lisp — dependency-free DeModFrame wire codec + self-cert.
;;;;
;;;; Loadable under bare SBCL (no Quicklisp, no CFFI). The certify-lisp CI loads
;;;; this file and exits non-zero unless the cross-language anchors match — the
;;;; Lisp analogue of the dependency-free C cert (C_SDK/tests/test_wire_certify.c).
;;;; The full SDK codec lives in hydramesh.lisp (its crc16-ccitt is identical and
;;;; self-certifies on load); this file lets CI prove the wire algorithm without
;;;; pulling the SDK's Quicklisp dependency graph.

(defpackage :dcf-wire
  (:use :cl)
  (:export :crc16-ccitt :encode-frame :decode-frame :syndrome :certify))
(in-package :dcf-wire)

(defconstant +sync+ #xD3)
(defconstant +version+ 1)
(defconstant +frame-size+ 17)
(defconstant +crc-cover+ 15)

(defun crc16-ccitt (vec &optional (start 0) (end (length vec)))
  "CRC-16/CCITT-FALSE (poly #x1021, init #xFFFF) over VEC[START..END)."
  (let ((crc #xFFFF))
    (loop for i from start below end do
      (setf crc (logand #xFFFF (logxor crc (ash (aref vec i) 8))))
      (loop repeat 8 do
        (setf crc (if (logbitp 15 crc)
                      (logand (logxor (ash crc 1) #x1021) #xFFFF)
                      (logand (ash crc 1) #xFFFF)))))
    crc))

(defun u16 (hi lo) (logior (ash hi 8) lo))

(defun encode-frame (&key (version 1) (type 0) (seq 0) (src 0) (dst 0)
                          (payload #(0 0 0 0)) (ts-us 0))
  "Serialise into a 17-byte (unsigned-byte 8) vector with an appended CRC."
  (let ((b (make-array +frame-size+ :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
    (setf (aref b 0) +sync+
          (aref b 1) (logior (ash (logand version #x0F) 4) (logand type #x0F))
          (aref b 2) (logand (ash seq -8) #xFF)  (aref b 3) (logand seq #xFF)
          (aref b 4) (logand (ash src -8) #xFF)  (aref b 5) (logand src #xFF)
          (aref b 6) (logand (ash dst -8) #xFF)  (aref b 7) (logand dst #xFF))
    (dotimes (i 4) (setf (aref b (+ 8 i)) (aref payload i)))
    (setf (aref b 12) (logand (ash ts-us -16) #xFF)
          (aref b 13) (logand (ash ts-us -8) #xFF)
          (aref b 14) (logand ts-us #xFF))
    (let ((crc (crc16-ccitt b 0 +crc-cover+)))
      (setf (aref b 15) (logand (ash crc -8) #xFF)
            (aref b 16) (logand crc #xFF)))
    b))

(defun syndrome (w)
  "Affine validity syndrome: W is CRC-valid iff this returns 0."
  (logxor (crc16-ccitt w 0 +crc-cover+) (u16 (aref w 15) (aref w 16))))

(defun decode-frame (w)
  "Return a plist of fields, or NIL if W is not a valid 17-byte frame."
  (when (and (= (length w) +frame-size+)
             (= (aref w 0) +sync+)
             (= (ash (aref w 1) -4) +version+)
             (zerop (syndrome w)))
    (list :version (ash (aref w 1) -4)
          :type (logand (aref w 1) #x0F)
          :seq (u16 (aref w 2) (aref w 3))
          :src (u16 (aref w 4) (aref w 5))
          :dst (u16 (aref w 6) (aref w 7))
          :payload (subseq w 8 12)
          :ts-us (logior (ash (aref w 12) 16) (ash (aref w 13) 8) (aref w 14)))))

(defun certify ()
  "Self-cert against the cross-language anchors; returns :CERTIFIED or signals."
  (let ((anchor (map '(vector (unsigned-byte 8)) #'char-code "123456789"))
        (zeros  (make-array 15 :element-type '(unsigned-byte 8) :initial-element 0))
        (want   (coerce #(#xD3 #x13 #x12 #x34 #x00 #x01 #xFF #xFF
                          #xDE #xAD #xBE #xEF #xAB #x12 #xCD #x24 #xC0)
                        '(vector (unsigned-byte 8)))))
    (assert (= (crc16-ccitt anchor) #x29B1) ()
            "crc16(\"123456789\")=#x~4,'0X, want #x29B1" (crc16-ccitt anchor))
    (assert (= (crc16-ccitt zeros) #x4EC3) ()
            "crc16(0^15)=#x~4,'0X, want #x4EC3" (crc16-ccitt zeros))
    ;; golden exampleFrame_full: Ctrl(3) seq #x1234 src 1 dst #xFFFF DEADBEEF ts #xAB12CD
    (let ((ex (encode-frame :version 1 :type 3 :seq #x1234 :src 1 :dst #xFFFF
                            :payload #(#xDE #xAD #xBE #xEF) :ts-us #xAB12CD)))
      (assert (equalp ex want) () "exampleFrame_full mismatch")
      (let ((d (decode-frame ex)))
        (assert d () "exampleFrame failed to decode")
        (assert (= (getf d :seq) #x1234) () "exampleFrame seq mismatch")
        (assert (equalp (getf d :payload) #(#xDE #xAD #xBE #xEF)) () "payload mismatch"))
      (let ((bad (copy-seq ex)))
        (setf (aref bad 9) (logxor (aref bad 9) 1))
        (assert (null (decode-frame bad)) () "corrupted frame was ACCEPTED")))
    :certified))

;; Self-certify on load (the certify-lisp CI just loads this file).
(eval-when (:load-toplevel :execute)
  (handler-case
      (progn (certify)
             (format t "~&;; dcf-wire (lisp): CERTIFIED — crc16 #x29B1 / #x4EC3, exampleFrame OK~%"))
    (error (e)
      (format *error-output* "~&;; dcf-wire (lisp): FAILED — ~A~%" e)
      #+sbcl (sb-ext:exit :code 1))))
