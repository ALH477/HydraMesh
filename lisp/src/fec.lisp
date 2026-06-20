;;;; SPDX-License-Identifier: LGPL-3.0-only
;;;; lisp/src/fec.lisp — dependency-free Reed-Solomon FEC adapter + self-cert.
;;;;
;;;; Loadable under bare SBCL (no Quicklisp). Byte-identical to feclab_core.py,
;;;; codec/demod_fec.h, codec/src/fec.rs, go/dcf/fec.go, JS/nodejs/src/fec.js,
;;;; cpp/include/dcf/fec.hpp, perl/lib/DCF/FEC.pm, and lua/dcf_fec.lua (pinned by
;;;; Documentation/fec_vectors.json). Systematic RS over GF(2^8) (prim #x11D,
;;;; generator 2, fcr 0); default 2t=16 parity -> corrects 8 byte-errors; the message
;;;; layer chunks + interleaves any-length payloads behind a self-protecting header.
;;;; Self-certifies on load (the certify-lisp CI loads this file).

(defpackage :dcf-fec
  (:use :cl)
  (:export :rs-encode :rs-decode :interleave :deinterleave
           :encode-message :decode-message :certify))
(in-package :dcf-fec)

(defconstant +gf-prim+ #x11D)
(defconstant +gf-gen+ 2)
(defconstant +fcr+ 0)
(defconstant +hdr-parity+ 16)
(defconstant +hdr-len+ 21)

(defparameter *exp* (make-array 512 :element-type 'fixnum :initial-element 0))
(defparameter *log* (make-array 256 :element-type 'fixnum :initial-element 0))
(let ((x 1))
  (dotimes (i 255)
    (setf (aref *exp* i) x (aref *log* x) i)
    (setf x (ash x 1))
    (when (logtest x #x100) (setf x (logxor x +gf-prim+))))
  (loop for i from 255 below 512 do (setf (aref *exp* i) (aref *exp* (- i 255)))))

(declaim (inline gmul gdiv gpow ginv))
(defun gmul (a b) (if (or (zerop a) (zerop b)) 0 (aref *exp* (+ (aref *log* a) (aref *log* b)))))
(defun gdiv (a b) (if (zerop a) 0 (aref *exp* (mod (- (aref *log* a) (aref *log* b)) 255))))
(defun gpow (a p) (aref *exp* (mod (* (aref *log* a) p) 255)))
(defun ginv (a) (aref *exp* (- 255 (aref *log* a))))

;; Polynomials are lists of fixnums, high-order coefficient first.
(defun pmul (p q)
  (let ((r (make-array (+ (length p) (length q) -1) :initial-element 0)))
    (loop for pc in p for i from 0 do
      (loop for qc in q for j from 0 do
        (setf (aref r (+ i j)) (logxor (aref r (+ i j)) (gmul pc qc)))))
    (coerce r 'list)))
(defun padd (p q)
  (let* ((lp (length p)) (lq (length q)) (n (max lp lq))
         (r (make-array n :initial-element 0)))
    (loop for pc in p for i from 0 do (setf (aref r (+ i (- n lp))) pc))
    (loop for qc in q for i from 0 do
      (setf (aref r (+ i (- n lq))) (logxor (aref r (+ i (- n lq))) qc)))
    (coerce r 'list)))
(defun pscale (p s) (mapcar (lambda (c) (gmul c s)) p))
(defun peval (seq x)               ; seq: a list or byte vector, high-order first
  (let ((y (elt seq 0)))
    (loop for i from 1 below (length seq) do (setf y (logxor (gmul y x) (elt seq i))))
    y))

(defun genpoly (np)
  (let ((g (list 1)))
    (dotimes (i np g) (setf g (pmul g (list 1 (gpow +gf-gen+ (+ +fcr+ i))))))))

(defun rs-encode (msg np)          ; msg byte vector -> codeword byte vector
  (let* ((m (length msg))
         (gen (genpoly np))
         (out (make-array (+ m np) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i m) (setf (aref out i) (aref msg i)))
    (dotimes (i m)
      (let ((coef (aref out i)))
        (unless (zerop coef)
          (loop for gj in (rest gen) for j from 1 do
            (setf (aref out (+ i j)) (logxor (aref out (+ i j)) (gmul gj coef)))))))
    (let ((code (make-array (+ m np) :element-type '(unsigned-byte 8))))
      (dotimes (i m) (setf (aref code i) (aref msg i)))
      (dotimes (i np) (setf (aref code (+ m i)) (aref out (+ m i))))
      code)))

(defun syndromes (cw np)           ; -> list, leading 0
  (cons 0 (loop for i from 0 below np collect (peval cw (gpow +gf-gen+ (+ +fcr+ i))))))

(defun err-locator (synd np)
  (let ((el (list 1)) (ol (list 1)))
    (dotimes (i np)
      (let ((delta (nth (1+ i) synd)))
        (loop for j from 1 below (length el) do
          (setf delta (logxor delta (gmul (nth (- (length el) 1 j) el) (nth (- (1+ i) j) synd)))))
        (setf ol (append ol (list 0)))
        (when (/= delta 0)
          (when (> (length ol) (length el))
            (let ((nl (pscale ol delta)))
              (setf ol (pscale el (ginv delta)))
              (setf el nl)))
          (setf el (padd el (pscale ol delta))))))
    (loop while (and el (zerop (first el))) do (pop el))
    el))

(defun rs-decode (codeword np &optional msglen)   ; -> (values msg-vector count)
  (let* ((cw (copy-seq codeword)) (n (length cw)))
    (unless msglen (setf msglen (- n np)))
    (let ((synd (syndromes cw np)))
      (if (every #'zerop synd)
          (values (subseq cw 0 msglen) 0)
          (let ((el (err-locator synd np)))
            (when (> (- (length el) 1) (floor np 2)) (error "fec: too many errors"))
            (let* ((elrev (reverse el)) (errs (- (length elrev) 1)) (pos nil))
              (dotimes (i n)
                (when (zerop (peval elrev (gpow +gf-gen+ i))) (push (- n 1 i) pos)))
              (setf pos (coerce (nreverse pos) 'vector))
              (when (/= (length pos) errs) (error "fec: error location failed"))
              (let ((eloc (list 1)) (xs nil))
                (loop for idx from 0 below (length pos) do
                  (let ((cp (- n 1 (aref pos idx))))
                    (setf eloc (pmul eloc (list (gpow +gf-gen+ cp) 1)))
                    (push (gpow +gf-gen+ cp) xs)))
                (setf xs (coerce (nreverse xs) 'vector))
                (let* ((prod (pmul (reverse synd) eloc))
                       (remlen (length eloc))
                       (rem (subseq prod (- (length prod) remlen))))
                  (dotimes (i (length xs))
                    (let ((xi (aref xs i)) (denom 1))
                      (dotimes (j (length xs))
                        (when (/= j i) (setf denom (gmul denom (logxor 1 (gmul (ginv xi) (aref xs j)))))))
                      (when (zerop denom) (error "fec: forney denom zero"))
                      (let* ((numer (gmul (peval rem (ginv xi)) (gpow xi (- 1 +fcr+))))
                             (p (aref pos i)))
                        (setf (aref cw p) (logxor (aref cw p) (gdiv numer denom))))))))
              (unless (every #'zerop (syndromes cw np)) (error "fec: residual syndrome"))
              (values (subseq cw 0 msglen) (length pos))))))))

(defun interleave (cws)            ; list of equal-length byte vectors -> stream vector
  (if (null cws)
      (make-array 0 :element-type '(unsigned-byte 8))
      (let* ((d (length cws)) (n (length (first cws))) (arrs (coerce cws 'vector))
             (out (make-array (* d n) :element-type '(unsigned-byte 8))))
        (dotimes (r d) (dotimes (c n) (setf (aref out (+ (* c d) r)) (aref (aref arrs r) c))))
        out)))
(defun deinterleave (stream depth n)   ; -> list of byte vectors
  (let* ((cws (loop repeat depth collect (make-array n :element-type '(unsigned-byte 8))))
         (cwv (coerce cws 'vector)))
    (dotimes (c n) (dotimes (r depth) (setf (aref (aref cwv r) c) (aref stream (+ (* c depth) r)))))
    cws))

(defun chunking (l np)
  (let* ((maxk (- 255 np))
         (nchunks (if (zerop l) 1 (ceiling l maxk)))
         (k (if (zerop l) 1 (ceiling l nchunks))))
    (values nchunks k)))

(defun encode-message (msg np)         ; msg byte vector -> blob byte vector
  (let ((l (length msg)))
    (multiple-value-bind (nchunks k) (chunking l np)
      (let ((hdr (make-array 5 :element-type '(unsigned-byte 8))))
        (setf (aref hdr 0) (logand (ash l -24) 255) (aref hdr 1) (logand (ash l -16) 255)
              (aref hdr 2) (logand (ash l -8) 255) (aref hdr 3) (logand l 255) (aref hdr 4) np)
        (let ((out (rs-encode hdr +hdr-parity+)) (cws nil))
          (dotimes (c nchunks)
            (let ((block (make-array k :element-type '(unsigned-byte 8) :initial-element 0)))
              (dotimes (i k) (let ((o (+ (* c k) i))) (when (< o l) (setf (aref block i) (aref msg o)))))
              (push (rs-encode block np) cws)))
          (concatenate '(vector (unsigned-byte 8)) out (interleave (nreverse cws))))))))

(defun decode-message (blob)
  (when (< (length blob) +hdr-len+) (error "fec: short blob"))
  (multiple-value-bind (hdr ignore) (rs-decode (subseq blob 0 +hdr-len+) +hdr-parity+ 5)
    (declare (ignore ignore))
    (let ((l (logior (ash (aref hdr 0) 24) (ash (aref hdr 1) 16) (ash (aref hdr 2) 8) (aref hdr 3)))
          (np (aref hdr 4)))
      (multiple-value-bind (nchunks k) (chunking l np)
        (let ((cwlen (+ k np)) (body (subseq blob +hdr-len+)))
          (when (/= (length body) (* nchunks cwlen)) (error "fec: blob length mismatch"))
          (let ((cws (deinterleave body nchunks cwlen)) (out nil) (total 0))
            (dolist (cw cws)
              (multiple-value-bind (block nc) (rs-decode cw np k)
                (incf total nc) (push block out)))
            (let ((joined (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse out))))
              (values (if (< l (length joined)) (subseq joined 0 l) joined) total))))))))

;; ── self-cert against embedded golden vectors (a fec_vectors.json subset) ─────
(defun hx (s)
  (let ((v (make-array (/ (length s) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length v) v)
      (setf (aref v i) (parse-integer s :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))
(defun bh (v)
  (string-downcase
   (with-output-to-string (s) (loop for b across v do (format s "~2,'0X" b)))))

(defun certify ()
  (dolist (c '(("d310000000000000000000000000005b80"
                "d310000000000000000000000000005b802c70076e2823cf5ee0bc717689f56efe")
               ("d31312340001ffffdeadbeefab12cd24c0"
                "d31312340001ffffdeadbeefab12cd24c0f318f5e24eefa07297dc976efa4a9e46")
               ("d310010203040506cafebabe010203f4af"
                "d310010203040506cafebabe010203f4afcc84e6dd5eae8b567aeddf51f7038c90")))
    (destructuring-bind (mh ch) c
      (assert (string= (bh (rs-encode (hx mh) 16)) ch) () "rs-encode mismatch")))
  ;; pinned corrupted codeword -> recovers the frame
  (let ((rec (rs-decode (hx "891000005a0000005a0000005a00005bda2c7007342823cf04e0bc712c89f56efe") 16 17)))
    (assert (string= (bh rec) "d310000000000000000000000000005b80") () "rs-decode mismatch"))
  ;; multi-codeword message: golden blob byte-identical + round-trip + burst
  (let* ((m17 "030a11181f262d343b424950575e656c73")
         (blob17 "00000011109c5909e6b614a1c89a033820722a7841030a11181f262d343b424950575e656c73c35135b3a0449ef708d87ccb305dd575")
         (b (encode-message (hx m17) 16)))
    (assert (string= (bh b) blob17) () "encode-message mismatch")
    (assert (string= (bh (decode-message b)) m17) () "decode-message round-trip")
    (let ((bad (copy-seq b)))
      (loop for i from (+ +hdr-len+ 1) below (+ +hdr-len+ 9)
            do (setf (aref bad i) (logxor (aref bad i) #x5A)))
      (assert (string= (bh (decode-message bad)) m17) () "burst recover")))
  :certified)

(eval-when (:load-toplevel :execute)
  (handler-case
      (progn (certify)
             (format t "~&;; dcf-fec (lisp): CERTIFIED — RS(GF256) encode + correct + multi-codeword~%"))
    (error (e)
      (format *error-output* "~&;; dcf-fec (lisp): FAILED — ~A~%" e)
      #+sbcl (sb-ext:exit :code 1))))
