(in-package :d-lisp)

;; SCTP Transport Plugin for DCF
;; Provides reliable, message-oriented transport with multi-streaming using CFFI bindings to libsctp.
;; Supports concurrent message flows.

(cffi:define-foreign-library libsctp
  (:unix "libsctp.so")
  (t (:default "libsctp")))

(cffi:use-foreign-library libsctp)

;; Foreign function definitions (simplified)
(cffi:defcfun "sctp_connect" :pointer (host :string) (port :int))
(cffi:defcfun "sctp_send" :int (sctp :pointer) (data :pointer) (len :int) (recipient :string))
(cffi:defcfun "sctp_receive" :int (sctp :pointer) (buf :pointer) (len :int))
(cffi:defcfun "sctp_close" :void (sctp :pointer))

(def-dcf-plugin sctp-transport :version "1.0"
  :setup (lambda (self config)
           (let ((sctp (sctp-connect (dcf-config-host config) (dcf-config-port config))))
             (setf (gethash "sctp" self) sctp)
             t))
  :send (lambda (self data recipient)
          (cffi:with-foreign-pointer (buf (length data))
            (loop for i from 0 below (length data)
                  do (setf (cffi:mem-aref buf :uint8 i) (aref data i)))
            (= (sctp-send (gethash "sctp" self) buf (length data) recipient) (length data))))
  :receive (lambda (self)
             (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
               (let ((len (sctp-receive (gethash "sctp" self) buf 1024)))
                 (when (> len 0)
                   (subseq buf 0 len)))))
  :destroy (lambda (self)
             (sctp-close (gethash "sctp" self))
             t))
