(in-package :d-lisp)

;; Serial Port Transport Plugin for DCF
;; Enables communication over serial ports for embedded systems and hardware interfaces.
;; Uses cl-serial library or CFFI bindings to libserialport.
;; Extends dcf-config with :serial-port and :baud-rate.

(cffi:define-foreign-library libserialport
  (:unix "libserialport.so")
  (t (:default "libserialport")))

(cffi:use-foreign-library libserialport)

;; Foreign function definitions (simplified)
(cffi:defcfun "sp_get_port_by_name" :pointer (name :string))
(cffi:defcfun "sp_open" :int (port :pointer) (mode :int))
(cffi:defcfun "sp_set_baudrate" :int (port :pointer) (baudrate :int))
(cffi:defcfun "sp_write" :int (port :pointer) (buf :pointer) (count :int))
(cffi:defcfun "sp_read" :int (port :pointer) (buf :pointer) (count :int))
(cffi:defcfun "sp_close" :int (port :pointer))

(def-dcf-plugin serial-transport :version "1.0"
  :setup (lambda (self config)
           (let ((port (sp-get-port-by-name (dcf-config-serial-port config))))
             (sp-open port 0)  ; SP_MODE_READ_WRITE
             (sp-set-baudrate port (dcf-config-baud-rate config))
             (setf (gethash "serial-port" self) port)
             t))
  :send (lambda (self data recipient)
          (declare (ignore recipient))  ; Serial is point-to-point
          (cffi:with-foreign-pointer (buf (length data))
            (loop for i from 0 below (length data)
                  do (setf (cffi:mem-aref buf :uint8 i) (aref data i)))
            (= (sp-write (gethash "serial-port" self) buf (length data)) (length data))))
  :receive (lambda (self)
             (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
               (let ((len (sp-read (gethash "serial-port" self) buf 1024)))
                 (when (> len 0)
                   (subseq buf 0 len)))))
  :destroy (lambda (self)
             (sp-close (gethash "serial-port" self))
             t))
