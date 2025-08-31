(in-package :d-lisp)

;; LoRaWAN Transport Plugin for DCF
;; Enables long-range, low-power wireless communication for IoT using CFFI bindings to liblorawan (e.g., based on LMIC or Semtech stack).
;; Supports LoRaWAN end-device operations with OTAA/ABP activation.
;; Extends dcf-config with :lorawan-device, :lorawan-app-eui, :lorawan-app-key.

(cffi:define-foreign-library liblorawan
  (:unix "liblorawan.so")
  (t (:default "liblorawan")))

(cffi:use-foreign-library liblorawan)

;; Foreign function definitions (simplified; assume liblorawan provides these)
(cffi:defcfun "lorawan_initialize" :pointer (device :string) (app-eui :string) (app-key :string))
(cffi:defcfun "lorawan_join_network" :int (lorawan :pointer))
(cffi:defcfun "lorawan_send" :int (lorawan :pointer) (data :pointer) (len :int) (recipient :string))
(cffi:defcfun "lorawan_receive" :int (lorawan :pointer) (buf :pointer) (len :int))
(cffi:defcfun "lorawan_leave_network" :void (lorawan :pointer))

(def-dcf-plugin lorawan-transport :version "1.0"
  :setup (lambda (self config)
           (let ((lorawan (lorawan-initialize (dcf-config-lorawan-device config)
                                              (dcf-config-lorawan-app-eui config)
                                              (dcf-config-lorawan-app-key config))))
             (lorawan-join-network lorawan)
             (setf (gethash "lorawan" self) lorawan)
             t))
  :send (lambda (self data recipient)
          (cffi:with-foreign-pointer (buf (length data))
            (loop for i from 0 below (length data)
                  do (setf (cffi:mem-aref buf :uint8 i) (aref data i)))
            (= (lorawan-send (gethash "lorawan" self) buf (length data) recipient) (length data))))
  :receive (lambda (self)
             (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
               (let ((len (lorawan-receive (gethash "lorawan" self) buf 1024)))
                 (when (> len 0)
                   (subseq buf 0 len)))))
  :destroy (lambda (self)
             (lorawan-leave-network (gethash "lorawan" self))
             t))
