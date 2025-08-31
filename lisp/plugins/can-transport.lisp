(in-package :d-lisp)

;; CAN Bus Transport Plugin for DCF
;; Facilitates communication over Controller Area Network (CAN) using CFFI bindings to libsocketcan.
;; Maps DCF messages to CAN frames with recipient ID.

(cffi:define-foreign-library libsocketcan
  (:unix "libsocketcan.so")
  (t (:default "libsocketcan")))

(cffi:use-foreign-library libsocketcan)

;; Foreign function definitions (simplified)
(cffi:defcfun "can_open" :pointer (interface :string))
(cffi:defcfun "can_send" :int (can :pointer) (frame :pointer))
(cffi:defcfun "can_receive" :int (can :pointer) (frame :pointer))
(cffi:defcfun "can_close" :void (can :pointer))

(defstruct can-frame
  id
  data)

(def-dcf-plugin can-transport :version "1.0"
  :setup (lambda (self config)
           (let ((can (can-open (dcf-config-can-interface config))))
             (setf (gethash "can" self) can)
             t))
  :send (lambda (self data recipient)
          (let ((frame (make-can-frame :id (parse-integer recipient) :data data)))  ; Simplified ID parsing
            (= (can-send (gethash "can" self) frame) 0)))
  :receive (lambda (self)
             (let ((frame (make-can-frame)))
               (when (= (can-receive (gethash "can" self) frame) 0)
                 (can-frame-data frame))))
  :destroy (lambda (self)
             (can-close (gethash "can" self))
             t))
