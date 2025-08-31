(in-package :d-lisp)

;; Bluetooth Transport Plugin for DCF
;; Implements short-range Bluetooth communication using CFFI bindings to libbluetooth (BlueZ).
;; Supports P2P with RTT measurement and AUTO mode role switching.
;; Simplified for demo; assumes CFFI bindings to BlueZ functions.

(cffi:define-foreign-library libbluetooth
  (:unix "libbluetooth.so")
  (t (:default "libbluetooth")))

(cffi:use-foreign-library libbluetooth)

;; Foreign function definitions (simplified from pasted C code)
(cffi:defcfun "hci_open_dev" :int (dev-id :int))
(cffi:defcfun "hci_close_dev" :void (dd :int))
(cffi:defcfun "hci_le_set_scan_enable" :int (dd :int) (enable :uint8) (filter-dup :uint8) (timeout :int))
(cffi:defcfun "str2ba" :int (str :string) (ba :pointer))
(cffi:defcfun "ba2str" :int (ba :pointer) (str :string))

(defstruct bluetooth-peer
  address
  rtt-ms)

(def-dcf-plugin bluetooth-transport :version "1.0"
  :setup (lambda (self config)
           (let ((dd (hci-open-dev (hci-get-route nil))))
             (when (< dd 0) (return-from setup nil))
             (setf (gethash "dd" self) dd)
             (let ((sock (socket AF-BLUETOOTH SOCK-STREAM BTPROTO-RFCOMM)))
               (setf (gethash "sock" self) sock))
             (let ((local-addr (make-sockaddr-rc :family AF-BLUETOOTH :bdaddr BDADDR-ANY :channel 1)))
               (bind (gethash "sock" self) local-addr (size-of local-addr)))
             (listen (gethash "sock" self) 5)
             (setf (gethash "mode" self) "p2p")
             (setf (gethash "peers" self) (make-array 10 :adjustable t :fill-pointer 0))
             (hci-le-set-scan-enable dd #x01 0 1000)
             t))
  :send (lambda (self data recipient)
          (let* ((bt (gethash "sock" self))
                 (client-sock (socket AF-BLUETOOTH SOCK-STREAM BTPROTO-RFCOMM))
                 (remote-addr (make-sockaddr-rc :family AF-BLUETOOTH :channel 1)))
            (str2ba recipient (bdaddr remote-addr))
            (if (< (connect client-sock remote-addr (size-of remote-addr)) 0)
                (progn
                  (close client-sock)
                  (loop for peer in (gethash "peers" self)
                        when (and (< (bluetooth-peer-rtt-ms peer) 50)
                                  (not (string= (bluetooth-peer-address peer) recipient)))
                        do (return (bluetooth-send self data (bluetooth-peer-address peer)))
                        finally (return nil)))
                (progn
                  (write-sequence data client-sock)
                  (close client-sock)
                  (let ((peer-index (position recipient (gethash "peers" self) :key #'bluetooth-peer-address :test #'string=)))
                    (if peer-index
                        t
                        (progn
                          (vector-push-extend (make-bluetooth-peer :address recipient :rtt-ms (measure-rtt self recipient)) (gethash "peers" self))
                          t)))))))
  :receive (lambda (self)
             (let ((client-sock (accept (gethash "sock" self) nil nil)))
               (if (< client-sock 0) nil
                   (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
                     (let ((len (read-sequence buf client-sock)))
                       (close client-sock)
                       (when (> len 0)
                         (subseq buf 0 len)))))))
  :destroy (lambda (self)
             (loop for peer in (gethash "peers" self)
                   do (free (bluetooth-peer-address peer)))
             (close (gethash "sock" self))
             (hci-close-dev (gethash "dd" self))
             t))

(defun measure-rtt (self target)
  "Measure RTT for Bluetooth peer."
  (let ((start (get-internal-real-time)))
    ;; Simplified ping; in practice, send and wait for response
    (bluetooth-send self (vector 1) target)
    (- (get-internal-real-time) start)))
