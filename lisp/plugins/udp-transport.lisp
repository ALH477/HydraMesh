(in-package :d-lisp)

;; UDP Transport Plugin for DCF
;; Implements handshakeless, low-latency UDP communication using USocket.
;; Supports P2P by binding locally and sending to parsed recipient addresses.
;; No batching; relies on DCF redundancy for reliability.
;; Non-blocking receive with short timeout for responsiveness.

(def-dcf-plugin udp-transport :version "1.0.0"
  :setup (lambda (self config)
           (let ((socket (usocket:socket-connect nil nil :protocol :datagram
                                                 :local-host (dcf-config-host config)
                                                 :local-port (dcf-config-port config))))
             (usocket:socket-option socket :non-blocking t)
             (setf (gethash "udp-socket" self) socket)
             (setf (gethash "sequence" self) 0)
             t))
  :send (lambda (self data recipient)
          (let* ((socket (gethash "udp-socket" self))
                 (seq (incf (gethash "sequence" self)))
                 (len (length data))
                 (header (make-array 8 :element-type '(unsigned-byte 8)))
                 (header-buf (flexi-streams:with-output-to-sequence (s header)
                               (write-byte seq s)  ; Simplified 1-byte seq for demo
                               (write-byte len s)))  ; Simplified 1-byte len
                 (packet (concatenate '(vector (unsigned-byte 8)) header data))
                 (parts (cl-ppcre:split ":" recipient))
                 (host (first parts))
                 (port (parse-integer (second parts))))
            (usocket:socket-send socket packet (length packet) :host host :port port)
            t))
  :receive (lambda (self)
             (let ((socket (gethash "udp-socket" self))
                   (buf (make-array 65536 :element-type '(unsigned-byte 8))))
               (multiple-value-bind (received sender-host sender-port)
                   (usocket:socket-receive socket buf nil :timeout 0.001)  ; 1ms timeout
                 (when received
                   (let ((header (subseq buf 0 8))
                         (data (subseq buf 8 received)))
                     (incf (gethash :udp-receives (metrics *node*) 0))
                     data)))))
  :destroy (lambda (self)
             (usocket:socket-close (gethash "udp-socket" self))
             t))
