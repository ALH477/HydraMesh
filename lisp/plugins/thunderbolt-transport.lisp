;;;; thunderbolt-transport.lisp
;;;; Thunderbolt/USB4 Transport Plugin for DeMoD Communications Framework (DCF)
;;;; D-LISP Implementation - Production Version
;;;;
;;;; Copyright © 2025 DeMoD LLC
;;;;
;;;; This file is part of DeMoD Communications Framework (DCF).
;;;;
;;;; DCF is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; DCF is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public License
;;;; along with DCF.  If not, see <https://www.gnu.org/licenses/>.

(defpackage :dcf.transport.thunderbolt
  (:use :cl :cffi :bordeaux-threads)
  (:import-from :usocket
                #:socket-stream
                #:make-socket)
  (:export #:make-thunderbolt-transport
           #:thunderbolt-transport
           #:transport-error
           #:send-message
           #:open-data-stream
           #:close-transport
           #:get-transport-stats
           #:reset-transport-stats
           #:measure-rtt))

(in-package :dcf.transport.thunderbolt)

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(define-condition transport-error (error)
  ((text :initarg :text :reader error-text)
   (errno :initarg :errno :initform nil :reader error-errno))
  (:report (lambda (condition stream)
             (if (error-errno condition)
                 (format stream "Thunderbolt transport error: ~A (errno: ~A)"
                         (error-text condition) (error-errno condition))
                 (format stream "Thunderbolt transport error: ~A"
                         (error-text condition))))))

;;; ---------------------------------------------------------------------------
;;; Foreign Constants and Functions
;;; ---------------------------------------------------------------------------

(define-foreign-library libc
  (:unix (:or "libc.so.6" "libc.so"))
  (t (:default "libc")))

(use-foreign-library libc)

;;; Socket Constants (Linux)
(defconstant +af-inet6+ 10)
(defconstant +sock-dgram+ 2)
(defconstant +sock-stream+ 1)
(defconstant +sol-socket+ 1)        ; Corrected from #xfff
(defconstant +so-bindtodevice+ 25)
(defconstant +ipproto-tcp+ 6)
(defconstant +tcp-nodelay+ 1)
(defconstant +so-sndbuf+ 7)
(defconstant +so-rcvbuf+ 8)
(defconstant +so-keepalive+ 9)
(defconstant +so-reuseaddr+ 2)
(defconstant +somaxconn+ 128)
(defconstant +eintr+ 4)
(defconstant +eagain+ 11)
(defconstant +ewouldblock+ 11)

;;; Socket Flags
(defconstant +msg-dontwait+ #x40)
(defconstant +o-nonblock+ #x800)     ; For fcntl
(defconstant +f-getfl+ 3)            ; Get file flags
(defconstant +f-setfl+ 4)            ; Set file flags

;;; System Calls
(defcfun ("socket" c-socket) :int 
  (domain :int) (type :int) (protocol :int))

(defcfun ("fcntl" c-fcntl-getfl) :int
  (fd :int) (cmd :int))

(defcfun ("fcntl" c-fcntl-setfl) :int
  (fd :int) (cmd :int) (flags :int))

(defcfun ("bind" c-bind) :int 
  (sockfd :int) (addr :pointer) (addrlen :unsigned-int))

(defcfun ("listen" c-listen) :int 
  (sockfd :int) (backlog :int))

(defcfun ("accept" c-accept) :int 
  (sockfd :int) (addr :pointer) (addrlen :pointer))

(defcfun ("connect" c-connect) :int 
  (sockfd :int) (addr :pointer) (addrlen :unsigned-int))

(defcfun ("sendto" c-sendto) :long
  (sockfd :int) (buf :pointer) (len :unsigned-long)
  (flags :int) (dest-addr :pointer) (addrlen :unsigned-int))

(defcfun ("recvfrom" c-recvfrom) :long
  (sockfd :int) (buf :pointer) (len :unsigned-long)
  (flags :int) (src-addr :pointer) (addrlen :pointer))

(defcfun ("send" c-send) :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int))

(defcfun ("recv" c-recv) :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int))

(defcfun ("setsockopt" c-setsockopt) :int
  (sockfd :int) (level :int) (optname :int)
  (optval :pointer) (optlen :unsigned-int))

(defcfun ("close" c-close) :int 
  (fd :int))

(defcfun ("shutdown" c-shutdown) :int
  (sockfd :int) (how :int))

(defcfun ("htons" htons) :unsigned-short 
  (hostshort :unsigned-short))

(defcfun ("ntohs" ntohs) :unsigned-short 
  (netshort :unsigned-short))

(defcfun ("inet_pton" inet-pton) :int 
  (af :int) (src :string) (dst :pointer))

(defcfun ("inet_ntop" inet-ntop) :string
  (af :int) (src :pointer) (dst :pointer) (size :unsigned-int))

(defcfun ("memset" memset) :pointer 
  (s :pointer) (c :int) (n :unsigned-long))

(defcfun ("memcpy" memcpy) :pointer
  (dest :pointer) (src :pointer) (n :unsigned-long))

(defcfun ("if_nametoindex" c-if-nametoindex) :unsigned-int 
  (ifname :string))

(defcfun ("strerror" c-strerror) :string 
  (errnum :int))

(defcfun ("__errno_location" c-errno-location) :pointer)

;;; Helper to get errno
(defun get-errno ()
  (mem-ref (c-errno-location) :int))

;;; ---------------------------------------------------------------------------
;;; sockaddr-in6 Structure
;;; ---------------------------------------------------------------------------

(defcstruct sockaddr-in6
  (sin6-family :unsigned-short)
  (sin6-port :unsigned-short)
  (sin6-flowinfo :unsigned-int)
  (sin6-addr :unsigned-char :count 16)
  (sin6-scope-id :unsigned-int))

(defconstant +sockaddr-in6-size+ (foreign-type-size '(:struct sockaddr-in6)))
(defconstant +inet6-addrstrlen+ 46)

;;; ---------------------------------------------------------------------------
;;; Helper Functions
;;; ---------------------------------------------------------------------------

(defun zero-memory (ptr size)
  "Zero out memory region."
  (memset ptr 0 size))

(defun socket-error (message &optional (errno (get-errno)))
  "Signal a transport error with errno information."
  (error 'transport-error 
         :text (format nil "~A: ~A" message (c-strerror errno))
         :errno errno))

(defun fill-sockaddr-in6 (addr-ptr host port scope-id)
  "Fill pre-allocated sockaddr-in6 structure. Returns T on success."
  (zero-memory addr-ptr +sockaddr-in6-size+)
  (setf (foreign-slot-value addr-ptr '(:struct sockaddr-in6) 'sin6-family) +af-inet6+)
  (setf (foreign-slot-value addr-ptr '(:struct sockaddr-in6) 'sin6-port) (htons port))
  
  (let ((result (inet-pton +af-inet6+ host
                          (foreign-slot-pointer addr-ptr '(:struct sockaddr-in6) 'sin6-addr))))
    (cond
      ((= result 1)
       (setf (foreign-slot-value addr-ptr '(:struct sockaddr-in6) 'sin6-scope-id) scope-id)
       t)
      ((= result 0)
       (error 'transport-error :text (format nil "Invalid IPv6 address: ~A" host)))
      (t
       (socket-error "inet_pton failed")))))

(defun parse-ipv6-address (peer-address default-scope-id)
  "Parse IPv6 address with optional scope (e.g., 'fe80::1%eth0' or 'fe80::1%2').
   Returns (values ip-string scope-id)."
  (let ((scope-pos (position #\% peer-address)))
    (if scope-pos
        (let* ((ip (subseq peer-address 0 scope-pos))
               (scope-str (subseq peer-address (1+ scope-pos)))
               (scope-id (or (parse-integer scope-str :junk-allowed t)
                           (let ((idx (c-if-nametoindex scope-str)))
                             (when (zerop idx)
                               (error 'transport-error 
                                      :text (format nil "Invalid scope identifier: ~A" scope-str)))
                             idx))))
          (values ip scope-id))
        (values peer-address default-scope-id))))

(defun get-peer-address (addr-ptr)
  "Extract IPv6 address string from sockaddr-in6."
  (with-foreign-pointer-as-string (str +inet6-addrstrlen+)
    (let ((result (inet-ntop +af-inet6+
                            (foreign-slot-pointer addr-ptr '(:struct sockaddr-in6) 'sin6-addr)
                            str
                            +inet6-addrstrlen+)))
      (if (null-pointer-p result)
          "unknown"
          result))))

(defun set-socket-option (sockfd level optname value)
  "Set socket option with proper error handling."
  (with-foreign-object (optval :int)
    (setf (mem-ref optval :int) value)
    (when (< (c-setsockopt sockfd level optname optval (foreign-type-size :int)) 0)
      (socket-error (format nil "Failed to set socket option ~D" optname)))))

(defun bind-to-device (sockfd interface-name)
  "Bind socket to specific network interface."
  (with-foreign-string (if-str interface-name)
    (let ((len (1+ (length interface-name))))
      (when (< (c-setsockopt sockfd +sol-socket+ +so-bindtodevice+ if-str len) 0)
        (socket-error (format nil "Failed to bind socket to device ~A" interface-name))))))

(defun set-nonblocking (sockfd)
  "Set socket to non-blocking mode using fcntl."
  (let ((flags (c-fcntl-getfl sockfd +f-getfl+)))
    (when (minusp flags)
      (socket-error "Failed to get socket flags"))
    (when (minusp (c-fcntl-setfl sockfd +f-setfl+ (logior flags +o-nonblock+)))
      (socket-error "Failed to set socket to non-blocking mode"))))

(defun configure-tcp-socket (sockfd &key (nodelay t) (keepalive t) (buffer-size (* 64 1024 1024)))
  "Configure TCP socket for optimal performance."
  (when nodelay
    (set-socket-option sockfd +ipproto-tcp+ +tcp-nodelay+ 1))
  (when keepalive
    (set-socket-option sockfd +sol-socket+ +so-keepalive+ 1))
  (when buffer-size
    (set-socket-option sockfd +sol-socket+ +so-sndbuf+ buffer-size)
    (set-socket-option sockfd +sol-socket+ +so-rcvbuf+ buffer-size))
  ;; Set non-blocking mode explicitly
  (set-nonblocking sockfd))

;;; ---------------------------------------------------------------------------
;;; Default Ports and Configuration
;;; ---------------------------------------------------------------------------

(defconstant +default-control-port+ 5000)
(defconstant +default-data-port+    5001)
(defconstant +default-buffer-size+ (* 64 1024 1024))  ; 64 MiB
(defconstant +max-udp-payload+ 65507)
(defconstant +connection-backlog+ 128)

;;; ---------------------------------------------------------------------------
;;; Statistics Tracking
;;; ---------------------------------------------------------------------------

(defclass transport-statistics ()
  ((messages-sent :initform 0 :accessor messages-sent)
   (messages-received :initform 0 :accessor messages-received)
   (bytes-sent :initform 0 :accessor bytes-sent)
   (bytes-received :initform 0 :accessor bytes-received)
   (connections-accepted :initform 0 :accessor connections-accepted)
   (connections-initiated :initform 0 :accessor connections-initiated)
   (errors :initform 0 :accessor errors)
   (last-reset :initform (get-universal-time) :accessor last-reset)
   (lock :initform (make-lock "Stats Lock") :reader stats-lock)))

(defmethod record-sent ((stats transport-statistics) bytes)
  (with-lock-held ((stats-lock stats))
    (incf (messages-sent stats))
    (incf (bytes-sent stats) bytes)))

(defmethod record-received ((stats transport-statistics) bytes)
  (with-lock-held ((stats-lock stats))
    (incf (messages-received stats))
    (incf (bytes-received stats) bytes)))

(defmethod record-error ((stats transport-statistics))
  (with-lock-held ((stats-lock stats))
    (incf (errors stats))))

(defmethod record-connection ((stats transport-statistics) direction)
  (with-lock-held ((stats-lock stats))
    (ecase direction
      (:accepted (incf (connections-accepted stats)))
      (:initiated (incf (connections-initiated stats))))))

(defmethod get-statistics ((stats transport-statistics))
  (with-lock-held ((stats-lock stats))
    (list :messages-sent (messages-sent stats)
          :messages-received (messages-received stats)
          :bytes-sent (bytes-sent stats)
          :bytes-received (bytes-received stats)
          :connections-accepted (connections-accepted stats)
          :connections-initiated (connections-initiated stats)
          :errors (errors stats)
          :uptime (- (get-universal-time) (last-reset stats)))))

(defmethod reset-statistics ((stats transport-statistics))
  (with-lock-held ((stats-lock stats))
    (setf (messages-sent stats) 0
          (messages-received stats) 0
          (bytes-sent stats) 0
          (bytes-received stats) 0
          (connections-accepted stats) 0
          (connections-initiated stats) 0
          (errors stats) 0
          (last-reset stats) (get-universal-time))))

;;; ---------------------------------------------------------------------------
;;; Transport Class
;;; ---------------------------------------------------------------------------

(defclass thunderbolt-transport ()
  ((interface-name :initarg :interface-name :reader interface-name
                   :documentation "Network interface name (e.g., 'thunderbolt0')")
   (if-index :reader if-index
             :documentation "Network interface index")
   (control-port :initarg :control-port :initform +default-control-port+ 
                 :reader control-port
                 :documentation "UDP port for control messages")
   (data-port :initarg :data-port :initform +default-data-port+ 
              :reader data-port
              :documentation "TCP port for bulk data transfers")
   (udp-sock :initform -1 :accessor udp-socket
             :documentation "UDP socket file descriptor")
   (tcp-listen-sock :initform -1 :accessor tcp-listen-socket
                    :documentation "TCP listening socket file descriptor")
   (acceptor-thread :initform nil :accessor acceptor-thread
                    :documentation "Background thread accepting TCP connections")
   (udp-receiver-thread :initform nil :accessor udp-receiver-thread
                        :documentation "Background thread receiving UDP messages")
   (shutdown-flag :initform nil :accessor shutdown-flag
                  :documentation "Flag to signal shutdown to background threads")
   (message-handler :initarg :message-handler :initform nil :reader message-handler
                    :documentation "Callback for incoming messages")
   (handler-lock :initform (make-lock "Handler Lock") :reader handler-lock
                 :documentation "Lock for message handler invocation")
   (stats :initform (make-instance 'transport-statistics) :reader transport-stats
          :documentation "Transport statistics")
   (logger :initarg :logger :initform nil :reader logger
           :documentation "Optional logging function"))
  (:documentation "Thunderbolt/USB4 transport layer using IPv6 link-local addresses.
Provides UDP control channel and TCP data streams for high-performance communication."))

;;; ---------------------------------------------------------------------------
;;; Initialization
;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((transport thunderbolt-transport) &key config)
  "Initialize the transport layer with sockets and background threads."
  (handler-case
      (progn
        (initialize-configuration transport config)
        (initialize-interface transport)
        (initialize-udp-socket transport)
        (initialize-tcp-socket transport)
        (start-background-threads transport)
        (add-finalizer transport))
    (error (e)
      (cleanup-transport transport)
      (error e))))

(defmethod initialize-configuration ((transport thunderbolt-transport) config)
  "Extract and validate configuration parameters."
  (with-slots (interface-name control-port data-port logger) transport
    (setf interface-name (or (getf config :interface) "thunderbolt0")
          control-port (or (getf config :control-port) +default-control-port+)
          data-port (or (getf config :data-port) +default-data-port+)
          logger (getf config :logger))))

(defmethod initialize-interface ((transport thunderbolt-transport))
  "Validate and get interface index."
  (with-slots (interface-name if-index) transport
    (setf if-index (c-if-nametoindex interface-name))
    (when (zerop if-index)
      (error 'transport-error 
             :text (format nil "Network interface not found: ~A" interface-name)))
    (log-message transport :info "Initialized interface ~A (index ~D)" 
                interface-name if-index)))

(defmethod initialize-udp-socket ((transport thunderbolt-transport))
  "Create and configure UDP socket for control messages."
  (with-slots (udp-sock interface-name control-port) transport
    (setf udp-sock (c-socket +af-inet6+ +sock-dgram+ 0))
    (when (< udp-sock 0)
      (socket-error "Failed to create UDP socket"))
    
    ;; Enable address reuse
    (set-socket-option udp-sock +sol-socket+ +so-reuseaddr+ 1)
    
    ;; Set non-blocking mode
    (set-nonblocking udp-sock)
    
    ;; Bind to specific interface
    (bind-to-device udp-sock interface-name)
    
    ;; Bind to port
    (with-foreign-object (addr '(:struct sockaddr-in6))
      (zero-memory addr +sockaddr-in6-size+)
      (setf (foreign-slot-value addr '(:struct sockaddr-in6) 'sin6-family) +af-inet6+)
      (setf (foreign-slot-value addr '(:struct sockaddr-in6) 'sin6-port) (htons control-port))
      (when (< (c-bind udp-sock addr +sockaddr-in6-size+) 0)
        (socket-error "Failed to bind UDP socket")))
    
    (log-message transport :info "UDP socket bound to port ~D (non-blocking)" control-port)))

(defmethod initialize-tcp-socket ((transport thunderbolt-transport))
  "Create and configure TCP listening socket for data streams."
  (with-slots (tcp-listen-sock interface-name data-port) transport
    (setf tcp-listen-sock (c-socket +af-inet6+ +sock-stream+ 0))
    (when (< tcp-listen-sock 0)
      (socket-error "Failed to create TCP listener socket"))
    
    ;; Enable address reuse
    (set-socket-option tcp-listen-sock +sol-socket+ +so-reuseaddr+ 1)
    
    ;; Bind to specific interface
    (bind-to-device tcp-listen-sock interface-name)
    
    ;; Bind to port
    (with-foreign-object (addr '(:struct sockaddr-in6))
      (zero-memory addr +sockaddr-in6-size+)
      (setf (foreign-slot-value addr '(:struct sockaddr-in6) 'sin6-family) +af-inet6+)
      (setf (foreign-slot-value addr '(:struct sockaddr-in6) 'sin6-port) (htons data-port))
      (when (< (c-bind tcp-listen-sock addr +sockaddr-in6-size+) 0)
        (socket-error "Failed to bind TCP listener"))
      (when (< (c-listen tcp-listen-sock +connection-backlog+) 0)
        (socket-error "Failed to listen on TCP socket")))
    
    (log-message transport :info "TCP listener bound to port ~D" data-port)))

(defmethod start-background-threads ((transport thunderbolt-transport))
  "Start background threads for accepting connections and receiving messages."
  (with-slots (acceptor-thread udp-receiver-thread) transport
    (setf acceptor-thread
          (make-thread (lambda () (tcp-acceptor-loop transport))
                       :name "Thunderbolt-TCP-Acceptor"))
    (setf udp-receiver-thread
          (make-thread (lambda () (udp-receiver-loop transport))
                       :name "Thunderbolt-UDP-Receiver"))
    (log-message transport :info "Background threads started")))

(defmethod add-finalizer ((transport thunderbolt-transport))
  "Add finalizer to ensure cleanup on GC."
  #+sbcl
  (sb-ext:finalize transport
                   (lambda ()
                     (ignore-errors (cleanup-transport transport))))
  #+ccl
  (ccl:terminate-when-unreachable transport
                                  (lambda ()
                                    (ignore-errors (cleanup-transport transport)))))

;;; ---------------------------------------------------------------------------
;;; Logging
;;; ---------------------------------------------------------------------------

(defmethod log-message ((transport thunderbolt-transport) level format-string &rest args)
  "Log a message if logger is configured."
  (with-slots (logger) transport
    (when logger
      (funcall logger level (apply #'format nil format-string args)))))

;;; ---------------------------------------------------------------------------
;;; Sending Functions
;;; ---------------------------------------------------------------------------

(defmethod send-message ((transport thunderbolt-transport) data peer-address)
  "Send small control message via UDP. DATA must be (vector (unsigned-byte 8))."
  (unless (vectorp data)
    (error 'transport-error :text "Data must be a vector"))
  (when (> (length data) +max-udp-payload+)
    (error 'transport-error 
           :text (format nil "Message too large: ~D bytes (max ~D)" 
                        (length data) +max-udp-payload+)))
  
  (with-slots (udp-sock if-index control-port stats) transport
    (multiple-value-bind (ip scope-id) 
        (parse-ipv6-address peer-address if-index)
      (with-foreign-object (addr '(:struct sockaddr-in6))
        (fill-sockaddr-in6 addr ip control-port scope-id)
        (with-foreign-pointer (buf (length data))
          ;; Copy data to foreign buffer
          (loop for i below (length data)
                do (setf (mem-aref buf :uint8 i) (aref data i)))
          
          (let ((sent (c-sendto udp-sock buf (length data) 0 
                               addr +sockaddr-in6-size+)))
            (cond
              ((minusp sent)
               (record-error stats)
               (socket-error "UDP sendto failed"))
              (t
               (record-sent stats sent)
               (log-message transport :debug "Sent ~D bytes to ~A" sent peer-address)
               sent))))))))

(defmethod open-data-stream ((transport thunderbolt-transport) peer-address)
  "Open a bidirectional TCP stream to peer for large transfers.
   Returns a binary stream (compatible with read-byte/write-byte)."
  (with-slots (if-index data-port interface-name stats) transport
    (multiple-value-bind (ip scope-id) 
        (parse-ipv6-address peer-address if-index)
      (let ((tcp-sock (c-socket +af-inet6+ +sock-stream+ 0)))
        (when (< tcp-sock 0)
          (socket-error "Failed to create outbound TCP socket"))
        
        (handler-case
            (progn
              ;; Bind to specific interface
              (bind-to-device tcp-sock interface-name)
              
              ;; Configure socket for optimal performance
              (configure-tcp-socket tcp-sock 
                                   :nodelay t 
                                   :keepalive t 
                                   :buffer-size +default-buffer-size+)
              
              ;; Connect to peer
              (with-foreign-object (addr '(:struct sockaddr-in6))
                (fill-sockaddr-in6 addr ip data-port scope-id)
                (when (< (c-connect tcp-sock addr +sockaddr-in6-size+) 0)
                  (socket-error (format nil "Failed to connect to ~A" peer-address))))
              
              (record-connection stats :initiated)
              (log-message transport :info "Established data stream to ~A" peer-address)
              
              ;; Wrap in usocket for idiomatic Lisp streams
              (socket-stream
               (make-socket :descriptor tcp-sock :element-type '(unsigned-byte 8))))
          (error (e)
            (c-close tcp-sock)
            (record-error stats)
            (error e)))))))

;;; ---------------------------------------------------------------------------
;;; Message Handler Invocation
;;; ---------------------------------------------------------------------------

(defmethod call-message-handler ((transport thunderbolt-transport) message-type data)
  "Thread-safe wrapper for message handler invocation."
  (with-slots (message-handler handler-lock) transport
    (when message-handler
      (handler-case
          (with-lock-held (handler-lock)
            (funcall message-handler message-type data))
        (error (e)
          (log-message transport :error "Message handler error: ~A" e)
          (record-error (transport-stats transport)))))))

;;; ---------------------------------------------------------------------------
;;; Receiver Loops
;;; ---------------------------------------------------------------------------

(defmethod tcp-acceptor-loop ((transport thunderbolt-transport))
  "Accept incoming TCP connections and spawn handlers."
  (with-slots (tcp-listen-sock shutdown-flag stats) transport
    (log-message transport :info "TCP acceptor loop started")
    (loop
      (when shutdown-flag
        (log-message transport :info "TCP acceptor loop shutting down")
        (return))
      
      (handler-case
          (with-foreign-object (addr '(:struct sockaddr-in6))
            (with-foreign-object (addrlen :unsigned-int)
              (setf (mem-ref addrlen :unsigned-int) +sockaddr-in6-size+)
              (let ((client-fd (c-accept tcp-listen-sock addr addrlen)))
                (cond
                  ((>= client-fd 0)
                   (record-connection stats :accepted)
                   (let ((peer-addr (get-peer-address addr)))
                     (log-message transport :debug "Accepted connection from ~A" peer-addr)
                     (make-thread
                      (lambda ()
                        (handle-data-connection transport client-fd peer-addr))
                      :name (format nil "TB-Data-~A" peer-addr))))
                  ((member (get-errno) (list +eintr+ +eagain+ +ewouldblock+))
                   ;; Interrupted or would block, continue
                   (sleep 0.01))
                  (t
                   (record-error stats)
                   (log-message transport :error "Accept failed: ~A" 
                               (c-strerror (get-errno)))
                   (sleep 1))))))
        (error (e)
          (log-message transport :error "TCP acceptor error: ~A" e)
          (record-error stats)
          (sleep 1))))))

(defmethod handle-data-connection ((transport thunderbolt-transport) client-fd peer-addr)
  "Handle an accepted TCP data connection."
  (let ((stream nil))
    (unwind-protect
         (handler-case
             (progn
               (configure-tcp-socket client-fd 
                                    :nodelay t 
                                    :keepalive t 
                                    :buffer-size +default-buffer-size+)
               (setf stream (socket-stream
                            (make-socket :descriptor client-fd 
                                       :element-type '(unsigned-byte 8))))
               (log-message transport :debug "Processing data stream from ~A" peer-addr)
               (call-message-handler transport :data-stream stream))
           (error (e)
             (log-message transport :error "Data connection error from ~A: ~A" 
                         peer-addr e)
             (record-error (transport-stats transport))))
      ;; Cleanup
      (when stream
        (ignore-errors (close stream)))
      (when (>= client-fd 0)
        (ignore-errors (c-close client-fd)))
      (log-message transport :debug "Closed connection from ~A" peer-addr))))

(defmethod udp-receiver-loop ((transport thunderbolt-transport))
  "Receive incoming UDP control messages."
  (with-slots (udp-sock shutdown-flag stats) transport
    (log-message transport :info "UDP receiver loop started")
    (with-foreign-pointer (buf +max-udp-payload+)
      (loop
        (when shutdown-flag
          (log-message transport :info "UDP receiver loop shutting down")
          (return))
        
        (handler-case
            (with-foreign-object (addr '(:struct sockaddr-in6))
              (with-foreign-object (addrlen :unsigned-int)
                (setf (mem-ref addrlen :unsigned-int) +sockaddr-in6-size+)
                (let ((recv (c-recvfrom udp-sock buf +max-udp-payload+ 
                                       +msg-dontwait+ addr addrlen)))
                  (cond
                    ((> recv 0)
                     ;; Copy received data to Lisp vector
                     (let ((data (make-array recv :element-type '(unsigned-byte 8))))
                       (loop for i below recv
                             do (setf (aref data i) (mem-aref buf :uint8 i)))
                       (record-received stats recv)
                       (let ((peer-addr (get-peer-address addr)))
                         (log-message transport :debug "Received ~D bytes from ~A" 
                                    recv peer-addr)
                         (call-message-handler transport :control-message data))))
                    ((and (minusp recv)
                          (member (get-errno) (list +eintr+ +eagain+ +ewouldblock+)))
                     ;; Would block or interrupted, sleep briefly
                     (sleep 0.01))
                    ((minusp recv)
                     (record-error stats)
                     (log-message transport :error "UDP receive failed: ~A" 
                                (c-strerror (get-errno)))
                     (sleep 1))))))
          (error (e)
            (log-message transport :error "UDP receiver error: ~A" e)
            (record-error stats)
            (sleep 1)))))))

;;; ---------------------------------------------------------------------------
;;; RTT Measurement
;;; ---------------------------------------------------------------------------

(defconstant +rtt-ping-magic+ #x5254545054) ; "RTTPT" in hex
(defconstant +rtt-pong-magic+ #x5254545053) ; "RTTPS" in hex

(defun pack-rtt-ping (timestamp)
  "Pack RTT ping message: magic (8 bytes) + timestamp (8 bytes)."
  (let ((msg (make-array 16 :element-type '(unsigned-byte 8))))
    ;; Pack magic number (big-endian)
    (loop for i from 0 to 7
          do (setf (aref msg i) 
                  (ldb (byte 8 (* 8 (- 7 i))) +rtt-ping-magic+)))
    ;; Pack timestamp (big-endian nanoseconds)
    (loop for i from 8 to 15
          do (setf (aref msg i)
                  (ldb (byte 8 (* 8 (- 15 i))) timestamp)))
    msg))

(defun unpack-rtt-pong (msg)
  "Unpack RTT pong message. Returns timestamp or NIL if invalid."
  (when (and (vectorp msg) (= (length msg) 16))
    (let ((magic 0)
          (timestamp 0))
      ;; Unpack magic
      (loop for i from 0 to 7
            do (setf magic (logior (ash magic 8) (aref msg i))))
      ;; Unpack timestamp
      (loop for i from 8 to 15
            do (setf timestamp (logior (ash timestamp 8) (aref msg i))))
      (when (= magic +rtt-pong-magic+)
        timestamp))))

(defun get-nanosecond-timestamp ()
  "Get current time in nanoseconds (monotonic if available)."
  #+sbcl
  (multiple-value-bind (sec nsec)
      (sb-ext:get-time-of-day)
    (+ (* sec 1000000000) nsec))
  #+ccl
  (let ((time (ccl:current-time-in-nanoseconds)))
    time)
  #-(or sbcl ccl)
  (* (get-internal-real-time) 
     (/ 1000000000 internal-time-units-per-second)))

(defmethod measure-rtt ((transport thunderbolt-transport) peer-address 
                       &key (timeout 5.0) (payload-size 16))
  "Measure round-trip time to peer by sending UDP ping and waiting for response.
   Peer must echo packets with magic +rtt-ping-magic+ to +rtt-pong-magic+.
   Returns RTT in seconds, or NIL on timeout."
  (declare (ignore payload-size)) ; Fixed at 16 bytes for now
  
  (with-slots (udp-sock if-index control-port) transport
    (multiple-value-bind (ip scope-id) 
        (parse-ipv6-address peer-address if-index)
      (with-foreign-object (addr '(:struct sockaddr-in6))
        (fill-sockaddr-in6 addr ip control-port scope-id)
        
        ;; Send ping
        (let* ((send-time (get-nanosecond-timestamp))
               (ping-msg (pack-rtt-ping send-time)))
          
          (with-foreign-pointer (send-buf 16)
            (loop for i below 16
                  do (setf (mem-aref send-buf :uint8 i) (aref ping-msg i)))
            
            (let ((sent (c-sendto udp-sock send-buf 16 0 addr +sockaddr-in6-size+)))
              (when (minusp sent)
                (log-message transport :error "RTT ping send failed")
                (return-from measure-rtt nil))))
          
          ;; Wait for pong with timeout
          (with-foreign-pointer (recv-buf 1024)
            (with-foreign-object (recv-addr '(:struct sockaddr-in6))
              (with-foreign-object (addrlen :unsigned-int)
                (setf (mem-ref addrlen :unsigned-int) +sockaddr-in6-size+)
                
                (let ((deadline (+ (get-internal-real-time)
                                  (* timeout internal-time-units-per-second))))
                  (loop
                    (when (>= (get-internal-real-time) deadline)
                      (log-message transport :debug "RTT measurement timed out")
                      (return-from measure-rtt nil))
                    
                    (let ((recv (c-recvfrom udp-sock recv-buf 1024 +msg-dontwait+
                                           recv-addr addrlen)))
                      (cond
                        ((> recv 0)
                         ;; Check if this is our pong
                         (let ((pong-msg (make-array recv :element-type '(unsigned-byte 8))))
                           (loop for i below recv
                                 do (setf (aref pong-msg i) (mem-aref recv-buf :uint8 i)))
                           
                           (let ((recv-timestamp (unpack-rtt-pong pong-msg)))
                             (when (and recv-timestamp (= recv-timestamp send-time))
                               (let* ((recv-time (get-nanosecond-timestamp))
                                      (rtt-ns (- recv-time send-time))
                                      (rtt-sec (/ rtt-ns 1.0d9)))
                                 (log-message transport :debug 
                                            "RTT to ~A: ~,6F ms" 
                                            peer-address (* rtt-sec 1000))
                                 (return-from measure-rtt rtt-sec))))))
                        
                        ((and (minusp recv)
                              (member (get-errno) (list +eagain+ +ewouldblock+)))
                         ;; Would block, wait briefly and retry
                         (sleep 0.001))
                        
                        ((minusp recv)
                         ;; Error
                         (log-message transport :error "RTT pong receive failed")
                         (return-from measure-rtt nil)))))))))))))

;;; ---------------------------------------------------------------------------
;;; Statistics Access
;;; ---------------------------------------------------------------------------

(defmethod get-transport-stats ((transport thunderbolt-transport))
  "Get current transport statistics."
  (get-statistics (transport-stats transport)))

(defmethod reset-transport-stats ((transport thunderbolt-transport))
  "Reset transport statistics."
  (reset-statistics (transport-stats transport))
  (log-message transport :info "Statistics reset"))

;;; ---------------------------------------------------------------------------
;;; Shutdown and Cleanup
;;; ---------------------------------------------------------------------------

(defmethod close-transport ((transport thunderbolt-transport))
  "Gracefully shut down the transport layer."
  (with-slots (shutdown-flag) transport
    (setf shutdown-flag t))
  (cleanup-transport transport)
  (log-message transport :info "Transport closed"))

(defmethod cleanup-transport ((transport thunderbolt-transport))
  "Clean up all resources (sockets, threads)."
  (with-slots (udp-sock tcp-listen-sock acceptor-thread udp-receiver-thread) transport
    ;; Close sockets first to unblock system calls
    (when (and udp-sock (>= udp-sock 0))
      (ignore-errors
        (c-shutdown udp-sock 2)  ; SHUT_RDWR
        (c-close udp-sock))
      (setf udp-sock -1))
    
    (when (and tcp-listen-sock (>= tcp-listen-sock 0))
      (ignore-errors
        (c-shutdown tcp-listen-sock 2)
        (c-close tcp-listen-sock))
      (setf tcp-listen-sock -1))
    
    ;; Give threads time to notice shutdown
    (sleep 0.1)
    
    ;; Interrupt threads if still alive
    (when (and acceptor-thread (thread-alive-p acceptor-thread))
      (ignore-errors
        (interrupt-thread acceptor-thread (lambda () (throw 'shutdown nil))))
      (sleep 0.05))
    
    (when (and udp-receiver-thread (thread-alive-p udp-receiver-thread))
      (ignore-errors
        (interrupt-thread udp-receiver-thread (lambda () (throw 'shutdown nil))))
      (sleep 0.05))))

;;; ---------------------------------------------------------------------------
;;; Plugin Registration
;;; ---------------------------------------------------------------------------

(defun make-thunderbolt-transport (&key (interface "thunderbolt0")
                                         (control-port +default-control-port+)
                                         (data-port +default-data-port+)
                                         message-handler
                                         logger)
  "Create a new Thunderbolt transport instance.
   
   Arguments:
     :interface - Network interface name (default: 'thunderbolt0')
     :control-port - UDP port for control messages (default: 5000)
     :data-port - TCP port for data streams (default: 5001)
     :message-handler - Callback function (lambda (type data) ...) for incoming messages
                       Type is :control-message or :data-stream
     :logger - Optional logging function (lambda (level message) ...)
               Level is :debug, :info, :warn, or :error
   
   Returns:
     A thunderbolt-transport instance
   
   Example:
     (make-thunderbolt-transport
       :interface \"thunderbolt0\"
       :message-handler (lambda (type data)
                          (case type
                            (:control-message 
                             (format t \"Got control message: ~A~%\" data))
                            (:data-stream
                             (format t \"Got data stream: ~A~%\" data))))
       :logger (lambda (level msg)
                 (format t \"[~A] ~A~%\" level msg)))"
  (make-instance 'thunderbolt-transport
                 :config (list :interface interface
                              :control-port control-port
                              :data-port data-port
                              :logger logger)
                 :message-handler message-handler))

(defun register-thunderbolt-transport ()
  "Register transport with DCF plugin manager if available."
  (when (find-package :dcf.plugin-manager)
    (let ((register-fn (find-symbol "REGISTER-TRANSPORT" :dcf.plugin-manager)))
      (when (fboundp register-fn)
        (funcall register-fn
                 "thunderbolt"
                 (lambda (config &key message-handler)
                   (make-instance 'thunderbolt-transport
                                 :config config
                                 :message-handler message-handler)))))))

;; Auto-register on load
(eval-when (:load-toplevel :execute)
  (register-thunderbolt-transport))

;;; ---------------------------------------------------------------------------
;;; Usage Examples and Documentation
;;; ---------------------------------------------------------------------------

#|

USAGE EXAMPLES:

1. Basic Setup:

(defvar *transport* 
  (make-thunderbolt-transport
    :interface "thunderbolt0"
    :message-handler (lambda (type data)
                       (case type
                         (:control-message 
                          (format t "Control: ~A~%" data))
                         (:data-stream
                          (format t "Stream: ~A~%" data))))))

2. Send Control Message:

(send-message *transport*
              #(1 2 3 4 5)  ; byte vector
              "fe80::1%thunderbolt0")

3. Open Data Stream:

(let ((stream (open-data-stream *transport* "fe80::1%thunderbolt0")))
  (unwind-protect
       (progn
         (write-byte 42 stream)
         (force-output stream)
         (let ((response (read-byte stream)))
           (format t "Got response: ~A~%" response)))
    (close stream)))

4. Get Statistics:

(get-transport-stats *transport*)
;; => (:MESSAGES-SENT 10 :MESSAGES-RECEIVED 5 :BYTES-SENT 1024 ...)

5. Clean Shutdown:

(close-transport *transport*)

PROTOCOL NOTES:

- Control messages: UDP datagrams up to 65507 bytes
  * Use for: heartbeats, small coordination messages, RTT pings
  * No delivery guarantee, but very low latency
  * RTT ping protocol: Send packet with magic 0x5254545054 + timestamp
    Peer echoes back with magic 0x5254545053 + same timestamp

- Data streams: TCP connections
  * Use for: large gradient transfers, model weights, checkpoints
  * Reliable, ordered delivery with flow control
  * Optimized with 64MB buffers, TCP_NODELAY, and non-blocking I/O

- IPv6 link-local addresses (fe80::/10) with interface scope
  * Format: "fe80::1%thunderbolt0" or "fe80::1%2"
  * Scope can be interface name or numeric index

- Bonding support: Create bond0 with multiple Thunderbolt ports:
    ip link add bond0 type bond mode 802.3ad
    ip link set thunderbolt0 master bond0
    ip link set thunderbolt1 master bond0
  Then use :interface "bond0" for aggregated bandwidth

THREADING MODEL:

- Background threads handle incoming connections and messages
- Message handler called from background threads (must be thread-safe)
- All public methods are thread-safe

PERFORMANCE CHARACTERISTICS:

- RTT: ~5-50μs typical for Thunderbolt 3/4
- Throughput: 20-40 Gbps typical (limited by PCIe, CPU, memory)
- UDP latency: ~10-100μs
- TCP throughput: Scales with buffer size and TCP tuning

ERROR HANDLING:

- All errors raise 'transport-error condition
- Includes errno information when available
- Background threads log errors and continue running
- Statistics track error counts

RESOURCE MANAGEMENT:

- Finalizers ensure cleanup on GC (SBCL, CCL)
- Explicit cleanup with (close-transport transport)
- All sockets properly closed on shutdown
- Threads gracefully terminated

|#
