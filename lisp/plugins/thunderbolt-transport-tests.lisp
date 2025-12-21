;;;; thunderbolt-transport-tests.lisp
;;;; Comprehensive test suite for Thunderbolt transport
;;;;
;;;; Copyright © 2025 DeMoD LLC

(defpackage :dcf.transport.thunderbolt.tests
  (:use :cl :fiveam :dcf.transport.thunderbolt)
  (:import-from :bordeaux-threads
                #:make-thread
                #:join-thread)
  (:export #:run-thunderbolt-tests))

(in-package :dcf.transport.thunderbolt.tests)

;;; ---------------------------------------------------------------------------
;;; Test Suite Definition
;;; ---------------------------------------------------------------------------

(def-suite thunderbolt-transport-suite
  :description "Test suite for Thunderbolt/USB4 transport layer")

(in-suite thunderbolt-transport-suite)

;;; ---------------------------------------------------------------------------
;;; Helper Functions
;;; ---------------------------------------------------------------------------

(defvar *test-logger-messages* nil)
(defvar *test-logger-lock* (bt:make-lock "Test Logger Lock"))

(defun test-logger (level message)
  "Test logger that captures messages for verification."
  (bt:with-lock-held (*test-logger-lock*)
    (push (list level message) *test-logger-messages*)))

(defun clear-test-log ()
  "Clear captured log messages."
  (bt:with-lock-held (*test-logger-lock*)
    (setf *test-logger-messages* nil)))

(defun find-log-message (level-or-substring)
  "Find a log message containing substring or matching level."
  (bt:with-lock-held (*test-logger-lock*)
    (find-if (lambda (entry)
               (destructuring-bind (level msg) entry
                 (or (eq level level-or-substring)
                     (and (stringp level-or-substring)
                          (search level-or-substring msg)))))
             *test-logger-messages*)))

(defun make-test-message (size)
  "Create a test message of specified size."
  (let ((msg (make-array size :element-type '(unsigned-byte 8))))
    (loop for i below size
          do (setf (aref msg i) (mod i 256)))
    msg))

(defun verify-test-message (msg expected-size)
  "Verify a test message has correct content."
  (and (= (length msg) expected-size)
       (loop for i below expected-size
             always (= (aref msg i) (mod i 256)))))

;;; ---------------------------------------------------------------------------
;;; Basic Functionality Tests
;;; ---------------------------------------------------------------------------

(test create-transport-loopback
  "Test creating transport on loopback interface"
  (clear-test-log)
  (let ((transport nil))
    (unwind-protect
         (progn
           (setf transport 
                 (make-thunderbolt-transport
                  :interface "lo"
                  :control-port 15000
                  :data-port 15001
                  :logger #'test-logger))
           (is (not (null transport)))
           (is (not (null (find-log-message "Initialized interface"))))
           (is (not (null (find-log-message "UDP socket bound")))))
      (when transport
        (close-transport transport)))))

(test transport-statistics
  "Test statistics tracking"
  (let ((transport (make-thunderbolt-transport
                    :interface "lo"
                    :control-port 15010
                    :data-port 15011
                    :logger #'test-logger)))
    (unwind-protect
         (let ((stats (get-transport-stats transport)))
           (is (getf stats :messages-sent) 0)
           (is (getf stats :messages-received) 0)
           (is (getf stats :bytes-sent) 0)
           (is (getf stats :bytes-received) 0)
           
           ;; Reset and verify
           (reset-transport-stats transport)
           (setf stats (get-transport-stats transport))
           (is (= (getf stats :errors) 0)))
      (close-transport transport))))

;;; ---------------------------------------------------------------------------
;;; Loopback Communication Tests
;;; ---------------------------------------------------------------------------

(test udp-loopback-echo
  "Test UDP message send/receive on loopback"
  (let ((received-messages nil)
        (received-lock (bt:make-lock "Received Lock"))
        (transport nil))
    (unwind-protect
         (progn
           (setf transport
                 (make-thunderbolt-transport
                  :interface "lo"
                  :control-port 15020
                  :data-port 15021
                  :message-handler (lambda (type data)
                                    (when (eq type :control-message)
                                      (bt:with-lock-held (received-lock)
                                        (push data received-messages))))
                  :logger #'test-logger))
           
           ;; Give transport time to start
           (sleep 0.5)
           
           ;; Send test message to ourselves
           (let ((test-msg (make-test-message 100)))
             (send-message transport test-msg "::1%lo")
             
             ;; Wait for echo
             (sleep 0.5)
             
             ;; Verify we received it
             (bt:with-lock-held (received-lock)
               (is (= (length received-messages) 1))
               (when (> (length received-messages) 0)
                 (is (verify-test-message (first received-messages) 100))))))
      (when transport
        (close-transport transport)))))

(test tcp-loopback-stream
  "Test TCP data stream on loopback"
  (let ((stream-received nil)
        (stream-lock (bt:make-lock "Stream Lock"))
        (transport nil))
    (unwind-protect
         (progn
           (setf transport
                 (make-thunderbolt-transport
                  :interface "lo"
                  :control-port 15030
                  :data-port 15031
                  :message-handler (lambda (type data)
                                    (when (eq type :data-stream)
                                      ;; Echo back what we receive
                                      (let ((byte (read-byte data nil nil)))
                                        (when byte
                                          (write-byte byte data)
                                          (force-output data)
                                          (bt:with-lock-held (stream-lock)
                                            (setf stream-received byte))))))
                  :logger #'test-logger))
           
           ;; Give transport time to start
           (sleep 0.5)
           
           ;; Open data stream and send byte
           (let ((stream (open-data-stream transport "::1%lo")))
             (unwind-protect
                  (progn
                    (write-byte 42 stream)
                    (force-output stream)
                    
                    ;; Read echo
                    (let ((response (read-byte stream)))
                      (is (= response 42)))
                    
                    ;; Verify handler saw it
                    (sleep 0.2)
                    (bt:with-lock-held (stream-lock)
                      (is (= stream-received 42))))
               (close stream))))
      (when transport
        (close-transport transport)))))

;;; ---------------------------------------------------------------------------
;;; RTT Measurement Tests
;;; ---------------------------------------------------------------------------

(test rtt-measurement-loopback
  "Test RTT measurement with echo responder"
  (let ((transport-server nil)
        (transport-client nil))
    (unwind-protect
         (progn
           ;; Server that echoes RTT pings
           (setf transport-server
                 (make-thunderbolt-transport
                  :interface "lo"
                  :control-port 15040
                  :message-handler (lambda (type data)
                                    (when (eq type :control-message)
                                      ;; Simple echo server for RTT
                                      (when (and (= (length data) 16)
                                               (= (aref data 0) #x00)
                                               (= (aref data 1) #x00)
                                               (= (aref data 2) #x00)
                                               (= (aref data 3) #x00)
                                               (= (aref data 4) #x52)
                                               (= (aref data 5) #x54)
                                               (= (aref data 6) #x54)
                                               (= (aref data 7) #x50)
                                               (= (aref data 8) #x54))
                                        ;; Change magic to PONG
                                        (setf (aref data 8) #x53)
                                        (send-message transport-server data "::1%lo"))))
                  :logger #'test-logger))
           
           ;; Client that measures RTT
           (setf transport-client
                 (make-thunderbolt-transport
                  :interface "lo"
                  :control-port 15041
                  :logger #'test-logger))
           
           ;; Give transports time to start
           (sleep 0.5)
           
           ;; Measure RTT (should be very low on loopback)
           (let ((rtt (measure-rtt transport-client "::1%lo" :timeout 2.0)))
             (is (not (null rtt)))
             (when rtt
               (is (numberp rtt))
               (is (< rtt 0.1))  ; Should be under 100ms on loopback
               (format t "~%Loopback RTT: ~,6F ms~%" (* rtt 1000)))))
      (when transport-server (close-transport transport-server))
      (when transport-client (close-transport transport-client)))))

;;; ---------------------------------------------------------------------------
;;; Large Transfer Tests
;;; ---------------------------------------------------------------------------

(test large-transfer-10mb
  "Test transferring 10 MB over TCP stream"
  (let ((bytes-received 0)
        (receive-lock (bt:make-lock "Receive Lock"))
        (transport nil))
    (unwind-protect
         (progn
           (setf transport
                 (make-thunderbolt-transport
                  :interface "lo"
                  :control-port 15050
                  :data-port 15051
                  :message-handler (lambda (type data)
                                    (when (eq type :data-stream)
                                      ;; Read all available data
                                      (loop for byte = (read-byte data nil nil)
                                            while byte
                                            do (bt:with-lock-held (receive-lock)
                                                 (incf bytes-received)))))
                  :logger #'test-logger))
           
           (sleep 0.5)
           
           ;; Send 10 MB
           (let* ((chunk-size (* 1024 1024))  ; 1 MB chunks
                  (total-size (* 10 1024 1024))  ; 10 MB
                  (stream (open-data-stream transport "::1%lo"))
                  (start-time (get-internal-real-time)))
             
             (unwind-protect
                  (progn
                    (loop for offset from 0 below total-size by chunk-size
                          for remaining = (- total-size offset)
                          for size = (min chunk-size remaining)
                          do (let ((chunk (make-test-message size)))
                               (loop for byte across chunk
                                     do (write-byte byte stream)))
                             (force-output stream))
                    
                    (let* ((end-time (get-internal-real-time))
                           (elapsed (/ (- end-time start-time)
                                      internal-time-units-per-second))
                           (mbps (/ (/ total-size 1024 1024) elapsed)))
                      (format t "~%Transferred 10 MB in ~,2F seconds (~,2F MB/s)~%"
                             elapsed mbps)
                      
                      ;; Wait for all data to be received
                      (sleep 1.0)
                      
                      (bt:with-lock-held (receive-lock)
                        (is (>= bytes-received (* 9 1024 1024)))  ; At least 9 MB
                        (format t "Received ~,2F MB~%" 
                               (/ bytes-received 1024.0 1024.0)))))
               (close stream))))
      (when transport
        (close-transport transport)))))

(test large-transfer-100mb
  "Test transferring 100 MB over TCP stream (stress test)"
  :depends-on '(large-transfer-10mb)
  (let ((bytes-received 0)
        (receive-lock (bt:make-lock "Receive Lock"))
        (transport nil))
    (unwind-protect
         (progn
           (setf transport
                 (make-thunderbolt-transport
                  :interface "lo"
                  :control-port 15060
                  :data-port 15061
                  :message-handler (lambda (type data)
                                    (when (eq type :data-stream)
                                      (handler-case
                                          (loop for byte = (read-byte data nil nil)
                                                while byte
                                                do (bt:with-lock-held (receive-lock)
                                                     (incf bytes-received)))
                                        (error (e)
                                          (format t "~%Receiver error: ~A~%" e)))))
                  :logger #'test-logger))
           
           (sleep 0.5)
           
           ;; Send 100 MB in 10 MB chunks
           (let* ((chunk-size (* 10 1024 1024))  ; 10 MB chunks
                  (total-size (* 100 1024 1024))  ; 100 MB
                  (stream (open-data-stream transport "::1%lo"))
                  (start-time (get-internal-real-time)))
             
             (unwind-protect
                  (progn
                    (format t "~%Sending 100 MB...~%")
                    (loop for offset from 0 below total-size by chunk-size
                          for chunk-num from 1
                          for remaining = (- total-size offset)
                          for size = (min chunk-size remaining)
                          do (progn
                               (let ((chunk (make-array size 
                                                       :element-type '(unsigned-byte 8)
                                                       :initial-element (mod chunk-num 256))))
                                 (loop for byte across chunk
                                       do (write-byte byte stream))
                                 (force-output stream))
                               (format t "Sent chunk ~D/10~%" chunk-num)))
                    
                    (let* ((end-time (get-internal-real-time))
                           (elapsed (/ (- end-time start-time)
                                      internal-time-units-per-second))
                           (mbps (/ (/ total-size 1024 1024) elapsed)))
                      (format t "Transferred 100 MB in ~,2F seconds (~,2F MB/s)~%"
                             elapsed mbps)
                      
                      ;; Wait for all data to be received
                      (sleep 2.0)
                      
                      (bt:with-lock-held (receive-lock)
                        (let ((received-mb (/ bytes-received 1024.0 1024.0)))
                          (format t "Received ~,2F MB~%" received-mb)
                          (is (>= bytes-received (* 90 1024 1024)))))))  ; At least 90 MB
               (close stream))))
      (when transport
        (close-transport transport)))))

;;; ---------------------------------------------------------------------------
;;; Error Handling and Recovery Tests
;;; ---------------------------------------------------------------------------

(test invalid-interface
  "Test error handling for invalid interface"
  (signals transport-error
    (make-thunderbolt-transport
     :interface "nonexistent-interface-xyz123")))

(test invalid-peer-address
  "Test error handling for invalid peer address"
  (let ((transport (make-thunderbolt-transport
                    :interface "lo"
                    :control-port 15070
                    :logger #'test-logger)))
    (unwind-protect
         (signals transport-error
           (send-message transport #(1 2 3) "invalid-address"))
      (close-transport transport))))

(test oversized-message
  "Test error handling for oversized UDP message"
  (let ((transport (make-thunderbolt-transport
                    :interface "lo"
                    :control-port 15080
                    :logger #'test-logger)))
    (unwind-protect
         (signals transport-error
           (let ((huge-msg (make-array 100000 :element-type '(unsigned-byte 8))))
             (send-message transport huge-msg "::1%lo")))
      (close-transport transport))))

(test connection-refused
  "Test handling of connection refused (no listener)"
  (let ((transport (make-thunderbolt-transport
                    :interface "lo"
                    :control-port 15090
                    :data-port 15091
                    :logger #'test-logger)))
    (unwind-protect
         (progn
           (sleep 0.2)
           ;; Try to connect to port where nothing is listening
           (signals transport-error
             (open-data-stream transport "::1%lo")))
      (close-transport transport))))

(test unplug-simulation
  "Simulate unplugging by closing socket during transfer"
  (let ((transport nil)
        (errors-before 0)
        (errors-after 0))
    (unwind-protect
         (progn
           (setf transport
                 (make-thunderbolt-transport
                  :interface "lo"
                  :control-port 15100
                  :data-port 15101
                  :message-handler (lambda (type data)
                                    (declare (ignore type data)))
                  :logger #'test-logger))
           
           (sleep 0.5)
           
           ;; Get error count before
           (setf errors-before (getf (get-transport-stats transport) :errors))
           
           ;; Open stream and then close transport (simulates unplug)
           (let ((stream (open-data-stream transport "::1%lo")))
             (sleep 0.1)
             (close-transport transport)
             (setf transport nil)
             
             ;; Try to write to closed stream
             (signals error
               (write-byte 42 stream)
               (force-output stream))
             
             (ignore-errors (close stream))))
      (when transport
        (close-transport transport)))))

;;; ---------------------------------------------------------------------------
;;; Performance Benchmarks
;;; ---------------------------------------------------------------------------

(test benchmark-udp-latency
  "Benchmark UDP message latency"
  (let ((transport nil)
        (received-count 0)
        (receive-lock (bt:make-lock "Receive Lock")))
    (unwind-protect
         (progn
           (setf transport
                 (make-thunderbolt-transport
                  :interface "lo"
                  :control-port 15110
                  :message-handler (lambda (type data)
                                    (declare (ignore data))
                                    (when (eq type :control-message)
                                      (bt:with-lock-held (receive-lock)
                                        (incf received-count))))
                  :logger #'test-logger))
           
           (sleep 0.5)
           
           (let* ((iterations 1000)
                  (msg (make-test-message 64))
                  (start-time (get-internal-real-time)))
             
             (dotimes (i iterations)
               (send-message transport msg "::1%lo"))
             
             (let* ((end-time (get-internal-real-time))
                    (elapsed (/ (- end-time start-time)
                               internal-time-units-per-second))
                    (avg-latency (/ elapsed iterations)))
               
               (format t "~%Sent ~D UDP messages in ~,3F seconds~%" iterations elapsed)
               (format t "Average latency: ~,6F ms~%" (* avg-latency 1000))
               (format t "Messages per second: ~,0F~%" (/ iterations elapsed))
               
               (is (< avg-latency 0.001)))))  ; Should be under 1ms
      (when transport
        (close-transport transport)))))

;;; ---------------------------------------------------------------------------
;;; Test Runner
;;; ---------------------------------------------------------------------------

(defun run-thunderbolt-tests (&key (verbose t))
  "Run all Thunderbolt transport tests."
  (format t "~%=== Thunderbolt Transport Test Suite ===~%~%")
  (format t "Note: These tests use loopback interface (lo) for safety.~%")
  (format t "For real Thunderbolt testing, manually test with actual hardware.~%~%")
  (let ((results (run 'thunderbolt-transport-suite)))
    (when verbose
      (format t "~%~%=== Test Results ===~%")
      (format t "Passed: ~D~%" (length (remove-if-not #'test-passed-p results)))
      (format t "Failed: ~D~%" (length (remove-if-not #'test-failure-p results)))
      (format t "Errors: ~D~%" (length (remove-if-not #'test-skipped-p results))))
    results))

;;; Export for easy running
(export 'run-thunderbolt-tests)
