
(in-package :d-lisp)

#+fiveam
(fiveam:def-suite d-lisp-suite
  :description "Test suite for D-LISP SDK.")

#+fiveam
(fiveam:in-suite d-lisp-suite)

#+fiveam
(fiveam:test version-test
  "Test version information."
  (fiveam:is (equal (dcf-version) '(:version "1.4.0" :dcf-version "5.0.0"))))

#+fiveam
(fiveam:test middleware-test
  "Test middleware application."
  (add-middleware (lambda (msg dir) (declare (ignore dir)) msg))
  (fiveam:is (equal (apply-middlewares (make-instance 'dcf-message :sender "test" :recipient "test" :data "test" :timestamp 0 :sync t :sequence 0 :redundancy-path "" :group-id "") :send)
                    (make-instance 'dcf-message :sender "test" :recipient "test" :data "test" :timestamp 0 :sync t :sequence 0 :redundancy-path "" :group-id ""))))

#+fiveam
(fiveam:test type-system-test
  "Test type system validation."
  (fiveam:signals dcf-error (make-instance 'dcf-message :sender 123)))

#+fiveam
(fiveam:test connection-pool-test
  "Test connection pooling."
  (let ((node (make-instance 'dcf-node)))
    (initialize-connection-pool node)
    (fiveam:is (arrayp (gethash "grpc" (connection-pool node))))))

#+fiveam
(fiveam:test network-scenario-test
  "Test network failure and recovery."
  (let ((node (make-instance 'dcf-node :config (make-dcf-config :peers '("peer1" "peer2")))))
    (dcf-group-peers)
    (dcf-simulate-failure "peer1")
    (fiveam:is (= (length (dcf-config-peers (config node))) 1))))

#+fiveam
(fiveam:test metrics-test
  "Test metrics collection."
  (let ((node (make-instance 'dcf-node)))
    (incf (gethash :tests (metrics node) 0))
    (fiveam:is (= (gethash :tests (dcf-get-metrics)) 1))))

#+fiveam
(fiveam:test visualize-test
  "Test topology visualization."
  (let ((node (make-instance 'dcf-node)))
    (setf (peer-groups node) (make-hash-table))
    (dcf-visualize-topology "test.dot")
    (fiveam:is (probe-file "test.dot"))
    (delete-file "test.dot")))

#+fiveam
(fiveam:test config-validation-test
  "Test configuration validation."
  (let ((valid-config-path "test-config.json"))
    (with-open-file (stream valid-config-path :direction :output :if-exists :supersede)
      (cl-json:encode-json
       '((:transport . "gRPC") (:host . "localhost") (:port . 50051) (:mode . "client") (:node-id . "test-node"))
       stream))
    (fiveam:is (equal (getf (dcf-init valid-config-path) :status) "success"))
    (delete-file valid-config-path)))

#+fiveam
(fiveam:test websocket-plugin-test
  "Test WebSocket plugin."
  (fiveam:is (equal (plugin-interface-version websocket-transport) "1.0")))
