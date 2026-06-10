(in-package :d-lisp)

#+fiveam
(fiveam:def-suite d-lisp-suite
  :description "Test suite for D-LISP SDK v2.0.0, covering StreamDB integration, async ops, transactions, and production features.")

#+fiveam
(fiveam:in-suite d-lisp-suite)

#+fiveam
(fiveam:test version-test
  "Test version information matches v2.0.0."
  (fiveam:is (equal (dcf-version) '(:version "2.0.0" :dcf-version "5.0.0"))))

#+fiveam
(fiveam:test middleware-test
  "Test middleware application for message processing."
  (let ((msg (make-instance 'dcf-message :sender "test" :recipient "test" :data "test" :timestamp 0 :sync t :sequence 0 :redundancy-path "" :group-id "")))
    (add-middleware (lambda (m d) (declare (ignore d)) (log:debug *dcf-logger* "Middleware: ~A" m) m))
    (fiveam:is (equalp (apply-middlewares msg :send) msg))
    (remove-middleware (car (dcf-node-middleware *node*)))))

#+fiveam
(fiveam:test type-system-test
  "Test type system validation for dcf-message."
  (fiveam:signals dcf-error
    (make-instance 'dcf-message :sender 123 :recipient "test" :data "test" :timestamp 0 :sync t :sequence 0 :redundancy-path "" :group-id ""))
  (fiveam:is-true (make-instance 'dcf-message :sender "test" :recipient "test" :data "test" :timestamp 0 :sync t :sequence 0 :redundancy-path "" :group-id "")))

#+fiveam
(fiveam:test connection-pool-test
  "Test connection pooling initialization."
  (let ((node (make-dcf-node :config (make-dcf-config))))
    (initialize-connection-pool node)
    (fiveam:is (arrayp (gethash "grpc" (connection-pool node))))
    ;; Cleanup
    (destroy-connection-pool node)))

#+fiveam
(fiveam:test network-scenario-test
  "Test network failure and recovery in P2P mode."
  (let ((node (make-dcf-node :config (make-dcf-config :peers '("peer1" "peer2") :storage "streamdb" :streamdb-path "test.streamdb"))))
    (setf *node* node)
    (unwind-protect
        (progn
          (dcf-group-peers)
          (dcf-simulate-failure "peer1")
          (fiveam:is (= (length (dcf-config-peers (dcf-node-config node))) 1))
          (when (string= (dcf-config-storage (dcf-node-config node)) "streamdb")
            (fiveam:is-true (dcf-db-query node "/state/peer-groups"))))
      (when (dcf-node-streamdb node) (streamdb_close (dcf-node-streamdb node))))))

#+fiveam
(fiveam:test metrics-test
  "Test metrics collection and StreamDB persistence."
  (let ((node (make-dcf-node :config (make-dcf-config :storage "streamdb" :streamdb-path "test.streamdb"))))
    (setf *node* node)
    (unwind-protect
        (progn
          (incf (gethash :tests (dcf-node-metrics node) 0))
          (dcf-db-insert node "/metrics/tests" (cl-json:encode-json-to-string '(:tests 1)))
          (fiveam:is (= (gethash :tests (dcf-get-metrics)) 1))
          (fiveam:is (equal (cl-json:decode-json-from-string (dcf-db-query node "/metrics/tests")) '(:tests 1))))
      (when (dcf-node-streamdb node) (streamdb_close (dcf-node-streamdb node))))))

#+fiveam
(fiveam:test visualize-test
  "Test topology visualization with Graphviz."
  (let ((node (make-dcf-node :peer-groups (make-hash-table))))
    (setf *node* node)
    (dcf-visualize-topology "test.dot")
    (fiveam:is-true (probe-file "test.dot"))
    (delete-file "test.dot")))

#+fiveam
(fiveam:test config-validation-test
  "Test configuration validation with JSON schema."
  (let ((valid-config-path "test-config.json"))
    (with-open-file (stream valid-config-path :direction :output :if-exists :supersede)
      (cl-json:encode-json
       '((:transport . "gRPC") (:host . "localhost") (:port . 50051) (:mode . "client") (:node-id . "test-node") (:storage . "streamdb") (:streamdb-path . "test.streamdb"))
       stream))
    (unwind-protect
        (let ((node (dcf-init valid-config-path)))
          (setf *node* node)
          (fiveam:is (equal (getf (dcf-status) :status) "running")))
      (when (dcf-node-streamdb *node*) (streamdb_close (dcf-node-streamdb *node*)))
      (delete-file valid-config-path))))

#+fiveam
(fiveam:test websocket-plugin-test
  "Test WebSocket plugin interface."
  (let ((node (make-dcf-node :config (make-dcf-config :transport "websocket"))))
    (setf *node* node)
    (unwind-protect
        (progn
          (dcf-load-plugin "plugins/websocket-transport.lisp")  ;; Assume exists
          (fiveam:is (equal (plugin-interface-version websocket-transport) "1.0")))
      (when (dcf-node-streamdb node) (streamdb_close (dcf-node-streamdb node))))))

#+fiveam
(fiveam:test streamdb-integration-test
  "Test StreamDB CRUD, async, and transaction operations."
  (let ((config (make-dcf-config :storage "streamdb" :streamdb-path "test.streamdb"))
        (test-data "{\"value\": \"test\"}"))
    (let ((*node* (make-dcf-node :config config)))
      (unwind-protect
          (progn
            (setf *node* (dcf-init "test-config.json"))
            ;; CRUD
            (dcf-db-insert *node* "/test/key" test-data :schema *streamdb-state-schema*)
            (fiveam:is (equal (dcf-db-query *node* "/test/key") test-data))
            (fiveam:is-true (find "/test/key" (dcf-db-search *node* "/test/")))
            (dcf-db-delete *node* "/test/key")
            (fiveam:is (null (dcf-db-query *node* "/test/key")))
            ;; Async
            (let ((result nil))
              (dcf-db-get-async *node* "/test/key" (lambda (data len err)
                                                     (setf result (if err nil (cffi:foreign-string-to-lisp data :count len)))))
              (fiveam:is (null result)))  ;; Should fail post-delete
            ;; Transactions
            (dcf-db-begin-transaction-async *node* (lambda (err)
                                                     (unless err
                                                       (dcf-db-insert *node* "/test/tx" test-data)
                                                       (dcf-db-commit-transaction-async *node* (lambda (err) (declare (ignore err)))))))
            (fiveam:is (equal (dcf-db-query *node* "/test/tx") test-data))
            (dcf-db-flush *node*))
        (when (dcf-node-streamdb *node*) (streamdb_close (dcf-node-streamdb *node*)))))))

#+fiveam
(fiveam:test retry-logic-test
  "Test retry logic for transient StreamDB errors."
  (let ((node (make-dcf-node :config (make-dcf-config :storage "streamdb" :streamdb-path "test.streamdb")))
        (attempts 0))
    (setf *node* node)
    (unwind-protect
        (progn
          ;; Simulate transient error
          (flet ((fail-first (fn)
                   (if (< attempts 2)
                       (progn (incf attempts) (signal-dcf-error :io "Simulated I/O error"))
                       (funcall fn))))
            (fiveam:is (equal (with-retry (lambda () (fail-first (lambda () (dcf-db-insert *node* "/test/retry" "{\"data\": \"ok\"}")))) "ok"))
            (fiveam:is (= attempts 2))))
      (when (dcf-node-streamdb node) (streamdb_close (dcf-node-streamdb node))))))

#+fiveam
(fiveam:test cache-thread-safety-test
  "Test LRU cache under concurrent access."
  (let ((node (make-dcf-node :config (make-dcf-config :storage "streamdb" :streamdb-path "test.streamdb") :cache '() :cache-size 10 :cache-lock (bt:make-lock))))
    (setf *node* node)
    (unwind-protect
        (progn
          (dcf-db-insert *node* "/test/cache" "{\"data\": \"cached\"}")
          (let ((threads (loop for i from 1 to 5
                               collect (bt:make-thread (lambda () (dotimes (j 100) (lru-get *node* "/test/cache")))))))
            (mapc #'bt:join-thread threads)
            (fiveam:is (equal (lru-get *node* "/test/cache") "{\"data\": \"cached\"}")))
          (fiveam:is (<= (length (dcf-node-cache *node*)) 10)))  ;; Eviction works
      (when (dcf-node-streamdb node) (streamdb_close (dcf-node-streamdb node))))))

#+fiveam
(fiveam:test wasm-streamdb-test
  "Simulate WASM environment for StreamDB compatibility."
  (let ((node (make-dcf-node :config (make-dcf-config :storage "streamdb" :streamdb-path "test.streamdb"))))
    (setf *node* node)
    (unwind-protect
        (progn
          (with-mocks ((wasm-target? () t))  ;; Simulate WASM
            (setf *node* (dcf-init "test-config.json"))
            (dcf-db-insert *node* "/test/wasm" "{\"data\": \"wasm\"}")
            (fiveam:is (equal (dcf-db-query *node* "/test/wasm") "{\"data\": \"wasm\"}")))
          (fiveam:is-true (find "/test/wasm" (dcf-db-search *node* "/test/"))))
      (when (dcf-node-streamdb *node*) (streamdb_close (dcf-node-streamdb *node*))))))

#+fiveam
(fiveam:test streamdb-vs-inmemory-benchmark
  "Benchmark StreamDB vs. in-memory for RTT-sensitive scenarios."
  (let ((in-memory (make-hash-table :test #'equal))
        (*node* (make-dcf-node :config (make-dcf-config :storage "streamdb" :streamdb-path "test.streamdb"))))
    (unwind-protect
        (progn
          (setf *node* (dcf-init "test-config.json"))
          (let ((in-time (get-internal-run-time))
                (in-result (loop repeat 1000 do (setf (gethash "/test" in-memory) "data")))
                (in-end (get-internal-run-time))
                (db-time (get-internal-run-time))
                (db-result (loop repeat 1000 do (with-retry (lambda () (dcf-db-insert *node* "/test" "data")))))
                (db-end (get-internal-run-time))
                (tx-time (get-internal-run-time))
                (tx-result (dcf-db-begin-transaction-async *node* (lambda (err)
                                                                   (unless err
                                                                     (dcf-db-insert *node* "/test/tx" "data")
                                                                     (dcf-db-commit-transaction-async *node* (lambda (err) (declare (ignore err))))))))
                (tx-end (get-internal-run-time)))
            (declare (ignore in-result db-result tx-result))
            (fiveam:is (< (- db-end db-time) (* 1.5 (- in-end in-time))))
            (fiveam:is (< (- tx-end tx-time) (* internal-time-units-per-second 0.01)))))
      (when (dcf-node-streamdb *node*) (streamdb_close (dcf-node-streamdb *node*))))))

#+fiveam
(defun run-tests ()
  "Run all FiveAM tests."
  (fiveam:run! 'd-lisp-suite)
  (log:info *dcf-logger* "FiveAM tests completed."))
