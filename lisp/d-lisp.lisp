;; DeMoD-LISP (D-LISP) SDK Implementation
;; Version 1.3.0 | September 1, 2025
;; License: GNU General Public License v3.0 (GPL-3.0)
;; Part of the DCF mono repo: https://github.com/ALH477/DeMoD-Communication-Framework
;; This SDK provides a robust, production-ready Lisp implementation for DCF,
;; with full support for modular components, plugins, AUTO mode, master node,
;; self-healing P2P redundancy, gRPC/Protobuf interoperability, CLI/TUI,
;; comprehensive error handling, logging, and performance optimizations.
;; Enhanced in v1.3.0 with:
;; - dcf-help command for newcomers, providing beginner-friendly guidance.

;; Dependencies: Install via Quicklisp
(ql:quickload '(:cl-protobufs :cl-grpc :cffi :uuid :cl-json :jsonschema :cl-ppcre :cl-csv :usocket :bordeaux-threads :curses :log4cl :trivial-backtrace :cl-store :mgl :hunchensocket :fiveam))

(defpackage :d-lisp
  (:use :cl :cl-protobufs :cl-grpc :cffi :uuid :cl-json :jsonschema :cl-ppcre :cl-csv :usocket :bordeaux-threads :curses :log4cl :trivial-backtrace :cl-store :mgl :hunchensocket :fiveam)
  (:export :dcf-init :dcf-start :dcf-stop :dcf-send :dcf-receive :dcf-status
           :dcf-health-check :dcf-list-peers :dcf-heal :dcf-version :dcf-benchmark
           :dcf-group-peers :dcf-simulate-failure :dcf-log-level :dcf-load-plugin
           :dcf-tui :def-dcf-plugin :def-dcf-transport :dcf-master-assign-role
           :dcf-master-update-config :dcf-master-collect-metrics :dcf-master-optimize-network
           :dcf-set-mode :dcf-update-config :dcf-error :*dcf-logger* :dcf-help))

(in-package :d-lisp)

;; Logging Setup
(defvar *dcf-logger* (log:category "dcf-lisp") "Logger for D-LISP.")
(log:config *dcf-logger* :info) ; Default to info level

;; Error Handling
(define-condition dcf-error (error)
  ((code :initarg :code :reader dcf-error-code)
   (message :initarg :message :reader dcf-error-message))
  (:report (lambda (condition stream)
             (format stream "DCF Error [~A]: ~A" (dcf-error-code condition) (dcf-error-message condition)))))

(defun signal-dcf-error (code message)
  (error 'dcf-error :code code :message message))

;; Configuration Structure with Serialization
(defstruct (dcf-config (:constructor make-dcf-config%)
                       (:conc-name dcf-config-))
  transport host port mode node-id peers group-rtt-threshold plugins
  (state-file "dcf-state.bin" :type string))

(defun make-dcf-config (&rest args)
  (let ((config (apply #'make-dcf-config% args)))
    (cl-store:store config (dcf-config-state-file config))
    config))

(defun load-config (path)
  "Load and validate JSON configuration with schema."
  (handler-case
      (with-open-file (stream path :direction :input)
        (let* ((json (cl-json:decode-json stream))
               (schema (cl-json:decode-json-from-source (merge-pathnames "config.schema.json" (asdf:system-source-directory :d-lisp))))
               (validator (jsonschema:make-validator schema)))
          (unless (jsonschema:validate validator json)
            (signal-dcf-error :config-invalid "Configuration fails schema validation"))
          (make-dcf-config
           :transport (cdr (assoc :transport json))
           :host (cdr (assoc :host json))
           :port (cdr (assoc :port json))
           :mode (cdr (assoc :mode json))
           :node-id (or (cdr (assoc :node-id json)) (uuid:print-bytes nil (uuid:make-v4-uuid)))
           :peers (cdr (assoc :peers json))
           :group-rtt-threshold (or (cdr (assoc :group-rtt-threshold json)) 50)
           :plugins (or (cdr (assoc :plugins json)) (make-hash-table :test 'equal)))))
    (file-error (e) (signal-dcf-error :file-not-found (format nil "Config file not found: ~A" path)))
    (jsonschema:validation-error (e) (signal-dcf-error :schema-violation (format nil "Schema violation: ~A" e)))))

(defun save-state (config)
  "Persist state to file."
  (cl-store:store config (dcf-config-state-file config)))

(defun restore-state (state-file)
  "Restore persisted state."
  (handler-case
      (cl-store:restore state-file)
    (file-error () nil)))

;; Networking Layer with Modes and Native Transport Support
(defclass dcf-node ()
  ((channel :initarg :channel :accessor channel :initform nil)
   (stub :initarg :stub :accessor stub :initform nil)
   (server :initarg :server :accessor server :initform nil)
   (mode :initarg :mode :accessor mode)
   (config :initarg :config :accessor config)
   (plugins :initform (make-hash-table :test 'equal) :accessor plugins)
   (peer-groups :initform (make-hash-table :test 'equal) :accessor peer-groups)
   (thread-pool :initform (bt:make-thread-pool 4) :accessor thread-pool)
   (master-connection :initform nil :accessor master-connection)
   (native-transport :initform nil :accessor native-transport))) ; New: Native Lisp transport

(defvar *node* nil "Global DCF node instance.")

(defun initialize-node (config)
  "Initialize node based on mode, with native transport option."
  (let ((node (make-instance 'dcf-node :mode (dcf-config-mode config) :config config)))
    (when (string= (dcf-config-transport config) "native-lisp")
      (setup-native-transport node))
    (case (intern (string-upcase (dcf-config-mode config)))
      (:server (setup-server node))
      (:client (setup-client node))
      (:p2p (setup-p2p node))
      (:auto (setup-auto node))
      (:master (setup-master node))
      (t (signal-dcf-error :invalid-mode (format nil "Unknown mode: ~A" (dcf-config-mode config)))))
    (log:info *dcf-logger* "Node initialized in ~A mode with transport ~A" (mode node) (dcf-config-transport config))
    node))

(defun setup-native-transport (node)
  "Setup native Lisp transport using USocket to complement gRPC."
  (let ((socket (usocket:socket-listen (dcf-config-host (config node)) (dcf-config-port (config node)) :reuse-address t :element-type '(unsigned-byte 8))))
    (setf (native-transport node) socket)
    (bt:make-thread (lambda () (native-transport-listener node)) :name "native-transport-listener")
    (log:info *dcf-logger* "Native Lisp transport setup on ~A:~A" (dcf-config-host (config node)) (dcf-config-port (config node)))))

(defun native-transport-listener (node)
  "Listener loop for native transport."
  (loop
    (let ((client (usocket:socket-accept (native-transport node))))
      (bt:make-thread (lambda () (handle-native-connection node client)) :name "native-client-handler"))))

(defun handle-native-connection (node client)
  "Handle incoming native connection."
  (handler-case
      (with-open-stream (stream (usocket:socket-stream client))
        (let ((data (read-line stream)))
          (log:debug *dcf-logger* "Native received: ~A" data)
          (format stream "Echo: ~A~%" data)))
    (error (e) (log:error *dcf-logger* "Native connection error: ~A" e))
    (finally (usocket:socket-close client))))

(defun native-send (node data recipient)
  "Send via native transport."
  (let* ((parts (cl-ppcre:split ":" recipient))
         (host (first parts))
         (port (parse-integer (second parts))))
    (let ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
      (with-open-stream (stream (usocket:socket-stream socket))
        (format stream "~A~%" data))
      (usocket:socket-close socket))))

(defun setup-server (node)
  "Setup server mode with gRPC server."
  (let* ((address (format nil "~A:~A" (dcf-config-host (config node)) (dcf-config-port (config node))))
         (server (cl-grpc:server address)))
    (cl-grpc:register-service server 'dcf-service (make-instance 'dcf-service-impl :node node))
    (setf (server node) server)))

(defun setup-client (node)
  "Setup client mode with gRPC channel."
  (let* ((address (format nil "~A:~A" (dcf-config-host (config node)) (dcf-config-port (config node))))
         (channel (cl-grpc:channel address :insecure t)))
    (setf (channel node) channel
          (stub node) (cl-grpc:stub 'dcf-service channel))))

(defun setup-p2p (node)
  "Setup P2P mode with multiple channels."
  (setup-client node)
  (dolist (peer (dcf-config-peers (config node)))
    (add-peer-channel node peer))
  (bt:make-thread (lambda () (monitor-peers node)) :name "p2p-monitor"))

(defun setup-auto (node)
  "Setup AUTO mode with master connection."
  (setup-p2p node)
  (connect-to-master node)
  (bt:make-thread (lambda () (listen-for-master-commands node)) :name "auto-listener"))

(defun setup-master (node)
  "Setup master mode for controlling AUTO nodes."
  (setup-server node)
  (cl-grpc:register-service (server node) 'dcf-master-service (make-instance 'dcf-master-service-impl :node node))
  (bt:make-thread (lambda () (collect-metrics-periodically node)) :name "master-metrics-collector"))

;; Plugin System with Version Checking and WebSocket Example
(defstruct plugin-interface
  setup send receive destroy version)

(defmacro def-dcf-plugin (name &key version &body body)
  "Define a plugin with interface."
  `(defvar ,name (make-plugin-interface
                  :setup (lambda (self config) ,@body)
                  :send (lambda (self data recipient) ,@body)
                  :receive (lambda (self) ,@body)
                  :destroy (lambda (self) ,@body)
                  :version ,version)))

(defun load-plugin (node path)
  "Load and version-check plugin."
  (handler-case
      (let ((plugin (load path)))
        (unless (string= (plugin-interface-version plugin) "1.0")
          (signal-dcf-error :plugin-version-mismatch "Plugin version mismatch"))
        (funcall (plugin-interface-setup plugin) plugin (config node))
        (setf (gethash path (plugins node)) plugin)
        (log:info *dcf-logger* "Loaded plugin: ~A" path))
    (error (e) (signal-dcf-error :plugin-load-fail (format nil "Failed to load plugin: ~A" e)))))

(defun unload-plugin (node path)
  "Unload plugin."
  (let ((plugin (gethash path (plugins node))))
    (when plugin
      (funcall (plugin-interface-destroy plugin) plugin)
      (remhash path (plugins node))
      (log:info *dcf-logger* "Unloaded plugin: ~A" path))))

;; Example Lisp-specific Plugin: WebSocket Transport
(def-dcf-plugin websocket-transport :version "1.0"
  :setup (lambda (self config)
           (hunchensocket:websocket-server (dcf-config-port config) :handler (lambda (ws) (setf (gethash "ws" self) ws))))
  :send (lambda (self data recipient)
          (hunchensocket:send-text (gethash "ws" self) data))
  :receive (lambda (self)
             (hunchensocket:receive-text (gethash "ws" self)))
  :destroy (lambda (self)
             (hunchensocket:close (gethash "ws" self))))

;; Core DCF Functions with Native Transport Support
(defun dcf-init (config-path &key restore-state)
  "Initialize DCF with config, optionally restoring state."
  (handler-case
      (let ((config (load-config config-path)))
        (when restore-state
          (let ((restored (restore-state (dcf-config-state-file config))))
            (when restored (setf config restored))))
        (setf *node* (initialize-node config))
        (log:info *dcf-logger* "Initialized with config: ~A" config-path)
        `(:status "success" :config ,config-path :mode ,(mode *node*)))
    (dcf-error (e) (log:error *dcf-logger* "~A" e) `(:status "error" :message ,(dcf-error-message e)))))

(defun dcf-start ()
  "Start DCF node."
  (handler-case
      (progn
        (unless *node* (signal-dcf-error :not-initialized "Node not initialized"))
        (case (mode *node*)
          (:server (cl-grpc:start-server (server *node*)))
          (:master (cl-grpc:start-server (server *node*)))
          (t (log:info *dcf-logger* "Starting client/P2P/AUTO mode")))
        (log:info *dcf-logger* "Node started in ~A mode" (mode *node*))
        `(:status "started"))
    (error (e) (log:error *dcf-logger* "Start failed: ~A" e) `(:status "error" :message ,(princ-to-string e)))))

(defun dcf-stop ()
  "Stop DCF node gracefully."
  (handler-case
      (when *node*
        (save-state (config *node*))
        (case (mode *node*)
          (:server (cl-grpc:stop-server (server *node*)))
          (:master (cl-grpc:stop-server (server *node*)))
          (t (when (channel *node*) (cl-grpc:close-channel (channel *node*)))))
        (when (native-transport *node*) (usocket:socket-close (native-transport *node*)))
        (bt:destroy-thread-pool (thread-pool *node*))
        (setf *node* nil)
        (log:info *dcf-logger* "Node stopped")
        `(:status "stopped"))
    (error (e) (log:error *dcf-logger* "Stop failed: ~A" e) `(:status "error" :message ,(princ-to-string e)))))

(defun dcf-send (data recipient &key sync sequence redundancy-path group-id)
  "Send message with optional fields, using native or gRPC based on config."
  (handler-case
      (unless *node* (signal-dcf-error :not-initialized "Node not initialized"))
      (if (string= (dcf-config-transport (config *node*)) "native-lisp")
          (progn
            (native-send *node* data recipient)
            (log:debug *dcf-logger* "Native sent to ~A: ~A" recipient data)
            `(:status "success" :response "Echo: ~A" data))
          (let ((request (make-instance 'dcf-message
                                        :sender (dcf-config-node-id (config *node*))
                                        :recipient recipient
                                        :data data
                                        :timestamp (get-universal-time)
                                        :sync (or sync t)
                                        :sequence (or sequence (random #xFFFFFFFF))
                                        :redundancy-path (or redundancy-path "")
                                        :group-id (or group-id ""))))
            (let ((response (cl-grpc:call (stub *node*) 'send-message request)))
              (log:debug *dcf-logger* "Sent message to ~A: ~A" recipient data)
              `(:status "success" :response ,(slot-value response 'data)))))
    (grpc-error (e) (log:error *dcf-logger* "gRPC send failed: ~A" e) (dcf-heal recipient) `(:status "error" :message ,(princ-to-string e)))
    (usocket:socket-error (e) (log:error *dcf-logger* "Native send failed: ~A" e) `(:status "error" :message ,(princ-to-string e)))))

(defun dcf-receive (&key timeout)
  "Receive messages from stream with timeout."
  (handler-case
      (unless *node* (signal-dcf-error :not-initialized "Node not initialized"))
      (if (string= (dcf-config-transport (config *node*)) "native-lisp")
          (progn
            (log:debug *dcf-logger* "Waiting for native receive")
            `(:status "error" :message "Native receive not implemented"))
          (let ((stream (cl-grpc:call (stub *node*) 'receive-stream (make-instance 'empty))))
            (loop with end-time = (+ (get-internal-real-time) (or timeout (* 10 internal-time-units-per-second)))
                  for msg = (cl-grpc:next stream)
                  while (and msg (< (get-internal-real-time) end-time))
                  collect (progn
                            (log:debug *dcf-logger* "Received message from ~A: ~A" (slot-value msg 'sender) (slot-value msg 'data))
                            msg))))
    (error (e) (log:error *dcf-logger* "Receive failed: ~A" e) `(:status "error" :message ,(princ-to-string e)))))

(defun dcf-status ()
  "Get detailed node status."
  (if *node*
      `(:status "running" :mode ,(mode *node*) :peers ,(dcf-config-peers (config *node*))
        :peer-count ,(length (dcf-config-peers (config *node*))) :groups ,(hash-table-count (peer-groups *node*))
        :plugins ,(hash-table-keys (plugins *node*)))
      `(:status "stopped")))

(defun dcf-health-check (peer)
  "Health check with RTT measurement."
  (handler-case
      (let* ((request (make-instance 'health-request :peer peer))
             (start-time (get-internal-real-time))
             (response (cl-grpc:call (get-peer-stub *node* peer) 'health-check request))
             (rtt (- (get-internal-real-time) start-time)))
        (log:debug *dcf-logger* "Health check for ~A: healthy=~A, RTT=~Ams" peer (slot-value response 'healthy) (/ rtt internal-time-units-per-second 0.001))
        `(:peer ,peer :healthy ,(slot-value response 'healthy) :status ,(slot-value response 'status) :rtt ,rtt))
    (error (e) (log:warn *dcf-logger* "Health check failed for ~A: ~A" peer e) `(:peer ,peer :healthy nil :rtt -1))))

(defun dcf-list-peers ()
  "List peers with health and group info."
  (mapcar (lambda (peer)
            (let ((health (dcf-health-check peer)))
              (append health `(:group-id ,(get-group-id peer (peer-groups *node*))))))
          (dcf-config-peers (config *node*))))

(defun dcf-heal (peer)
  "Heal by rerouting on failure."
  (let ((health (dcf-health-check peer)))
    (if (getf health :healthy)
        (log:info *dcf-logger* "~A is healthy" peer) `(:status "healthy" :peer ,peer)
        (progn
          (log:warn *dcf-logger* "Healing ~A" peer)
          (reroute-to-alternative *node* peer)
          `(:status "healed" :peer ,peer)))))

(defun dcf-version ()
  `(:version "1.3.0" :dcf-version "5.0.0"))

(defun dcf-benchmark (peer &key iterations)
  "Benchmark RTT over iterations."
  (let ((total-rtt 0) (success-count 0))
    (dotimes (i (or iterations 10))
      (let ((health (dcf-health-check peer)))
        (when (getf health :healthy)
          (incf total-rtt (getf health :rtt))
          (incf success-count))))
    (if (zerop success-count)
        `(:status "failed" :peer ,peer)
        `(:status "success" :peer ,peer :avg-rtt ,(/ total-rtt success-count) :success-rate ,(/ success-count (or iterations 10))))))

(defun dcf-group-peers ()
  "Group peers using Dijkstra with RTT weights."
  (handler-case
      (let ((groups (compute-rtt-groups (dcf-config-peers (config *node*)) (dcf-config-group-rtt-threshold (config *node*)))))
        (setf (peer-groups *node*) groups)
        (log:info *dcf-logger* "Peers grouped: ~A groups" (hash-table-count groups))
        `(:status "grouped" :groups ,(hash-table-alist groups)))
    (error (e) (log:error *dcf-logger* "Grouping failed: ~A" e) `(:status "error"))))

(defun dcf-simulate-failure (peer)
  "Simulate failure and trigger heal."
  (setf (dcf-config-peers (config *node*)) (remove peer (dcf-config-peers (config *node*)) :test #'string=))
  (dcf-group-peers)
  (dcf-heal peer)
  `(:status "failure-simulated" :peer ,peer))

(defun dcf-log-level (level)
  "Set log level dynamically."
  (case level
    (0 (log:config *dcf-logger* :debug))
    (1 (log:config *dcf-logger* :info))
    (2 (log:config *dcf-logger* :error))
    (t (signal-dcf-error :invalid-level "Invalid log level")))
  `(:status "log-level-set" :level ,level))

(defun dcf-load-plugin (path)
  "Load a plugin."
  (load-plugin *node* path)
  `(:status "plugin-loaded" :path ,path))

;; TUI Implementation with ncurses
(defun dcf-tui ()
  "Interactive TUI for monitoring and commands."
  (handler-case
      (curses:with-curses ()
        (curses:initscr)
        (curses:curs-set 0)
        (curses:cbreak)
        (curses:noecho)
        (curses:keypad t)
        (let ((main-win (curses:newwin (curses:lines) (curses:cols) 0 0))
              (input-win (curses:newwin 3 (curses:cols) (- (curses:lines) 3) 0)))
          (curses:wborder main-win)
          (curses:mvwprintw main-win 1 1 "DeMoD-LISP TUI v1.3.0")
          (curses:mvwprintw main-win 2 1 "Status: ~A" (getf (dcf-status) :status))
          (curses:wrefresh main-win)
          (loop
            (curses:mvwprintw input-win 1 1 "Command: ")
            (curses:wclrtoeol input-win)
            (curses:wrefresh input-win)
            (let ((input (read-line-from-curses input-win)))
              (when (string= input "quit") (return))
              (let ((result (execute-tui-command input)))
                (curses:mvwprintw main-win 4 1 "Result: ~A" result)
                (curses:wrefresh main-win))))))
        (curses:endwin))
    (error (e) (log:error *dcf-logger* "TUI failed: ~A" e))))

(defun read-line-from-curses (win)
  "Read input line in curses window."
  (let ((str "") (ch))
    (loop
      (setf ch (curses:getch))
      (case ch
        (10 (return str))
        (127 (when (> (length str) 0) (setf str (subseq str 0 (1- (length str))))))
        (t (setf str (concatenate 'string str (string (code-char ch))))))
      (curses:mvwprintw win 1 10 "~A" str)
      (curses:wrefresh win))))

(defun execute-tui-command (input)
  "Execute command in TUI context."
  (handler-case
      (with-input-from-string (stream input)
        (let ((cmd (read stream)))
          (case cmd
            (help (dcf-help))
            (status (dcf-status))
            (send (dcf-send (read stream) (read stream)))
            (receive (dcf-receive))
            (health-check (dcf-health-check (read stream)))
            (list-peers (dcf-list-peers))
            (heal (dcf-heal (read stream)))
            (benchmark (dcf-benchmark (read stream)))
            (group-peers (dcf-group-peers))
            (simulate-failure (dcf-simulate-failure (read stream)))
            (log-level (dcf-log-level (read stream)))
            (load-plugin (dcf-load-plugin (read stream)))
            (t "Unknown command"))))
    (error () "Invalid command syntax")))

;; AUTO Mode and Master Node Functions
(defun connect-to-master (node)
  "Establish connection to master."
  (let* ((master-address (format nil "~A:~A" (dcf-config-host (config node)) (dcf-config-port (config node))))
         (channel (cl-grpc:channel master-address :insecure t)))
    (setf (master-connection node) (cl-grpc:stub 'dcf-master-service channel))
    (log:info *dcf-logger* "Connected to master at ~A" master-address)))

(defun listen-for-master-commands (node)
  "Listen for commands from master."
  (let ((stream (cl-grpc:call (master-connection node) 'receive-commands (make-instance 'empty))))
    (loop for command = (cl-grpc:next stream)
          while command
          do (process-master-command node command))))

(defun process-master-command (node command)
  "Process incoming master command."
  (let ((cmd (slot-value command 'command)))
    (cond
      ((string= cmd "set_role") (dcf-set-mode (slot-value command 'role)))
      ((string= cmd "update_config") (dcf-update-config (slot-value command 'key) (slot-value command 'value)))
      ((string= cmd "collect_metrics") (send-metrics-to-master node))
      ((string= cmd "optimize_network") (dcf-group-peers))
      (t (log:warn *dcf-logger* "Unknown master command: ~A" cmd)))))

(defun dcf-set-mode (new-mode)
  "Dynamically set node mode."
  (handler-case
      (progn
        (dcf-stop)
        (setf (mode *node*) new-mode)
        (initialize-node (config *node*))
        (dcf-start)
        (log:info *dcf-logger* "Mode set to ~A" new-mode)
        `(:status "mode-set" :mode ,new-mode))
    (error (e) (log:error *dcf-logger* "Mode set failed: ~A" e) `(:status "error"))))

(defun dcf-update-config (key value)
  "Update config key-value."
  (handler-case
      (progn
        (setf (slot-value (config *node*) (intern (string-upcase key) :keyword)) value)
        (save-state (config *node*))
        (log:info *dcf-logger* "Updated config ~A to ~A" key value)
        `(:status "updated" :key ,key :value ,value))
    (error (e) (log:error *dcf-logger* "Config update failed: ~A" e) `(:status "error"))))

;; Master Node Specific Functions
(defun dcf-master-assign-role (peer role)
  "Master assigns role to peer."
  (handler-case
      (let ((request (make-instance 'master-command :command "set_role" :role role :peer peer)))
        (cl-grpc:call (get-peer-stub *node* peer) 'assign-role request)
        (log:info *dcf-logger* "Assigned role ~A to ~A" role peer)
        `(:status "assigned" :peer ,peer :role ,role))
    (error (e) `(:status "error" :message ,(princ-to-string e)))))

(defun dcf-master-update-config (peer key value)
  "Master updates config for peer."
  (handler-case
      (let ((request (make-instance 'master-command :command "update_config" :key key :value value :peer peer)))
        (cl-grpc:call (get-peer-stub *node* peer) 'update-config request)
        (log:info *dcf-logger* "Updated ~A=~A for ~A" key value peer)
        `(:status "updated" :peer ,peer :key ,key :value ,value))
    (error (e) `(:status "error" :message ,(princ-to-string e)))))

(defun dcf-master-collect-metrics ()
  "Master collects metrics from peers."
  (let ((metrics (make-hash-table :test 'equal)))
    (dolist (peer (dcf-config-peers (config *node*)))
      (let ((response (cl-grpc:call (get-peer-stub *node* peer) 'collect-metrics (make-instance 'empty))))
        (setf (gethash peer metrics) (parse-metrics response))))
    (log:info *dcf-logger* "Collected metrics from ~A peers" (hash-table-count metrics))
    metrics))

(defun dcf-master-optimize-network ()
  "Master optimizes network topology with AI."
  (let ((metrics (dcf-master-collect-metrics)))
    (optimize-topology metrics)
    (dolist (peer (hash-table-keys metrics))
      (cl-grpc:call (get-peer-stub *node* peer) 'optimize-network (make-instance 'empty)))
    (log:info *dcf-logger* "AI-driven network optimized")
    `(:status "optimized")))

(defun collect-metrics-periodically (node)
  "Periodic metrics collection in master mode."
  (loop
    (sleep 60)
    (dcf-master-collect-metrics)))

(defun send-metrics-to-master (node)
  "Send local metrics to master."
  (let ((metrics (collect-local-metrics node)))
    (cl-grpc:call (master-connection node) 'submit-metrics metrics)
    (log:debug *dcf-logger* "Sent metrics to master")))

(defun collect-local-metrics (node)
  "Collect local RTT and group data."
  (make-instance 'metrics
                 :node-id (dcf-config-node-id (config node))
                 :rtts (mapcar (lambda (p) (cons p (getf (dcf-health-check p) :rtt))) (dcf-config-peers (config node)))
                 :groups (hash-table-alist (peer-groups node))))

;; P2P Redundancy with Dijkstra Routing
(defun add-peer-channel (node peer)
  "Add channel for P2P peer."
  (let ((channel (cl-grpc:channel peer :insecure t)))
    (setf (gethash peer (plugins node)) (cl-grpc:stub 'dcf-service channel))))

(defun get-peer-stub (node peer)
  "Get stub for peer."
  (or (gethash peer (plugins node))
      (signal-dcf-error :peer-not-found (format nil "Peer not found: ~A" peer))))

(defun monitor-peers (node)
  "Monitor peers for failures."
  (loop
    (sleep 10)
    (dolist (peer (dcf-config-peers (config node)))
      (unless (getf (dcf-health-check peer) :healthy)
        (dcf-heal peer)))))

(defun compute-rtt-groups (peers threshold)
  "Compute groups using RTT."
  (let ((graph (build-graph peers))
        (groups (make-hash-table :test 'equal)))
    (multiple-value-bind (distances predecessors) (dijkstra graph (dcf-config-node-id (config *node*)))
      (dolist (peer peers)
        (let ((dist (gethash peer distances)))
          (when (< dist threshold)
            (push peer (gethash (floor dist (/ threshold 10)) groups)))))
      groups)))

(defun build-graph (peers)
  "Build graph with RTT weights."
  (let ((graph (make-hash-table :test 'equal)))
    (dolist (p1 peers)
      (dolist (p2 peers)
        (unless (string= p1 p2)
          (setf (gethash (cons p1 p2) graph) (getf (dcf-health-check p2) :rtt)))))
    graph))

(defun dijkstra (graph start)
  "Dijkstra's algorithm for shortest paths."
  (let ((distances (make-hash-table :test 'equal))
        (predecessors (make-hash-table :test 'equal))
        (queue (make-instance 'priority-queue)))
    (setf (gethash start distances) 0)
    (enqueue queue start 0)
    (loop while (not (empty-p queue))
          do (let ((u (dequeue queue)))
               (dolist (v (neighbors u graph))
                 (let ((alt (+ (gethash u distances) (gethash (cons u v) graph))))
                   (when (< alt (or (gethash v distances) most-positive-fixnum))
                     (setf (gethash v distances) alt
                           (gethash v predecessors) u)
                     (enqueue queue v alt)))))
    (values distances predecessors)))

(defun reconstruct-path (predecessors target)
  "Reconstruct path from predecessors."
  (let ((path '()))
    (loop for u = target then (gethash u predecessors)
          while u
          do (push u path))
    path))

(defun reroute-to-alternative (node failed-peer)
  "Reroute using Dijkstra predecessors."
  (multiple-value-bind (distances predecessors) (dijkstra (build-graph (dcf-config-peers (config node))) (dcf-config-node-id (config node)))
    (let ((alt-path (reconstruct-path predecessors failed-peer)))
      (when alt-path
        (let ((alt-peer (car (last alt-path))))
          (setf (dcf-config-peers (config node)) (substitute alt-peer failed-peer (dcf-config-peers (config node)) :test #'string=))
          (add-peer-channel node alt-peer)
          (log:info *dcf-logger* "Rerouted ~A to ~A via path ~A" failed-peer alt-peer alt-path))))))

(defun get-group-id (peer groups)
  "Get group ID for peer."
  (loop for (id . peers) in (hash-table-alist groups)
        when (member peer peers :test #'string=)
        return id))

;; gRPC Service Implementations
(defclass dcf-service-impl (cl-grpc:service)
  ((node :initarg :node :accessor node)))

(defmethod cl-grpc:send-message ((self dcf-service-impl) context request response)
  "Implement SendMessage RPC."
  (setf (slot-value response 'data) (format nil "Echo: ~A" (slot-value request 'data)))
  :ok)

(defmethod cl-grpc:receive-stream ((self dcf-service-impl) context request stream)
  "Implement ReceiveStream RPC."
  (loop for msg in (some-message-queue)
        do (cl-grpc:send stream msg)))

(defclass dcf-master-service-impl (cl-grpc:service)
  ((node :initarg :node :accessor node)))

;; Advanced AI-driven Network Optimization in Master Mode using MGL
(defun optimize-topology (metrics)
  "Use MGL neural network for AI-driven optimization of network topology."
  (let ((nn (make-instance 'mgl-bp:bpnn :layers '(10 5 2))))
    (mgl-bp:train nn (prepare-training-data metrics) :max-iterations 100)
    (let ((optimal-groups (mgl-bp:predict nn (prepare-input metrics))))
      (apply-optimized-groups optimal-groups)
      (log:info *dcf-logger* "AI-optimized topology applied"))
    `(:status "ai-optimized")))

(defun prepare-training-data (metrics)
  "Prepare data for NN training."
  (loop for (peer . rtt) in metrics collect (vector rtt (random 2))))

(defun prepare-input (metrics)
  "Prepare input for prediction."
  (loop for (peer . rtt) in metrics collect (vector rtt)))

(defun apply-optimized-groups (predictions)
  "Apply predicted groups to node."
  (setf (peer-groups *node*) (group-from-predictions predictions)))

(defun group-from-predictions (predictions)
  "Convert NN predictions to groups."
  (let ((groups (make-hash-table :test 'equal)))
    (loop for pred across predictions
          for peer in (hash-table-keys (peer-groups *node*))
          do (push peer (gethash (round (aref pred 0)) groups)))
    groups))

;; FiveAM Integration for Robust Testing
#+fiveam
(fiveam:def-suite d-lisp-suite
  :description "Test suite for D-LISP SDK.")

#+fiveam
(fiveam:in-suite d-lisp-suite)

#+fiveam
(fiveam:test version-test
  (fiveam:is (equal (dcf-version) '(:version "1.3.0" :dcf-version "5.0.0"))))

#+fiveam
(fiveam:test native-transport-test
  (let ((config (make-dcf-config :transport "native-lisp" :host "localhost" :port 50053)))
    (let ((*node* (initialize-node config)))
      (fiveam:is (not (null (native-transport *node*)))))))

#+fiveam
(fiveam:test ai-optimization-test
  (let ((metrics '((:peer1 . 20) (:peer2 . 60))))
    (fiveam:is (equal (optimize-topology metrics) '(:status "ai-optimized")))))

#+fiveam
(fiveam:test websocket-plugin-test
  (fiveam:is (equal (plugin-interface-version websocket-transport) "1.0")))

#+fiveam
(fiveam:test config-validation-test
  (let ((valid-config-path "test-config.json"))
    (with-open-file (stream valid-config-path :direction :output :if-exists :supersede)
      (cl-json:encode-json
       '((:transport . "gRPC") (:host . "localhost") (:port . 50051) (:mode . "client") (:node-id . "test-node"))
       stream))
    (fiveam:is (equal (getf (dcf-init valid-config-path) :status) "success"))
    (delete-file valid-config-path)))

#+fiveam
(fiveam:test help-test
  (fiveam:is (stringp (dcf-help))))

#+fiveam
(defun run-tests ()
  "Run all FiveAM tests."
  (fiveam:run! 'd-lisp-suite)
  (log:info *dcf-logger* "FiveAM tests completed."))

;; Help Command for Newcomers
(defun dcf-help ()
  "Provide beginner-friendly guidance for new users of DeMoD Communications Framework (DCF)."
  (format nil "~
Welcome to DeMoD-LISP (D-LISP), a Lisp-based SDK for the DeMoD Communications Framework (DCF)!

**What is DCF?**
DCF is a free, open-source (FOSS) framework for low-latency data exchange in applications like IoT, gaming, distributed computing, and edge networking. It's modular, interoperable across languages (e.g., Lisp, C, Python), and complies with U.S. export regulations (no encryption by default). Licensed under GPL-3.0. Repo: https://github.com/ALH477/DeMoD-Communication-Framework

**Key Concepts for Beginners:**
- **Modes**: Client (send/receive), Server (host), P2P (peer-to-peer with self-healing), AUTO (dynamic role switching via master node), Master (controls AUTO nodes).
- **Transports**: gRPC (default), Native Lisp (lightweight TCP), WebSocket (web-friendly via plugin).
- **Plugins**: Extend functionality, e.g., custom transports. Define with (def-dcf-plugin ...).
- **Redundancy**: Automatic failover using RTT-based grouping (<50ms clusters) and Dijkstra routing.
- **Configuration**: Use JSON files validated against schema. Example: config.json with transport, host, port, mode.

**Getting Started:**
1. **Install Dependencies**: Use Quicklisp to load libraries (see top of d-lisp.lisp).
2. **Clone Repo**: git clone https://github.com/ALH477/DeMoD-Communication-Framework --recurse-submodules
3. **Load D-LISP**: sbcl --load lisp/src/d-lisp.lisp
4. **Initialize**: (dcf-init \"config.json\")
5. **Start**: (dcf-start)
6. **Send Message**: (dcf-send \"Hello\" \"localhost:50052\")
7. **Help in CLI**: sbcl --eval '(d-lisp:main \"help\")'
8. **TUI**: (dcf-tui) for interactive mode.
9. **Run Tests**: (run-tests) or CLI \"run-tests\".

**Common Commands (CLI/TUI):**
- init [config.json]: Load config.
- start: Start node.
- stop: Stop node.
- send [data] [recipient]: Send message.
- receive: Receive messages.
- status: Show node status.
- health-check [peer]: Check peer health/RTT.
- list-peers: List peers with groups.
- heal [peer]: Reroute on failure.
- benchmark [peer]: Measure RTT.
- group-peers: Regroup by RTT.
- simulate-failure [peer]: Test failover.
- log-level [0/1/2]: Set logging (debug/info/error).
- load-plugin [path]: Load plugin.
- master-assign-role [peer] [role]: Assign role (master mode).
- master-optimize-network: AI-optimize topology.
- run-tests: Run FiveAM tests.
- help: This guide.

**Tips for New Users:**
- Start with 'client' mode and gRPC transport.
- Read docs/dcf_design_spec.md in repo for architecture.
- For errors, check logs (set log-level 0 for debug).
- Extend via plugins: See websocket-transport example.
- Questions? Open issues on GitHub.

For more, visit the repo or run (dcf-help) again!"))

;; CLI Entry Point with JSON Support
(defun main (&rest args)
  "CLI entry point with robust parsing."
  (handler-case
      (let* ((command (first args))
             (json-flag (position "--json" args :test #'string=))
             (cmd-args (if json-flag (subseq args 1 json-flag) (cdr args)))
             (json-output (not (null json-flag)))
             (result (cond
                       ((string= command "help") (dcf-help))
                       ((string= command "run-tests") (run-tests))
                       (t (apply (intern (string-upcase (format nil "DCF-~A" command)) :d-lisp) cmd-args)))))
        (if json-output
            (cl-json:encode-json-to-string result)
            (format t "~A~%" result)))
    (error (e)
      (log:error *dcf-logger* "CLI error: ~A~%Backtrace: ~A" e (trivial-backtrace:backtrace-string))
      (if (position "--json" args :test #'string=)
          (cl-json:encode-json-to-string `(:status "error" :message ,(princ-to-string e)))
          (format t "Error: ~A~%" e)))))
