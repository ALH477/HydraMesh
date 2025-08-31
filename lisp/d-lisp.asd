(asdf:defsystem #:d-lisp
  :description "Lisp SDK for DeMoD Communications Framework (DCF)"
  :author "ALH477"
  :license "GPL-3.0"
  :version "1.4.0"
  :depends-on (:cl-protobufs :cl-grpc :cffi :uuid :cl-json :jsonschema
               :cl-ppcre :cl-csv :usocket :bordeaux-threads :curses
               :log4cl :trivial-backtrace :cl-store :mgl :hunchensocket
               :fiveam :cl-dot)
  :components ((:file "src/d-lisp")
               (:file "tests/main" :depends-on ("src/d-lisp")))
  :in-order-to ((test-op (test-op :d-lisp/tests))))

(asdf:defsystem #:d-lisp/tests
  :depends-on (:d-lisp :fiveam)
  :components ((:file "tests/main"))
  :perform (test-op (o c) (symbol-call :d-lisp :run-tests)))
