(defsystem "run"
  :version "0.1.0"
  :author "fgatherlet"
  :license "MIT"
  :depends-on ("series")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "run external program easily."
  :in-order-to ((test-op (test-op "run/tests"))))

(defsystem "run/tests"
  :author "fgatherlet"
  :license "MIT"
  :depends-on ("run"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for run"
  :perform (test-op (op c) (symbol-call :rove :run c)))
