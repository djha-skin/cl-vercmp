(defsystem "cl-vercmp"
  :version "0.0.1"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "alexandria"
               "cl-ppcre"
               "cl-ppcre-unicode"
               "arrows"
               "uiop"
  )
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Version comparison functions written in Common Lisp"
  :in-order-to ((test-op (test-op "cl-vercmp/tests"))))

(defsystem "cl-vercmp/tests"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on ("cl-vercmp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-vercmp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
