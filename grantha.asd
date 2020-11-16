(defsystem "grantha"
  :version "0.1.0"
  :author "Edgard Bikelis"
  :license "GPLv3"
  ;; :depends-on '("")
  :components ((:file "grantha"))
  :description "Personal library of recurrent sundries."
  :in-order-to ((test-op (test-op "grantha/tests"))))

(defsystem "grantha/tests"
  :author "Edgard Bikelis"
  :license "GPLv3"
  :depends-on ("grantha"
               "rove")
  :components ((:file "grantha-tests"))
  :description "Test system for grantha"
  :perform (test-op (op c) (symbol-call :rove :run c)))
