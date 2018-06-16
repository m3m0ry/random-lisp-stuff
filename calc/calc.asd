(defsystem "calc"
  :depends-on ("esrap")
  :components ((:file "calc"))
  :in-order-to ((test-op (test-op "calc/tests"))))

(defsystem "calc/tests"
  :depends-on ("calc" "fiveam")
  :components ((:file "calc-tests"))
  :perform (test-op (o c) (uiop:symbol-call 'calc-tests 'run-all-tests)))
