(defsystem "nlp"
  :depends-on ("str")
  :components ((:file "nlp"))
  :in-order-to ((test-op (test-op "nlp/tests"))))

(defsystem "nlp/tests"
  :depends-on ("nlp" "fiveam")
  :components ((:file "nlp-tests"))
  :perform (test-op (o c) (uiop:symbol-call 'nlp-tests 'run-all-tests)))
