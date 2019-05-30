
(asdf:defsystem #:neuwerk
  :description "Neuronal network project with petalisp"
  :author "Jan Hoenig <jan.hoenig@fau.de>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:petalisp)
  :serial t
  :components ((:file "package")
               (:file "base")))
