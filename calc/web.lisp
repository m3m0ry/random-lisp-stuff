(defpackage :web
  (:use :cl :hunchentoot :cl-who :cl-slug :calc)
  (:export #:start-server #:stop-server #:restart-server))

; cl-slug Use slugify or asciify

(in-package :web)

(setf *print-case* :downcase)
(setf (html-mode) :html5)

(defvar *acceptor* nil)

(defun start-server ()
  (stop-server)
  (start (setf *acceptor*
               (make-instance 'easy-acceptor
                              :port 4242))))

(defun stop-server ()
  (when *acceptor*
    (stop *acceptor*)
    (setf *acceptor* nil)))

(defun restart-server ()
  (stop-server)
  (start-server))


(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
            (:body
             ,@body)))))

(define-easy-handler (main :uri "/") ()
  (standard-page (:title "hr0m's wEbSiTe")
    (:a :href "/calc" "Calculator!")))

(define-easy-handler (calc :uri "/calc") (query)
  (standard-page (:title "hr0m's wEbSiTe")
    (:h1 "Enter an algebraic expression")
    (:form :action "/calc"
           (:input :type "text" :name "query" :autofocus t :size 40 :value query)
           (:input :type "submit" :value "Go!"))
    (handler-case
        (let ((answer (calc:epel query)))
          (htm :p "Out: " (str answer)))
      (error (condition)
        (htm :p (esc (write-to-string condition)))))))
