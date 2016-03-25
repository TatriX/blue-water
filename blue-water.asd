(in-package :cl-user)
(defpackage blue-water-asd
  (:use :cl :asdf))
(in-package :blue-water-asd)

(defsystem blue-water
  :version "0.1"
  :author "TatriX"
  :license ""
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop
               :osicat
               :cl-date-time-parser
               :simple-date-time
               :clss
               :drakma

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :cl-markup
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "parser" :depends-on ("config" "db"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (load-op blue-water-test))))
