(in-package :cl-user)
(defpackage blue-water-test-asd
  (:use :cl :asdf))
(in-package :blue-water-test-asd)

(defsystem blue-water-test
  :author "TatriX"
  :license ""
  :depends-on (:blue-water
               :prove)
  :components ((:module "t"
                :components
                ((:file "blue-water"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
