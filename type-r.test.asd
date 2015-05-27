#|
  This file is a part of type-r project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage type-r.test-asd
  (:use :cl :asdf))
(in-package :type-r.test-asd)


(defsystem type-r.test
  :author "Masataro Asai"
  :description "Test system for type-r"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:type-r
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) ))
