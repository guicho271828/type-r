#|
  This file is a part of type-r project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage type-r-asd
  (:use :cl :asdf))
(in-package :type-r-asd)


(defsystem type-r
  :version "0.1"
  :author "Masataro Asai"
  :description "Collections of accessor functions and patterns to access the elements in compound type specifier, e.g. `dimensions' in `(array element-type dimensions)'"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia :alexandria)
  :pathname "src/"
  :components ((:file "package")
               (:file "patterns"))
  :serial t
  :in-order-to ((test-op (load-op type-r.test))))
