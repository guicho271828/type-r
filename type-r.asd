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
  :description "Collections of Accessor Functions and Patterns to Handle Compound Types"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:optima :alexandria :introspect-environment)
  :pathname "src/"
  :components ((:file "binding-pattern")
               (:file "package")
               (:file "patterns"))
  :serial t
  :description ""
  :in-order-to ((test-op (load-op type-r.test))))
