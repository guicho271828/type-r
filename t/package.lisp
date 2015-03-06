#|
  This file is a part of type-r project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :type-r.test
  (:use :cl
        :type-r
        :fiveam
        :optima :alexandria :iterate))
(in-package :type-r.test)



(def-suite :type-r)
(in-suite :type-r)

;; run test with (run! test-name) 
;;   test as you like ...

(test type-r

  )


