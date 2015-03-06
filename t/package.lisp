#|
  This file is a part of type-r project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :type-r.test
  (:use :cl
        :type-r
        :fiveam
        :optima :alexandria :iterate)
  (:shadow :fail))
(in-package :type-r.test)



(def-suite :type-r)
(in-suite :type-r)

;; run test with (run! test-name) 
;;   test as you like ...

(test t1
  
  (match '(string 50)
    ((string-type size) (is (= 50 size)))) ; --> 50

  (match '(string)
    ((string-type size) (is (eq '* size)))) ; --> '*

  (match 'string
    ((string-type size) (is (eq '* size)))) ; --> '*
)
(test fn
  (is (eq 50 (string-type-size '(string 50)))) ; --> 50
  (is (eq '* (string-type-size '(string))))    ; --> '*
  (is (eq '* (string-type-size 'string)))      ; --> '*
)

(test subtype  
  ;; ematch throws an error when no clause matches
  (signals error 
    (ematch '(simple-array * 3)
      ((array-type _ rank) rank))) ; --> error!

  (ematch '(simple-array * 3)
    ((general-array-type _ rank) (is (= 3 rank)))) ; --> 3

  (ematch '(simple-array * (3 2))
    ((general-array-type _ (list _ column)) (is (= 2 column)))) ; --> 2

)

(test optional
  (ematch '(simple-string 5)
    ((simple-string-type size) (is (= 5 size)))) ; --> 5

  (ematch '(simple-string 5)
    ((simple-string-type _ type) (is (eq 'character type)))) ; --> 'character

  (ematch '(base-string 5)
    ((base-string-type _ type) (is (eq 'base-char type)))) ; --> 'base-char
)

(test number

  (match '(float -0.08 1.7)
    ((float-type low _)
     (is (= -0.08 low))))

  (match '(float -0.08 1.7)
    ((general-float-type low _)
     (is (= -0.08 low))))

  (match 'fixnum
    ((general-integer-type low _)
     (is (= MOST-NEGATIVE-FIXNUM low))))
)



(eval-when (:load-toplevel :execute)
  (run! :type-r))



