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


;; copied from Bike/compiler-macro. Its license is WTFPL, right?
(test types
  ;; ⇓these are not me saying :)
  ;; fuck, man.
  ;; a lot of this should be rexamined - using type equality or set equality predicates, that sort of thing.
  (is (equal '(values integer &optional)
	     (function-type-return-type '(function nil (values integer &optional)))))
  (is (eql 'integer (values-type-primary 'integer)))
  (is (eql 'null (values-type-primary '(values))))
  (is (eql 'integer (values-type-primary '(values integer))))
  (is (eql 'integer (values-type-primary '(values &optional integer))))
  ;; this is modified because I no longer support this function.
  ;; use (values-type-primary (function-type-return-type type)) instead
  ;; (is (eql 'integer (function-type-primary-value '(function nil (values integer &optional)))))
  (is (eql 'integer (values-type-primary (function-type-return-type '(function nil (values integer &optional))))))
  (is (eql 'integer (array-type-element-type '(array integer))))

  ;; different from compiler-macro. similar incompatibility is here, I stop describing it any more.
  ;; (is-true (subtypep (array-type-element-type 'string) 'character))
  (is-true (subtypep (general-array-type-element-type 'string) 'character))

  (is (eql '* (general-array-type-dimensions '(simple-array * *))))
  (is (eql '* (general-array-type-dimensions 'vector)))
  (is (equal '(4 *) (general-array-type-dimensions '(simple-array nil (4 *)))))
  ;; intersection, union -> and,or
  (is (equal '(integer) (or-type-types '(or integer))))
  (is (equal '(integer) (and-type-types '(and integer))))
  (is (eql 'integer (not-type-type '(not integer))))
  (is (eql '* (general-real-type-low 'integer)))
  (signals error (general-real-type-low '(complex integer)))
  (is (= most-negative-fixnum (general-real-type-low 'fixnum)))
  (is (= (- (ash 1 (1- 7))) (general-real-type-low '(signed-byte 7))))
  (is (eql '* (general-real-type-low '(signed-byte))))
  (is (= 7 (general-real-type-low '(integer 7))))
  (is (zerop (general-real-type-low '(mod 12))))
  (is (equal '(0.7) (general-real-type-low '(short-float (0.7) 4.7))))
  (is (= most-positive-fixnum (general-real-type-high 'fixnum)))
  (is (= (1- (ash 1 12)) (general-real-type-high '(unsigned-byte 12))))
  (is (equal '(long-float (6.6)) (complex-type-element-type '(complex (long-float (6.6))))))
  (is (eql #\a (eql-type-object '(eql #\a))))
  (is (equal '(#\b) (member-type-members '(member #\b))))
  (is (eql 'plusp (satisfies-type-function '(satisfies plusp))))
  (is (equal '(cons integer) (cons-type-car-type '(cons (cons integer) float))))
  (is (eql 'float (cons-type-cdr-type '(cons (cons integer) float)))))



(eval-when (:load-toplevel :execute)
  (run! :type-r))



