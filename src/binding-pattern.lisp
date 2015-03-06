

(in-package :cl-user)
(defpackage :binding-pattern
  (:export :<> :<>-pattern :make-<>-pattern))
(in-package :optima)

(defstruct (binding-pattern:<>-pattern
            (:include constructor-pattern)
            (:constructor binding-pattern:make-<>-pattern
                          (name arg value &aux (subpatterns (list name)))))
  name arg value)

(defmethod constructor-pattern-destructor-sharable-p
    ((x binding-pattern:<>-pattern) (y binding-pattern:<>-pattern))
  nil)

(defmethod constructor-pattern-make-destructor
    ((pattern binding-pattern:<>-pattern) matched)
  (with-slots (arg value) pattern
     (make-destructor
      :bindings `((,arg ,matched))
      :predicate-form t
      :accessor-forms `(,value))))

(defmethod unparse-pattern ((pattern binding-pattern:<>-pattern))
  (with-slots (name arg value) pattern
     `(binding-pattern:<> ,name ,arg ,value)))

(defmethod parse-constructor-pattern ((name (eql 'binding-pattern:<>)) &rest args)
  (match args
    ((list name (and arg (symbol)) value)
     (binding-pattern:make-<>-pattern (parse-pattern name) arg value))
    (otherwise
     (error "Bad binding pattern: ~S" (list* name args)))))

#+nil
(match 'x
  ((<> a 3)
   (print a)))
