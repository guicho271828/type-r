
(in-package :type-r)

(defpattern qor (first &rest rest)
  "Quoting OR pattern"
  (if rest
      `(or (quote ,first)
           (qor ,@rest))
      `(quote ,first)))

;;;; macros: defpattern-with-accessors
;;;; defpattern-with-accessor macro provides:
;;;;   1. it is a simple wrapper around optima:defpattern.
;;;;   2. it defines accessors like function-type-return-type automatically,
;;;;      based on the names of the arguments.
;;;;   3. it defines predicates like function-type-p automatically.
;;;;   4. using these predicates, implementation of form-typep can be simplified.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defpattern-with-accessors (name args &body body)
    "
 defpattern-with-accessor macro provides:
   1. it is a simple wrapper around optima:defpattern.
   2. it defines accessors like function-type-return-type automatically,
      based on the names of the arguments.
   3. it defines predicates like function-type-p automatically.
   4. using these predicates, implementation of form-typep can be simplified.

several assumptions: only &rest keywords can be recognized.

"
    (let ((violated (intersection args lambda-list-keywords)))
      (assert (not violated) nil "~a found in ~a" violated args))
    (let* ((len (length args))
           (obj (gensym "OBJ"))
           (pred-name (symbolicate name '-p)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',name)
         (defpattern ,name (&optional ,@(wrap-wildcards args))
           ,@body)
         ;; define field accessors
         ,@(mapcar (lambda (field-name i)
                     (let ((func-name (symbolicate name '- field-name)))
                       `(progn
                          (export ',func-name)
                          (defun ,func-name (,obj)
                          (match ,obj
                            ((,name ,@(wildcards-but-nth len i field-name))
                             ,field-name)
                            (_ (error 'type-error
                                      :expected-type '(,name ,@args)
                                      :datum ,obj)))))))
                   args (iota len))
         ;; define predicates
         (export ',pred-name)
         (defun ,pred-name (,obj)
           (match ,obj
             ((,name) t) (_ nil))))))

  ;; (wildcards-but-nth 5 2 'X) == '(_ _ X _ _)
  (defun wildcards-but-nth (length n sym)
    (let ((list (make-list length :initial-element '_)))
      (setf (elt list n) sym)
      list))

  (defun wrap-wildcards (args)
    (loop with acc = nil
          for arg in args
          do
       (match arg
         ((type list) (push arg acc))
         ((symbol) (push (list arg ''_) acc)))
       finally (return (nreverse acc)))))

;;;; function: make-type-matcher : handle abbreviations of types,
;;;; like vector, (vector), (vector 'fixnum), (vector 'fixnum 2).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-types-matcher (name arglist &optional fixed)
    "
Generates a matcher for variations of compound types, e.g.
  vector, (vector), (vector 'fixnum), (vector 'fixnum 2).

name    : the symbol denoting the atomic type specifier e.g. vector
arglist : (variable default)* --- follows the syntax of &optional arguments of types.
fixed   : (variable default)* --- specifies the types that can be inferred from the array type.
"
    `(or ,(make-atomic-type-matcher name (append arglist fixed)) ; atomic type specifier
         ,(make-compound-type-matcher name arglist fixed))) ; compound type specifier

  (defun make-atomic-type-matcher (name arglist)
    `(and ',name ,@(mapcar #'make-binder arglist)))

  (defun make-binder (pair)
    (destructuring-bind (arg default) pair
      `(<> ,arg _ ',default)))

  #+nil
  (print (make-atomic-array-type 'string '((element-type character)
                                           (size *))))

  (defun make-compound-type-matcher (name specified unspecified)
    (labels ((list-specified (specified)
               `(list ',name ,@(mapcar #'car specified)))
             (render (specified unspecified)
               `(and ,(list-specified (reverse specified))
                     ,@(mapcar #'make-binder unspecified)))
             (consume (specified unspecified)
               (match specified
                 (nil (list (render specified unspecified)))
                 ((cons first rest)
                  (cons (render specified unspecified)
                        (consume rest (cons first unspecified)))))))
      `(or ,@(consume (reverse specified) unspecified))))

  #+nil
  (print (make-compound-array-types-matcher
          'string '((size *)) '((element-type character))))

  #+nil
  (print (make-types-matcher
          'string '((size *)) '((element-type character)))))


;;;; function type, values type

(defpattern-with-accessors function-type (args-types return-type)
  (make-types-matcher 'function `((,args-types (&rest *)) (,return-type *))))

(defpattern-with-accessors values-type (primary)
  "this is tricky, since values type also takes &optional &rest etc."
  `(or (list* 'values
              (<> ,primary rest
                  (or (find-if (lambda (x) (not (member x lambda-list-keywords)))
                               rest)
                      'null ;; take care of (values)
                      )))
       (and '* (<> ,primary _ t))
       (and ,primary)))

;;;; arrays

(defpattern-with-accessors string-type (dimensions element-type)
  (make-types-matcher 'string `((,dimensions *)) `((,element-type character))))

(defpattern-with-accessors simple-string-type (dimensions element-type)
  (make-types-matcher 'simple-string `((,dimensions *)) `((,element-type character))))

(defpattern-with-accessors simple-vector-type (dimensions element-type)
  (make-types-matcher 'simple-vector `((,dimensions *)) `((,element-type *))))

(defpattern-with-accessors vector-type (dimensions element-type)
  (make-types-matcher 'vector `((,element-type *) (,dimensions *))))

(defpattern-with-accessors raw-array-type (dimensions element-type)
  (make-types-matcher 'array `((,element-type *) (,dimensions *))))

(defpattern-with-accessors simple-array-type (dimensions element-type)
  (make-types-matcher 'simple-array `((,element-type *) (,dimensions *))))

(defpattern-with-accessors bit-vector-type (dimensions element-type)
  (make-types-matcher 'bit-vector `((,dimensions *)) `((,element-type bit))))

(defpattern-with-accessors simple-bit-vector-type (dimensions element-type)
  (make-types-matcher 'simple-bit-vector `((,dimensions *)) `((,element-type bit))))

;; now try this out!
#+nil
(mapcar (lambda (type)
          (print (optima.core:pattern-expand-1 `(,type DIMENSIONSX ELEMY))))
        '(string-type
          simple-string-type
          simple-vector-type
          vector-type
          array-type
          simple-array-type
          bit-vector-type
          simple-bit-vector-type))

(defpattern-with-accessors general-string-type (dimensions element-type)
  `(or (string-type            ,dimensions ,element-type)
       (simple-string-type     ,dimensions ,element-type)))

(defpattern-with-accessors general-vector-type (dimensions element-type)
  `(or (vector-type            ,dimensions ,element-type)
       (simple-vector-type     ,dimensions ,element-type)))

(defpattern-with-accessors general-bitvector-type (dimensions element-type)
  `(or (bit-vector-type        ,dimensions ,element-type)
       (simple-bit-vector-type ,dimensions ,element-type)))

(defpattern-with-accessors general-simple-array-type (dimensions element-type)
  `(or (simple-string-type     ,dimensions ,element-type)
       (simple-vector-type     ,dimensions ,element-type)
       (simple-array-type      ,dimensions ,element-type)
       (simple-bit-vector-type ,dimensions ,element-type)))

(defpattern-with-accessors general-array-type (dimensions element-type) 
  `(or (string-type            ,dimensions ,element-type)
       (simple-string-type     ,dimensions ,element-type)
       (simple-vector-type     ,dimensions ,element-type)
       (vector-type            ,dimensions ,element-type)
       (raw-array-type         ,dimensions ,element-type)
       (simple-array-type      ,dimensions ,element-type)
       (bit-vector-type        ,dimensions ,element-type)
       (simple-bit-vector-type ,dimensions ,element-type)))

(defpattern-with-accessors array-type (dimensions element-type)
  `(general-array-type ,dimensions ,element-type))


;;;; union. intersection, etc.

(defpattern-with-accessors union-type (types)
  `(list* 'or ,types))
(defpattern-with-accessors intersection-type (types)
  `(list* 'and ,types))
(defpattern-with-accessors negation-type (type)
  `(list 'not ,type))

;;;; numeric types

;;;;; integer types

(defpattern-with-accessors mod-type (low high)
  (with-gensyms (n)
    `(and (list 'mod (<> ,high ,n (1- ,n)))
          (<> ,low _ 0))))

(defun nb (n)
  (1- (expt 2 n)))

(defpattern-with-accessors unsigned-byte-type (low high)
  (with-gensyms (n)
    `(and (or (list 'unsigned-byte
                    (and (type fixnum)
                         (<> ,high ,n (1- (expt 2 ,n)))))
              (list 'unsigned-byte
                    (and '* ,high))
              (and (list 'unsigned-byte) (<> ,high _ '*)))
          (<> ,low _ 0))))

(defpattern-with-accessors signed-byte-type (low high)
  (with-gensyms (n)
    `(or (list 'signed-byte
               (and (type fixnum)
                    (<> ,high ,n (1- (expt 2 (1- ,n))))
                    (<> ,low  ,n (-  (expt 2 (1- ,n))))))
         (list 'signed-byte
               (and '* ,high ,low))
         (and (or 'signed-byte (list 'signed-byte))
              (<> ,high _ '*)
              (<> ,low  _ '*)))))

(defpattern-with-accessors fixnum-type (low high)
  (make-types-matcher 'fixnum nil `((,low ,MOST-NEGATIVE-FIXNUM)
                                    (,high ,MOST-POSITIVE-FIXNUM))))
(defpattern-with-accessors integer-type (low high)
  (make-types-matcher 'integer `((,low *) (,high *))))

(defpattern-with-accessors general-integer-type (low high)
  `(or (mod-type ,low ,high)
       (unsigned-byte-type ,low ,high)
       (signed-byte-type ,low ,high)
       (fixnum-type ,low ,high)
       (integer-type ,low ,high)))

;;;;; float types

(defpattern-with-accessors float-type (low high)
  (make-types-matcher 'float `((,low *) (,high *))))
(defpattern-with-accessors single-float-type (low high)
  (make-types-matcher 'single-float `((,low *) (,high *))))
(defpattern-with-accessors double-float-type (low high)
  (make-types-matcher 'double-float `((,low *) (,high *))))
(defpattern-with-accessors long-float-type (low high)
  (make-types-matcher 'long-float `((,low *) (,high *))))
(defpattern-with-accessors short-float-type (low high)
  (make-types-matcher 'short-float `((,low *) (,high *))))
(defpattern-with-accessors general-float-type (low high)
  `(or (float-type ,low ,high)
       (short-float-type ,low ,high)
       (single-float-type ,low ,high)
       (double-float-type ,low ,high)
       (long-float-type ,low ,high)))

;;;;; misc real types

(defpattern-with-accessors real-type (low high)
  (make-types-matcher 'real `((,low *) (,high *))))

(defpattern-with-accessors rational-type (low high)
  (make-types-matcher 'rational `((,low *) (,high *))))

(defpattern-with-accessors ratio-type (low high)
  (make-types-matcher 'ratio nil `((,low *) (,high *))))
  
(defpattern-with-accessors numeric-type (low high)
  `(or (general-integer-type ,low ,high)
       (general-float-type ,low ,high)
       (real-type ,low ,high)
       (rational-type ,low ,high)
       (ratio-type ,low ,high)))

;;;;; complex number type
(defpattern-with-accessors complex-type (element-type)
  (make-types-matcher 'complex `((,element-type *))))

;;;; combining type specifiers

(defpattern-with-accessors eql-type (object)
  `(list 'eql ,object))

(defpattern-with-accessors member-type (members)
  `(list* 'member ,members))

(defpattern-with-accessors satisfies-type (function)
  `(list 'satisfies ,function))

(defpattern-with-accessors cons-type (car-type cdr-type)
  (make-types-matcher 'cons `((,car-type *) (,cdr-type *))))


(defun function-type-primary-value (function-type)
  "Given a function type, return the type of the function's primary return value."
  (values-type-primary (function-type-return-type function-type)))
