(declare (unit to-string)
         (uses callable token instance))

(import (chicken port)
        coops
        coops-primitive-objects)


(define-generic (lox->string lox-object))

(define-method (lox->string (lox-object #t))
  (call-with-output-string (lambda (out) (write lox-object out))))

(define-method (lox->string (lox-object <null>))
  "nil")

(define-method (lox->string (lox-object <boolean>))
  (if lox-object "true" "false"))

(define-method (lox->string (lox-object <native-fn>))
  "<native fn>")

(define-method (lox->string (lox-object <lox-fn>))
   (string-append "<fn "
                  (token-lexeme
                    (slot-value (slot-value lox-object 'declaration)
                                'name))
                  ">"))

(define-method (lox->string (lox-object <lox-class>))
  (slot-value lox-object 'name))

(define-method (lox->string (lox-object <instance>))
  (string-append (slot-value (slot-value lox-object 'class) 'name)
                 " instance"))
