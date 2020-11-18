(declare (unit to-string)
         (uses callable token))

(import (chicken port)
        coops)


(define-method (lox->string (lox-object #t))
  (cond
    ((null? lox-object) "nil")
    ((eq? lox-object #t) "true")
    ((eq? lox-object #f) "false")
    (else (call-with-output-string (lambda (out) (write lox-object out))))))

(define-method (lox->string (lox-object <native-fn>))
  "<native-fn>")

(define-method (lox->string (lox-object <function>))
   (string-append "<fn "
                  (token-lexeme
                    (slot-value (slot-value lox-object 'declaration)
                                'name))
                  ">"))
