(declare (unit callable))

(import coops)

(define-class <callable> ())

(define-class <function> (<callable>) (declaration closure is-initializer))
(define-class <class> (<callable>) (name superclass methods))


(define-generic (arity callable))


(define-method arity (callable <native>)
               )

(define-method (arity (callable <function>))
  (length (slot-value callable 'arguments)))

(define-method (arity (callable <class>))
  (let ((initializer (find-method callable "init")))
    (if initializer
        (arity initializer)
        0)))


(define-generic (lox-apply callable))

(define-method (lox-apply (callable <function>) )
  ())


(define (find-method class name)
  (hash-table-ref/default (slot-value class 'methods) name #f))
