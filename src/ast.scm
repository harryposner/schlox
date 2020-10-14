(declare (unit ast))

(import coops)

;;; Expressions
(define-class <assignment> () (name value))
(define-class <binary> () (left operator right))
(define-class <call> () (callee paren arguments))
(define-class <get> () (object name))
(define-class <grouping> () (expression))
(define-class <literal> () (value))
(define-class <logical> () (left operator right))
(define-class <set> () (object name value))
(define-class <super> () (keyword method))
(define-class <this> () (keyword))
(define-class <unary> () (operator right))
(define-class <variable> () (name))

;;; Statements
(define-class <block> () (statements))
(define-class <class> () (name superclass methods))
(define-class <expr-stmt> () (expression))
(define-class <function> () (name params body))
(define-class <if> () (condition then-branch else-branch))
(define-class <print> () (expression))
(define-class <return> () (keyword value))
(define-class <var-stmt> () (name initializer))
(define-class <while> () (condition body))

;;; Used in REPL (chapter 8, challenge #1)
; (define (expression? ast-node)
;   (memq (class-of ast-node)
;         (list <assignment> <binary> <call> <get> <grouping> <literal> <logical>
;               <set> <super> <this> <unary> <variable>)))
