(declare (unit pretty-print)
         (uses ast token to-string))

(import (chicken string)
        coops)


(define (parenthesize name . exprs)
  (string-append
    "(" (string-intersperse (cons name (map pretty-print exprs))) ")"))

(define (parenthesize-slots expr name slot-names)
  (apply parenthesize
         name
         (map (lambda (slot) (slot-value expr slot)) slot-names)))

;;; Expressions
(define-method (pretty-print (expr <assignment>))
  (parenthesize "assign"
                (token-lexeme (slot-value expr 'name))
                (slot-value expr 'value)))

(define-method (pretty-print (expr <binary>))
  (parenthesize-slots expr
                      (token-lexeme (slot-value expr 'operator))
                      '(left right)))

(define-method (pretty-print (expr <call>))
  (apply parenthesize
         "call"
         (slot-value expr 'callee)
         (slot-value expr 'arguments)))

; (define-method (pretty-print (expr <get>)))

(define-method (pretty-print (expr <grouping>))
  (parenthesize "group" (slot-value expr 'expression)))

(define-method (pretty-print (expr <literal>))
  (lox->string (slot-value expr 'value)))

(define-method (pretty-print (expr <logical>))
  (parenthesize-slots expr
                      (token-lexeme (slot-value expr 'operator))
                      '(left right)))

; (define-method (pretty-print (expr <set>)))
; (define-method (pretty-print (expr <super>)))
; (define-method (pretty-print (expr <this>)))

(define-method (pretty-print (expr <unary>))
  (parenthesize (token-lexeme (slot-value expr 'operator))
                (slot-value expr 'right)))

(define-method (pretty-print (expr <variable>))
  (token-lexeme (slot-value expr 'name)))


;;; Statements
(define-method (pretty-print (expr <block>))
  (apply parenthesize "block" (slot-value expr 'statements)))

; (define-method (pretty-print (expr <class>)))

(define-method (pretty-print (expr <expr-stmt>))
  (pretty-print (slot-value expr 'expression)))

(define-method (pretty-print (expr <function>))
  (string-append "(function "
                 (token-lexeme (slot-value expr 'name))
                 " ("
                 (string-intersperse
                   (map token-lexeme (slot-value expr 'params)))
                 ") "
                 (pretty-print (slot-value expr 'body))
                 ")"))

(define-method (pretty-print (expr <if>))
  (let ((condition (slot-value expr 'condition))
        (then-branch (slot-value expr 'then-branch))
        (else-branch (slot-value expr 'else-branch)))
    (if else-branch
        (parenthesize "if" condition then-branch else-branch)
        (parenthesize "if" condition then-branch))))

(define-method (pretty-print (expr <print>))
  (parenthesize "print" (slot-value expr 'expression)))

(define-method (pretty-print (expr <return>))
  (parenthesize "return" (slot-value expr 'value)))

(define-method (pretty-print (expr <var-stmt>))
  (let ((name (token-lexeme (slot-value expr 'name)))
        (initializer (slot-value expr 'initializer)))
    (if initializer
        (parenthesize "var" name (pretty-print initializer))
        (parenthesize "var" name))))

(define-method (pretty-print (expr <while>))
  (parenthesize-slots expr "while" '(condition body)))


;;; Default
(define-method (pretty-print (expr #t))
  expr)
