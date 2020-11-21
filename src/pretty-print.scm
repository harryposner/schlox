(declare (unit pretty-print)
         (uses ast token to-string))

(include "src/utils.scm")

(import (chicken string)
        coops
        coops-primitive-objects)


(define (parenthesize . exprs)
  (string-append
    "(" (string-intersperse (map pretty-print exprs)) ")"))

(define (parenthesize-slots expr name slot-names)
  (apply parenthesize
         name
         (map (lambda (slot) (slot-value expr slot)) slot-names)))


(define-generic (pretty-print ast-node))

;;; Expressions
(define-method (pretty-print (expr <assignment>))
  (parenthesize-slots expr "assign" '(name value)))

(define-method (pretty-print (expr <binary>))
  (parenthesize-slots expr
                      (slot-value expr 'operator)
                      '(left right)))

(define-method (pretty-print (expr <call>))
  (apply parenthesize
         "call"
         (slot-value expr 'callee)
         (slot-value expr 'arguments)))

(define-method (pretty-print (expr <get>))
  (parenthesize-slots expr "get" '(object name)))

(define-method (pretty-print (expr <grouping>))
  (parenthesize "group" (slot-value expr 'expression)))

(define-method (pretty-print (expr <literal>))
  (lox->string (slot-value expr 'value)))

(define-method (pretty-print (expr <logical>))
  (parenthesize-slots expr
                      (slot-value expr 'operator)
                      '(left right)))

(define-method (pretty-print (expr <set>))
  (parenthesize-slots expr "set" '(object name value)))

; (define-method (pretty-print (expr <super>)))

(define-method (pretty-print (expr <this>))
  "this")

(define-method (pretty-print (expr <unary>))
  (parenthesize (slot-value expr 'operator)
                (slot-value expr 'right)))

(define-method (pretty-print (expr <variable>))
  (token-lexeme (slot-value expr 'name)))


;;; Statements
(define-method (pretty-print (stmt <block>))
  (apply parenthesize "block" (slot-value stmt 'statements)))

(define-method (pretty-print (stmt <class>))
  (apply parenthesize
         "class"
         (slot-value stmt 'name)
         (if-let ((superclass (slot-value stmt 'superclass)))
                 superclass
                 "")
         (slot-value stmt 'methods)))

(define-method (pretty-print (stmt <expr-stmt>))
  (pretty-print (slot-value stmt 'expression)))

(define-method (pretty-print (stmt <function>))
  (parenthesize "fun"
                (slot-value stmt 'name)
                (apply parenthesize (slot-value stmt 'params))
                (slot-value stmt 'body)))

(define-method (pretty-print (stmt <if>))
  (let ((condition (slot-value stmt 'condition))
        (then-branch (slot-value stmt 'then-branch))
        (else-branch (slot-value stmt 'else-branch)))
    (if else-branch
        (parenthesize "if" condition then-branch else-branch)
        (parenthesize "if" condition then-branch))))

(define-method (pretty-print (stmt <print>))
  (parenthesize "print" (slot-value stmt 'expression)))

(define-method (pretty-print (stmt <return>))
  (parenthesize "return" (slot-value stmt 'value)))

(define-method (pretty-print (stmt <var-stmt>))
  (let ((name (slot-value stmt 'name))
        (initializer (slot-value stmt 'initializer)))
    (if initializer
        (parenthesize "var" name (pretty-print initializer))
        (parenthesize "var" name))))

(define-method (pretty-print (stmt <while>))
  (parenthesize-slots stmt "while" '(condition body)))


;;; Default
(define-primitive-class <token> (<record>) token?)
(define-method (pretty-print (token <token>))
  (token-lexeme token))

(define-method (pretty-print (node #t))
  node)
