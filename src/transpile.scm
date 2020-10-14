(declare (uses ast token)
         (unit transpile))

(include "src/utils.scm")

(import coops)

(define binary-operators
  '((#:BANG-EQUAL . not-equal?)
    (#:EQUAL-EQUAL . equal?)
    (#:GREATER . >)
    (#:GREATER-EQUAL >=)
    (#:LESS . <)
    (#:LESS-EQUAL . <=)
    (#:MINUS . -)
    (#:PLUS . lox-plus)
    (#:SLASH . /)
    (#:STAR . *)))

(define unary-operators
  '((#:BANG . lox-not)
    (#:MINUS . -)))

(define preamble
  ;;; Definitions for operators not in the Scheme standard
  '(begin
     (define (not-equal? x y) (not (equal? x y)))
     (define (lox-plus x y)
       (cond
         ((and (number? x) (number? y)) (+ x y))
         ((and (string? x) (string? y)) (string-append x y))
         (else (error "'+' operates only on numbers and strings"))))
     (define (lox-not x) (or (not x) (null? x)))
     ))
;;; for interactive use
(eval preamble)

(define (transpile statements)
  (cons preamble (map ast->sexp statements)))

;;; Expressions

(define-method (ast->sexp (node <assignment>))
  (let ((name (string->symbol (token-lexeme (slot-value node 'name))))
        (value (ast->sexp (slot-value node 'value))))
    `(set! ,name ,value)))

;;; The reason <logical> is distinct from <binary> is that, if I write an
;;; evaluator (rather than inheriting it from Scheme), I can have separate
;;; methods for the operators that short circuit (i.e. the same reason that's
;;; given in the book).
(define-method (ast->sexp (node <logical>))
  (let ((operator (token-type (slot-value node 'operator))))
    (list (condp eq? operator (#:OR 'or) (#:AND 'and) (else #f))
          (ast->sexp (slot-value node 'left))
          (ast->sexp (slot-value node 'right)))))

(define-method (ast->sexp (node <binary>))
  (list (cdr (assoc (token-type (slot-value node 'operator))
                    binary-operators))
        (ast->sexp (slot-value node 'left))
        (ast->sexp (slot-value node 'right))))

(define-method (ast->sexp (node <grouping>))
  (ast->sexp (slot-value node 'expression)))

(define-method (ast->sexp (node <literal>))
  (slot-value node 'value))

(define-method (ast->sexp (node <unary>))
  (list (cdr (assoc (token-type (slot-value node 'operator))
                    unary-operators))
        (ast->sexp (slot-value node 'right))))

(define-method (ast->sexp (node <variable>))
  (string->symbol (token-lexeme (slot-value node 'name))))

;;; Statements

(define-method (ast->sexp (node <block>))
  (let ((body (map ast->sexp (slot-value node 'statements))))
    `((lambda () ,body))))

(define-method (ast->sexp (node <expr-stmt>))
  (ast->sexp (slot-value node 'expression)))

(define-method (ast->sexp (node <if>))
  (let ((then-branch (slot-value node 'then-branch))
        (else-branch (slot-value node 'else-branch)))
    (if else-branch
        `(if ,then-branch ,else-branch)
        `(if ,then-branch))))

(define-method (ast->sexp (node <print>))
  (let ((expr (ast->sexp (slot-value node 'expression))))
     `(begin
        (display ,expr)
        (newline))))

(define-method (ast->sexp (node <var-stmt>))
  (let ((name (string->symbol (token-lexeme (slot-value node 'name))))
        (init-node (slot-value node 'initializer)))
    (if init-node
        `(define ,name ,(ast->sexp init-node))
        `(define ,name))))

