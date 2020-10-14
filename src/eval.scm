(declare (uses token callable)
         (unit lox-eval))

(include "src/utils.scm")

(import coops srfi-69)


;;; Helpers for Lox operators that don't work like their Scheme counterparts
(define (lox-truthy? x) (and x (not (null? x))))
(define (lox-not? x) (not (lox-truthy? x)))
(define (lox-equal? x y)
  (if (and (number? x) (number? y))
      (= x y)
      (equal? x y)))
(define (lox-not-equal? x y) (lox-not? (lox-equal? x y)))


;;; Error handling
(define (check-number-operand! operator operand)
  (if (not (number? operand))
      (runtime-error! operator "Operand must be a number.")))

(define (check-number-operands! operator left right)
  (if (not (and (number? left) (number? right)))
      (runtime-error! operator "Operands must be numbers.")))


(define runtime-error!)
(define (lox-eval-top expr)
  (call/cc
    (lambda (cont)
      (set! runtime-error!
        (lambda (token message)
          (runtime-error (token-line token) message)
          (cont #f)))
      (lox-eval expr))))


;;; State and Environments
(define-record env enclosing hash)
(define (make-environment enclosing)
  (make-env enclosing (make-hash-table)))
(define global-environment
  (make-environment #f))
(define current-env global-environment)
(define (env-define! env name value)
  (hash-table-set! (env-hash env) (token-lexeme name) value))

(define (resolve env name-token)
  (cond
    ((not env)
     (runtime-error! name-token
                     (string-append "Undefined variable '"
                                    (token-lexeme name-token)
                                    "'.")))
    ((hash-table-exists? (env-hash env) (token-lexeme name-token))
     env)
    (else (resolve (env-enclosing env) name-token))))

(define (env-set! env name-token value)
  (let ((env-for-var (resolve env name-token)))
    (hash-table-set! (env-hash env-for-var) (token-lexeme name-token) value)))

(define (env-ref env name-token)
  (let ((env-for-var (resolve env name-token)))
    (hash-table-ref (env-hash env-for-var) (token-lexeme name-token))))


;;; Expressions

(define-method (lox-eval (expr <assignment>))
  (let ((value (lox-eval (slot-value expr 'value))))
    (env-set! current-env (slot-value expr 'name) value)
    value))

(define-method (lox-eval (expr <binary>))
  (let* ((left (lox-eval (slot-value expr 'left)))
         (right (lox-eval (slot-value expr 'right)))
         (operator (slot-value expr 'operator)))
    (unless (memq (token-type operator) '(#:PLUS #:BANG-EQUAL #:EQUAL-EQUAL))
      (check-number-operands! operator left right))
    ((condp eq? (token-type operator)
            (#:MINUS -)
            (#:PLUS
             ;;; Can't lift this out since the error handling needs the
             ;;; operator token
             (cond
               ((and (number? left) (number? right)) +)
               ((and (string? left) (string? right)) string-append)
               (else (runtime-error!
                       operator
                       "Operands must be two numbers or two strings"))))
            (#:SLASH
             ;;; Chapter 7, challenge #3
             (if (and (exact? right) (= right 0))
                 (runtime-error! operator "Cannot divide by exact zero")
                 /))
            (#:STAR *)
            (#:GREATER >)
            (#:GREATER-EQUAL >=)
            (#:LESS <)
            (#:LESS-EQUAL <=)
            (#:BANG-EQUAL lox-not-equal?)
            (#:EQUAL-EQUAL lox-equal?)
            (else (error "UNREACHABLE: lox-eval <binary>")))
     left right)))


(define-method (lox-eval (expr <call>))
  (let* ((callee (lox-eval (slot-value expr 'callee)))
         (args (map lox-eval (slot-value expr 'arguments))))
    (if (not (callable? callee))
        (runtime-error! (slot-value expr 'paren)
                        "Can only call functions and classes."))
    (let ((expected (arity callee))
          (got (length args)))
      (if (not (= got expected))
          (runtime-error!
            (slot-value expr 'paren)
            (string-append "Expected " (number->string expected) " arguments "
                           "but got " (number->string got) "."))))
    (lox-apply callee args)))

; (define-method (lox-eval (expr <get>)))

(define-method (lox-eval (expr <grouping>))
  (lox-eval (slot-value expr 'expression)))

(define-method (lox-eval (expr <literal>))
  (slot-value expr 'value))

(define-method (lox-eval (expr <logical>))
  (let ((left-result (lox-eval (slot-value expr 'left)))
        (right-expr (slot-value expr 'right)))
    (condp eq? (token-type (slot-value expr 'operator))
           (#:AND (if (lox-truthy? left-result)
                      (lox-eval right-expr)
                      left-result))
           (#:OR (if (lox-truthy? left-result)
                     left-result
                     (lox-eval right-expr)))
           (else (error "UNREACHABLE: lox-eval <logical>")))))

; (define-method (lox-eval (expr <set>)))

; (define-method (lox-eval (expr <super>)))

; (define-method (lox-eval (expr <this>)))

(define-method (lox-eval (expr <unary>))
  (let ((operator (slot-value expr 'operator))
        (right (lox-eval (slot-value expr 'right))))
    ((condp eq? (token-type operator)
            (#:MINUS (check-number-operand! operator right) -)
            (#:BANG lox-not?)
            (else (error "UNREACHABLE: lox-eval <unary>")))
   right)))

(define-method (lox-eval (expr <variable>))
  (env-ref current-env (slot-value expr 'name)))


;;; Statements

(define-method (lox-eval (expr <block>))
  (let ((previous-env current-env))
    (dynamic-wind
      ;;; The first clause doesn't need to be in the dynamic-wind, since we
      ;;; never call a continuation that brings us back into the call stack,
      ;;; only one that unwinds the call stack.
      (lambda () (set! current-env (make-environment current-env)))
      (lambda () (for-each lox-eval (slot-value expr 'statements)))
      (lambda () (set! current-env previous-env)))))

; (define-method (lox-eval (expr <class>)))
(define-method (lox-eval (expr <expr-stmt>))
  (lox-eval (slot-value expr 'expression)))
; (define-method (lox-eval (expr <function>)))
(define-method (lox-eval (expr <if>))
  (if (lox-truthy? (lox-eval (slot-value expr 'condition)))
      (lox-eval (slot-value expr 'then-branch))
      (let ((alternative (slot-value expr 'else-branch)))
        ;;; "if" is a statement in Lox, so we don't use the return value of
        ;;; this expression, so it's okay to use the single-armed "if" even
        ;;; though it can return an undefined value.
        (if alternative
            (lox-eval alternative)))))

(define-method (lox-eval (expr <print>))
  (let ((result (lox-eval (slot-value expr 'expression))))
    (display (if (string? result)
                 result
                 (lox->string result))))
  (newline))

; (define-method (lox-eval (expr <return>)))

(define-method (lox-eval (expr <var-stmt>))
  (let ((initializer (slot-value expr 'initializer)))
    (env-define! current-env
                 (slot-value expr 'name)
                 (if initializer
                     (lox-eval initializer)
                     '()))))

(define-method (lox-eval (expr <while>))
  (when (lox-truthy? (lox-eval (slot-value expr 'condition)))
    (lox-eval (slot-value expr 'body))
    (lox-eval expr)))
