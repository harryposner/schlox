(declare (uses token callable to-string)
         (unit lox-eval))

(include "src/utils.scm")

(import (chicken time)
        coops
        srfi-69)


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
      (lox-eval expr global-environment))))


;;; State and Environments
(define-record env enclosing hash)
(define (make-environment enclosing)
  (make-env enclosing (make-hash-table)))
(define global-environment
  (make-environment #f))

(define (env-define! env name-token value)
  (hash-table-set! (env-hash env) (token-lexeme name-token) value))

(define (env-for-var env name-token error-on-undefined)
  (cond
    ((and (not env) error-on-undefined)
     (runtime-error!
       name-token
       (string-append "Undefined variable '" (token-lexeme name-token) "'.")))
    ((not env) #f)
    ((hash-table-exists? (env-hash env) (token-lexeme name-token))
     env)
    (else (env-for-var (env-enclosing env) name-token error-on-undefined))))

(define (env-set! env name-token value)
  (hash-table-set! (env-hash (env-for-var env name-token #t))
                   (token-lexeme name-token)
                   value))

(define (env-ref env name-token)
  (hash-table-ref (env-hash (env-for-var env name-token #t))
                  (token-lexeme name-token)))


;;; Native functions

;;; It's not strictly necessary to define this macro, but it will be useful if
;;; we want to implement more native functions (e.g. those required to run
;;; LoxLox).
(define-syntax define-native
  (syntax-rules ()
    ((_ (name args ...) (body ...))
     (hash-table-set! (env-hash global-environment)
                      (symbol->string 'name)
                      (make <native-fn>
                            'arity (length '(args ...))
                            'procedure (lambda (args ...) (body ...)))))))

(define-native (clock) (/ (current-milliseconds) 1000))

;;; Expressions

(define-method (lox-eval (expr <assignment>) env)
  (let ((value (lox-eval (slot-value expr 'value) env)))
    (env-set! env (slot-value expr 'name) value)
    value))

(define-method (lox-eval (expr <binary>) env)
  (let* ((left (lox-eval (slot-value expr 'left) env))
         (right (lox-eval (slot-value expr 'right) env))
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


(define-method (lox-eval (expr <call>) env)
  (let* ((callee (lox-eval (slot-value expr 'callee) env))
         (args (map (lambda (arg) (lox-eval arg env))
                    (slot-value expr 'arguments))))
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

; (define-method (lox-eval (expr <get>) env))

(define-method (lox-eval (expr <grouping>) env)
  (lox-eval (slot-value expr 'expression) env))

(define-method (lox-eval (expr <literal>) env)
  (slot-value expr 'value))

(define-method (lox-eval (expr <logical>) env)
  (let ((left-result (lox-eval (slot-value expr 'left) env))
        (right-expr (slot-value expr 'right)))
    (condp eq? (token-type (slot-value expr 'operator))
           (#:AND (if (lox-truthy? left-result)
                      (lox-eval right-expr env)
                      left-result))
           (#:OR (if (lox-truthy? left-result)
                     left-result
                     (lox-eval right-expr env)))
           (else (error "UNREACHABLE: lox-eval <logical>")))))

; (define-method (lox-eval (expr <set>) env))

; (define-method (lox-eval (expr <super>) env))

; (define-method (lox-eval (expr <this>) env))

(define-method (lox-eval (expr <unary>) env)
  (let ((operator (slot-value expr 'operator))
        (right (lox-eval (slot-value expr 'right) env)))
    ((condp eq? (token-type operator)
            (#:MINUS (check-number-operand! operator right) -)
            (#:BANG lox-not?)
            (else (error "UNREACHABLE: lox-eval <unary>")))
   right)))

(define-method (lox-eval (expr <variable>) env)
  (env-ref env (slot-value expr 'name)))


;;; Statements

(define-method (lox-eval (expr <block>) env)
  (let ((block-env (make-environment env)))
    (for-each (lambda (stmt) (lox-eval stmt env))
              (slot-value expr 'statements))))

; (define-method (lox-eval (expr <class>) env))

(define-method (lox-eval (expr <expr-stmt>) env)
  (lox-eval (slot-value expr 'expression) env))

(define-method (lox-eval (expr <function>) env)
  (env-define! env
               (slot-value expr 'name)
               (make <lox-fn>
                     'declaration expr
                     'closure (make-environment env))))

(define-method (lox-eval (expr <if>) env)
  (if (lox-truthy? (lox-eval (slot-value expr 'condition) env))
      (lox-eval (slot-value expr 'then-branch) env)
      (let ((alternative (slot-value expr 'else-branch)))
        ;;; "if" is a statement in Lox, so we don't use the return value of
        ;;; this expression, so it's okay to use the single-armed "if" even
        ;;; though it can return an undefined value.
        (if alternative
            (lox-eval alternative env)))))

(define-method (lox-eval (expr <print>) env)
  (let ((result (lox-eval (slot-value expr 'expression) env)))
    (display (if (string? result)
                 result
                 (lox->string result))))
  (newline))

(define-method (lox-eval (expr <return>) env)
  ;;; This will get the return continuation for the innermost enclosing
  ;;; function.
  (let ((keyword-token (slot-value expr 'keyword)))
    (if (env-for-var env keyword-token #f)
        ((env-ref env keyword-token) (lox-eval (slot-value expr 'value) env))
        (runtime-error! keyword-token "Can't return from top-level code."))))

(define-method (lox-eval (expr <var-stmt>) env)
  (let ((initializer (slot-value expr 'initializer)))
    (env-define! env
                 (slot-value expr 'name)
                 (if initializer
                     (lox-eval initializer env)
                     '()))))

(define-method (lox-eval (expr <while>) env)
  (when (lox-truthy? (lox-eval (slot-value expr 'condition) env))
    (lox-eval (slot-value expr 'body) env)
    (lox-eval expr env)))
