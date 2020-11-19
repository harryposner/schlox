(declare (uses token callable to-string resolver)
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
(define (lox-eval-top statements)
  (unless (or *had-runtime-error* (null? statements))
    (call/cc
      (lambda (cont)
        (set! runtime-error!
          (lambda (token message)
            (runtime-error (token-line token) message)
            (cont #f)))
        (lox-eval (car statements) global-environment)
        (lox-eval-top (cdr statements))))))


;;; State and Environments
(define-record env enclosing hash)
(define (make-environment enclosing)
  (make-env enclosing (make-hash-table string=? string-hash)))
(define global-environment
  (make-environment #f))

(define (env-ancestor env depth)
  (if (zero? depth)
      env
      (env-ancestor (env-enclosing env) (sub1 depth))))

(define (env-define! env name-token value)
  (hash-table-set! (env-hash env) (token-lexeme name-token) value))

(define (env-for-var env name-token)
  (cond
    ((not env)
     (runtime-error!
       name-token
       (string-append "Undefined variable '" (token-lexeme name-token) "'.")))
    ((hash-table-exists? (env-hash env) (token-lexeme name-token))
     env)
    (else (env-for-var (env-enclosing env) name-token))))

(define (env-set! env name-token value)
  (hash-table-set! (env-hash (env-for-var env name-token))
                   (token-lexeme name-token)
                   value))

(define (env-set-at! env name-token value depth)
  (hash-table-set! (env-ancestor env depth)
                   (token-lexeme name-token)
                   value))

(define (env-ref env name-token)
  (hash-table-ref (env-hash (env-for-var env name-token))
                  (token-lexeme name-token)))

(define (env-ref-at env name-token depth)
  (hash-table-ref (env-hash (env-ancestor env depth))
                  (token-lexeme name-token)))

(define (look-up-variable env name-token expr)
  (let ((distance (hash-table-ref/default locals expr #f)))
    (if distance
        (env-ref-at env name-token distance)
        (env-ref global-environment name-token))))


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
  (let ((value (lox-eval (slot-value expr 'value) env))
        (distance (hash-table-ref/default locals expr #f))
        (name (slot-value expr 'name)))
    (if distance
        (env-set-at! env name value distance)
        (env-set! global-environment name value))
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
  (look-up-variable env (slot-value expr 'name) expr))


;;; Statements

(define-method (lox-eval (stmt <block>) env)
  (let ((block-env (make-environment env)))
    (for-each (lambda (stmt) (lox-eval stmt block-env))
              (slot-value stmt 'statements))))

; (define-method (lox-eval (stmt <class>) env))

(define-method (lox-eval (stmt <expr-stmt>) env)
  (lox-eval (slot-value stmt 'expression) env))

(define-method (lox-eval (stmt <function>) env)
  (env-define! env
               (slot-value stmt 'name)
               (make <lox-fn>
                     'declaration stmt
                     'closure env)))

(define-method (lox-eval (stmt <if>) env)
  (if (lox-truthy? (lox-eval (slot-value stmt 'condition) env))
      (lox-eval (slot-value stmt 'then-branch) env)
      (let ((alternative (slot-value stmt 'else-branch)))
        ;;; "if" is a statement in Lox, so we don't use the return value of
        ;;; this expression, so it's okay to use the single-armed "if" even
        ;;; though it can return an undefined value.
        (if alternative
            (lox-eval alternative env)))))

(define-method (lox-eval (stmt <print>) env)
  (let ((result (lox-eval (slot-value stmt 'expression) env)))
    (display (if (string? result)
                 result
                 (lox->string result))))
  (newline))

(define-method (lox-eval (stmt <return>) env)
  ;;; This will get the return continuation for the innermost enclosing
  ;;; function.  The resolver catches any top-level returns at compile time, so
  ;;; we know that the return continuation does exist in some enclosing
  ;;; environment.
  ((look-up-variable env (slot-value stmt 'keyword) stmt)
   (lox-eval (slot-value stmt 'value) env)))

  ; (let ((keyword-token (slot-value stmt 'keyword)))
  ;   ((env-ref env keyword-token) (lox-eval (slot-value stmt 'value) env))))

(define-method (lox-eval (stmt <var-stmt>) env)
  (let ((initializer (slot-value stmt 'initializer)))
    (env-define! env
                 (slot-value stmt 'name)
                 (if initializer
                     (lox-eval initializer env)
                     '()))))

(define-method (lox-eval (stmt <while>) env)
  (when (lox-truthy? (lox-eval (slot-value stmt 'condition) env))
    (lox-eval (slot-value stmt 'body) env)
    (lox-eval stmt env)))
