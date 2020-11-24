(declare (unit resolver)
         (uses token))

(include "src/utils.scm")

(import coops srfi-69)


(define scopes '())
(define scope-count 0)

(define-syntax with-scope
  (syntax-rules ()
    ((_ body ...)
     (dynamic-wind
       ;;; dynamic-wind isn't strictly necessary, since we just walk the entire
       ;;; tree once without jumping around with continuations.  But it's the
       ;;; right way to implement this macro.
       (lambda ()
         (set! scopes (cons (make-hash-table string=? string-hash) scopes))
         (set! scope-count (add1 scope-count)))
       (lambda ()
         body ...)
       (lambda ()
         (set! scopes (cdr scopes))
         (set! scope-count (sub1 scope-count)))))))

(define locals (make-hash-table eq? eq?-hash))

(define (resolve-local! expr name)
  (let loop ((remaining scopes)
             (depth 0))
    (cond
      ((null? remaining))
      ((hash-table-exists? (car remaining) (token-lexeme name))
       (hash-table-set! locals expr depth))
      (else (loop (cdr remaining) (add1 depth))))))

(define (declare! name-token)
  (unless (null? scopes)
    (let ((scope (car scopes))
          (name (token-lexeme name-token)))
      (if (hash-table-exists? scope name)
          (lox-error name-token
                     "Already variable with this name in this scope.")
          (hash-table-set! scope name #f)))))

(define (define! name-token)
  (unless (null? scopes)
    (hash-table-set! (car scopes) (token-lexeme name-token) #t)))

(define current-function #f)

(define (resolve-function! stmt function-type)
  (let ((enclosing-function current-function)
        (return-token (make-token #:RETURN "return" #f #f)))
    (set! current-function function-type)
    (with-scope
      (declare! return-token)
      (define! return-token)
      (for-each (lambda (param) (declare! param) (define! param))
                (slot-value stmt 'params))
      (for-each (lambda (body-stmt) (resolve body-stmt))
                (slot-value (slot-value stmt 'body) 'statements)))
    (set! current-function enclosing-function)))

(define current-class #f)


(define-generic (resolve ast-node))

;;; Expressions

(define-method (resolve (expr <assignment>))
  (resolve (slot-value expr 'value))
  (resolve-local! expr (slot-value expr 'name)))

(define-method (resolve (expr <binary>))
  (resolve (slot-value expr 'left))
  (resolve (slot-value expr 'right)))

(define-method (resolve (expr <call>))
  (resolve (slot-value expr 'callee))
  (for-each resolve (slot-value expr 'arguments)))

(define-method (resolve (expr <get>))
  (resolve (slot-value expr 'object)))

(define-method (resolve (expr <grouping>))
  (resolve (slot-value expr 'expression)))

(define-method (resolve (expr <literal>))
  #f)

(define-method (resolve (expr <logical>))
  (resolve (slot-value expr 'left))
  (resolve (slot-value expr 'right)))

(define-method (resolve (expr <set>))
  (resolve (slot-value expr 'value))
  (resolve (slot-value expr 'object)))

(define-method (resolve (expr <super>))
  (cond
    ((not current-class)
     (lox-error (slot-value expr 'keyword)
                "Can't use 'super' outside of a class."))
    ((not (eq? current-class #:SUBCLASS))
     (lox-error (slot-value expr 'keyword)
                "Can't use 'super' in a class with no superclass."))
    (else (resolve-local! expr (slot-value expr 'keyword)))))

(define-method (resolve (expr <this>))
  (if (not current-class)
      (lox-error (slot-value expr 'keyword)
                 "Can't use 'this' outside of a class.")
      (resolve-local! expr (slot-value expr 'keyword))))

(define-method (resolve (expr <unary>))
  (resolve (slot-value expr 'right)))

(define-method (resolve (expr <variable>))
  (let ((name (slot-value expr 'name)))
    (if (and (not (null? scopes))
             (not (hash-table-ref/default (car scopes)
                                          (token-lexeme name)
                                          #t)))
        (lox-error name "Can't read local variable in its own initializer.")
        (resolve-local! expr name))))


;;; Statements
(define-method (resolve (stmt <block>))
  (with-scope
    (for-each resolve (slot-value stmt 'statements))))

(define-method (resolve (stmt <class>))
  (let ((enclosing-class current-class))
    (set! current-class #:CLASS)
    (declare! (slot-value stmt 'name))
    (define! (slot-value stmt 'name))
    (when-let ((superclass (slot-value stmt 'superclass)))
      (set! current-class #:SUBCLASS)
      (if (string=? (token-lexeme (slot-value stmt 'name))
                    (token-lexeme (slot-value superclass 'name)))
          (lox-error (slot-value superclass 'name)
                     "A class can't inherit from itself.")
          (resolve superclass)))

    (if (slot-value stmt 'superclass)
        (with-scope
          (hash-table-set! (car scopes) "super" #t)
          (add-this! stmt))
        (add-this! stmt))
    (set! current-class enclosing-class)))

(define (add-this! stmt)
  (with-scope
    (hash-table-set! (car scopes) "this" #t)
    (for-each
      (lambda (method)
        (resolve-function!
          method
          (if (string=? "init" (token-lexeme (slot-value method 'name)))
              #:INITIALIZER
              #:METHOD)))
      (slot-value stmt 'methods))))

(define-method (resolve (stmt <expr-stmt>))
  (resolve (slot-value stmt 'expression)))

(define-method (resolve (stmt <function>))
  (declare! (slot-value stmt 'name))
  (define! (slot-value stmt 'name))
  (resolve-function! stmt #:FUNCTION))

(define-method (resolve (stmt <if>))
  (resolve (slot-value stmt 'condition))
  (resolve (slot-value stmt 'then-branch))
  (if-let ((alternative (slot-value stmt 'else-branch)))
          (resolve alternative)))

(define-method (resolve (stmt <print>))
  (resolve (slot-value stmt 'expression)))

(define-method (resolve (stmt <return>))
  (if (not current-function)
      (lox-error (slot-value stmt 'keyword)
                 "Can't return from top-level code.")
      (begin
        (if (slot-value stmt 'value)
            (if (eq? current-function #:INITIALIZER)
                (lox-error (slot-value stmt 'keyword)
                           "Can't return a value from an initializer.")
                (resolve (slot-value stmt 'value))))
        (resolve-local! stmt (slot-value stmt 'keyword)))))

(define-method (resolve (stmt <var-stmt>))
  (declare! (slot-value stmt 'name))
  (if-let ((initializer (slot-value stmt 'initializer)))
          (resolve initializer))
  (define! (slot-value stmt 'name)))


(define-method (resolve (stmt <while>))
  (resolve (slot-value stmt 'condition))
  (resolve (slot-value stmt 'body)))

