(declare (uses token ast)
         (unit parser))

(import coops)

(include "src/utils.scm")


(define-syntax define-binary-ops
  (syntax-rules (->)
    ((_ ((name -> next-down) (tokens ...) <node-type>))
     (define (name)
       (let loop ((expr (next-down)))
         (if (not (match! tokens ...))
             expr
             (let ((operator prev-token))
               (loop (make <node-type>
                           'left expr
                           'operator operator
                           'right (next-down))))))))
    ((_ one-op more-ops ...)
     (begin (define-binary-ops one-op)
            (define-binary-ops more-ops ...)))))

(define (parse tokens)
  (define prev-token)
  (define statements '())

  (define (parse-tokens)
    (if (at-end?)
        (reverse statements)
        (begin
          (set! statements (cons (top-of-grammar) statements))
          (parse-tokens))))

  ;;; Instead of throwing an exception on an error and catching it at a
  ;;; synchronization point, we'll set continue! to the continuation at the
  ;;; synchronization point and call it if we have an error.
  (define continue!)
  (define (top-of-grammar)
    (call/cc
      (lambda (cont)
        (set! continue! (lambda () (synchronize!) (cont #f)))
        (declaration))))

  ;;; Grammar rules

  (define (declaration)
    (cond
      ((match! #:CLASS) (class-declaration))
      ((match! #:FUN) (function "function"))
      ((match! #:VAR) (var-declaration))
      (else (statement))))

  (define (class-declaration)
    (let* ((name (consume! #:IDENTIFIER "Expect class name."))
           (superclass (if (match! #:LESS-THAN)
                           (begin
                             (consume! #:IDENTIFIER "Expect superclass name.")
                             (make <variable> 'name prev-token))
                           #f)))
      (consume! #:LEFT-BRACE "Expect '{' before class body.")
      (let loop ((methods '()))
        (if (or (check? #:RIGHT-BRACE) (at-end?))
            (begin
              (consume! #:RIGHT-BRACE "Expect '}' after class body.")
              (make <class>
                    'name name
                    'methods (reverse methods)
                    'superclass superclass))
            (loop (cons (function "method") methods))))))

  (define (function kind)
    (let ((name (consume! #:IDENTIFIER
                          (string-append "Expect " kind " name."))))
      (consume! #:LEFT-PAREN (string-append "Expect '(' after " kind " name."))
      (let ((params (if (check? #:RIGHT-PAREN) '() (parameters))))
        (consume! #:RIGHT-PAREN "Expect ')' after parameters.")
        (consume! #:LEFT-BRACE
                  (string-append "Expect '{' before " kind " body."))
        (make <function>
              'name name
              'params params
              'body (block)))))

  (define (parameters)
    (let loop ((first-loop #t)  ;;; emulating do-while loop
               (param-count 1)
               (param-list '()))
      (cond
        ((>= param-count 255)
         (lox-error (peek) "Can't have more than 255 parameters."))
        ((or first-loop (match! #:COMMA))
         (loop #f
               (add1 param-count)
               (cons (consume! #:IDENTIFIER "Expect parameter name.")
                     param-list)))
        (else (reverse param-list)))))

  (define (var-declaration)
    ;;; Need to use let* because consume! and match! have side effects
    (let* ((name (consume! #:IDENTIFIER "Expect variable name."))
           ;;; This seems clearer than setting a default value for initializer
           ;;; in the class definition
           (initializer (if (match! #:EQUAL) (expression) #f)))
      (consume! #:SEMICOLON "Expect ';' after variable declaration")
      (make <var-stmt> 'name name 'initializer initializer)))

  (define (statement)
    (cond
      ((match! #:FOR) (for-statement))
      ((match! #:IF) (if-statement))
      ((match! #:PRINT) (print-statement))
      ((match! #:RETURN) (return-statement))
      ((match! #:WHILE) (while-statement))
      ((match! #:LEFT-BRACE) (block))
      (else (expression-statement))))

  (define (for-statement)
    (consume! #:LEFT-PAREN "Expect '(' after 'for'.")
    (let* ((initializer (cond ((match! #:SEMICOLON) #f)
                              ((match! #:VAR) (var-declaration))
                              (else (expression-statement))))
           (condition (if (check? #:SEMICOLON)
                          (make <literal> 'value #t)
                          (expression))))
      (consume! #:SEMICOLON "Expect ';' after loop condition.")
      (let ((increment (if (check? #:RIGHT-PAREN) #f (expression))))
        (consume! #:RIGHT-PAREN "Expect ')' after for clauses.")
        (let ((body (statement)))
          (if increment
              (set! body (make <block> 'statements (list body increment))))
          (set! body (make <while> 'condition condition 'body body))
          (if initializer
              (set! body (make <block> 'statements (list initializer body))))
          body))))

  (define (if-statement)
    (consume! #:LEFT-PAREN "Expect '(' after 'if'.")
    (let ((condition (expression)))
      (consume! #:RIGHT-PAREN "Expect ')' after if condition.")
      (let* ((then-branch (statement))
             (else-branch (if (match! #:ELSE) (statement) #f)))
        (make <if>
              'condition condition
              'then-branch then-branch
              'else-branch else-branch))))

  (define (print-statement)
    (let ((value (expression)))
      (consume! #:SEMICOLON "Expect ';' after value.")
      (make <print> 'expression value)))

  (define (return-statement)
    (let* ((keyword prev-token)
           (value (if (check? #:SEMICOLON)
                      #f
                      (expression))))
      (consume! #:SEMICOLON "Expect ';' after return value.")
      (make <return> 'keyword keyword 'value value)))

  (define (while-statement)
    (consume! #:LEFT-PAREN "Expect '(' after 'while'.")
    (let ((condition (expression)))
      (consume! #:RIGHT-PAREN "Expect ')' after condition.")
      (make <while> 'condition condition 'body (statement))))

  (define (block)
    (let loop ((statements '()))
      (if (or (check? #:RIGHT-BRACE) (at-end?))
          (begin
            (consume! #:RIGHT-BRACE "Expect '}' after block.")
            (make <block> 'statements (reverse statements)))
          (loop (cons (declaration) statements)))))

  (define (expression-statement)
    (let ((expr (expression)))
      (consume! #:SEMICOLON "Expect ';' after expression.")
      (make <expr-stmt> 'expression expr)))

  (define (expression)
    (assignment))

  (define (assignment)
    (let ((expr (logic-or)))
      (if (match! #:EQUAL)
          (let* ((equals prev-token)
                 (value (assignment)))
            (condp eq? (class-of expr)
                   (<variable>
                     (make <assignment>
                           'name (slot-value expr 'name)
                           'value value))
                   (<get>
                     (make <set>
                           'object (slot-value expr 'object)
                           'name (slot-value expr 'name)
                           'value value))
                   (else (lox-error equals "Invalid assignment target."))))
          expr)))

  (define-binary-ops
    ((logic-or -> logic-and) (#:OR) <logical>)
    ((logic-and -> equality) (#:AND) <logical>)
    ((equality -> comparison) (#:BANG-EQUAL #:EQUAL-EQUAL) <binary>)
    ((comparison -> addition)
     (#:GREATER #:GREATER-EQUAL #:LESS #:LESS-EQUAL)
     <binary>)
    ((addition -> multiplication) (#:MINUS #:PLUS) <binary>)
    ((multiplication -> unary) (#:SLASH #:STAR) <binary>))

  (define (unary)
    (cond
      ((match! #:BANG #:MINUS)
       (let ((operator prev-token))
         ;;; This let is necessary because the order in which Scheme evaluates
         ;;; the operands of a procedure is undefined.  The recursive call to
         ;;; unary may modify prev-token before prev-token is resolved.
         ;;; (see SICP exercise 4.1)
         (make <unary> 'operator operator 'right (unary))))
      ;;; Chapter 6, challenge #3 : error productions
      ((match! #:BANG-EQUAL #:EQUAL-EQUAL #:GREATER #:GREATER-EQUAL #:LESS
               #:LESS-EQUAL #:PLUS #:SLASH #:STAR)
       (lox-error prev-token "Binary operator used as unary")
       (unary))
      (else (call))))

  (define (call)
    (let loop ((expr (primary)))
      (cond
        ((match! #:LEFT-PAREN) (loop (finish-call expr)))
        ((match! #:DOT)
         (loop (make <get>
                     'object expr
                     'name (consume! #:IDENTIFIER
                                     "Expect property name after '.'."))))
        (else expr))))

  (define (finish-call callee)
    (let* ((args (call-arguments))
           (paren (consume! #:RIGHT-PAREN "Expect ')' after arguments.")))
      (make <call> 'callee callee 'paren paren 'arguments args)))

  (define (call-arguments)
    (if (check? #:RIGHT-PAREN)
        '()
        (let loop ((args (list (expression))))
          (if (>= (length args) 255)
              ;;; Intentionally reporting error but not panicking
              (lox-error (peek) "Can't have more than 255 arguments."))
          (if (match! #:COMMA)
              (loop (cons (expression) args))
              (reverse args)))))

  (define (primary)
    (cond
      ((match! #:FALSE) (make <literal> 'value #f))
      ((match! #:TRUE) (make <literal> 'value #t))
      ((match! #:NIL) (make <literal> 'value '()))
      ((match! #:NUMBER #:STRING)
       (make <literal> 'value (token-literal prev-token)))
      ((match! #:LEFT-PAREN)
        (let ((expr (expression)))
          (consume! #:RIGHT-PAREN "Expect ')' after expression.")
          (make <grouping> 'expression expr)))
      ((match! #:THIS) (make <this> 'keyword prev-token))
      ((match! #:IDENTIFIER) (make <variable> 'name prev-token))
      (else (lox-error (peek) "Expect expression.")
            (continue!))))

  ;;; Helper procedures

  (define (synchronize!)
    (advance!)
    (unless
      (or (at-end?)
          (eq? prev-token #:SEMICOLON)
          (memq (token-type (peek))
                '(#:CLASS #:FUN #:VAR #:FOR #:IF #:WHILE #:PRINT #:RETURN)))
      (synchronize!)))

  (define (match! . token-types)
    (cond
      ((null? token-types) #f)
      ((check? (car token-types))
       (advance!)
       #t)
      (else (apply match! (cdr token-types)))))

  (define (consume! token-type message)
    (if (check? token-type)
        (advance!)
        (begin
          (lox-error (peek) message)
          (continue!))))

  (define (check? expected-type)
    (if (at-end?)
        #f
        (eq? (token-type (peek)) expected-type)))

  (define (at-end?)
    (eq? (token-type (peek)) #:EOF))

  (define (peek)
    (car tokens))

  (define (advance!)
    (when (not (at-end?))
      (set! prev-token (car tokens))
      (set! tokens (cdr tokens)))
    prev-token)

  (parse-tokens))
