(declare (uses token)
         (unit scanner))

(include "src/utils.scm")


(define (scanning-error line message)
  (report line "" message))

(define keywords
  '(("and"    . #:AND)
    ("class"  . #:CLASS)
    ("else"   . #:ELSE)
    ("false"  . #:FALSE)
    ("for"    . #:FOR)
    ("fun"    . #:FUN)
    ("if"     . #:IF)
    ("nil"    . #:NIL)
    ("or"     . #:OR)
    ("print"  . #:PRINT)
    ("return" . #:RETURN)
    ("super"  . #:SUPER)
    ("this"   . #:THIS)
    ("true"   . #:TRUE)
    ("var"    . #:VAR)
    ("while"  . #:WHILE)))


(define (scan source)
  (define tokens '())
  (define start 0)
  (define current 0)
  (define line 1)

  (define (scan-tokens!)
    (if (at-end?)
        (reverse (cons (make-token #:EOF "" #f line) tokens))
        (begin
          (set! start current)
          (scan-token!)
          (scan-tokens!))))

  (define (scan-token!)
    (let ((char (advance!)))
      (condp char=? char

        ;;; Single-character tokens
        (#\( (add-token! #:LEFT-PAREN))
        (#\) (add-token! #:RIGHT-PAREN))
        (#\{ (add-token! #:LEFT-BRACE))
        (#\} (add-token! #:RIGHT-BRACE))
        (#\, (add-token! #:COMMA))
        (#\. (add-token! #:DOT))
        (#\- (add-token! #:MINUS))
        (#\+ (add-token! #:PLUS))
        (#\; (add-token! #:SEMICOLON))
        (#\* (add-token! #:STAR))

        ;;; Two-character tokens
        (#\! (add-token! (if (match! #\=) #:BANG-EQUAL #:BANG)))
        (#\= (add-token! (if (match! #\=) #:EQUAL-EQUAL #:EQUAL)))
        (#\< (add-token! (if (match! #\=) #:LESS-EQUAL #:LESS)))
        (#\> (add-token! (if (match! #\=) #:GREATER-EQUAL #:GREATER)))

        ;;; Comments and /
        (#\/ (cond
               ((match! #\/) (scan-comment!))
               ((match! #\*) (scan-block-comment!))
               (else (add-token! #:SLASH))))

        ;;; Whitespace
        (#\space)
        (#\return)
        (#\tab)
        (#\newline (set! line (add1 line)))

        ;;; Strings
        (#\" (scan-string-literal!))

        ;;; Keywords

        (else
          (cond
            ((digit? char) (scan-number-literal!))
            ((alpha? char) (scan-identifier!))
            (else (scanning-error line "Unexpected character.")))))))




  (define (scan-comment!)
    (unless (or (at-end?) (char=? (peek) #\newline))
      (advance!)
      (scan-comment!)))

  (define (scan-block-comment!)
    (advance!)
    (cond
      ((at-end?) (scanning-error line "Unterminated block comment."))
      ((and (char=? (peek) #\*) (char=? (peek-next) #\/))
       (advance!)
       (advance!))
      (else (scan-block-comment!))))

  (define (scan-string-literal!)
    (cond
      ((at-end?) (scanning-error line "Unterminated string."))
      ((char=? (peek) #\")
       (advance!) ;;; closing quote
       (add-token! #:STRING (substring source (add1 start) (sub1 current))))

      (else
        (if (char=? (peek) #\newline) (set! line (add1 line)))
        (advance!)
        (scan-string-literal!))))

  (define (scan-number-literal!)
    (let loop ((found-dot #f)
               (found-i #f))
      (cond
        ((and (not found-i) (digit? (peek)))
         (advance!)
         (loop found-dot found-i))
        ((and (not found-dot) (char=? (peek) #\.) (digit? (peek-next)))
         (advance!)
         (loop #t found-i))
        ((and (not found-i) (char=? (peek) #\i))
         (advance!)
         (loop found-dot #t))
        (else
          (add-token!
            #:NUMBER
            (let ((lexeme (substring source start current)))
              (string->number
                (if found-i
                    (string-append "0+" lexeme)
                    lexeme))))))))

  (define (scan-identifier!)
    (if (alphanumeric? (peek))
        (begin
          (advance!)
          (scan-identifier!))
        (let ((keyword (assoc (substring source start current) keywords)))
          (add-token! (if keyword (cdr keyword) #:IDENTIFIER)))))

  (define (advance!)
    (set! current (add1 current))
    (string-ref source (sub1 current)))

  (define (add-token! type #!optional literal)
    (let ((text (substring source start current)))
      (set! tokens (cons (make-token type text literal line) tokens))))

  (define (match! expected)
    (cond
      ((at-end?) #f)
      ((not (char=? (string-ref source current) expected)) #f)
      (else (set! current (add1 current)) #t)))

  (define (peek)
    (if (at-end?)
        #\null
        (string-ref source current)))

  (define (peek-next)
    (if (>= (add1 current) (string-length source))
        #\null
        (string-ref source (add1 current))))

  (define (at-end?)
    (>= current (string-length source)))

  (define (digit? char)
    (and (char>=? char #\0) (char<=? char #\9)))

  (define (alpha? char)
    (or (and (char>=? char #\a) (char<=? char #\z))
        (and (char>=? char #\A) (char<=? char #\Z))
        (char=? char #\_)))

  (define (alphanumeric? char)
    (or (alpha? char) (digit? char)))


  (scan-tokens!))
