(declare (uses scanner parser pretty-print lox-eval))

(include "src/utils.scm")

(import (chicken io)
        (chicken process-context)
        coops)


(define (main)
  (let ((args (command-line-arguments)))
    (cond
      ((= (length args) 0)
       (run-prompt run))
      ((and (= (length args) 1) (equal? (car args) "--pretty-print"))
       (run-prompt run-pretty-print))
      ((= (length args) 1)
       (run-file run (car args)))
      ((and (= (length args) 2) (equal? (car args) "--pretty-print"))
       (run-file run-pretty-print (cadr args)))
      (else
       (display "Usage: schlox [--pretty-print] [script]")
       (newline)
       (exit 64)))))


;;; Running things
(define (run-file run-procedure filename)
  (call-with-input-file filename
                        (lambda (port) (run-procedure (read-string #f port))))
  (if *had-error* (exit 65))
  (if *had-runtime-error* (exit 70)))

(define (run-prompt run-procedure)
  (let loop ()
    (display "> ")
    (let ((line (read-line)))
      (unless (eof-object? line)
        (run-procedure line)
        (set! *had-error* #f)
        (loop))))
  (newline)
  (display "Bye-bye for now!")
  (newline))

(define (run source)
  (let ((statements (parse (scan source))))
    (if (not *had-error*)  ;;; catches syntax errors
        (for-each lox-eval-top statements))))

(define (run-pretty-print source)
  (let ((statements (parse (scan source))))
      (if (not *had-error*)  ;;; catches syntax errors
          (for-each
            (lambda (stmt) (display (pretty-print stmt)) (newline))
            statements))))

;;; Error Handling
(define *had-error* #f)
(define *had-runtime-error* #f)

(define (report line where message)
  (for-each (lambda (str) (write-string str #f (current-error-port)))
            `("[line "
              ,(number->string line)
              "] Error"
              ,where
              ": "
              ,message))
  (newline (current-error-port))
  (set! *had-error* #t))

(define (runtime-error line message)
  (display message (current-error-port))
  (newline (current-error-port))
  (for-each (lambda (str) (write-string str #f (current-error-port)))
            `("[line " ,(number->string line) "]"))
  (newline (current-error-port))
  (set! *had-runtime-error* #t))

(main)
