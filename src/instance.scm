(declare (unit instance)
         (uses token callable))

(import coops srfi-69)

(define-class <instance> ()
  (class (fields (make-hash-table string=?))))

(define (instance-set! instance name-token value)
  (hash-table-set! (slot-value instance 'fields)
                   (token-lexeme name-token)
                   value))

(define (instance-property instance name-token)
  (let ((fields (slot-value instance 'fields))
        (name (token-lexeme name-token)))
    (if (hash-table-exists? fields name)
        (hash-table-ref fields name)
        (let ((method (find-method (slot-value instance 'class) name)))
          (if method
              (bind method instance)
              (runtime-error! name-token
                              (string-append "Undefined property '"
                                             (token-lexeme name-token)
                                             "'.")))))))
