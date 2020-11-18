(define-syntax condp-helper
  (syntax-rules (else)
    ((_ pred? expr (else result-expr ...))
     (begin result-expr ...))
    ((_ pred? expr (test-expr result-expr ...) more-clauses ...)
     (if (pred? test-expr expr)
         (begin result-expr ...)
         (condp-helper pred? expr more-clauses ...)))))

(define-syntax condp
  (syntax-rules (else)
    ((_ pred? expr clauses ...)
     ;;; So expr doesn't get evaluated for every test clause
     (let ((p pred?)
           (to-test expr))
       (condp-helper p to-test clauses ...)))))
