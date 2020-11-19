(declare (unit environment))

(import srfi-69)

(define-record env enclosing hash)
(define (make-environment enclosing)
  (make-env enclosing (make-hash-table string=? string-hash)))

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

(define (env-set-at! env name value depth)
  (hash-table-set! (env-hash (env-ancestor env depth))
                   name
                   value))

(define (env-ref env name-token)
  (hash-table-ref (env-hash (env-for-var env name-token))
                  (token-lexeme name-token)))

(define (env-ref-at env name depth)
  (hash-table-ref (env-hash (env-ancestor env depth))
                  name))

(define (look-up-variable env name-token expr)
  (let ((distance (hash-table-ref/default locals expr #f)))
    (if distance
        (env-ref-at env (token-lexeme name-token) distance)
        (env-ref global-environment name-token))))
