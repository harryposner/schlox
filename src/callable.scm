(declare (unit callable))

(import coops srfi-69)

(define-class <native-fn> () (arity procedure))
(define-class <lox-fn> () (declaration closure is-initializer))
(define-class <lox-class> () (name superclass methods))


(define-generic (arity callable))

(define-method (arity (callable <native-fn>))
  (slot-value callable 'arity))

(define-method (arity (callable <lox-fn>))
  (length (slot-value (slot-value callable 'declaration)
                      'params)))

(define-method (arity (callable <lox-class>))
  (let ((initializer (find-method callable "init")))
    (if initializer
        (arity initializer)
        0)))


(define-generic (lox-apply callable args))

(define-method (lox-apply (callable <native-fn>) args)
  (apply (slot-value callable 'procedure) args))

(define-method (lox-apply (callable <lox-fn>) args)
  (let ((declaration (slot-value callable 'declaration))
        (local-env (make-environment (slot-value callable 'closure))))
    (call/cc (lambda (return!)
      ;;; Since return is a keyword, you can't define anything named "return"
      ;;; in Lox, so naming the continuation "return" in the environment won't
      ;;; clash with anything.  Not using env-define! since that takes a token
      ;;; for the name.
      (hash-table-set! (env-hash local-env) "return" return!)
      (for-each
        (lambda (param-token arg-val)
          (env-define! local-env param-token arg-val))
        (slot-value declaration 'params)
        args)
      ;;; The lox-eval needs to be part of the call/cc function since, if we do
      ;;; call the continuation (i.e. explicitly return from the function), we
      ;;; don't want to evaluate it again.
      (lox-eval (slot-value declaration 'body) local-env)
      ;;; If there's no early return, then return nil
      '()))))

(define (callable? x)
  (memq (class-of x) (list <native-fn> <lox-fn> <lox-class>)))

(define (find-method lox-class name)
  (hash-table-ref/default (slot-value lox-class 'methods) name #f))
