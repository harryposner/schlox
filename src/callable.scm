(declare (unit callable)
         (uses environment))

(include "src/utils.scm")

(import coops srfi-69)

(define-class <native-fn> () (arity procedure))
(define-class <lox-fn> () (declaration closure is-initializer))
(define-class <lox-class> ()
  (name superclass (methods (make-hash-table string=? string-hash))))


(define-generic (arity callable))

(define-method (arity (callable <native-fn>))
  (slot-value callable 'arity))

(define-method (arity (callable <lox-fn>))
  (length (slot-value (slot-value callable 'declaration)
                      'params)))

(define-method (arity (callable <lox-class>))
  (if-let ((initializer (find-method callable "init")))
          (arity initializer)
          0))


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
      (env-set-at! local-env
                   "return"
                   (lambda (return-val)
                     (return! (if (slot-value callable 'is-initializer)
                                  (env-ref-at local-env "this" 1)
                                  return-val)))
                   0)
      (for-each
        (lambda (param-token arg-val)
          (env-define! local-env param-token arg-val))
        (slot-value declaration 'params)
        args)
      ;;; The lox-eval needs to be part of the call/cc function since, if we do
      ;;; call the continuation (i.e. explicitly return from the function), we
      ;;; don't want to evaluate it again.
      (lox-eval (slot-value declaration 'body) local-env)
      ;;; Return value in absence of explicit return
      (if (slot-value callable 'is-initializer)
          (env-ref-at local-env "this" 1)
          '())))))

(define-method (lox-apply (callable <lox-class>) args)
  (let ((instance (make <instance> 'class callable))
        (initializer (find-method callable "init")))
    (if initializer
        (lox-apply (bind initializer instance) args))
    instance))


(define (callable? x)
  (memq (class-of x) (list <native-fn> <lox-fn> <lox-class>)))

(define (find-method lox-class name)
  (if-let ((method (hash-table-ref/default (slot-value lox-class 'methods)
                                           name
                                           #f)))
          method
          (if-let ((superclass (slot-value lox-class 'superclass)))
                  (find-method superclass name)
                  #f)))

(define (bind method instance)
  (let ((instance-env (make-environment (slot-value method 'closure))))
    (env-set-at! instance-env "this" instance 0)
    (make <lox-fn>
          'declaration (slot-value method 'declaration)
          'closure instance-env
          'is-initializer (slot-value method 'is-initializer))))
