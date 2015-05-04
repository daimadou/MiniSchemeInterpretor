#lang racket
(require racket/match)
(require racket/trace)

(define (eval-program exp)
    (eval (get-defines exp) (env-initial)))


(define (eval exp env)
  (match exp
    [`(letrec ,binds ,eb) (eval-letrec binds eb env)]
    [`(let    ,binds ,eb) (eval-let binds eb env)]
    [`(if ,ec ,et ,ef) (if (eval ec env)
                           (eval et env)
                           (eval ef env))]
    [`(lambda ,vs . ,e) `(closure (lambda ,vs ,(get-defines e)) ,env)]
    [(? symbol?) (env-lookup env exp)]
    [(? number?) exp]
    [(? boolean?) exp]
    [`(,f . ,args) (apply-proc
                    (eval f env)
                    (map (eval-with env) args))]))

(define (eval-with env)
  (lambda (exp) (eval exp env)))

(define (get-defines body)
  (define (create-lambda parameters body)
    `(lambda ,parameters ,body))
  (define (create-letrec body defines)
    (match body 
    [`(,first-exp . ,rest-exps) 
     (match first-exp
       [`(define (,f . ,parameters) ,body) 
        (create-letrec rest-exps (append defines `((,f ,(create-lambda parameters body)))))]
       [`(define ,var ,val) 
        (create-letrec rest-exps (append defines `((,var ,val))))]
       [else `(letrec ,defines ,body)])]))
  (let ((empty-defines '()))
    (create-letrec body empty-defines)))

(trace get-defines)

(define (eval-letrec bindings body env)
  (if (null? bindings)
      (eval-seqences body env)
      (let* ((vars (map car bindings))
             (exps (map cadr bindings))
             (fs   (map (lambda _ #f) bindings))
             (env* (env-extend* env vars fs))
             (vals (map (eval-with env*) exps)))
        (env-set!* env* vars vals)
        (eval-seqences body env*))))

(define (eval-let bindings body env)
  (let* ((vars (map car bindings))
         (exps (map cadr bindings))
         (vals (map (eval-with env) exps))
         (env* (env-extend* env vars vals)))
    (eval-seqences body env*)))

(define (eval-define var value env)
  (match var 
    [(? symbol?) (env-set! env var (eval value env))]
    [`(,f . ,parameters) (env-set! env f `(lambda ,parameters ,value))]))

(define (apply-proc f values)
  (match f
    [(? void?) (void)]
    [`(closure (lambda ,vs . ,exps) ,env) 
     (eval-seqences exps (env-extend* env vs values))]
    [`(primitive ,p)
     (apply p values)]))
;;(trace apply-proc)

(define (eval-seqences exps env)
  (define (eval-subsequences exps)
      (match exps 
        [`(,first-exp . ,rest-exps) 
         (if (null? rest-exps)
             (eval first-exp env) 
             (begin (eval first-exp env) (eval-seqences rest-exps env)))]))
    (eval-subsequences exps))

;;(trace eval-seqences)

(define-struct cell ([value #:mutable]))

(define env-empty hash)
(define (env-initial)
  (env-extend*
   (env-empty)
   '(+ - * / void)
   (map (lambda (s) (list 'primitive s)) `(,+ ,- ,* ,/ ,void))))

(define (env-lookup env var)
  (cell-value (hash-ref env var)))

; sets a value in an environment:
(define (env-set! env var value)
  (set-cell-value! (hash-ref env var) value))

(define (env-extend* env vars values)
  (match `(,vars ,values)
    [`((,v . ,vars) (,val . ,values))
     (env-extend* (hash-set env v (make-cell val)) vars values)]
    [`(() ())
     env]))

(define (env-set!* env vars values)
  (match `(,vars ,values)
    [`((,v . ,vars) (,val . ,values))
     (begin
       (env-set! env v val)
       (env-set!* env vars values))]
    [`(() ())
     (void)]))




(define (unit-test expect-value exp)
  (let ((actual-value (eval-program exp)))
    (if (eq? expect-value actual-value)
      (displayln "test pass")
      (begin(error  "test fail" exp "\n"
                    "expect value:" expect-value "\n"
                    "actual value" actual-value "\n")))))
;;(unit-test (void) '((void 1 2)))
;;(unit-test (+ 3 4) '(+ 3 4))
;;(unit-test (+ 3 4) '((lambda ()(+ 3 4))))
;;(unit-test 2 '((lambda (v2)((lambda (v1) (+ v1 v2) (* v1 v2)) 1)) 2))
;;(unit-test 3 '((lambda (v2)((lambda (v1) (+ v1 v2)) 1)) 2))
;;(eval-program )
(eval-program '((define (f v1 v2)(+ v1 v2)) (f 3 4)))

