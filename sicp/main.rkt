#lang racket

(require racket/provide
         (prefix-in r5rs: r5rs)
         (rename-in racket [random racket:random]))

(provide (filtered-out (λ (name) (regexp-replace #px"^r5rs:" name ""))
                       (combine-out
                        (except-out (all-from-out r5rs) r5rs:#%module-begin)
                        (rename-out [r5rs:lambda r5rs:λ])))
         (rename-out [module-begin #%module-begin]))

(define-syntax (define+provide stx)
  (syntax-case stx ()
    [(_ (id . args) . body) #'(begin
                                (provide id)
                                (define (id . args) . body))]
    [(_ id expr) #'(begin
                     (provide id)
                     (define id expr))]))

(provide
 true
 false
 error
 compose
 compose1
 identity
 null
 add1
 sub1
 (rename-out
  [null  nil]
  [null  the-empty-stream]
  [null? stream-null?]
  [add1 inc]
  [sub1 dec]
  [add1 1+]
  [sub1 1-]
  [sub1 -1+]))
(define+provide (runtime)
  (inexact->exact (truncate (* 1000 (current-inexact-milliseconds)))))
(define+provide (random n)
  (if (and (integer? n) (exact? n))
      (racket:random n)
      (* n (racket:random))))

(provide cons-stream)
(define-syntax cons-stream
  (syntax-rules ()
    [(_ A B) (r5rs:cons A (r5rs:delay B))]))


(provide amb)

(define (amb-fail) (error "amb tree exhausted"))
(define (set-amb-fail! x) (set! amb-fail x))

(define-syntax-rule (explore +prev-amb-fail +sk alt)
  (let/cc +fk
    (set-amb-fail!
     (thunk
      (set-amb-fail! +prev-amb-fail)
      (+fk 'fail)))
    (+sk alt)))

(define-syntax amb
  (syntax-rules ()
    [(_) (amb-fail)]
    [(_ alt) alt]
    [(_ alt ...)
     (let ([+prev-amb-fail amb-fail])
       (let/cc +sk
         (explore +prev-amb-fail +sk alt)
         ...
         (+prev-amb-fail)))]))

(define-syntax module-begin
  (syntax-rules ()
    ((_ . forms)
     (#%printing-module-begin
      (module configure-runtime '#%kernel
        (print-as-expression #f)
        (print-pair-curly-braces  #t)
        (print-mpair-curly-braces #f))
      . forms))))
