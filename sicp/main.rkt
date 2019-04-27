#lang racket

(require racket/provide
         (prefix-in r5rs: r5rs)
         (rename-in racket [random racket:random]))

(provide (filtered-out (λ (name) (regexp-replace #px"^r5rs:" name ""))
                       (except-out (all-from-out r5rs) r5rs:#%module-begin))
         (rename-out [module-begin #%module-begin]))

(define-syntax (define+provide stx)
  (syntax-case stx ()
    [(_ (id . args) . body) #'(begin
                                (provide id)
                                (define (id . args) . body))]
    [(_ id expr) #'(begin
                     (provide id)
                     (define id expr))]))

(provide true)
(provide false)
(provide error)
(provide identity)
(define+provide nil '())
(define+provide the-empty-stream '())
(define+provide stream-null? null?)
(define+provide (inc x) (+ x 1))
(define+provide (dec x) (- x 1))
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
  (call/cc
   (lambda (+fk)
     (set-amb-fail!
      (thunk
       (set-amb-fail! +prev-amb-fail)
       (+fk 'fail)))
     (+sk alt))))

(define-syntax-rule (amb alt ...)
  (let ([+prev-amb-fail amb-fail])
    (call/cc
     (lambda (+sk)
       (explore +prev-amb-fail +sk alt) ...
       (+prev-amb-fail)))))

(define-syntax module-begin
  (syntax-rules ()
    ((_ . forms)
     (#%printing-module-begin
      (module configure-runtime '#%kernel
        (print-as-expression #f)
        (print-pair-curly-braces  #t)
        (print-mpair-curly-braces #f))
      . forms))))
