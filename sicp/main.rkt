#lang racket

(require racket/provide
         (prefix-in r5rs: r5rs)
         (only-in r5rs [lambda r5rs:λ])
         (only-in racket/base [random racket:random]))

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
                     (define id
                       (letrec ([id expr])
                         (if (and (procedure? id)
                                  (not (eq? (object-name id) 'id)))
                             (procedure-rename id 'id)
                             id))))]))

(provide true false
         raise error
         compose compose1 identity
         empty? empty null)
(define+provide inc add1)
(define+provide dec sub1)
(define+provide 1+  add1)
(define+provide 1-  sub1)
(define+provide -1+ sub1)
(define+provide nil '())
(define+provide (runtime)
  (inexact->exact (truncate (* 1000 (current-inexact-milliseconds)))))
(define+provide (random n)
  (if (and (integer? n) (exact? n))
      (racket:random n)
      (* n (racket:random))))

(provide cons-stream stream* stream
         (rename-out
          [the-empty-stream empty-stream]
          [cons-stream      stream-cons]
          [stream-null?     stream-empty?]
          [stream-car       stream-first]
          [stream-cdr       stream-rest]))
(define-syntax cons-stream
  (syntax-rules ()
    [(_ A B) (r5rs:cons A (r5rs:delay B))]))
(define-syntax stream*
  (syntax-rules ()
    [(_ A) A]
    [(_ A B ...) (cons-stream A (stream* B ...))]))
(define-syntax stream
  (syntax-rules ()
    [(_ A ...) (stream* A ... the-empty-stream)]))
(define+provide the-empty-stream '())
(define+provide stream-null? null?)
(define+provide (stream? v)
  (or (stream-null? v)
      (and (r5rs:pair? v)
           #;(r5rs:promise? (r5rs:cdr v)))))
(define+provide stream-car r5rs:car)
(define+provide stream-cdr (compose1 r5rs:force r5rs:cdr))


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
