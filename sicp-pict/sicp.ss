(module sicp mzscheme

  ;;; PLT Picture Language
  (provide (all-from "painters.ss"))
  (require "painters.ss")

  ;;; STREAMS
  (provide cons-stream)
  (provide stream-car)
  (provide stream-cdr)
  (provide the-empty-stream)
  (provide stream-null?)

  (define-syntax cons-stream
    (syntax-rules ()
      [(cons-stream x xs)
       (cons x (delay xs))]))

  (define (stream-car x)
    (car x))

  (define (stream-cdr x)
    (force (cdr x)))

  (define the-empty-stream '())

  (define (stream-null? x)
    (null? x))

  ;;; AMB
  (provide amb)

  (require (lib "defmacro.ss"))

  (define amb-fail '*)

  (define (initialize-amb-fail)
    (set! amb-fail
          (lambda ()
            (error "amb tree exhausted"))))

  (initialize-amb-fail)

  (define (set-amb-fail! x)
    (set! amb-fail x))

  (define-syntax (amb stx)
    (syntax-case stx ()
      [(_ alts ...)
       #`(let ((+prev-amb-fail amb-fail))
           (call/cc
            (lambda (+sk)
              #,@(map (lambda (alt)
                        #`(call/cc
                           (lambda (+fk)
                             (set-amb-fail!
                                   (lambda ()
                                     (set-amb-fail! +prev-amb-fail)
                                     (+fk 'fail)))
                             (+sk #,alt))))
                      (syntax->list #'(alts ...)))
              (+prev-amb-fail))))])))
