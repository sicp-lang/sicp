(module sicp mzscheme
  
  ;;; PLT Picture Language
  (provide (all-from "painters.ss"))
  (require "painters.ss")
  
  ;;; STREAMS
  (provide cons-stream)
  
  
  (define-syntax cons-stream
    (syntax-rules ()
      [(cons-stream x xs)
       (cons x (delay xs))]))
  
  ;;; AMB
  (provide amb)
  
  (require (lib "defmacro.ss"))

  (define amb-fail '*)
  
  (define (initialize-amb-fail)
    (set! amb-fail
          (lambda ()
            (error "amb tree exhausted"))))
  
  (initialize-amb-fail)
  
  (define-syntax (amb stx)
    (syntax-case stx ()
      [(_ alts ...)
       #`(let ((+prev-amb-fail amb-fail))
           (call/cc
            (lambda (+sk)
              #,@(map (lambda (alt)
                        #`(call/cc
                           (lambda (+fk)
                             (set! amb-fail
                                   (lambda ()
                                     (set! amb-fail +prev-amb-fail)
                                     (+fk 'fail)))
                             (+sk #,alt))))
                      (syntax->list #'(alts ...)))
              (+prev-amb-fail))))])))
