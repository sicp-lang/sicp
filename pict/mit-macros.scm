(define-syntax access
  (syntax-rules (floating-vector-ref floating-vector-set!)
    ((access floating-vector-ref environment) vector-ref)
    ((acccess floating-vector-set! environment) vector-set!)
    ((access name environment) name)))
