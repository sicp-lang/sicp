(define false #f)

;; flo:+
;; fix:+ fix:> fix:>= fix:<=

(define flo:+ +)
(define fix:+ +)
(define fix:> >)
(define fix:>= >=)
(define fix:<= <=)

(define (round->exact n)
  (inexact->exact (round n)))
(define (floor->exact n)
  (inexact->exact (floor n)))
(define (ceiling->exact n)
  (inexact->exact (floor n)))

(define (graphics-device? window)
  #t)					; yeah, yeah ...

(define (picture-display window picture dummy-1 dummy-2)
  (picture->snip picture))
