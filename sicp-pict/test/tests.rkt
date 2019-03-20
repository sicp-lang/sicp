#lang racket

(require sicp-pict
         rackunit)

(define (get-pixels painter)
  (define obj (send (paint painter) get-bitmap))
  (define width (send obj get-width))
  (define height (send obj get-height))
  (define out (make-bytes (* width height 4)))
  (send obj get-argb-pixels 0 0 width height out)
  out)

(define rng '(10000 10100))

(check-equal? (apply subbytes (get-pixels einstein) rng)
              (apply subbytes (get-pixels (flip-horiz (flip-horiz einstein))) rng))

(check-equal? (apply subbytes (get-pixels einstein) rng)
              (apply subbytes (get-pixels (flip-vert (flip-vert einstein))) rng))

(check-not-equal? (apply subbytes (get-pixels einstein) rng)
                  (apply subbytes (get-pixels (flip-horiz einstein)) rng))

(check-not-equal? (apply subbytes (get-pixels einstein) rng)
                  (apply subbytes (get-pixels (flip-vert einstein)) rng))
