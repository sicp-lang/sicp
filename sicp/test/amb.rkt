#lang sicp

(#%require rackunit)

(check-equal? (let ([x (amb 0 1 2)])
                (cond
                  [(= x 0) (amb)]
                  [(= x 1) -1]
                  [else -2])) -1)

(check-equal? (amb (amb) 42 1337) 42)
