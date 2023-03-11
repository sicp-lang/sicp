#lang info

(define collection 'multi)
(define version    "1.0")
(define test-omit-paths '(#px"^((?!/test/).)*$"))
(define deps '("base"
               "draw-lib"
               "r5rs-lib"
               "rackunit-lib"
               "snip-lib"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "r5rs-doc"
                     "racket-doc"
                     "scribble-lib"))
