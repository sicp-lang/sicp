#lang info

(define collection 'multi)
(define version    "1")
(define deps '("draw-lib"
               "snip-lib"
               "mzscheme"
               "base"
               "compatibility-lib"
               "gui-lib"
               "scheme-lib"
               "r5rs-lib"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "r5rs-doc"
                     "at-exp-lib"
                     "mzscheme"
                     "scribble-lib"
                     "racket-doc"))
(define compile-omit-paths '("sicp-pict-old"))
(define test-omit-paths '("sicp-pict-old"))
