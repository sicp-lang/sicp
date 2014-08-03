#lang setup/infotab

(define name "SICP")
(define blurb '("Support for the picture language used in SICP. The primtives cons-stream and amb are also provided."))
(define primary-file "sicp.ss")
; (define doc.txt "doc.txt")
(define categories '(media))
(define scribblings '(("sicp-manual.scrbl" ())))
(define repositories '("4.x"))
(define release-notes '("Fixed orientation related bug (aka the rotate90 bug)."))

(define compile-omit-files
  '("einstein.gif"
    "einstein.pgm"
    "example.scm"
    "hend.scm"
    "hutils.scm"
    "mit-support.scm"
    "prmpnt.scm"
    "psgo.scm"
    "primitives.scm"
    "mit-macros.scm"))
