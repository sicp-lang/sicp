#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))

; TEST BITMAP PAINTERS
(paint einstein)
(display "should be: image of einstein\n")

(paint (rotate90 einstein))
(display "should be: image of einstein rotate 90 degrees counter clockwise\n")


; TEST SEGMENTS PAINTERS
(paint (segments->painter 
        (list (make-segment (make-vect 0.0 0.0) 
                            (make-vect 0.5 0.5))))) 
(display "should be: image with segment from lower left to the center\n")

(paint (rotate90 
        (segments->painter 
         (list (make-segment (make-vect 0.0 0.0) 
                             (make-vect 0.5 0.5))))))
(display "should be: image with segment from lower right to the center\n")
 
; TEST NUMBER PAINTERS
(paint (number->painter 0))
(display "should be: black square\n")
(paint (number->painter 128))
(display "should be: grey square\n")
(paint (number->painter 255))
(display "should be: white square\n")

; TEST PROCEDURE PAINTERS
(paint (procedure->painter (lambda (x y) (* 255 x y))))
(display "should be: shaded image, where the lower left is black and the upper right white\n")