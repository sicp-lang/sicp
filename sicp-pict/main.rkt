#lang racket
;;;
;;; SICP Picture Language
;;;

; This is a new implementation of the SICP Picture Language.
; The picture language is inspired by Henderson's work.

(require (for-syntax syntax/parse)
         racket/draw
         racket/snip
         racket/runtime-path)

(define-runtime-path einstein-file "einstein2.jpg")

;;;
;;; Vectors
;;;

;; Points and vectors are represented as vect a structure
;; that holds the x- and the y-coordinate.

(provide (contract-out
          [struct vect  ([x real?] [y real?])]         ; structure
          [vector-xcor  (->   vect?        real?)]     ; access x-coordinate
          [vector-ycor  (->   vect?        real?)]     ; access y-coordinate
          [vector-add   (->   vect? vect?  vect?)]     ; add two vectors
          [vector-sub   (->   vect? vect?  vect?)]     ; subtract two vectors
          [vector-scale (->   real? vect?  vect?)]     ; scale a vector
          [zero-vector  vect?]))

(struct vect (x y)
  #:extra-constructor-name make-vect
  #:transparent)

(define vector-xcor vect-x)
(define vector-ycor vect-y)

(define (vector-add v w)
  (match* (v w) [((vect vx vy) (vect wx wy)) (vect (+ vx wx) (+ vy wy))]))

(define (vector-sub v w)
  (match* (v w) [((vect vx vy) (vect wx wy)) (vect (- vx wx) (- vy wy))]))

(define (vector-scale s v)
  (match v [(vect vx vy) (vect (* s vx) (* s vy))]))

(define zero-vector (vect 0. 0.))

;;;
;;; Frames
;;;

(provide (contract-out
          [struct frame ([origin vect?] [edge1 vect?] [edge2 vect?])]       ; structure
          [frame-coord-map     (-> frame?             (-> vect? vect?))]    
          [make-relative-frame (-> vect? vect? vect?  (-> frame? frame?))]))

(struct frame (origin edge1 edge2)
  #:extra-constructor-name make-frame
  #:transparent)

; frame-coord-map : frame -> (vect -> vect)
;   Given a frame whose coordinates are given in a coordinate system S,
;   return a procedure that maps coordinates in the frame coordinates to coordinates in S
(define (frame-coord-map a-frame)
  (lambda (point-in-frame-coords)
    (match* (a-frame point-in-frame-coords)
      [((frame origin edge1 edge2) (vect x y))
       (vector-add origin
                   (vector-add (vector-scale x edge1)
                               (vector-scale y edge2)))]
      [(_ _) (raise-type-error 'frame-coord-map "got" (list a-frame point-in-frame-coords))])))

; make-relative-frame : vect vect vect -> (frame -> frame)
(define (make-relative-frame origin corner1 corner2)
  (λ (frame)
    (define m (frame-coord-map frame))
    (define new-origin (m origin))
    (make-frame new-origin
                (vector-sub (m corner1) new-origin)
                (vector-sub (m corner2) new-origin))))

;;;
;;; Transformations
;;;

;; Affine transformations are represented by
;;     (struct trans (xx xy yx yy x0 y0) ...))
;; The point (x,y) is transformed to:
;;     xnew = xx*x + xy*y + x0
;;     ynew = yx*x + yy*y + y0
;; Think of an affine transformation as a linear transformation followed by a translation.

;; Note: The initial matrix has the same order: xx xy yx yy x0 y0
;;       So we could just keep the vector returned from the drawing context by get-initial-matrix

(provide (contract-out
          [struct trans ([xx real?] [xy real?] [yx real?] [yy real?] [x0 real?] [y0 real?])]
          [compose-transformation (-> trans? trans? trans?)]
          [vector->transformation (-> vector? trans?)]
          [transformation->vector (-> trans? vector?)]
          [frame->transformation  (-> frame? trans?)]))

(struct trans (xx xy yx yy x0 y0)
  #:transparent)

(define (compose-transformation t1 t2)
  ; ((compose-trans t1 t2) v) = (t1 (t2 v))  
  ; Use t2 to transform (x0,y0) into (x1,y1)
  ;   x1 = g x0 + h y0 + k
  ;   y1 = i x0 + j y0 + l
  ; Use t1 to transform (x1,y1) into (x2,y2)
  ;   x2 = a x1 + b y1 + e
  ;   y2 = c x1 + d y1 + f
  ; The composed transformation is (computed by a CAS):
  ;   x2 = (a g + b i) x0 + (a h + b j) y0 + ak + bl + e
  ;   y2 = (c g + d i) x0 + (c h + d j) y0 + ck + dl + f    
  (match-define (trans a b c d e f) t1)
  (match-define (trans g h i j k l) t2)  
  (trans (+ (* a g) (* b i))   (+ (* a h) (* b j)) 
         (+ (* c g) (* d i))   (+ (* c h) (* d j))
         (+ (* a k) (* b l) e) (+ (* c k) (* d l) f)))

(define (vector->transformation v)
  (match v [(vector a b c d e f) (trans  a c b d e f)]))

(define (transformation->vector t)
  (match t [(trans  a b c d e f) (vector a c b d e f)]))

; frame->transformation : frame -> tranformation
;   return the transformation that converts coordinates in
;   system given by the frame into the coordinate system
;   in which the coordinates of the origin and edges of
;   the frame are given.

(define (frame->transformation f)
  (match f
    [(frame (vect ox oy) (vect e1x e1y) (vect e2x e2y))
     (trans e1x e2x e1y e2y ox oy)]))


;;;
;;; Segments
;;;

(provide (contract-out
          [struct segment ([start vect?] [end vect?])]
          [vects->segments (-> (sequence/c vect?) (listof segment?))]))

; A segment represents a line segment from start point to end point.
; The start and end points are represented as vects.

(struct segment (start end)
  #:extra-constructor-name make-segment
  #:transparent)

; vects->segments : sequence-of-vect -> list-of-segment
(define (vects->segments vects)
  (for/list ([v vects] [w (sequence-tail vects 1)])
    (segment v w)))

;;;
;;; COLORS, PENS, AND, BRUSHES
;;;

(provide (contract-out
          [color-object?      (-> any/c boolean?)]
          [pen-object?        (-> any/c boolean?)]
          [brush-object?      (-> any/c boolean?)]
          [new-color          any/c]
          #;[new-color          (or/c (-> real? real? real?                    color-object?)
                                      (-> (or/c number? string? color-object?) color-object?))]
          [new-pen            (-> any/c pen-object?)]
          [new-brush          (-> any/c brush-object?)]
          [new-stipple-brush  (-> any/c brush-object?)]
          [black-color        color-object?]
          [white-color        color-object?]
          [black-pen          pen-object?]
          [black-brush        brush-object?]
          [transparent-brush  brush-object?]))

(define (color-object? o) (and (object? o) (is-a? o color%)))
(define (pen-object? o)   (and (object? o) (is-a? o pen%)))
(define (brush-object? o) (and (object? o) (is-a? o brush%)))

(define new-color
  (let () ; make a cache of colors in order to reuse them
    (define colors (make-hash))
    (λ ns
      (hash-ref! colors ns
                 (λ ()
                   (match ns
                     [(list (? number? n))        (let ([n (inexact->exact (floor n))])
                                                    (make-object color% n n n))]
                     [(list r g b)                (let ([r (inexact->exact (floor r))]
                                                        [g (inexact->exact (floor g))]
                                                        [b (inexact->exact (floor b))])
                                                    (make-object color% r g b))]
                     [(list (? string? s))        (make-object color% s)]                     
                     [(list (? color-object? c))  c]
                     [_ (error 'new-color)]))))))

(define new-pen ; draws lines and outlines
  (let () 
    (define pens (make-hash))  ; make a cache of pens in order to reuse them
    (λ (color) (hash-ref! pens color
                          (λ () ; a pen of width 0 means "as thin as possible"
                            (new pen%
                                 [color color]
                                 [width 0]
                                 [style 'solid]
                                 [cap   'butt]
                                 [stipple #f]))))))

(define new-brush ; fill in areas
  (let () (define brushes (make-hash))
    (λ (color) (hash-ref! brushes color
                          (λ ()
                            (new brush%
                                 [color color]
                                 [style 'solid]))))))

(define new-stipple-brush ; fill in area with bitmap
  (let () (define brushes (make-hash))
    (λ (bm) (hash-ref! brushes bm
                       (λ () (new brush% [style 'solid] [stipple bm]))))))

;; Useful pens and brushes
(define black-color       (new-color "black"))
(define white-color       (new-color "white"))
(define black-pen         (new-pen   "black"))
(define black-brush       (new-brush "black"))
(define transparent-brush (new-brush "transparent"))

;;;
;;;  Current Drawing Context
;;;

; A painter needs to paint on something.
; We will use a parameter  current-dc  to hold the drawing context
; of "what is currently being drawn to".
; In practice this will hold the a drawing context for a bitmap.

(define current-bm (make-parameter #f))
(define current-dc (make-parameter #f))

(define painter/c (-> frame? any/c))

; To get a painting from a painter, we need to create a new
; bitmap into which the painter can draw.
(define (paint painter #:width [width 200] #:height [height 200])
  (define-values (bm dc) (make-painter-bitmap width height))
  (parameterize ([current-bm bm]
                 [current-dc dc])
    (send dc scale 0.99 0.99) ; make the entire unit square visible
    (painter (frame (vect 0. 0.) (vect 1. 0.) (vect 0. 1.)))
    (make-object image-snip% bm)))

; For compatibility with old texts.
(define paint-hi-res paint)
(define paint-hires  paint)

; Painters assume the image as coordinates (0,0) in the
; lower left corner and (1,1) in the upper right corner.
; We therefore need to set the initial transformation matrix
; such that both axis are scaled and the y-axis is flipped.
; Flipping the y-axis also implies we need to translate
; the origin in the y-direction
(define (make-painter-bitmap width height)
  (define bm (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap bm]))
  (send dc set-pen black-pen)
  (send dc set-brush black-brush)
  ; (send dc set-smoothing 'smoothed)  
  (define w (* 1. width))
  (define h (* 1. height))
  ; Map unit square to screen coordinates - also flip y-axis
  ; Initial Matrix (Logical to Device coordinates)
  ;                                   xx xy yx  yy       x0 y0
  (send dc set-initial-matrix (vector w  0. 0. (* -1. h) 0. h))
  (values bm dc))


; For debugging: print the paint expression then paint.
; This makes it easy to see the expression that was used to produce an image.
(define-syntax (echo stx)
  (syntax-parse stx
    [(_ painter-expr)
     #'(begin (displayln 'painter-expr)
              (paint painter-expr))]))

;;;
;;; Syntactic Sugar
;;;

; SYNTAX  (with-transformation transformation body ...)
;   Store the initial-matrix of thed rawing context given by current-dc.
;   Install  transformation  as the initial-matrix
;   Evaluate body
;   Restore the saved initial-matrix
(define-syntax (with-transformation stx)
  (syntax-parse stx
    [(_with-transformation transformation body ...)
     (syntax/loc stx
       (let ()
         (define dc (current-dc))
         (define old-vector         (send dc get-initial-matrix))
         (define old-transformation (vector->transformation old-vector))
         (define new-transformation
           ; transform frame coordinates into input coordinates of current transform
           (compose-transformation old-transformation transformation))
         (send dc set-initial-matrix (transformation->vector new-transformation))
         ; (send dc transform (transformation->vector transformation))
         (begin0
           (begin body ...)
           (send dc set-initial-matrix old-vector))))]))

; SYNTAX  (with-frame frame body ...)
;   Evaluate body ... while the initial-matrix of the drawing context current-dc
;   is given by the transformation corresponding to frame.
(define-syntax (with-frame stx)
  (syntax-parse stx
    [(_with-frame frame #:who who body ...)
     (syntax/loc stx
       (begin
         (unless (current-dc)
           (raise-arguments-error 'who "should be called with the paint procedure without supplying a manual frame argument"))
         (with-transformation (frame->transformation frame)
           body ...)))]))

; SYNTAX  (with-pen pen body ...)
;   Evaluate body ... while pen is installed in the drawing context given by current-dc
(define-syntax (with-pen stx)
  (syntax-parse stx
    [(_with-pen pen body ...)
     (syntax/loc stx
       (let ()
         (define dc (current-dc))
         (define old-pen (send dc get-pen))
         (send dc set-pen pen)
         (begin0
           (begin body ...)
           (send dc set-pen old-pen))))]))

; SYNTAX  (with-brush brush body ...)
;   Evaluate body ... while brush is installed in the drawing context given by current-dc
(define-syntax (with-brush stx)
  (syntax-parse stx
    [(_with-brush brush body ...)
     (syntax/loc stx
       (let ()
         (define dc (current-dc))
         (define old-brush (send dc get-brush))
         (send dc set-brush brush)
         (begin0
           (begin body ...)
           (send dc set-brush old-brush))))]))
;;;
;;; Primitive Painters
;;;

(provide painter/c
         ;
         with-transformation
         with-frame
         with-pen
         with-brush
         ;
         paint
         paint-hi-res
         paint-hires
         ;

         (contract-out [number->painter (-> (and/c natural-number/c (<=/c 255)) any/c)]
                       [color->painter (-> (is-a?/c color%) painter/c)]
                       [segments->painter (-> (sequence/c segment?) any/c)]
                       [vects->painter (-> (sequence/c vect?) painter/c)]
                       [procedure->painter (-> procedure? any/c)]
                       [bitmap->painter (-> (or/c path-string?
                                                  (is-a?/c bitmap%)) any/c)]
                       [load-painter (-> (or/c path-string?
                                               (is-a?/c bitmap%)) any/c)]))

;;; Color Painter
;;;     A color painter fills the unit square with a solid color
(define (color->painter c)
  (define color (new-color c))
  (define pen   (new-pen color))
  (define brush (new-brush color))
  (λ (frame)
    (with-frame frame #:who color->painter
      (with-pen pen
        (with-brush brush
          (send (current-dc) draw-rectangle 0. 0. 1.0 1.0)))))) ; x y w h

;;; Number Painter
;;;     A number painter is a color painter that draws a gray color from 0 to 255.
(define (number->painter number-or-color)
  (define n number-or-color)
  (unless (and (number? n) (<= 0 n 255))
    (raise-type-error 'number->painter "number between 0 and 255" n))
  (color->painter (new-color n)))


;;; Segment Painter
;;;     A segment painter draws a series of line segments.

(define (segments->painter segments)
  (define pen   black-pen)
  (define brush black-brush)
  (λ (frame)
    (with-frame frame #:who segments->painter
      (with-pen pen
        (with-brush brush
          (for ([a-segment segments])
            (match-define (segment (vect x1 y1) (vect x2 y2)) a-segment)
            (send (current-dc) draw-line x1 y1 x2 y2)))))))

(define (vects->painter vects)
  (segments->painter (vects->segments vects)))

;;; Bitmap Painter
;;;     A bitmap painter draws a bitmap.
(define (bitmap->painter bitmap)
  (define (new-bm) (if (path-string? bitmap)
                       (make-object bitmap% bitmap)
                       bitmap))
  (define bm         (new-bm))
  (define bm-dc      (new bitmap-dc% [bitmap bm]))
  (define flipped-bm (new-bm))
  (define flipped-dc (new bitmap-dc% [bitmap flipped-bm]))
  (define w (* 1. (send bm get-width)))
  (define h (* 1. (send bm get-height)))
  (send flipped-dc set-initial-matrix (vector 1 0 0 -1 0 h))
  (send flipped-dc draw-bitmap bm 0 0)  
  (λ (frame)
    (with-frame frame #:who bitmap->painter
      (send (current-dc) draw-bitmap-section-smooth
            flipped-bm ; source
            0. 0.      ; dest-x dest-y
            1. 1.      ; dest-width dest-height
            0. 0.      ; src-x src-y
            w  h       ; src-width src-height
            ))))

(define load-painter bitmap->painter)

;;; Procedure Painter
(define (procedure->painter f [size 100])
  ; f : vect -> color
  (define bm (make-object bitmap% size size))
  (define dc (new bitmap-dc% [bitmap bm]))
  (define size.0 (* 1.0 size))
  (for* ([x (in-range size)] [y (in-range size)])
    (define x.0 (/ x size.0))
    (define y.0 (/ y size.0))
    (send dc set-pen (new-pen (new-color (f x.0 y.0))))
    (send dc draw-point x y))
  (λ (frame)
    (with-frame frame #:who procedure->painter
      (send (current-dc) draw-bitmap-section-smooth
            bm 0. 0. 1. 1. 0. 0. size size))))

;;;
;;; General Utility
;;;

(define (repeated f n)
  (cond
    [(= n 0) identity]
    [(= n 1) f]
    [else (compose f (repeated f (- n 1)))]))

;;;
;;; Higher Order Painters
;;;

;; See SICP for a description of these painters
(provide transform-painter
         flip-horiz flip-vert rotate90 rotate180 rotate270
         superpose beside beside3 above3 below)

(define (transform-painter painter origin corner1 corner2)
  (compose painter (make-relative-frame origin corner1 corner2)))

(define (flip-horiz p) (transform-painter p (vect 1. 0.) (vect 0. 0.) (vect 1. 1.)))
(define (flip-vert p)  (transform-painter p (vect 0. 1.) (vect 1. 1.) (vect 0. 0.)))
(define (rotate90 p)   (transform-painter p (vect 1. 0.) (vect 1 1)   (vect 0. 0.)))
(define rotate180      (repeated rotate90 2))
(define rotate270      (repeated rotate90 3))

(define (superpose . painters)
  (λ (frame)
    (for ([painter painters])
      (painter frame))))

(define (beside painter1 painter2)
  (define split-point (vect .5 0.))
  (superpose
   (transform-painter painter1 zero-vector split-point (vect 0. 1.))
   (transform-painter painter2 split-point (vect 1 0)  (vect .5 1.))))

(define (beside3 painter1 painter2 painter3)
  (define split-point1 (vect (/ 1. 3) 0.))
  (define split-point2 (vect (/ 2. 3) 0.))  
  (superpose
    (transform-painter painter1 zero-vector  split-point1  (vect    0.    1.))
    (transform-painter painter2 split-point1 split-point2  (vect (/ 1. 3) 1.))
    (transform-painter painter3 split-point2 (vect 1. 0.)  (vect (/ 2. 3) 1.))))

(define (above3 painter1 painter2 painter3)
  (define 1/3. (/ 1. 3.))
  (define 2/3. (/ 2. 3.))
  (superpose
   (transform-painter painter1 (vect 0. 2/3.) (vect 1. 2/3.) (vect 0.   1.))
   (transform-painter painter2 (vect 0. 1/3.) (vect 1. 1/3.) (vect 0. 2/3.))
   (transform-painter painter3 (vect 0. 0.)   (vect 1. 0.)   (vect 0. 1/3.))))

(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter2)
                     (rotate90 painter1))))

;;;
;;; Predefined Basic Painters
;;;
(provide black white gray diagonal-shading mark-of-zorro einstein escher)
(provide echo)

(define black            (number->painter   0))
(define white            (number->painter 255))
(define gray             (number->painter 150))
(define diagonal-shading (procedure->painter (λ (x y) (* 100 (+ x y)))))
(define mark-of-zorro    (vects->painter (list (vect .1 .9) (vect .8 .9) (vect .1 .2) (vect .9 .3))))
(define einstein         (bitmap->painter einstein-file))

;;; Escher Example

; Henderson's papers:
;         http://users.ecs.soton.ac.uk/ph/funcgeo.pdf
;         http://eprints.soton.ac.uk/257577/1/funcgeo2.pdf
; Blog:   https://goo.gl/18L938

(define (grid w h segs)
  (define (Vect x y) (vect (/ x (* 1.0 w)) (/ y (* 1.0 h))))
  (define (->segment l) (match l [(list    (list x1 y1) (list x2 y2))
                                  (segment (Vect x1 y1) (Vect x2 y2))]))
  (segments->painter (map ->segment segs)))

(define P (grid 16 16
                 '[(( 4  4) ( 6  0)) (( 0  3) ( 3  4)) (( 3  4) ( 0  8))
                   (( 0  8) ( 0  3)) (( 4  5) ( 7  6)) (( 7  6) ( 4 10))
                   (( 4 10) ( 4  5)) ((11  0) (10  4)) ((10  4) ( 8  8))
                   (( 8  8) ( 4 13)) (( 4 13) ( 0 16)) ((11  0) (14  2))
                   ((14  2) (16  2)) ((10  4) (13  5)) ((13  5) (16  4))
                   (( 9  6) (12  7)) ((12  7) (16  6)) (( 8  8) (12  9))
                   ((12  9) (16  8)) (( 8 12) (16 10)) (( 0 16) ( 6 15))
                   (( 6 15) ( 8 16)) (( 8 16) (12 12)) ((12 12) (16 12))
                   ((10 16) (12 14)) ((12 14) (16 13)) ((12 16) (13 15))
                   ((13 15) (16 14)) ((14 16) (16 15)) ((16  0) (16  8))
                   ((16 12) (16 16))]))
(define Q (grid 16 16
                 '[(( 2  0) ( 4  5)) (( 4  5) ( 4  7)) (( 4  0) ( 6  5))
                   (( 6  5) ( 6  7)) (( 6  0) ( 8  5)) (( 8  5) ( 8  8))
                   (( 8  0) (10  6)) ((10  6) (10  9)) ((10  0) (14 11))
                   ((12  0) (13  4)) ((13  4) (16  8)) ((16  8) (15 10))
                   ((15 10) (16 16)) ((16 16) (12 10)) ((12 10) ( 6  7))
                   (( 6  7) ( 4  7)) (( 4  7) ( 0  8)) ((13  0) (16  6))
                   ((14  0) (16  4)) ((15  0) (16  2)) (( 0 10) ( 7 11))
                   (( 9 12) (10 10)) ((10 10) (12 12)) ((12 12) ( 9 12))
                   (( 8 15) ( 9 13)) (( 9 13) (11 15)) ((11 15) ( 8 15))
                   (( 0 12) ( 3 13)) (( 3 13) ( 7 15)) (( 7 15) ( 8 16))
                   (( 2 16) ( 3 13)) (( 4 16) ( 5 14)) (( 6 16) ( 7 15))
                   (( 0  0) ( 8  0)) ((12  0) (16  0))]))

(define R (grid 16 16
                 '[(( 0 12) ( 1 14)) (( 0  8) ( 2 12)) (( 0  4) ( 5 10))
                   (( 0  0) ( 8  8)) (( 1  1) ( 4  0)) (( 2  2) ( 8  0))
                   (( 3  3) ( 8  2)) (( 8  2) (12  0)) (( 5  5) (12  3))
                   ((12  3) (16  0)) (( 0 16) ( 2 12)) (( 2 12) ( 8  8))
                   (( 8  8) (14  6)) ((14  6) (16  4)) (( 6 16) (11 10))
                   ((11 10) (16  6)) ((11 16) (12 12)) ((12 12) (16  8))
                   ((12 12) (16 16)) ((13 13) (16 10)) ((14 14) (16 12))
                   ((15 15) (16 14))]))

(define S (grid 16 16
                 '[(( 0  0) ( 4  2)) (( 4  2) ( 8  2)) (( 8  2) (16  0))
                   (( 0  4) ( 2  1)) (( 0  6) ( 7  4)) (( 0  8) ( 8  6))
                   (( 0 10) ( 7  8)) (( 0 12) ( 7 10)) (( 0 14) ( 7 13))
                   (( 8 16) ( 7 13)) (( 7 13) ( 7  8)) (( 7  8) ( 8  6))
                   (( 8  6) (10  4)) ((10  4) (16  0)) ((10 16) (11 10))
                   ((10  6) (12  4)) ((12  4) (12  7)) ((12  7) (10  6))
                   ((13  7) (15  5)) ((15  5) (15  8)) ((15  8) (13  7))
                   ((12 16) (13 13)) ((13 13) (15  9)) ((15  9) (16  8))
                   ((13 13) (16 14)) ((14 11) (16 12)) ((15  9) (16 10))]))


(define (escher)
  ; combinators
  (define (above p1 p2)
    (below p2
           p1))
  (define (quartet p1 p2 p3 p4)
    (above (beside p1 p2)
           (beside p3 p4)))
  (define (nonet p1 p2 p3 p4 p5 p6 p7 p8 p9)
    (above3 (beside3 p1 p2 p3)
            (beside3 p4 p5 p6)
            (beside3 p7 p8 p9)))
  (define (cycle p1)
    (quartet      p1  (rot (rot (rot p1)))
             (rot p1)      (rot (rot p1))))
  (define rot     rotate90)
  (define b       white) ; blank
  (define-values (p q r s) (values P Q R S))
  (define t       (quartet p q r s))
  (define side1   (quartet b b (rot t) t))
  (define side2   (quartet side1 side1 (rot t) t))
  (define u       (cycle (rot q)))
  (define corner1 (quartet b b b u))
  (define corner2 (quartet corner1 side1 (rot side1) u))  
  (define corner  (nonet   corner2     side2    side2
                           (rot side2)     u     (rot t)
                           (rot side2)  (rot t)    q))
  (define square-limit (cycle corner))
  square-limit)

;(echo (escher))
