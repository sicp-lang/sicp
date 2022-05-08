#lang typed/racket/base

(require typed/racket/unsafe
         typed/racket/draw
         typed/racket/snip)


(require (only-in "../../sicp-pict/main.rkt"
                  ;;;
                  ;;; Syntactic Sugar
                  ;;;
                  with-transformation
                  with-frame
                  with-pen
                  with-brush
                  echo))

(define-type Vect    vect)
(define-type Frame   frame)
(define-type Trans   trans)
(define-type Segment segment)
(define-type Painter (-> Frame Any))

(require/typed/provide "../../sicp-pict/main.rkt"
  ;;;
  ;;; Vects
  ;;;
  [#:struct vect ([x : Real] [y : Real])]
  [vector-xcor  (-> Vect Real)]
  [vector-ycor  (-> Vect Real)]
  [vector-add   (-> Vect Vect Vect)]
  [vector-sub   (-> Vect Vect Vect)]
  [vector-scale (-> Real Vect Vect)]
  [zero-vector  Vect]

  ;;;
  ;;; Frames
  ;;;
  [#:struct frame ([origin : Vect] [edge1 : Vect] [edge2 : Vect])]
  [frame-coord-map     (-> Frame (-> Vect Vect))]
  [make-relative-frame (-> Vect Vect Vect (-> Frame Frame))]

  ;;;
  ;;; Transformations
  ;;;
  [#:struct trans ([xx : Real] [xy : Real] [yx : Real] [yy : Real] [x0 : Real] [y0 : Real])]
  [compose-transformation (-> Trans Trans Trans)]
  [vector->transformation (-> (Vector Real Real Real Real Real Real) Trans)]
  [transformation->vector (-> Trans (Vector Real Real Real Real Real Real))]
  [frame->transformation  (-> Frame Trans)]

  ;;;
  ;;; Segments
  ;;;
  [#:struct segment ([start : Vect] [end : Vect])]
  [vects->segments (-> (Sequenceof Vect) (Listof Segment))]

  ;;;
  ;;; COLORS, PENS, AND, BRUSHES
  ;;;
  [new-color         (case-> (->* (Real Real Real) (Real) (Instance Color%))
                             (-> (U Number String (Instance Color%)) (Instance Color%)))]
  [new-pen           (-> (U String (Instance Color%)) (Instance Pen%))]
  [new-brush         (-> (U String (Instance Color%)) (Instance Brush%))]
  [new-stipple-brush (-> (Option (Instance Bitmap%))  (Instance Brush%))]
  [black-color       (Instance Color%)]
  [white-color       (Instance Color%)]
  [black-pen         (Instance Pen%)]
  [black-brush       (Instance Brush%)]
  [transparent-brush (Instance Brush%)]

  ;;;
  ;;; Primitive Painters
  ;;;
  [paint              (-> Painter [#:width Positive-Integer] [#:height Positive-Integer] (Instance Image-Snip%))]
  [paint-hires        (-> Painter [#:width Positive-Integer] [#:height Positive-Integer] (Instance Image-Snip%))]
  [paint-hi-res       (-> Painter [#:width Positive-Integer] [#:height Positive-Integer] (Instance Image-Snip%))]
  [number->painter    (-> Byte Painter)]
  [color->painter     (-> (Instance Color%) Painter)]
  [segments->painter  (-> (Sequenceof Segment) Painter)]
  [vects->painter     (-> (Sequenceof Vect) Painter)]
  [procedure->painter (->* ((-> Real Real (U Number String (Instance Color%)))) (Real) Painter)]
  [bitmap->painter    (-> (U Path-String (Instance Bitmap%)) Painter)]
  [load-painter       (-> (U Path-String (Instance Bitmap%)) Painter)]

  ;;;
  ;;; Higher Order Painters
  ;;;
  [transform-painter (-> Painter Vect Vect Vect (-> Painter Painter))]
  [flip-horiz        (-> Painter Painter)]
  [flip-vert         (-> Painter Painter)]
  [rotate90          (-> Painter Painter)]
  [rotate180         (-> Painter Painter)]
  [rotate270         (-> Painter Painter)]
  [superpose         (-> Painter * Painter)]
  [beside            (-> Painter Painter Painter)]
  [beside3           (-> Painter Painter Painter Painter)]
  [above             (-> Painter Painter Painter)]
  [above3            (-> Painter Painter Painter Painter)]
  [below             (-> Painter Painter Painter)]

  ;;;
  ;;; Predefined Basic Painters
  ;;;
  [blank            Painter]
  [black            Painter]
  [white            Painter]
  [gray             Painter]
  [diagonal-shading Painter]
  [mark-of-zorro    Painter]
  [einstein         Painter]
  [escher           (-> Painter)])

(unsafe-require/typed/provide "../../sicp-pict/main.rkt"
  [color-object? (pred (Instance Color%))]
  [pen-object?   (pred (Instance Pen%))]
  [brush-object? (pred (Instance Brush%))]

  ;;;
  ;;;  Current Drawing Context
  ;;;
  [current-bm (Parameter (Option (Instance Bitmap%)))]
  [current-dc (Parameter (Option (Instance Bitmap-DC%)))])


(provide (all-from-out "../../sicp-pict/main.rkt")
         (all-defined-out))