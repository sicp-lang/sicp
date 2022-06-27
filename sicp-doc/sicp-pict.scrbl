#lang scribble/doc

@(require scribble/manual scribble/eval
          (for-label sicp-pict
                     racket/base
                     (only-in racket/contract
                              -> any/c and/c or/c
                              listof contract? parameter/c
                              real-in >/c <=/c natural-number/c)
                     (only-in racket/sequence sequence/c)
                     (only-in racket/class is-a?/c)
                     (only-in racket/draw
                              dc<%>
                              bitmap% bitmap-dc%
                              color% pen% brush%
                              make-bitmap)
                     (only-in racket/snip image-snip%)))

@(define the-eval (make-base-eval))
@(the-eval '(require sicp-pict))

@title{SICP Picture Language}
@defmodule*[(sicp-pict typed/sicp-pict)]

@index["painter"]
@index["geometry"]
@index["picture"]
@index["Escher"]

@section[#:tag "sicp-pict-intro"]{Introduction}

The SICP Picture Language is a small language for drawing pictures.
It shows the power of data abstraction and closure. The picture language
stems from Peter Henderson's 1982 paper ``Functional Geometry'' and was
included by Hal Abelson in ``Structure and Interpretation of Computer
Programs''.

The basic concept of the picture language is a @emph{painter}, which draws
its image (shifted and scaled) within a frame given by a parallelogram.
Painters can be combined to construct new painters.

Before using this package, read
@link["https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.4"]{section 2.2.4 of SICP},
which is an excellent introduction to the ideas of the picture language.
This manual is meant as a reference guide.

Peter Henderson has written an updated version of
@link["http://eprints.ecs.soton.ac.uk/7577/01/funcgeo2.pdf"]{Functional Geometry},
which explains how to construct the Escher fish image.

@section{Example}

Using @racket[sicp-pict] from a @tt{#lang sicp} program:

@codeblock{
  #lang sicp
  (#%require sicp-pict)
  (paint einstein)
}

Using @racket[sicp-pict] from a @tt{#lang racket} program:

@codeblock{
  #lang racket
  (require sicp-pict)
  (paint einstein)
}

From the REPL:

@schemeblock[
> (require sicp-pict)
> (paint (number->painter 0))
> (paint diagonal-shading)
> (paint (below (beside diagonal-shading
                (rotate90 diagonal-shading))
         (beside (rotate270 diagonal-shading)
                 (rotate180 diagonal-shading))))
> (paint einstein)
]

@section{Vectors}

A mathematical vector is called a @emph{vect} here, in order
to avoid confusion with the builtin vectors of Scheme.

@defproc[(vect? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a vect, @racket[#f] otherwise.
}

@defproc[(make-vect [x real?] [y real?]) vect?]{
Constructs a vect with the given coordinates.
}

@defproc[(vector-xcor [v vect?]) real?]{
Returns the x-coordinate.
}

@defproc[(vector-ycor [v vect?]) real?]{
Returns the y-coordinate.
}

@defproc[(vector-add [v vect?] [w vect?]) vect?]{
Adds the two vects by adding their coordinates pairwise.
}

@defproc[(vector-sub [v vect?] [w vect?]) vect?]{
Subtracts the two vects by subtracting their coordinates pairwise.
}

@defproc[(vector-scale [s real?] [v vect?]) vect?]{
Scales the vect by multiplying each coordinate of @racket[v] with
the number @racket[s].
}

@defthing[zero-vector vect?]{
An alias for @racket[(make-vect 0. 0.)]
}

@section{Frames}

A @emph{frame} is descibed by three vectors.
@verbatim{
      ^
      | frame edge2 vector
      |
     _|__________>
     /|         frame edge1 vector
    /
   /
  / frame origin pointer
}

@defproc[(frame? [f any/c]) boolean?]{
  Returns @racket[#t] is @racket[f] is a frame, @racket[#f] otherwise.
}

@defproc[(make-frame [origin vect?] [edge1 vect?] [edge2 vect?]) frame?]{
  Constructs a frame from a frame origin vector and two frame edge vectors.
}

@deftogether[(@defproc[(frame-origin [f frame?]) vect?]
              @defproc[(frame-edge1 [f frame?]) vect?]
              @defproc[(frame-edge2 [f frame?]) vect?])]{
  Extracts the origin, first edge or second edge from a frame.
}

@defproc[(make-relative-frame [origin vect?]
                              [corner1 vect?]
                              [corner2 vect?]) (frame? . -> . frame?)]{
  The function @scheme[make-relative-frame] provides a convenient way to
  transform frames. Given a frame and three points: @racket[origin],
  @racket[corner1], and @racket[corner2] (expressed in frame coordinates),
  it returns a new frame with those corners.
}

@defproc[(frame-coord-map [f frame?]) (vect? . -> . vect?)]{
  Each frame determines a system of ``frame coordinates'' @math{(x,y)} where
  @math{(0,0)} is the origin of the frame, @math{x} represents the displacement
  along the first edge (as a fraction of the length of the edge) and
  @math{y} is the displacement along the second edge.

  The frame coordinate map is returned by @racket[frame-coord-map]. E.g.
  these expression return the same value:

  @itemlist[
    @item{@scheme[((frame-coord-map a-frame) (make-vect 0 0))]}
    @item{@scheme[(frame-origin a-frame)]}
  ]
}

@section{Segments}

A pair of vects determines a @emph{directed line segment}
(or simply a @emph{segment}) which runs from the endpoint of
the first vect to the endpoint of the second vect.

@defproc[(segment? [s any/c]) boolean?]{
  Returns @racket[#t] if @racket[s] is a segment, @racket[#f] otherwise.
}

@defproc[(make-segment [from vect?] [to vect?]) segment?]{
  Constructs a segment from @racket[from] to @racket[to].
}

@deftogether[(@defproc[(segment-start [s segment?]) vect?]
              @defproc[(segment-end [s segment?]) vect?])]{
  Returns the start and the end of a segment @racket[s] respectively.
}

@defproc[(vects->segments [lov (sequence/c vect?)]) (listof segment?)]{
  Partitions consecutive vect in @racket[lov] into chunks of size 2 and
  returns a list of segments where each segment is constructed by each chunk.
  If @racket[lov]'s length is odd, the last element will be discarded.

  @examples[#:eval the-eval
  (vects->segments (list (make-vect 1 2) (make-vect 3 4) (make-vect 5 6) (make-vect 7 8)))]
}

@section{Colors, Pens, and Brushes}
@defproc[(color-object? [v any/c]) boolean?]{
  Defined as @racket[(is-a?/c color%)].
}

@defproc[(pen-object? [v any/c]) boolean?]{
  Defined as @racket[(is-a?/c pen%)].
}

@defproc[(brush-object? [v any/c]) boolean?]{
  Defined as @racket[(is-a?/c brush%)].
}

@defproc*[([(new-color [color (or/c real? string? color-object?)]) color-object?]
           [(new-color [r real?] [g real?] [b real?] [a (real-in 0 1) 1.0]) color-object?])]{
  Create a color.
}

@defproc[(new-pen [color (or/c string? color-object?)]) pen-object?]{
  Create a pen to draw lines and outlines.
}

@defproc[(new-brush [color (or/c string? color-object?)]) brush-object?]{
  Create a brush to fill in areas.
}

@defproc[(new-stipple-brush [bm (or/c #f (is-a?/c bitmap%))]) brush-object?]{
  Create a brush to fill in area with bitmap.
}

@defthing[black-color color-object?]{
  Defined as @racket[(new-color "black")].
}

@defthing[white-color color-object?]{
  Defined as @racket[(new-color "white")].
}

@defthing[black-pen pen-object?]{
  Defined as @racket[(new-pen "black")].
}

@defthing[black-brush brush-object?]{
  Defined as @racket[(new-brush "black")].
}

@defthing[transparent-brush brush-object?]{
  Defined as @racket[(new-brush "transparent")].
}

@section{Syntactic Sugar}
@defform[(echo painter-expr)]{
  For debugging: print the @racket[paint-expr] then paint.
  This makes it easy to see the expression that was used to produce an image.
}

@defform[(with-transformation transformation-expr body ...)]{
  @itemlist[
    @item{Store the initial-matrix of the drawing context given by @racket[current-dc].}
    @item{Install @racket[transformation-expr] as the initial-matrix.}
    @item{Evaluate @racket[body ...].}
    @item{Restore the saved initial-matrix.}
  ]

  See also @method[dc<%> get-initial-matrix].
}

@defform[(with-frame frame-expr body ...)]{
  Evaluate @racket[body ...] while the initial-matrix of
  the drawing context @racket[current-dc] is given by the transformation
  corresponding to @racket[frame-expr].
}

@defform[(with-pen pen-expr body ...)]{
  Evaluate @racket[body ...] while @racket[pen-expr] is installed
  in the drawing context given by @racket[current-dc].
}

@defform[(with-brush brush-expr body ...)]{
  Evaluate @racket[body ...] while @racket[brush-expr] is installed
  in the drawing context given by @racket[current-dc].
}

@section{Primitive Painters}

Painters take a frame and draw an image, transformed to fit inside the frame.

Note that our implementation doesn't have a concept of @emph{picture}s, so
@racket[picture->painter] which is commonly found in other implementations
doesn't exist in our implementation. If you wish to load an image file,
use @racket[bitmap->painter].

@defthing[painter/c contract?]{
A contract that recognizes a painter. This is the same as @racket[(-> frame? any/c)].
}

@defproc[(number->painter [color (and/c natural-number/c (<=/c 255))]) painter/c]{
Constructs a painter that fills the frame with a gray color indicated
by the number. 0 is black and 255 is white.
}

@defproc[(color->painter [color (is-a?/c color%)]) painter/c]{
Constructs a painter that fills the frame with the given color.
}

@defproc[(segments->painter [los (sequence/c segment?)]) painter/c]{
Constructs a painter that draws a stick figure given by the
segments (w.r.t. the unit square).}

@defproc[(vects->painter [los (sequence/c vect?)]) painter/c]{
Constructs a painter that draws a stick figure given by the
vects (w.r.t. the unit square).}

@defthing[painter-procedure/c contract?]{
A contract that recognizes a procedure which can be used to create painter.
}

@defproc[(procedure->painter [f painter-procedure/c]
                             [size real? 100]) painter/c]{

Creates painters from procedures. We assume that the procedure
@racket[f] is defined on the unit square of size @racket[size].

Then to plot a point p in the target frame, we find the inverse image
T^-1(p) of p under the transformation that maps the unit square to the
target, and find the value of @racket[f] at T-1(p).
}

@;{
@defproc[(picture->painter [p picture?]) painter/c]{
The picture @racket[p] is defined on some frame.

Given a point @racket[p] in the target frame, we compute T^-1(p) where T
is the transformation that takes the picture frame to the
target frame, and find the picture value at the closest
integer point.
}
}

@deftogether[(@defproc[(bitmap->painter [bm (or/c path-string? (is-a?/c bitmap%))])
                        painter/c]
              @defproc[(load-painter [bm (or/c path-string? (is-a?/c bitmap%))])
                        painter/c])]{
Uses an image given by @racket[bm] (either a path to the image or a bitmap object)
to create a painter.}

@section{Higher Order Painters}

@defproc[(transform-painter [p painter/c]
                            [origin vect?]
                            [corner1 vect?]
                            [corner2 vect?]) (painter/c . -> . painter/c)]{
Returns a function that takes a painter as argument and returns
a painter that is just like the original painter but is on
the transformed frame characterized by @racket[origin], @racket[corner1],
and @racket[corner2].
}

@defproc[(flip-horiz [p painter/c]) painter/c]{
Returns a painter that flips the image horizontally.}

@defproc[(flip-vert [p painter/c]) painter/c]{
Returns a painter that flips the image vertically.}

@deftogether[(@defproc[(rotate90 [p painter/c]) painter/c]
              @defproc[(rotate180 [p painter/c]) painter/c]
              @defproc[(rotate270 [p painter/c]) painter/c])]{
Returns a painter that rotates the image.}

@defproc[(beside [p1 painter/c] [p2 painter/c]) painter/c]{
Constructs a painter that paints the images side-by-side.}

@defproc[(beside3 [p1 painter/c] [p2 painter/c] [p3 painter/c]) painter/c]{
Constructs a painter that paints the images one beside the other.}

@defproc[(above [p1 painter/c] [p2 painter/c]) painter/c]{
Constructs a painter that paints the first image above the second.}

@defproc[(above3 [p1 painter/c] [p2 painter/c] [p3 painter/c]) painter/c]{
Constructs a painter that paints the images one above the other.}

@defproc[(below [p1 painter/c] [p2 painter/c]) painter/c]{
Constructs a painter that paints the first image below the second.}

@defproc[(superpose [p painter/c] ...) painter/c]{
Constructs a painter that paints all images on top of each other.}

@section{Simple Built-In Painters}

The following painter values are built-in:

@defthing[blank painter/c]{
  Draws nothing.
}

@deftogether[(@defthing[black painter/c]
              @defthing[white painter/c]
              @defthing[gray painter/c])]{
  Fills the frame with black (0), white (255) or gray (150).
}

@defthing[diagonal-shading painter/c]{
  Fills the frame with a shades of gray. The color transition
  goes from black in the upper left corner is black, to gray
  in the bottom right corner.
}

@defthing[mark-of-zorro painter/c]{
  Draws the Mark of Zorro.
}

@defthing[einstein painter/c]{
  Draws an image of Einstein.
}

@defproc[(escher) painter/c]{
  Draws Escher's @link["https://www.wikiart.org/en/m-c-escher/square-limit"]{Square Limit}.
}

@section{Painting}

Painting turns a painter into an @emph{image snip} which can be displayed in DrRacket automatically.

@deftogether[(@defthing[current-bm (parameter/c (or/c #f (is-a?/c bitmap%)))]
              @defthing[current-dc (parameter/c (or/c #f (is-a?/c bitmap-dc%)))])]{
  A painter needs to paint on something.
  We will use a parameter  @racket[current-dc]  to hold the drawing context
  of ``what is currently being drawn to''.
  In practice this will hold the drawing context (@racket[current-dc])
  for a bitmap (@racket[current-bm]).
}

@defproc[(paint [p painter/c]
                [alpha? any/c #t]
                [#:width width exact-positive-integer? 200]
                [#:height height exact-positive-integer? 200]
                [#:backing-scale backing-scale (>/c 0.0) 1.0])
         (is-a?/c image-snip%)]{
  Returns an image snip that contains the painter's image with
  the specified @racket[alpha?], @racket[width], @racket[height],
  and @racket[backing-scale].

  See also @racket[make-bitmap].
}

@deftogether[(@defproc[(paint-hi-res [p painter/c]
                                     [alpha? any/c #t]
                                     [#:width width exact-positive-integer? 200]
                                     [#:height height exact-positive-integer? 200]
                                     [#:backing-scale backing-scale (>/c 0.0) 1.0])
                       (is-a?/c image-snip%)]
              @defproc[(paint-hires [p painter/c]
                                    [alpha? any/c #t]
                                    [#:width width exact-positive-integer? 200]
                                    [#:height height exact-positive-integer? 200]
                                    [#:backing-scale backing-scale (>/c 0.0) 1.0])
                       (is-a?/c image-snip%)])]{
  Aliases of @racket[paint]. They are provided for compatibility with old texts.
}
