#lang scribble/doc

@(require scribble/manual scribble/eval
          (for-label scheme
                     sicp-pict
                     (only-in racket/contract and/c <=/c)))

@title{SICP Collections}
@defmodule[sicp]
@defmodule[sicp-pict]
@index["SICP"]{}
@index["sicp"]{}
@index["painter"]{}
@index["geometry"]{}
@index["picture"]{}
@index["Escher"]{}

@section{Introduction}

This package contains two collections.

The @racket[sicp] collection contains a @tt{#lang sicp} language ideal
for studying the book "Structure and Interpretation of Computer Programs"
by Gerald Jay Sussman and Hal Abelson. The book is usually referred
to simply as SICP. The non-standard primitives @scheme[cons-stream] is
also provided.

The second @racket[sicp-pict] collection contains the picture language used in SICP.

@section{Installation}
Use DrRacket to install the sicp package like this:

@itemlist[
@item{Open the Package Manager:
      in DrRacket choose the menu "File" then choose "Package Manager...".}
@item{In the tab "Do What I Mean" find the text field and enter:  @racket{sicp}}
@item{Finally click the "Install" button.}
@item{Test it. Make sure DrRacket has "Determine language from source" in the bottom left corner.
      Write the following program and click run:

@verbatim{#lang sicp
          (inc 42)}

The expected output is @racket[43].}

          #:style 'ordered]
          


@section{Introduction to the @tt{#lang sicp} language}

The programs in the book are written in (a subset of) the programming language Scheme.
As the years have passed the programming language Scheme has evolved.
The language @tt{#lang sicp} provides you with a version of R5RS (the fifth revision of Scheme)
changed slightly in order for programs in SICP to run as is.

The changes are as follows:

The identifers @racket[true], @racket[false], and, @racket[nil] are provided
with values @racket[#t], @racket[#f], and, @racket['()] respectively.

The following functions of one variable are provided:
@verbatim{
    (define (identity x) x)         the identity function
    (define (inc x) (+ x 1))        increment 
    (define (dec x) (- x 1))        decrement}

There are no streams in R5RS, so the sicp language provides the
primitives @racket[cons-stream] and @racket[stream-null?] that
respectively constructs a stream and tests whether a stream is the null stream.
The null stream is provided as @racket[the-empty-stream].

Finally the function @racket[runtime] is provided. It gives you the current
time measured as the number of seconds passed since a fixed beginning.

To use the @tt{sicp} language simply use @tt{#lang sicp} as the
first line of your program. If you need to use Racket libraries,
then use @racket[#%require] (Note: R5RS has no @racket[require] so
in order not to break any programs using the name @racket[require] to
other things, the name @racket[#%require] is used instead.





@section{Introduction to the SICP Picture Language}

The SICP Picture Language is a small language for drawing pictures.
It shows the power of data abstraction and closure. The picture language
stems from Peter Henderson's 1982 paper "Functional Geometry" and was
included by Hal Abelson in "Structure and Interpretation of Computer 
Programs".  

Before using this package, read 
@link["https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.4" 
      "section 2.2.4 of SICP"],
which is an excellent introduction to the ideas of the picture language.
This manual is meant as a reference guide.

Peter Henderson has written an updated version of 
@link["http://eprints.ecs.soton.ac.uk/7577/01/funcgeo2.pdf" "\"Functional Geometry\""],
which explains how to construct the Escher fish image.

Note: The primitives @scheme[cons-stream] and @scheme[amb] needed 
in other chapters of SICP are also provided.

@section{Reference}

The basic concept of the picture language is a @emph["painter"]. A painter draws 
it's image (shifted and scaled) within a frame given by a parallelogram.
Painters can be combined to construct new painters.

@section{Example}

Using @racket[sicp-pict] from a @racket{#lang sicp} program:

@verbatim{
          #lang sicp
          (#%require sicp-pict)
          (paint einstein)}


Using @racket[sicp-pict] from a @racket{#lang racket} program:

@verbatim{
          #lang racket
          (require sicp-pict)
          (paint einstein)}

From the REPL:

@schemeblock[
> (require sicp-pict)
> (paint (number->painter 0))
> (paint diagonal-shading)
> (paint-hires  (below (beside diagonal-shading 
                        (rotate90 diagonal-shading))
                (beside (rotate270 diagonal-shading)
                        (rotate180 diagonal-shading))))
> (paint einstein)
]

@section{Vectors}

A mathematical vector is called a @emph["vect"] here, in order
to avoid confusion with the builtin vectors of Scheme.

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
Scales the vect by multiplying each coordinate of @scheme{v} with 
the number @scheme{s}.
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

@defproc[(make-frame [origin vect?] [edge1 vect?] [edge2 vect]) frame?]{
Constructs a frame from a frame origin vector and two frame edge vectors.
}

@defproc[(frame-origin [f frame?]) vect?]{}
@defproc[(frame-edge1 [f frame?]) vect?]{}
@defproc[(frame-edge2 [f frame?]) vect?]{
Extracts the origin, first edge or second edge from a frame.
}

@defproc[(make-relative-frame [origin vect?]
                              [corner1 vect?]
                              [corner2 vect?]) (frame? . -> . frame?)]{
The function @scheme[make-relative-frame] provides a convenient way to
transform frames.  Given a frame and three points : @scheme[origin], 
@scheme[corner1], and @scheme[corner2] (expressed in frame coordinates), 
it returns a new frame with those corners.
}

@defproc[(frame-coord-map [f frame?]) (vect? . -> . vect?)]{
Each frame determines a system of "frame coordinates" (x,y) where
(0,0) is the origin of the frame, x represents the displacement 
along the first edge (as a fraction of the length of the edge) and 
y is the displacement along the second edge.

The frame coordinate map is returned by frame-coord-map. E.g.
these expression return the same value:

@scheme[((frame-coord-map a-frame) (make-vect 0 0))]

@scheme[(frame-origin a-frame)]

}

@section{Segments}

A pair of vectors determines a directed line segment - the segment
running from the endpoint of the first vector to the endpoint of the
second vector.

@defproc[(make-segment [from vect?] [to vect?]) segment?]{}
@defproc[(segment-start [s segment?]) vect?]{}
@defproc[(segment-end [s segment?]) vect?]{} 


@section{Primitive Painters}

Painters take a frame and draw an image, transformed to fit inside the frame.

There are four ways to create painters:
@itemize[@item{from a constant: @scheme[number->painter]}
         @item{from a list of line segments:  @scheme[segment->painter]}
         @item{form a procedure:              @scheme[procedure->painter]}
         @item{from a picture:                @scheme[picture->painter]}]

@defproc[(number->painter [color (and/c natural-number/c (<=/c 255))]) painter?]{
Constructs a painter that fills the frame with a gray color indicated
by the number. 0 is black and 255 is white.
}

@defproc[(segments->painter [los list-of-segment?]) painter?]{
Constructs a painter that draws a stick figure given by the 
segments (wrt the unit square).}


@defproc[(procedure->painter [p procedure?]) painter?]{

Creates painters from procedures.  We assume that the procedure
f is defined on the unit square.

Then to plot a point p in the target frame, we find the inverse image 
T^-1(p) of p under the transformation that maps the unit square to the 
target, and find the value of f at T-1(p).
}

@defproc[(picture->painter [p picture]) painter?]{
The picture p is defined on some frame. 

Given a point p in the target frame, we compute T^-1(p) where T
is the transformation that takes the picture frame to the 
target frame, and find the picture value at the closest
integer point.
}

@defproc[(load-painter [filename path?]) painter?]{
Uses the image file given by filename to create a painter.}


@section{Higher Order Painters}

@defproc[(transform-painter [origin vect?]
                            [corner1 vect?]
                            [corner2 vect?]) (painter? . -> . painter?)]{
A painter can be transformed to produce a new painter which, when
given a frame, calls the original painter on the transformed frame.

Transform-painter will given an origin and two corners, return
a function that takes a painter as argument and returns
a transformed painter.
}

@defproc[(flip-horiz [p painter]) painter?]{
Returns a painter that flips the image horizontally.}

@defproc[(flip-vert [p painter]) painter?]{
Returns a painter that flips the image vertically.}

@defproc[(rotate90 [p painter]) painter?]{}
@defproc[(rotate180 [p painter]) painter?]{}
@defproc[(rotate270 [p painter]) painter?]{
Returns a painter that rotates the image.}

@defproc[(beside [p1 painter] [p2 painter]) painter?]{
Constructs a painter that paints the images side-by-side.}

@defproc[(below [p1 painter] [p2 painter]) painter?]{
Constructs a painter that paints the second image
below the first.}

@defproc[(superpose [p1 painter] [p2 painter]) painter?]{
Constructs a painter that paints the two images
on top of each other.}



@section{Simple Builtin Painters}

The following painter values are buitin:

@deftogether[(@defthing[black painter?]
              @defthing[white painter?]
              @defthing[gray painter?])]{
  Fills the frame with black (0), white (255) or gray (150).
}

@defthing[diagonal-shading painter?]{
  Fills the frame with a shades of gray. The color transition
  goes from black in the upper left corner is black, to gray
  in the bottom right corner.
}

@defthing[einstein painter?]{
  Draws an image of Einstein.
}

@section{Painting}

@deftogether[(@defproc[(paint [p painter?]) snip?]
              @defproc[(paint-hi-res [p painter?]) snip?]
              @defproc[(paint-hires [p painter?]) snip?])]{
  Take a painter as input and return a snip containing the painter's image.
  A snip is an image that DrRacket can display automatically.

  @racket[paint-hi-res] and @racket[paint-hires] are the same as @racket[paint].
  They are provided for compatibility with old texts.
}

@section{Authors}

Abelson & Sussman: 
@link["https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.4"
      "Structure and Interpretation of Computer Programs"].

Daniel Coore: Original MIT Scheme code.

Mike Sperber: PLT port.

Jens Axel Søgaard: Documentation.

Javier Olaechea: Fixed amb.

Neil Van Dyke: The SICP language. Maintained the sicp package for years.                 

@section{Other}

See also the
@link["https://mitpress.mit.edu/sites/default/files/sicp/psets/ps4hnd/readme.html"
      "readme.html"] 
from the SICP web-site for more documentation and exercises.

Peter Henderson's 
@link["http://eprints.ecs.soton.ac.uk/7577/01/funcgeo2.pdf" "\"Functional Geometry\""].

@index-section{}

