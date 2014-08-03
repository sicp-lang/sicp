#lang scribble/doc

@(require scribble/manual
          scribble/eval
          (for-label scheme 
                     (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1))))

@title{The SICP Picture Language}
@defmodule[(planet "sicp.ss" ("soegaard" "sicp.plt" 2 1))]
@index["SICP"]{}
@index["sicp"]{}
@index["painter"]{}
@index["geometry"]{}
@index["picture"]{}
@index["Escher"]{}

This package provides support for the picture language used in SICP.
The non-standard primitives @scheme[cons-stream] and @scheme[amb] are
also provided.


@section{Introduction}

The SICP Picture Language is a small language for drawing pictures.
It shows the power of data abstraction and closure. The picture language
stems from Peter Henderson's 1982 paper "Functional Geometry" and was
included by Hal Abelson in "Structure and Interpretation of Computer 
Programs".  

Before using this package, read 
@link["http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.4" "section 2.2.4 of SICP"],
which is an excellent introduction to the ideas of the picture language.
This manual meant as a reference guide.

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

@schemeblock[
> (require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
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

@defproc[(make-vect [x number?] [y number?]) vect?]{
Constructs a vect with the given coordinates.
}

@defproc[(vector-xcor [v vect?]) number?]{
Returns the x-coordinate.
}

@defproc[(vector-ycor [v vect?]) number?]{
Returns the y-coordinate.
}

@defproc[(vector-add [v vect?] [w vect?]) vect?]{
Adds the two vects by adding their coordinates pairwise.
}

@defproc[(vector-sub [v vect?] [w vect?]) vect?]{
Subtracts the two vects by subtracting their coordinates pairwise.
}

@defproc[(vector-scale [s number?] [v vect?]) vect?]{
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

@defproc[(make-relative-frame [origin vect?] [corner1 vect?] [corner2 vect?]) (frame? -> frame?)]{
The function @scheme[make-relative-frame] provides a convenient way to
transform frames.  Given a frame and three points : @scheme[origin], 
@scheme[corner1], and @scheme[corner2] (expressed in frame coordinates), 
it returns a new frame with those corners.
}

@defproc[(frame-coord-map [f frame?]) (vect? -> vect?)]{
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
@itemize{@item{from a constant: @scheme[number->painter]}
              @item{from a list of line segments:  @scheme[segment->painter]}
              @item{form a procedure:              @scheme[procedure->painter]}
              @item{from a picture:                @scheme[picture->painter]}}

@defproc[(number->painter [color 0..255]) painter?]{
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

@defproc[(transform-painter [origin vect?] [corner1 vect?] [corner2 vect?]) (painter? -> painter?)]{
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

  @scheme[black], @scheme[white] and @scheme[gray] 
     Fills the frame with black (0), white (255) or gray (150).

  @scheme[diagonal-shading]  
    Fills the frame with a shades of gray. The color transition
    goes from black in the upper left corner is black, to gray
    in the bottom right corner.

  @scheme[einstein] 
    Draws an image of Einstein.


@section{Painting}

The procedures paint and paint-hi-res takes a painter as input
and return a snip containing the painter's image. A snip is
an image that DrScheme can display automatically.

@defproc[(paint [p painter?]) snip?]{}
@defproc[(paint-hi-res [p painter?]) snip?]{}


@section{Authors}

Abelson & Sussman: 
@link["http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.4"
      "Structure and Interpretation of Computer Programs"].

Daniel Coore: Original MIT Scheme code.

Mike Sperber: PLT port.

Jens Axel Søgaard: Documentation.

@section{Other}

See also the
@link["http://mitpress.mit.edu/sicp/psets/ps4hnd/readme.html"
      "readme.html"] 
from the SICP web-site for more documentation and exercises.

Peter Henderson's 
@link["http://eprints.ecs.soton.ac.uk/7577/01/funcgeo2.pdf" "\"Functional Geometry\""].

@index-section{}

