#lang scribble/doc

@(require scribble/manual scribble/eval
          (for-label (except-in sicp #%app #%datum #%top true false identity error)
                     (only-in racket require true false identity error
                                     natural-number/c any/c)))

@title{SICP Language}
@defmodule[sicp #:lang]

@index["SICP"]
@index["sicp"]

@section[#:tag "sicp-intro"]{Introduction}

The programs in the book are written in (a subset of) the programming language Scheme.
As the years have passed the programming language Scheme has evolved.
The language @tt{#lang sicp} provides you with a version of R5RS (the fifth revision of Scheme)
changed slightly in order for programs in SICP to run as is.

To use the @tt{sicp} language simply use @tt{#lang sicp} as the
first line of your program. If you need to use Racket libraries,
then use @racket[#%require].
@margin-note*{
  R5RS has no @racket[require] to avoid breaking programs that use the name @racket[require].
  @racket[#%require] is therefore used instead.
}

@section{Built-In}

@defthing[nil null?]{
  An alias for @racket['()].
}

@defproc[(inc [x number?]) number?]{
  Returns @racket[(+ x 1)].
}

@defproc[(dec [x number?]) number?]{
  Returns @racket[(- x 1)].
}

@defthing[the-empty-stream stream?]{
  The null/empty stream.
}

@defform[(cons-stream first-expr rest-expr)]{
  Produces a stream
}

@defproc[(stream-null? [s stream?]) boolean?]{
  Returns @racket[#t] if @racket[s] is @racket[the-empty-stream],
  @racket[#f] otherwise.
}

@defproc[(runtime) natural-number/c]{
  Returns the current time measured as the number of microseconds passed since a fixed beginning.
}

@defproc[(random [n positive?]) real?]{
  Returns an random integer between 0 and n-1 (inclusive) if @racket[n] is
  an exact integer, otherwise returns a random inexact number between 0 and n
  (exclusive).
}

@defform[(amb expr ...)]{
  The amb operator.
}

Additionally, @racket[true], @racket[false], @racket[identity], and @racket[error] are provided from Racket.
