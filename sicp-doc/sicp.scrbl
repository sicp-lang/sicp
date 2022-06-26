#lang scribble/doc

@(require scribble/manual scribble/eval
          (for-label (except-in sicp #%app #%datum #%top)
                     (only-in racket/base require list*)
                     (only-in racket/contract
                              any any/c
                              and/c or/c not/c
                              natural-number/c)))

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

@deftogether[(@defproc[(inc [x number?]) number?]
              @defproc[(1+  [x number?]) number?])]{
  Returns @racket[(+ x 1)].
}

@deftogether[(@defproc[(dec [x number?]) number?]
              @defproc[(-1+ [x number?]) number?]
              @defproc[(1-  [x number?]) number?])]{
  Returns @racket[(- x 1)].
}

@deftogether[(@defthing[the-empty-stream stream?]
              @defthing[empty-stream stream?])]{
  The null/empty stream.
}

@deftogether[(@defform[(cons-stream first-expr rest-expr)]
              @defform[(stream-cons first-expr rest-expr)])]{
  Produces a stream whose first element is determined by
  @racket[first-expr] and whose rest is determined by
  @racket[rest-expr].
}

@defform[(stream v ...)]{
  A shorthand for nested @racket[cons-stream]s ending with @racket[the-empty-stream].
}

@defform[(stream* v ... tail)]{
  A shorthand for nested @racket[cons-stream]s, but the @racket[tail]
  must produce a stream when it is forced, and that stream
  is used as the rest of the stream instead of @racket[the-empty-stream].
  Similar to @racket[list*] but for streams.
}

@deftogether[(@defproc[(stream-null?  [s stream?]) boolean?]
              @defproc[(stream-empty? [s stream?]) boolean?])]{
  Returns @racket[#t] if @racket[s] is @racket[the-empty-stream],
  @racket[#f] otherwise.
}

@defproc[(stream? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is @racket[the-empty-stream] or a pair,
  @racket[#f] otherwise.
  Although the expectation is a pair whose second element is a promise,
  @racket[stream?] doesn't check it, since @racket[promise?] is undefined.
}

@deftogether[(@defproc[(stream-car   [s (and/c stream? (not/c stream-null?))]) any]
              @defproc[(stream-first [s (and/c stream? (not/c stream-null?))]) any])]{
  Returns the value(s) of the first element in @racket[s].
}

@deftogether[(@defproc[(stream-cdr  [s (and/c stream? (not/c stream-null?))]) stream?]
              @defproc[(stream-rest [s (and/c stream? (not/c stream-null?))]) stream?])]{
  Returns a stream that is equivalent to @racket[s] without its first element.
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

Additionally, @racket[true], @racket[false], @racket[raise], @racket[error],
@racket[compose], @racket[compose1], @racket[identity],
@racket[empty?], @racket[empty], @racket[null],
@racket[add1], and @racket[sub1] are provided from Racket.
