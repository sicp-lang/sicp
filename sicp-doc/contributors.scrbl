#lang scribble/doc

@(require scribble/manual
          (for-label (only-in sicp random)))

@title{Contributors}

The following individuals contributed to the implementation and documentation of SICP language:

@itemlist[
  @item{Abelson & Sussman wrote @link["https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.4"]{Structure and Interpretation of Computer Programs}.}
  @item{Daniel Coore designed and implemented the
        @link["https://mitpress.mit.edu/sites/default/files/sicp/psets/ps4hnd/readme.html"]{original image display code} in MIT Scheme.}
  @item{Mike Sperber ported the code to PLT Scheme / Racket.}
  @item{Neil Van Dyke maintained the original SICP language package for years.}
  @item{Dorai Sitaram implemented the initial version of amb in his
        @link["http://ds26gte.github.io/tyscheme/index-Z-H-16.html#node_sec_14.2"]{Teach Yourself Scheme in Fixnum Days}.}
  @item{Javier Olaechea fixed bugs in amb.}
  @item{Leif Andersen fixed several packaging configuration mistakes.}
  @item{Ed Moore fixed a missing function.}
  @item{Chuan Wei Foo improved the README file.}
  @item{Graeme McCutcheon fixed a typo.}
  @item{Huma Zafar updated the documentation to match the new picture language implementation.}
  @item{Pavan Maddamsetti implemented the missing @racket[random] function.}
  @item{Jiezhe Wang fixed the top-level printing so that mutable pairs are displayed similar to r5rs.}
  @item{Noah Ma implemented typed/sicp-pict.}
  @item{Sorawee Porncharoenwase is a current maintainer of the package.}
  @item{Jens Axel Søgaard is a current maintainer of the package,
        implementing the picture language and maintaining the package for years.}
]