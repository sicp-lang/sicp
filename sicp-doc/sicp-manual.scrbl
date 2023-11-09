#lang scribble/manual

@(require scribble/eval
          (for-label (except-in sicp #%app #%datum #%top)))

@title{SICP Collections}

This package contains two collections.

The @racket[sicp] collection contains a @tt{#lang sicp} language ideal
for studying the book
@hyperlink["https://mitp-content-server.mit.edu/books/content/sectbyfn/\
books_pres_0/6515/sicp.zip/index.html"]{Structure and Interpretation of
 Computer Programs}
by Gerald Jay Sussman and Hal Abelson. The book is usually referred
to simply as SICP.

The second @racket[sicp-pict] collection contains the picture language used in SICP.

@centered{
 The wizard book
 
 @image["SICP-cover.jpg"]
}
@include-section["installation.scrbl"]
@include-section["sicp.scrbl"]
@include-section["sicp-pict.scrbl"]
@include-section["contributors.scrbl"]
@include-section["external-links.scrbl"]

@index-section{}

