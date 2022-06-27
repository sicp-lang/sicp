#lang scribble/manual

@(require scribble/eval
          (for-label (except-in sicp #%app #%datum #%top)))

@title{SICP Collections}

This package contains two collections.

The @racket[sicp] collection contains a @tt{#lang sicp} language ideal
for studying the book "Structure and Interpretation of Computer Programs"
by Gerald Jay Sussman and Hal Abelson. The book is usually referred
to simply as SICP.

The second @racket[sicp-pict] collection contains the picture language used in SICP.

@include-section["installation.scrbl"]
@include-section["sicp.scrbl"]
@include-section["sicp-pict.scrbl"]
@include-section["contributors.scrbl"]
@include-section["external-links.scrbl"]

@index-section{}

