#lang scribble/doc

@(require scribble/manual
          (for-label (only-in sicp inc)))

@title{Installation}

Use DrRacket to install the sicp package like this:

@itemlist[#:style 'ordered
@item{Open the Package Manager:
      in DrRacket choose the menu "File" then choose "Package Manager...".}
@item{In the tab "Do What I Mean" find the text field and enter: @tt{sicp}}
@item{Finally click the "Install" button.}
@item{Test it. Make sure DrRacket has "Determine language from source" in the bottom left corner.
      Write the following program and click run:

@codeblock{
  #lang sicp
  (inc 42)
}

The expected output is @racket[43].}
]