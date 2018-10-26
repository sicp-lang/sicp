(module primitives mzscheme

  (provide picture-width picture-height picture-data
	   (rename exported-make-picture make-picture)
	   picture?
	   
	   invalidate-cached-values
	   
	   image-file->picture
	   picture->bitmap picture->snip)

  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))

  (require (lib "include.ss"))
  (include "primitives.scm"))
