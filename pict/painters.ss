;;; painters.ss

(module painters mzscheme

  (provide make-vect
	   vector-xcor vector-ycor
	   vector-add vector-sub vector-scale
           
           make-segment segment-start segment-end
           
           make-frame frame-origin frame-edge1 frame-edge2
           frame-coord-map make-relative-frame
           
	   ; compose
	   
	   number->painter procedure->painter segments->painter picture->painter
           load-painter
	   
	   flip-horiz flip-vert
	   rotate90 rotate180 rotate270
	   beside below superpose
	   
	   transform-painter
	   
	   black white gray
	   einstein diagonal-shading mark-of-zorro
	   
	   paint 
           paint-hi-res
           paint-hires)

  (require "primitives.ss")

  (require (lib "include.ss"))

  (include "mit-macros.scm")

  (include "mit-support.scm")

  (include "hutils.scm")
  (include "prmpnt.scm")
  (include "hend.scm"))
