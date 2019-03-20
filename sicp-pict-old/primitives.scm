;; Floating vectors

;; picture?
;; picture-width
;; picture-height
;; picture-data -> vector of rows
;; invalidate-cached-values

;; make-picture

; pgm-file->picture

;; We'll just take pictures to be BITMAP-DC% objects

(define-struct picture (width height data))

(define (exported-make-picture width height grey)
  (let ((data (make-vector (+ 1 height)))) ; cater to roundoff problem
    (vector-set! data height (make-vector (+ 1 width) 0.0))
    (do ((row 0 (+ 1 row)))
	((= row height))
      (let ((row-data (make-vector (+ 1 width)))) ; cater to roundoff problem
	(do ((column 0 (+ 1 column)))
	    ((= column width))
	  (vector-set! row-data column grey))
	(vector-set! data row row-data)))
    (make-picture width height data)))

(define (invalidate-cached-values screen)
  'fick-dich-ins-knie)

(define (image-file->picture filename)
  (let ((bitmap-dc (instantiate bitmap-dc% ()
				(bitmap (instantiate bitmap% (filename))))))
    (let-values (((width height) (send bitmap-dc get-size)))
      (flip-picture
       (make-picture (inexact->exact (round width))
                     (inexact->exact (round height))
                     (bitmap-dc->picture-data bitmap-dc))))))

(define (bitmap-dc->picture-data bitmap-dc)
  (let*-values (((width height) (send bitmap-dc get-size))
		((width) (inexact->exact (round width)))
		((height) (inexact->exact (round height))))

    (let ((data (make-vector (+ 1 height)))) ; cater to roundoff problem
      (vector-set! data height (make-vector (+ 1 width) 0.0))
      (do ((color (make-object color% 0 0 0))
	   (row 0 (+ 1 row)))
	  ((= row height))
	(let ((row-data (make-vector (+ 1 width)))) ; cater to roundoff problem
          
          (do ((column 0 (+ 1 column)))
            ((= column width))
            (send bitmap-dc get-pixel column row color)
	    (let ((grey (/ (+ (exact->inexact (send color red))
			      (exact->inexact (send color green))
			      (exact->inexact (send color blue)))
			   3.0)))
	      (vector-set! row-data column grey)))
	  
          (vector-set! data row row-data)))
      
      data)))

(define (flip-picture picture)
  (let ([height (picture-height picture)]
        [width  (picture-width picture)]
        [data   (picture-data picture)])
    (let ([rows (make-vector (+ 1 height))])
      (do ([row 0 (+ 1 row)])
        [(= row height)]
        (vector-set! rows (- height row 1) (vector-ref data row)))
      (make-picture width height rows))))

(define (picture->bitmap picture)
  (let ([picture (flip-picture picture)])
    (let ((width  (picture-width picture))
          (height (picture-height picture)))
      (let* ((bitmap (make-object bitmap% width height
                       #f)) ; not monochrome
             (bitmap-dc (make-object bitmap-dc% bitmap))
             (data (picture-data picture)))
        (do ((row 0 (+ 1 row)))
          ((= row height))
          (do ((row-data (vector-ref data row))
               (column 0 (+ 1 column)))
            ((= column width))
            (let* ((grey (inexact->exact (round (vector-ref row-data column))))
                   (color (make-object color% grey grey grey)))
              (send bitmap-dc set-pixel column row color))))
        
        ;; if we don't do this, BITMAP will be almost useless
        (send bitmap-dc set-bitmap #f)
        
        bitmap))))

(define (picture->snip picture)
  (make-object image-snip% (picture->bitmap picture)))
