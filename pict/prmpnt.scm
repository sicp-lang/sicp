;;;; Primitive Painters

;;;; Painters take a frame and
;;;; draw an image, transformed to fit inside the frame.
;;;; there are four ways to create painters:
;;;; (1) from a constant:              number->painter
;;;; (2) from a list of line segments: segment->painter
;;;; (3) form a procedure:             procedure->painter
;;;; (4) from a picture:               picture->painter

;;;; This file assumes the following data structures have been defined:
;;;; frames: make-frame, frame-origin, frame-edge1, frame-edge2
;;;; vectors: make-vect, vector-xcor, vector-ycor, vector-scale, vector-add, vector-sub
;;;; segments: make-segment, segment-start, segment-end

;; (declare (usual-integrations))

;;;; get floating vector ops 

(define floating-vector-ref (access floating-vector-ref (->environment '(student pictures))))
(define floating-vector-set! (access floating-vector-set! (->environment '(student pictures))))

;;;; The variable *the-screen* represents the target image array that will be 
;;;; shown in the graphics window.  The size of *the-screen* determines the resolution
;;;; of the image.

;;;; 128x128 images (displayed on the screen in 257x257 windows) seem
;;;; about right for 6.001.  We can get better quality by doubling the
;;;; resolution, but this is rather slow.   Note: There's some
;;;; (roundoff?) problem that occasioanally tries to plot a point in
;;;; row 256, which is why the windows are 257x257.


(define *screen-width* 128)
(define *screen-height* 128)
(define *last-screen-row* (- *screen-height* 1))
(define *last-screen-column* (- *screen-width* 1))

(define *the-screen* (make-picture *screen-width* *screen-height* 255.))

(define (screen-frame)
  (make-frame (make-vect 0 0)
              (make-vect *screen-width* 0)
              (make-vect 0 *screen-height*)))

(define (set-painter-resolution! res)
  (let ((res (inexact->exact res)))
    (set! *screen-width* res)
    (set! *screen-height* res)
    (set! *last-screen-row* (- *screen-height* 1))
    (set! *last-screen-column* (- *screen-width* 1))
    (set! *the-screen* (make-picture *screen-width* *screen-height* 255.))
    'set))


;;; The simplest painter is just a constant

(define (number->painter number)
  (let ((picture-data (access picture-data (->environment '(student pictures))))
        (invalidate-cached-values (access invalidate-cached-values
                                          (->environment '(student pictures))))
        (h (exact->inexact *screen-width*))
        (v (exact->inexact *screen-height*))
        (value (exact->inexact number)))
    ;;we're going to draw by scanning along the integer points in the frame
    (lambda (frame)
      (let* ((frame (make-inexact-frame frame)) 
             ;;force coords to inexact, because optimized inner
             ;;loops assume inexact
             (frame-map (frame-mapping-info h v  frame)))
        ;;if frame degenerates to a line then don't try to draw
        (if (degenerate? frame-map)
            'done
            (let ((segments (frame-map-segments frame-map))
                  (lowest-row (frame-map-lowest-row frame-map))
                  (highest-row (frame-map-highest-row frame-map)))
              (invalidate-cached-values *the-screen*)
              (let rowloop ((r lowest-row))
                (if (> r highest-row)
                    'done
                    (let ((intersections (row-intersects-segments r segments)))
                      (if (null? intersections)
                          'done
                          (let ((start-col (max 0 (car intersections)))
                                (stop-col (min *last-screen-column* (cadr intersections))))
                            ;;(write-line `(row ,r start: ,start-col stop: ,stop-col))
                            (fill-row-with-value
                             start-col stop-col
                             (vector-ref (picture-data *the-screen*) r)
                             value)))
                      (rowloop (+ r 1)))))))))))

(define (fill-row-with-value start-column end-column row-vector value)
  (if (> start-column end-column)
      'done
      (let fill-loop ((column start-column))
        (if (fix:> column end-column)
            'done
            ;;this is the inner loop
            (begin
              (floating-vector-set! row-vector column value)
              (fill-loop (fix:+ column 1)))))))


;;;; Creating painters from a list of line segments, defined wrt the unit square.

(define (segments->painter segments)
  (lambda (frame)
    (let ((coord-map
           (lambda (v)
             (vector-add (frame-origin frame)
                         (vector-add (vector-scale (vector-xcor v) (frame-edge1 frame))
                                     (vector-scale (vector-ycor v) (frame-edge2 frame)))))))
      (for-each
       (lambda (seg)
         (draw-line-on-screen (coord-map (segment-start seg))
                              (coord-map (segment-end seg))))
       segments))))
         
;;; Draw a line on *the-screen* using Bresenham's algorithm, as
;;; described in Hegron, "Image Synthesis," MIT Press, 1988, p. 23
;;; Clip the points to be on the screen.  We're being inefficient by checking
;;; each point individually to see if it's on screen.

(define (draw-line-on-screen start end)
  (let* ((value (exact->inexact 0))                     ; black pix on white blackground
         (data ((access picture-data (->environment '(student pictures)))
                *the-screen*))
         (invalidate-cached-values (access invalidate-cached-values
                                           (->environment '(student pictures))))
         (xs (vector-xcor start))
         (ys (vector-ycor start))
         (xe (vector-xcor end))
         (ye (vector-ycor end))
         (xinc (if (< xs xe) 1 -1))
         (dx (round->exact (abs (- xs xe))))
         (yinc (if (< ys ye) 1 -1))
         (dy (round->exact (abs (- ys ye))))
         (dx2 (* dx 2))
         (dy2 (* dy 2))
         (x (round->exact xs))
         (y (round->exact ys)))
    (invalidate-cached-values *the-screen*)
    (plot-if-on-screen x y data value)
    (if (> dx dy)
        (let ((dxy (- dy2 dx2)) (s (- dy2 dx)))
          (let loop ((i 1))
            (if (fix:> i dx)
                'done
                (begin
                  (if (fix:>= s 0)
                      (begin (set! y (fix:+ y yinc))
                             (set! s (fix:+ s dxy)))
                      (set! s (fix:+ s dy2)))
                  (set! x (fix:+ x xinc))
                  (plot-if-on-screen x y data value)
                  (loop (fix:+ i 1))))))
        (let ((dxy (- dx2 dy2)) (s (- dx2 dy)))
          (let loop ((i 1))
            (if (fix:> i dy)
                'done
                (begin
                  (if (fix:>= s 0)
                      (begin (set! x (fix:+ x xinc))
                             (set! s (fix:+ s dxy)))
                      (set! s (fix:+ s dx2)))
                  (set! y (fix:+ y yinc))
                  (plot-if-on-screen x y data value)
                  (loop (fix:+ i 1)))))))))

(define (plot-if-on-screen x y data value)
  (if (and (fix:>= x 0)
           (fix:>= y 0)
           (fix:<= x *last-screen-column*)
           (fix:<= y *last-screen-row*))
      (floating-vector-set! (vector-ref data y) x value)))

;;;; Creating painters from procedures

;;;; Creating painters from procedures.  We assume that the procedure
;;;; f is defined on the unit square.
;;;; Then to plot a point p in the target frame,
;;;; we find the inverse image T-1(p) of p under the transformation that 
;;;; maps the unit square to the target, and find the value of f at T-1(p).


(define (procedure->painter proc)
  (if (not (procedure? proc))
      (error "Argument not a procedure--PROCEDURE->PAINTER" proc))
  (let ((picture-data (access picture-data (->environment '(student pictures))))
        (invalidate-cached-values
         (access invalidate-cached-values (->environment '(student pictures))))
        (h 1.0)                         ;width and height of unit square
        (v 1.0))
    ;;we're going to draw by scanning along the integer points in the frame
    (lambda (frame)
      (let* ((frame (make-inexact-frame frame)) 
             ;;force coords to inexact, because optimized inner
             ;;loops assume inexact
             (frame-map (frame-mapping-info h v frame)))
        ;;if frame degenerates to a line then don't try to draw
        (if (degenerate? frame-map)
            'done
            (let ((segments (frame-map-segments frame-map))
                  (lowest-row (frame-map-lowest-row frame-map))
                  (highest-row (frame-map-highest-row frame-map))
                  (picture-x-step (frame-map-x-step frame-map))
                  (picture-y-step (frame-map-y-step frame-map))
                  (inv-map (frame-map-inv-map frame-map)))
              (invalidate-cached-values *the-screen*)
              (let rowloop ((r lowest-row))
                (if (> r highest-row)
                    'done
                    (let ((intersections (row-intersects-segments r segments)))
                      (if (null? intersections)
                          'done
                          (let ((start-col (max 0 (car intersections)))
                                (stop-col (min *last-screen-column* (cadr intersections))))
                            ;;(write-line `(row ,r start: ,start-col stop: ,stop-col))
                            (fill-row-from-procedure
                             r start-col stop-col inv-map
                             picture-x-step picture-y-step
                             (vector-ref (picture-data *the-screen*) r)
                             proc)))
                      (rowloop (+ r 1)))))))))))


(define (fill-row-from-procedure row start-column end-column inv-map
                                 picture-x-step picture-y-step
                                 row-vector
                                 proc
                                 )
  (let* ((inexr (exact->inexact row))
         (picture-start-point (inv-map (make-vect (exact->inexact start-column) inexr)))
         (picture-start-x (vector-xcor picture-start-point))
         (picture-start-y (vector-ycor picture-start-point)))
    (if (> start-column end-column)
        'done
        (let fill-loop ((picture-x picture-start-x)
                        (picture-y picture-start-y)
                        (column start-column))
          (if (fix:> column end-column)
              'done
              ;;this is the inner loop
              (begin
                (floating-vector-set!
                 row-vector
                 column
                 (exact->inexact (proc picture-x picture-y)))
                (fill-loop (flo:+ picture-x picture-x-step)
                           (flo:+ picture-y picture-y-step)
                           (fix:+ column 1))))))))

;;;;Creating painters from pictures

;;;;The picture p is defined on some frame, 
;;;; Given a point p in the target frame, we compute T-1(p) where T
;;;;is the transformation that takes the picture frame to the 
;;;;target frame, and find the picture value at the closest
;;;;integer point.

(define (picture->painter picture)
  (if (not (access picture? (->environment '(student pictures))))
      (error "Argument not a picture--PICTURE->PAINTER" picture))
  (let ((h (exact->inexact (picture-width picture)))
        (v (exact->inexact (picture-height picture)))
        (picture-data (access picture-data (->environment '(student pictures))))
        (invalidate-cached-values (access invalidate-cached-values
                                          (->environment '(student pictures)))))
    ;;we're going to draw by scanning along the integer points in the frame
    (lambda (frame)
      (let* ((frame (make-inexact-frame frame))
             ;;force coords to inexact, because optimized inner
             ;;loops assume inexact
             (frame-map (frame-mapping-info h v frame)))
        ;;if frame degenerates to a line then don't try to draw
        (if (degenerate? frame-map)
            'done
            (let ((segments (frame-map-segments frame-map))
                  (lowest-row (frame-map-lowest-row frame-map))
                  (highest-row (frame-map-highest-row frame-map))
                  (picture-x-step (frame-map-x-step frame-map))
                  (picture-y-step (frame-map-y-step frame-map))
                  (inv-map (frame-map-inv-map frame-map))
                  (picture-array (picture-data picture))
                  (picture-row-max (- (picture-height picture) 1))
                  (picture-row-min 0)
                  (picture-column-max (- (picture-width picture) 1))
                  (picture-column-min 0))
              (invalidate-cached-values *the-screen*)
              (let rowloop ((r lowest-row))
                (if (> r highest-row)
                    'done
                    (let ((intersections (row-intersects-segments r segments)))
                      (if (null? intersections)
                          'done
                          (let ((start-col (max 0 (car intersections)))
                                (stop-col (min *last-screen-column* (cadr intersections))))
                            ;;(write-line `(row ,r start: ,start-col stop: ,stop-col))
                            (fill-row-from-picture
                             r start-col stop-col inv-map
                             picture-x-step picture-y-step
                             (vector-ref (picture-data *the-screen*) r)
                             picture-array
                             picture-row-min picture-row-max
                             picture-column-min picture-column-max)))
                      (rowloop (+ r 1)))))))))))


;;;Here is the loop that fills across a row of the target frame.
;;;It is a little hairy because it needs to make sure that the points
;;;referenced in the picture are actually in bounds.  (Roundoff error
;;;in computing the inverse transformation can drive things out of bounds.)

(define (fill-row-from-picture row start-column end-column inv-map
                               picture-x-step picture-y-step
                               row-vector picture-array
                               picture-row-min picture-row-max
                               picture-column-min picture-column-max
                               )
  (let* ((inexr (exact->inexact row))
         (picture-start-point (inv-map (make-vect (exact->inexact start-column) inexr)))
         (picture-start-x (vector-xcor picture-start-point))
         (picture-start-y (vector-ycor picture-start-point))
         (picture-end-point (inv-map (make-vect (exact->inexact end-column) inexr)))
         (picture-end-x (vector-xcor picture-end-point))
         (picture-end-y (vector-ycor picture-end-point))
	 ;;see comment on trimming, below
	 (x-start->exact (if (> picture-x-step 0) floor->exact ceiling->exact))
	 (y-start->exact (if (> picture-y-step 0) floor->exact ceiling->exact))
	 (x-end->exact (if (> picture-x-step 0) ceiling->exact floor->exact))
	 (y-end->exact (if (> picture-y-step 0) ceiling->exact floor->exact)))
    ;;trim the row from the start and end to make sure that all points
    ;;are in bounds.  When we round to check if the endpoints are in
    ;;the picture, we have to round "outwards", according to the
    ;;direction we are scanning x and y.  I would have thought that
    ;;using round->exact would be enough, but there are screw cases.
    (let find-real-start-loop ()
      (if (or (point-in-picture (x-start->exact picture-start-x) (y-start->exact picture-start-y)
                                picture-row-min picture-row-max
                                picture-column-min picture-column-max)
              (> start-column end-column))
          'done
          (begin (set! picture-start-x (+ picture-start-x picture-x-step))
                 (set! picture-start-y (+ picture-start-y picture-y-step))
                 (set! start-column (+ start-column 1))
                 (find-real-start-loop))))
    (let find-real-end-loop ()
      (if (or (point-in-picture (x-end->exact picture-end-x) (y-end->exact picture-end-y)
                                picture-row-min picture-row-max
                                picture-column-min picture-column-max)
              (< end-column start-column))
          'done
          (begin (set! picture-end-x (- picture-end-x picture-x-step))
                 (set! picture-end-y (- picture-end-y picture-y-step))
                 (set! end-column (- end-column 1))
                 (find-real-end-loop))))
    (if (> start-column end-column)
        'done
        ;;the next two loops are the real inner loops
        (if (= picture-y-step 0)
            ;;optimized loop where picture row doesn't change
            ;;this saves a level of vector access and an add
            (let fill-loop ((picture-x picture-start-x)
                            (column start-column))
              (let ((picture-row  (vector-ref picture-array (round->exact picture-start-y))))
                (if (fix:> column end-column)
                    'done
                    (begin
                      (floating-vector-set! row-vector
                                   column
                                   (floating-vector-ref picture-row (round->exact picture-x)))
                      (fill-loop (flo:+ picture-x picture-x-step)
                                 (fix:+ column 1))))))
            ;;general case where picture row changes
            (let fill-loop ((picture-x picture-start-x)
                            (picture-y picture-start-y)
                            (column start-column))
              (if (fix:> column end-column)
                  'done
                  (begin
                    (floating-vector-set!
                     row-vector
                     column
                     (floating-vector-ref (vector-ref picture-array (round->exact picture-y))
                                          (round->exact picture-x)))
                    (fill-loop (flo:+ picture-x picture-x-step)
                               (flo:+ picture-y picture-y-step)
                               (fix:+ column 1)))))))))

(define (point-in-picture picture-access-column picture-access-row
                          picture-row-min picture-row-max
                          picture-column-min picture-column-max)
  (and (fix:<= picture-access-row picture-row-max)
       (fix:>= picture-access-row picture-row-min)
       (fix:<= picture-access-column picture-column-max)
       (fix:>= picture-access-column picture-column-min)))

;;; given source and target frames compute mapping information, used above.

(define (frame-mapping-info h v frame)
  (let* ((frame-org (frame-origin frame))
         ;;now compute the transformation from picture to frame
         ;;ignoring shift --> matrix [[a11 a12][a21 a22]]
         (a11 (/ (vector-xcor (frame-edge1 frame)) h))
         (a12 (/ (vector-xcor (frame-edge2 frame)) v))
         (a21 (/ (vector-ycor (frame-edge1 frame)) h))
         (a22 (/ (vector-ycor (frame-edge2 frame)) v))
         (det (- (* a11 a22) (* a21 a12))))
    (if (<= (abs det) 1.e-4)
        (list 'degenerate)
        ;;compute inverse transformation 
        (let* ((b11 (/ a22 det))
               (b21 (/ (- a21) det))
               (b12 (/ (- a12) det))
               (b22 (/ a11 det))
               (inv-map
                (lambda (v)
                  (let ((v (vector-sub v frame-org)))
                    (let ((x (vector-xcor v)) (y (vector-ycor v)))
                      (make-vect (+ (* b11 x) (* b12 y))
                                 (+ (* b21 x) (* b22 y)))))))
               ;;now, as we step horizontally along the frame,
               ;;we step by [x,y]-->[x+b11,y+b21] along the picture
               ;;we are going to step along the frame in integer coords
               ;;and step along the picture in real number coords
               (x-step b11)                
               (y-step b21)
               (vertices
                (list (frame-origin frame)
                      (vector-add (frame-origin frame) (frame-edge1 frame))
                      (vector-add (frame-origin frame) (frame-edge2 frame))
                      (vector-add (frame-origin frame)
                                  (vector-add (frame-edge1 frame) (frame-edge2 frame)))))
               (segments (points->segment-checks vertices))
               (vector-ycors (map vector-ycor vertices))
               (lowest-row (max 0 (floor->exact (apply min vector-ycors))))
               (highest-row (min *last-screen-row* (ceiling->exact (apply max vector-ycors)))))
          (list 'ok ;not degenerate                
                x-step y-step
                segments
                lowest-row highest-row
                inv-map)))))

(define (degenerate? frame-map)
  (eq? (car frame-map) 'degenerate))

(define (frame-map-x-step frame-map) (list-ref frame-map 1))
(define (frame-map-y-step frame-map) (list-ref frame-map 2))
(define (frame-map-segments frame-map) (list-ref frame-map 3))
(define (frame-map-lowest-row frame-map) (list-ref frame-map 4))
(define (frame-map-highest-row frame-map) (list-ref frame-map 5))
(define (frame-map-inv-map frame-map) (list-ref frame-map 6))


;;;generate data structures to check intersections, given
;;;the four points of a quadrilateral

(define (points->segment-checks points)
  (let* ((p1 (list-ref points 0))
         (p2 (list-ref points 1))
         (p3 (list-ref points 2))
         (p4 (list-ref points 3))
         (s1 (make-segment-check p1 p2))
         (s2 (make-segment-check p1 p3))
         (s3 (make-segment-check p2 p4))
         (s4 (make-segment-check  p3 p4)))
    (list s1 s2 s3 s4)))
         
(define (make-segment-check p1 p2)
  (define (make-seg lower higher)
    (let* ((xs (vector-xcor lower))
           (xe (vector-xcor higher))
           (ys (vector-ycor lower))
           (ye (vector-ycor higher))
           (dx (- xe xs))
           (dy (- ye ys))
           (dx/dy (if (< (abs dy) 1.e-4) false (/ dx dy))))
      (list xs xe ys ye dx/dy)))
   (if (< (vector-ycor p1) (vector-ycor p2))
       (make-seg p1 p2)
       (make-seg p2 p1)))

(define (segment-check-xs s) (list-ref s 0))
(define (segment-check-xe s) (list-ref s 1))
(define (segment-check-ys s) (list-ref s 2))
(define (segment-check-ye s) (list-ref s 3))
(define (segment-check-dx/dy s) (list-ref s 4))


;;;given the row index and the segment structures, find the col indices of the
;;intersections

(define (row-intersects-segments row segments)
  (let* ((y (exact->inexact row))
         (intersections
          (map (lambda (seg) (row-intersects-segment y seg)) segments))
         (flush-nulls (apply append intersections)))
    (if (null? flush-nulls)
        '()
        (let ((start-col (apply min flush-nulls))
              (stop-col (apply max flush-nulls)))
          (list start-col stop-col)))))
          
(define (row-intersects-segment y s)
  (let* ((xs (segment-check-xs s))
         (ys (segment-check-ys s))
         (xe (segment-check-xe s))
         (ye (segment-check-ye s))
         (dx/dy (segment-check-dx/dy s)))
    (if  (not dx/dy)
        ;;if segment is horizontal, it's either at than coord or not
        (if (= (round->exact y) (round->exact ys))
            (list (round->exact xs) (round->exact xe))
            '())
        (if (<= ys y ye)
            (list (round->exact
                   (+ xs (* (- y ys) dx/dy))))
            '()))))

(define (make-inexact-frame frame)
  (make-frame (make-inexact-vect (frame-origin frame))
             (make-inexact-vect (frame-edge1 frame))
             (make-inexact-vect (frame-edge2 frame))))

(define (make-inexact-vect vector)
  (make-vect (exact->inexact (vector-xcor vector))
             (exact->inexact (vector-ycor vector))))


      
