;;;; HUTLS.SCM
;;; These are utility procedures for Henderson-like graphics systems

;;; Representing points, vectors, segments

(define make-vect cons)
(define vector-xcor car)
(define vector-ycor cdr)

(define zero-vector
  (make-vect 0 0))

(define make-segment cons)
(define segment-start car)
(define segment-end cdr)

(define (vector-add v1 v2)
  (make-vect (+ (vector-xcor v1) (vector-xcor v2))
             (+ (vector-ycor v1) (vector-ycor v2))))

(define (vector-sub v1 v2)
  (vector-add v1 (vector-scale -1 v2)))

(define (vector-scale x v)
  (make-vect (* x (vector-xcor v))
             (* x (vector-ycor v))))

;;; repeating an operation

(define (identity x) x)

(define (compose f g)
  (define (f*g x)
    (f (g x)))
  f*g)

(define (repeated f n)
  (cond ((= n 0) identity)
	((= n 1) f)
	(else (compose f (repeated f (- n 1))))))


;;; FOR-EACH is a system procedure.  It is 
;;;  shown here for reference.
;;;(define (for-each proc list)
;;;  (cond ((null? list) "done")
;;;        (else (proc (car list))
;;;              (for-each proc (cdr list)))))
