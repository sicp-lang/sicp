;;setup PS4


(if (lexical-unreferenceable? user-initial-environment 'g1)
    (define g1 false))
(if (lexical-unreferenceable? user-initial-environment 'g2)
    (define g2 false))
(if (lexical-unreferenceable? user-initial-environment 'g3)
    (define g3 false))


(define (setup)
  (load "~u6001/psets/ps4/prmpnt")
  (load "~u6001/psets/ps4/hutils")
  (load "~u6001/psets/ps4/hend.scm")
  (setup-windows))


(define (setup-windows)
  (if (and g1 (graphics-device? g1))
      (graphics-close g1))
  (begin (set! g1 (make-window 256 256 -10 +10))
	 (graphics-set-coordinate-limits g1 0.0 0.0 1.0 1.0)
	 (graphics-operation g1 'set-window-name "Graphics: g1"))
  (if (and g2 (graphics-device? g2))
      (graphics-close g2))
  (begin (set! g2 (make-window 256 256 -10 +320))
	 (graphics-set-coordinate-limits g2 0.0 0.0 1.0 1.0)
	 (graphics-operation g2 'set-window-name "Graphics: g2"))
  (if (and g3 (graphics-device? g3))
      (graphics-close g3))
  (begin (set! g3 (make-window 256 256 -10 +630))
	 (graphics-set-coordinate-limits g3 0.0 0.0 1.0 1.0)
	 (graphics-operation g3 'set-window-name "Graphics: g3"))
  )

(setup)


