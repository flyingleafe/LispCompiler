(define makeCounter
  (lambda (init)
    (lambda ()
      (progn
	(set init (+ init 1))
	init))))

(define makeDoubleCounter
  (lambda (init)
    (cons
     (lambda () (progn
		  (set init (+ init 1))
		  init))
     (lambda () (progn
		  (set init (+ init 1))
		  init)))))

(let ((cpair (makeDoubleCounter 5))
      (c1 (car cpair))
      (c2 (cdr cpair)))
  (progn
    (printInt (c1))
    (printInt (c2))
    (printInt (c1))
    (printInt (c2))))
