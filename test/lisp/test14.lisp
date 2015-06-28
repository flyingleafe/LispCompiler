(define foo (lambda (x)
              (lambda (y) (+ x 2))))

(printInt ((foo 2) 2))
