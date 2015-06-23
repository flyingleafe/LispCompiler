(define compare (lambda (a b)
                  (if (> a b)
                      1
                    (if (< a b)
                        -1
                      0))))

(define compare1 (lambda (a b)
                   (if (= a b)
                       0
                     (if (< a b)
                         -1
                       1))))

(define less3 (lambda (a b c d e f)
                (if (< a d)
                    1
                  (if (and (= a d) (< b e))
                      1
                    (if (and (and (= a d) (= b e)) (< c f))
                        1
                      0)))))
