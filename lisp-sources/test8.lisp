(define ackermann (lambda (m n)
                    (if (= m 0)
                        (+ n 1)
                      (if (and (> m 0) (= n 0))
                          (ackermann (- m 1) 1)
                        (if (and (> m 0) (> n 0))
                            (ackermann (- m 1) (ackermann m (- n 1)))
                          -228)))))
