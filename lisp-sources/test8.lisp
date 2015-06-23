;; Wow such ackermannn
(define ackermann (lambda (m n)
                    (if (= m 0) ; Case 1
                        (+ n 1)
                      (if (and (= n 0) (> m 0)) ; Case 2
                          (ackermann (- m 1) 1)
                        (if (and (> m 0) (> n 0)) ; Case 3
                            (ackermann (- m 1) (ackermann m (- n 1)))
                          0)))))
