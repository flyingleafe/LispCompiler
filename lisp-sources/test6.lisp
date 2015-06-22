(define fibonacci (lambda (n)
                    (if (= n 1)
                        1
                      (if (= n 0)
                          1
                        (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))
