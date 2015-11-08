(define factorial (lambda (n)
                    (let ((fac (lambda (acc n)
                                 (if (= n 0)
                                     acc
                                   (recur (* acc n) (- n 1))))))
                      (fac 1 n))))

(printString "Enter the value:")
(printInt (factorial (readInt)))
