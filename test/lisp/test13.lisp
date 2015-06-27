(define sum (lambda (ls)
              (if (not ls)
                  0
                (+ (car ls) (sum (cdr ls))))))

(printInt (sum (quote (1 2 3 4 5))))
