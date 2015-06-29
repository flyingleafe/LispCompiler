(define map (lambda (f ls)
              (if (not ls)
                  0
                (cons (f (car ls)) (map f (cdr ls))))))

(printList (map (lambda (x) (* x 2)) '(1 2 3 4)))
