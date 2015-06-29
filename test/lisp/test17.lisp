(define readList (lambda (n)
                   (if (= n 0)
                       0
                     (cons (readInt) (readList (- n 1))))))

(printList (readList 5))
