(define foldl (lambda (f init list)
                (if (not list)
                    init
                  (foldl f (f init (car list)) (cdr list)))))

(define reverse (lambda (list)
                  (foldl (lambda (ls x) (cons x ls)) 0 list)))

(define map (lambda (f ls)
              (reverse (foldl (lambda (res x) (cons (f x) res)) 0 ls))))

(define zipWith (lambda (f xs ys)
                  (if (not xs) 0
                    (if (not ys) 0
                      (cons (f (car xs) (car ys))
                            (zipWith f (cdr xs) (cdr ys)))))))

(define iterate (lambda (fun zero n)
                  (if (= n 0)
                      '(zero)
                    (cons zero (iterate fun (fun zero) (- n 1))))))

(printList (iterate (lambda (a) (+ a 1)) 0 10))
