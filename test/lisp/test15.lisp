(define foldl (lambda (f init list)
                (if (not list)
                    init
                  (foldl f (f init (car list)) (cdr list)))))

(define append (lambda (x list)
                 (if (not list)
                     (cons x 0)
                   (cons (car list) (append x (cdr list))))))

(define reverse (lambda (list)
                  (foldl (lambda (ls x) (cons x ls)) 0 list)))

(define map (lambda (f ls)
              (reverse (foldl (lambda (res x) (cons (f x) res)) 0 ls))))

(define zipWith (lambda (f xs ys)
                  (if (not xs) 0
                    (if (not ys) 0
                      (cons (f (car xs) (car ys))
                            (zipWith f (cdr xs) (cdr ys)))))))

(printList (map (lambda (x) (* x 2)) '(1 2 3 4)))
(printList (zipWith (lambda (x y) (- x y)) '(10 20 30) '(5 5 5 5)))
(printList (let ((app (lambda (f x) (f x)))
                 (makePlus (lambda (n)
                             (lambda (m) (+ n m)))))
             (zipWith app (map makePlus '(1 2 3 4 5)) '(10 20 30 40 50))))
(printInt (foldl (lambda (x y) (+ x y)) 0 '(1 2 3 4 5)))
