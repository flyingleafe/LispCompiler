(let ((x 5))
  (progn
    (printString "Current value: 5")
    (printString "Add some number:")
    (let ((y (readInt)))
      (progn
        (set x (+ x y))
        (printString "New value:")
        (printInt x)))))
