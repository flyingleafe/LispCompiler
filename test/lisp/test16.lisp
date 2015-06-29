;;; Testing printStr/readStr and string-lisp conversions
(printString "This is the start of test")
(printString (readString))
(printString "This is the middle of test")
(printString (cons 49 (readString)))
(printString "This is the end of test")
