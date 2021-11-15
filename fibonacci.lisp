(defun nth-fib (n)
  (if (<= n 1)
      n
      (+ (nth-fib (- n 1)) (nth-fib ( - n 2))))
)

;; testing 
(print (nth-fib 9))

#| Write a function called fib that given a number n, returns the first n numbers in the Fibonacci
sequence.
Example: (fib 9) should return the list (0 1 1 2 3 5 8 13 21).
|#

(defun fib (n)
  (if (zerop n)
    NIL
    (fib-helper n))
)

(defun fib-helper (n &optional (a 0) (b 1))
  (if (zerop n)
    NIL
    (cons a (fib-helper (- n 1) b (+ a b))))
)

(print(fib 5)) ; ==> (0 1 1 2 3)

(defun fib-lt (n)
  (if (zerop n)
    NIL
    (lt-helper n))
)

(defun lt-helper (n &optional (a 0) (b 1))
  (if (< n a)
    NIL
    (cons a (lt-helper n b (+ a b))))
)

(print(fib-lt 100))






















