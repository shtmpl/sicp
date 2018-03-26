(define (foldr f z xs)
  (if (null? xs)
      z
      (f (car xs) (foldr f z (cdr xs)))))

(foldr + 0 (list 1 2 3 4))
(+ 1 (foldr + 0 (list 2 3 4)))
(+ 1 (+ 2 (foldr + 0 (list 3 4))))
(+ 1 (+ 2 (+ 3 (foldr + 0 (list 4)))))
(+ 1 (+ 2 (+ 3 (+ 4 (foldr + 0 (list))))))
(+ 1 (+ 2 (+ 3 (+ 4 0))))
(+ 1 (+ 2 (+ 3 4)))
(+ 1 (+ 2 7))
(+ 1 9)
10


(define (foldl f z xs)
  (if (null? xs)
      z
      (foldl f (f z (car xs)) (cdr xs))))

;; Applicative-order evaluation
(foldl + 0 (list 1 2 3 4))
(foldl + (+ 0 1) (list 2 3 4))
(foldl + 1 (list 2 3 4))
(foldl + (+ 1 2) (list 3 4))
(foldl + 3 (list 3 4))
(foldl + (+ 3 3) (list 4))
(foldl + 6 (list 4))
(foldl + (+ 6 4) (list))
(foldl + 10 (list))
10

;; Normal-order evaluation
(foldl + 0 (list 1 2 3 4))
(foldl + (+ 0 1) (list 2 3 4))
(foldl + (+ (+ 0 1) 2) (list 3 4))
(foldl + (+ (+ (+ 0 1) 2) 3) (list 4))
(foldl + (+ (+ (+ (+ 0 1) 2) 3) 4) (list))
(+ (+ (+ (+ 0 1) 2) 3) 4)
(+ (+ (+ 1 2) 3) 4)
(+ (+ 3 3) 4)
(+ 6 4)
10