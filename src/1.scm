;; 1
10 ; => 10
(+ 5 3 4) ; => 12
(- 9 1) ; => 8
(/ 6 2) ; => 3
(+ (* 2 4) (- 4 6)) ; => 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b)) ; => 19
(= a b) ; => #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; => 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; => 16
(+ 2 (if (> b a) b a)) ; => 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; => 16


;; 2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;; 3
(define (f x y z)
  (define (sum-of-squares x y)
    (+ (* x x) (* y y)))
  (cond ((<= x y z) (sum-of-squares y z))
        ((<= x z y) (sum-of-squares z y))
        ((<= y x z) (sum-of-squares x z))
        ((<= y z x) (sum-of-squares z x))
        ((<= z x y) (sum-of-squares x y))
        ((<= z y x) (sum-of-squares y x))))


;; 4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


;; 5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p)) ; applicative-order evaluation => recur
             ; normal-order evaluation      => 0


;; 6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5) ; => 5
(new-if (= 1 1) 0 5) ; => 0

; The new-if is an ordinary procedure.
; Given the applicative-order evaluation, the procedure application requires its arguments to be evaluated first.
; The sqrt-iter combination supplied as an argument to the procedure causes the procedure application to recur indefinitely.
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))


;; 7
(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.0001))

(define (improve-guess guess x)
 (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve-guess guess x) x)))


(define (good-enough-yet? previous-guess guess)
  (< (abs (- guess previous-guess) 0.0001)))

(define (sqrt-iter-yet previous-guess guess x)
  (if (good-enough-yet? previous-guess guess)
      guess
      (sqrt-iter guess (improve-guess guess x) x)))

(define (sqrt x)
 (sqrt-iter 1.0 x))

(define (sqrt-yet x)
 (sqrt-iter-yet x 1.0 x))

(sqrt 0.000000001) ; => 0.007812542664015912
(sqrt 2e20) ; => ...

(sqrt-yet 0.000000001) ; => 0.000031622776601684046
(sqrt-yet 2e20) ; => 14142135623.73095


;; 8
(define (good-enough-approx? approx prev-approx)
  (< (abs (- approx prev-approx)) 0.0001))

(define (improve-approx approx x)
  (/ (+ (/ x (* approx approx)) (* 2 approx)) 3))

(define (cbrt-recursive x approx prev-approx)
  (if (good-enough-approx? approx prev-approx)
      approx
      (cbrt-recursive x (improve-approx approx x) approx)))

(define (cbrt x)
  (cbrt-recursive x 1.0))

(cbrt 2) ; => 1.2599210500177698
(cbrt 8) ; => 2.000000000012062


;; 9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 4 5)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)
9


;; 10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 32768)
65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 4)
...

(define (f n) (A 0 n)) ; => 2*n
(f n)
(A 0 n)
(* 2 n)

(define (g n) (A 1 n)) ; => 2^n
(g n)
(A 1 n)
(A 0 (A 1 (- n 1)))
(* 2 (A 1 (- n 1)))
(* 2 (A 0 (A 1 (- n 2))))
(* 2 (* 2 (A 1 (- n 2))))
(* 2 (* 2 (A 0 (A 1 (- n 3)))))
(* 2 (* 2 (* 2 (A 1 (- n 3)))))
...
(g 1) ; => 2
(g 2) ; => 4
(g 3) ; => 8
(g 4) ; => 16

(define (h n) (A 2 n)) ; => 2^...
(h n)
(A 2 n)
(A 1 (A 2 (- n 1)))
(^ 2 (A 2 (- n 1)))
(^ 2 (A 1 (A 2 (- n 2))))
(^ 2 (^ 2 (A 2 (- n 2))))
(^ 2 (^ 2 (A 1 (A 2 (- n 3)))))
(^ 2 (^ 2 (^ 2 (A 2 (- n 3)))))
...
(h 1) ; => 2
(h 2) ; => 4
(h 3) ; => 16
(h 4) ; => 65536


;; 11
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (* 1 (f-recursive (- n 1)))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(define (f-iterative n a b c)
  (if (= n 0)
      c
      (f-iterative (- n 1) (+ (* 1 a) (* 2 b) (* 3 c)) a b)))

(define (f n) (f-recursive n))
(define (f n) (f-iterative n 2 1 0))

(f 0) ; => 0
(f 1) ; => 1
(f 2) ; => 2
(f 3) ; => 4
(f 4) ; => 11
(f 5) ; => 25
(f 6) ; => 59
(f 7) ; => 142
(f 8) ; => 335
(f 9) ; => 796


;; 12
(define (triangle-element row column)
  (cond ((< row column) 0)
        ((= column 0) 1)
        ((= row column) 1)
        (else (+ (triangle-element (- row 1) (- column 1))
                 (triangle-element (- row 1) column)))))


(triangle-element 0 0) ; => 1

(triangle-element 1 0) ; => 1
(triangle-element 1 1) ; => 1

(triangle-element 2 0) ; => 1
(triangle-element 2 1) ; => 2
(triangle-element 2 2) ; => 1

(triangle-element 3 0) ; => 1
(triangle-element 3 1) ; => 3
(triangle-element 3 2) ; => 3
(triangle-element 3 3) ; => 1

(triangle-element 4 0) ; => 1
(triangle-element 4 1) ; => 4
(triangle-element 4 2) ; => 6
(triangle-element 4 3) ; => 4
(triangle-element 4 4) ; => 1

(triangle-element 5 0) ; => 1
(triangle-element 5 1) ; => 5
(triangle-element 5 2) ; => 10
(triangle-element 5 3) ; => 10
(triangle-element 5 4) ; => 5
(triangle-element 5 5) ; => 1


;; 13
(define phi (/ (+ 1 (sqrt 5)) 2))
(define psi (/ (- 1 (sqrt 5)) 2))
(define (fib n)
  (/ (- (^ phi n) (^ psi n)) (sqrt 5))
  (/ (+ (^ (+ 1 (sqrt 5)) n) (- (^ (- 1 (sqrt 5)) n))) (* (^ 2 n) (sqrt 5))))

; Basis:
(fib 0)
(/ (+ (^ (+ 1 (sqrt 5)) 0) (- (^ (- 1 (sqrt 5)) 0))) (* (^ 2 0) (sqrt 5)))
(/ (+ 1 (- 1)) (* 1 (sqrt 5)))
(/ (+ 1 -1) (* 1 (sqrt 5)))
(/ 0 (* 1 (sqrt 5)))
0

(fib 1)
(/ (+ (^ (+ 1 (sqrt 5)) 1) (- (^ (- 1 (sqrt 5)) 1))) (* (^ 2 1) (sqrt 5)))
(/ (+ (+ 1 (sqrt 5)) (- (- 1 (sqrt 5)))) (* 2 (sqrt 5)))
(/ (+ 1 (sqrt 5) -1 (sqrt 5)) (* 2 (sqrt 5)))
(/ (+ (sqrt 5) (sqrt 5)) (* 2 (sqrt 5)))
(/ (* 2 (sqrt 5)) (* 2 (sqrt 5)))
1

(fib 2)
(/ (+ (^ (+ 1 (sqrt 5)) 2) (- (^ (- 1 (sqrt 5)) 2))) (* (^ 2 2) (sqrt 5)))
(/ (+ (+ 1 (* 2 (sqrt 5)) 5) (- (+ 1 (- (* 2 (sqrt 5))) 5))) (* 4 (sqrt 5)))
(/ (+ 1 (* 2 (sqrt 5)) 5 -1 (* 2 (sqrt 5)) -5) (* 4 (sqrt 5)))
(/ (+ (* 2 (sqrt 5)) (* 2 (sqrt 5))) (* 4 (sqrt 5)))
(/ (* 4 (sqrt 5)) (* 4 (sqrt 5)))
1

(fib 3)
(/ (+ (^ (+ 1 (sqrt 5)) 3) (- (^ (- 1 (sqrt 5)) 3))) (* (^ 2 3) (sqrt 5)))
(/ (+ (+ 1 (* 3 (sqrt 5)) (* 3 5) (* 5 (sqrt 5))) (- (+ 1 (- (* 3 (sqrt 5))) (* 3 5) (- (* 5 (sqrt 5)))))) (* 8 (sqrt 5)))
(/ (+ 1 (* 3 (sqrt 5)) 15 (* 5 (sqrt 5)) -1 (* 3 (sqrt 5)) -15 (* 5 (sqrt 5))) (* 8 (sqrt 5)))
(/ (+ (* 3 (sqrt 5)) (* 5 (sqrt 5)) (* 3 (sqrt 5)) (* 5 (sqrt 5))) (* 8 (sqrt 5)))
(/ (* 16 (sqrt 5)) (* 8 (sqrt 5)))
2

; Hypothesis:
(fib n)
(/ (+ (^ (+ 1 (sqrt 5)) n) (- (^ (- 1 (sqrt 5)) n))) (* (^ 2 n) (sqrt 5)))

; Step
(fib (+ n 1))
(+ (fib n) (fib (- n 1)))
(+ (/ (+ (^ (+ 1 (sqrt 5)) n) (- (^ (- 1 (sqrt 5)) n))) (* (^ 2 n) (sqrt 5)))
   (/ (+ (^ (+ 1 (sqrt 5)) (- n 1)) (- (^ (- 1 (sqrt 5)) (- n 1)))) (* (^ 2 (- n 1)) (sqrt 5))))
(+ (/ (* 2 (+ (^ (+ 1 (sqrt 5)) n) (- (^ (- 1 (sqrt 5)) n)))) (* 2 (^ 2 n) (sqrt 5)))
   (/ (* 2 2 (+ (^ (+ 1 (sqrt 5)) (- n 1)) (- (^ (- 1 (sqrt 5)) (- n 1))))) (* 2 2 (^ 2 (- n 1)) (sqrt 5))))
(+ (/ (+ (* 2 (^ (+ 1 (sqrt 5)) n)) (- (* 2 (^ (- 1 (sqrt 5)) n)))) (* (^ 2 (+ n 1) (sqrt 5))))
   (/ (+ (* 4 (^ (+ 1 (sqrt 5)) (- n 1))) (- (* 4 (^ (- 1 (sqrt 5)) (- n 1))))) (* (^ 2 (+ n 1)) (sqrt 5))))
(/ (+ (* 2 (^ (+ 1 (sqrt 5)) n))
      (- (* 2 (^ (- 1 (sqrt 5)) n)))
      (* 4 (^ (+ 1 (sqrt 5)) (- n 1)))
      (- (* 4 (^ (- 1 (sqrt 5)) (- n 1)))))
   (* (^ 2 (+ n 1) (sqrt 5))))
(/ (+ (* (^ (+ 1 (sqrt 5)) (+ n 1)) (+ (/ 2 (+ 1 (sqrt 5))) (/ 4 (^ (+ 1 (sqrt 5)) 2))))
      (- (* (^ (- 1 (sqrt 5)) (+ n 1)) (+ (/ 2 (- 1 (sqrt 5))) (/ 4 (^ (- 1 (sqrt 5)) 2))))))
   (* (^ 2 (+ n 1) (sqrt 5))))
(/ (+ (* (^ (+ 1 (sqrt 5)) (+ n 1)) 1)
      (- (* (^ (- 1 (sqrt 5)) (+ n 1)) 1)))
   (* (^ 2 (+ n 1) (sqrt 5))))
(/ (+ (^ (+ 1 (sqrt 5)) (+ n 1)) (- (^ (- 1 (sqrt 5)) (+ n 1)))) (* (^ 2 (+ n 1) (sqrt 5))))

;; 14
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((< amount 0) 0)
        ((= amount 0) 1)
        ((= kinds-of-coins 0) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 11)
(cc 11 5)
(+ (cc 11 4) 0)
(+ (+ (cc 11 3) 0) 0)
(+ (+ (+ (cc 11 2) (cc 1 3)) 0) 0)
(+ (+ (+ (+ (cc 11 1) (cc 6 2)) (+ (cc 1 2) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (cc 10 1)) (+ (cc 6 1) (cc 1 2))) (+ (+ (cc 1 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (cc 9 1))) (+ (+ 0 (cc 5 1)) (+ (cc 1 1) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (cc 8 1)))) (+ (+ 0 (+ 0 (cc 4 1))) (+ (+ 0 1) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (cc 7 1))))) (+ (+ 0 (+ 0 (+ 0 (cc 3 1)))) (+ (+ 0 1) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 6 1)))))) (+ (+ 0 (+ 0 (+ 0 (+ 0 (cc 2 1))))) (+ (+ 0 1) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 5 1))))))) (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 1 1)))))) (+ (+ 0 1) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 4 1)))))))) (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1)))))) (+ (+ 0 1) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 3 1))))))))) (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1)))))) (+ (+ 0 1) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 2 1)))))))))) (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0(+ 0 1)))))) (+ (+ 0 1) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 1 1))))))))))) (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1)))))) (+ (+ 0 1) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1))))))))))) (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1)))))) (+ (+ 0 1) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)


;; 15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (<= (abs angle) 0.1)
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15) ; => -0.39980345741334
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))
...



;; 16
(define (even? n) (= (mod n 2) 0))
(define (odd? n) (not (even? n)))

(define (fast-exp-iterative b n r)
  (cond ((= n 0) r)
        ((odd? n) (fast-exp-iterative b (- n 1) (* r b)))
        (else (fast-exp-iterative (* b b) (/ n 2) r))))

(define (fast-exp b n)
  (fast-exp-iterative b n 1))

(fast-exp 2 0) ; => 1
(fast-exp 2 1) ; => 2
(fast-exp 2 2) ; => 4
(fast-exp 2 3) ; => 8
(fast-exp 2 4) ; => 16
(fast-exp 2 5) ; => 32
(fast-exp 2 6) ; => 64
(fast-exp 2 7) ; => 128
(fast-exp 2 8) ; => 256
(fast-exp 2 9) ; => 512

(fast-exp 2 9)                 ; Invariant quantity:
(fast-exp-iterative   2 9   1) ;   1 *   2 ^ 9
(fast-exp-iterative   2 8   2) ;   2 *   2 ^ 8
(fast-exp-iterative   4 4   2) ;   2 *   4 ^ 4
(fast-exp-iterative  16 2   2) ;   2 *  16 ^ 2
(fast-exp-iterative 256 1   2) ;   2 * 256 ^ 1
(fast-exp-iterative 256 0 512) ; 512 * 256 ^ 0
512


;; 17
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(* 3 4)
(+ 3 (* 3 3))
(+ 3 (+ 3 (* 3 2)))
(+ 3 (+ 3 (+ 3 (* 3 1))))
(+ 3 (+ 3 (+ 3 (+ 3 (* 3 0)))))
(+ 3 (+ 3 (+ 3 (+ 3 0))))
(+ 3 (+ 3 (+ 3 3)))
(+ 3 (+ 3 6))
(+ 3 9)
12

(define (even? n) (= (mod n 2) 0))
(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (mult-recursive x y)
  (cond ((= y 0) 0)
        ((= y 1) x)
        ((even? y) (double (mult-recursive x (halve y))))
        (else (+ x (mult-recursive x (- y 1))))))

(mult-recursive 3 17)
(+ 3 (mult-recursive 3 16))
(+ 3 (double (mult-recursive 3 8)))
(+ 3 (double (double (mult-recursive 3 4))))
(+ 3 (double (double (double (mult-recursive 3 2)))))
(+ 3 (double (double (double (double (mult-recursive 3 1))))))
(+ 3 (double (double (double (double 3)))))
(+ 3 (double (double (double 6))))
(+ 3 (double (double 12)))
(+ 3 (double 24))
(+ 3 48)
51


;; 18
(define (even? n) (= (mod n 2) 0))
(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (mult-iterative x y r)
 (cond ((= y 0) r)
  ((even? y) (mult-iterative (double x) (halve y) r))
  (else (mult-iterative x (- y 1) (+ r x)))))

(mult-iterative  3 17  0)
(mult-iterative  3 16  3)
(mult-iterative  6  8  3)
(mult-iterative 12  4  3)
(mult-iterative 24  2  3)
(mult-iterative 48  1  3)
(mult-iterative 48  0 51)
51


;; 19
(define (sq x) (* x x))
(define (even? n) (= (mod n 2) 0))

(define (fib n)
  (fib-iterative 1 0 0 1 n))
(define (fib-iterative a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iterative a
                                      b
                                      (+ (sq p) (sq q))
                                      (+ (* 2 p q) (sq q))
                                      (/ count 2)))
        (else (fib-iterative (+ (* b q) (* a q) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- count 1)))))

(fib 0) ; => 0
(fib 1) ; => 1
(fib 2) ; => 1
(fib 3) ; => 2
(fib 4) ; => 3
(fib 5) ; => 5
(fib 6) ; => 8
(fib 7) ; => 13
(fib 8) ; => 21
(fib 9) ; => 34

(fib 42) ;             a         b       p       q
(fib-iterative         1         0       0       1 42)
(fib-iterative         1         0       1       1 21)
(fib-iterative         2         1       1       1 20)
(fib-iterative         2         1       2       3 10)
(fib-iterative         2         1      13      21  5)
(fib-iterative        89        55      13      21  4)
(fib-iterative        89        55     610     987  2)
(fib-iterative        89        55 1346269 2178309  1)
(fib-iterative 433494437 267914296 1346269 2178309  0)
267914296


;; 20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (rem a b))))

; Normal-order evaluation
(gcd 206 40)
(if (= 40 0)
    206
    (gcd 40
         (rem 206 40)))
(gcd 40
     (rem 206 40))
(if (= (rem 206 40) 0) ; => (+ 1)
    40
    (gcd (rem 206 40)
         (rem 40 (rem 206 40))))
(gcd (rem 206 40)
     (rem 40 (rem 206 40)))
(if (= (rem 40 (rem 206 40)) 0) ; => (+ 1 1)
    (rem 206 40)
    (gcd (rem 40 (rem 206 40))
         (rem (rem 206 40) (rem 40 (rem 206 40)))))
(gcd (rem 40 (rem 206 40))
     (rem (rem 206 40) (rem 40 (rem 206 40))))
(if (= (rem (rem 206 40) (rem 40 (rem 206 40))) 0) ; => (+ 1 1 1 1)
    (rem 40 (rem 206 40))
    (gcd (rem (rem 206 40) (rem 40 (rem 206 40)))
         (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))))
(gcd (rem (rem 206 40) (rem 40 (rem 206 40)))
     (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))))
(if (= (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))) 0) ; => (+ 1 1 1 1 1 1 1)
    (rem (rem 206 40) (rem 40 (rem 206 40)))
    (gcd (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))
         (rem (rem (rem 206 40) (rem 40 (rem 206 40))) (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))))))
(rem (rem 206 40) (rem 40 (rem 206 40))) ; => (+ 1 1 1 1)
2 ; total of 18 `rem's

; Applicative-order evaluation
(gcd 206 40)
(gcd 40 (rem 206 40)) ; => (+ 1)
(gcd 40 6)
(gcd 6 (rem 40 6)) ; => (+ 1)
(gcd 6 4)
(gcd 4 (rem 6 4)) ; => (+ 1)
(gcd 4 2)
(gcd 2 (rem 4 2)) ; => (+ 1)
(gcd 2 0)
2 ; total of 4 `rem's


;; 21
(define (sq x) (* x x))

(define (divides? a b)
 (= (mod b a) 0))

(define (find-divisor n test-divisor)
 (cond ((> (sq test-divisor) n) n)
  ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
 (find-divisor n 2))

(smallest-divisor 199) ; => 199
(smallest-divisor 1999) ; => 1999
(smallest-divisor 19999) ; => 7


;; 22
(define (sq x) (* x x))
(define (divides? a b) (= (mod b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (sq test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))


(define milliseconds-in-a-year 31556952000)
(define milliseconds-in-a-month 2629746000)
(define milliseconds-in-a-day 86400000)
(define milliseconds-in-an-hour 3600000)
(define milliseconds-in-a-minute 60000)
(define milliseconds-in-a-second 1000)

(define (date->ms date)
  (+ (* milliseconds-in-a-year (date-year date))
     (* milliseconds-in-a-month (date-month date))
     (* milliseconds-in-a-day (date-day date))
     (* milliseconds-in-an-hour (date-hour date))
     (* milliseconds-in-a-minute (date-minute date))
     (* milliseconds-in-a-second (date-second date))
     (date-millisecond date)))

(define (date-diff-in-ms from-date to-date)
  (- (date->ms to-date) (date->ms from-date)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-date)
  (if (prime? n)
      (report-prime (date-diff-in-ms start-date (current-date)))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-date)))

(define (search-for-primes from to)
  (cond ((even? from) (search-for-primes (+ from 1) to))
        ((< from to) (timed-prime-test from)
                     (search-for-primes (+ from 1) to))))

; (sqrt 10) ~ 3.16227766017
;    1009 *** 1
;    1013 *** 1
;    1019 *** 1
;   10007 *** 3
;   10009 *** 3
;   10037 *** 3
;  100003 *** 10
;  100019 *** 10
;  100043 *** 11
; 1000003 *** 32
; 1000033 *** 32
; 1000037 *** 31


;; 23
(define (sq x) (* x x))
(define (divides? a b) (= (mod b a) 0))

(define (find-divisor n test-divisor)
  (define (next-odd n)
    (if (even? n)
        (+ n 1)
        (+ n 2)))
  (cond ((> (sq test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-odd test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

;    1009 *** 1
;    1013 *** 0
;    1019 *** 1
;   10007 *** 3
;   10009 *** 2
;   10037 *** 2
;  100003 *** 6
;  100019 *** 6
;  100043 *** 6
; 1000003 *** 20
; 1000033 *** 20
; 1000037 *** 21


;; 24
(define (sq x) (* x x))
(define (even? n) (= (mod n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (mod (sq (expmod base (/ exp 2) m)) m))
        (else (mod (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))


(define milliseconds-in-a-year 31556952000)
(define milliseconds-in-a-month 2629746000)
(define milliseconds-in-a-day 86400000)
(define milliseconds-in-an-hour 3600000)
(define milliseconds-in-a-minute 60000)
(define milliseconds-in-a-second 1000)

(define (date->ms date)
  (+ (* milliseconds-in-a-year (date-year date))
     (* milliseconds-in-a-month (date-month date))
     (* milliseconds-in-a-day (date-day date))
     (* milliseconds-in-an-hour (date-hour date))
     (* milliseconds-in-a-minute (date-minute date))
     (* milliseconds-in-a-second (date-second date))
     (date-millisecond date)))

(define (date-diff-in-ms from-date to-date)
  (- (date->ms to-date) (date->ms from-date)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-date)
  (if (fast-prime? n 1)
      (report-prime (date-diff-in-ms start-date (current-date)))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-date)))

(define (search-for-primes from to)
  (cond ((even? from) (search-for-primes (+ from 1) to))
        ((< from to) (timed-prime-test from)
        (search-for-primes (+ from 1) to))))

; times = 1
;    1009 *** 0
;    1013 *** 1
;    1019 *** 0
; 1000003 *** 1
; 1000033 *** 1
; 1000037 *** 1

; times = 10
;    1009 *** 5
;    1013 *** 6
;    1019 *** 6
; 1000003 *** 9
; 1000033 *** 9
; 1000037 *** 10

; times = 100
;    1009 *** 57
;    1013 *** 59
;    1019 *** 64
; 1000003 *** 92
; 1000033 *** 88
; 1000037 *** 92


;; 25
(define (even? n) (= (mod n 2) 0))
(define (sq x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (mod (sq (expmod base (/ exp 2) m)) m))
        (else (mod (* base (expmod base (- exp 1) m)) m))))

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (sq (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

(define (fast-expmod base exp m)
  (mod (fast-exp base exp) m))

(expmod 2 17 17)
(mod (* 2 (expmod 2 16 17)) 17)
(mod (* 2 (mod (sq (expmod 2 8 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (expmod 2 4 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (expmod 2 2 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (expmod 2 1 17)) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (* 2 (expmod 2 0 17)) 17)) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (* 2 1) 17)) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq 2) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod 4 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq 16) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod 256 17)) 17)) 17)
(mod (* 2 (mod (sq 1) 17)) 17)
(mod (* 2 (mod 1 17)) 17)
(mod (* 2 1) 17)
(mod 2 17)
2

(fast-expmod 2 17 17)
(mod (fast-exp 2 17) 17)
(mod (* 2 (fast-exp 2 16)) 17)
(mod (* 2 (sq (fast-exp 2 8))) 17)
(mod (* 2 (sq (sq (fast-exp 2 4)))) 17)
(mod (* 2 (sq (sq (sq (fast-exp 2 2))))) 17)
(mod (* 2 (sq (sq (sq (sq (fast-exp 2 1)))))) 17)
(mod (* 2 (sq (sq (sq (sq (* 2 (fast-exp 2 0))))))) 17)
(mod (* 2 (sq (sq (sq (sq (* 2 1)))))) 17)
(mod (* 2 (sq (sq (sq (sq 2))))) 17)
(mod (* 2 (sq (sq (sq 4)))) 17)
(mod (* 2 (sq (sq 16))) 17)
(mod (* 2 (sq 256)) 17)
(mod (* 2 65536) 17)
(mod 131072 17)
2

(expmod 2 129 129)
(mod (* 2 (expmod 2 128 129)) 129)
(mod (* 2 (mod (sq (expmod 2 64 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (expmod 2 32 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (expmod 2 16 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (expmod 2 8 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (expmod 2 4 129)) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (expmod 2 2 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (expmod 2 1 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (* 2 (expmod 2 0 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (* 2 1) 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod 2 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq 2) 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod 4 129)) 129)) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq 4) 129)) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq (mod 16 129)) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (sq 16) 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod 256 129)) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq 127) 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod 16129 129)) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod (sq 4) 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq (mod 16 129)) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod (sq 16) 129)) 129)) 129)
(mod (* 2 (mod (sq (mod 256 129)) 129)) 129)
(mod (* 2 (mod (sq 127) 129)) 129)
(mod (* 2 (mod 16129 129)) 129)
(mod (* 2 4) 129)
(mod 8 129)
8

(fast-expmod 2 129 129)
(mod (fast-exp 2 129) 129)
(mod (* 2 (fast-exp 2 128)) 129)
(mod (* 2 (sq (fast-exp 2 64))) 129)
(mod (* 2 (sq (sq (fast-exp 2 32)))) 129)
(mod (* 2 (sq (sq (sq (fast-exp 2 16))))) 129)
(mod (* 2 (sq (sq (sq (sq (fast-exp 2 8)))))) 129)
(mod (* 2 (sq (sq (sq (sq (sq (fast-exp 2 4))))))) 129)
(mod (* 2 (sq (sq (sq (sq (sq (sq (fast-exp 2 2)))))))) 129)
(mod (* 2 (sq (sq (sq (sq (sq (sq (sq (fast-exp 2 1))))))))) 129)
(mod (* 2 (sq (sq (sq (sq (sq (sq (sq (* 2 (fast-exp 2 0)))))))))) 129)
(mod (* 2 (sq (sq (sq (sq (sq (sq (sq (* 2 1))))))))) 129)
(mod (* 2 (sq (sq (sq (sq (sq (sq (sq 2)))))))) 129)
(mod (* 2 (sq (sq (sq (sq (sq (sq 4))))))) 129)
(mod (* 2 (sq (sq (sq (sq (sq 16)))))) 129)
(mod (* 2 (sq (sq (sq (sq 256))))) 129)
(mod (* 2 (sq (sq (sq 65536)))) 129)
(mod (* 2 (sq (sq 4294967296))) 129)
(mod (* 2 (sq 18446744073709552000)) 129)
(mod (* 2 3.402823669209385e+38) 129)
(mod 6.80564733841877e+38 129)
0


;; 26
(define (even? n) (= (mod n 2) 0))
(define (sq x) (* x x))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (mod (sq (expmod base (/ exp 2) m)) m))
        (else (mod (* base (expmod base (- exp 1) m)) m))))

(expmod 2 17 17)
(mod (* 2 (expmod 2 16 17)) 17)
(mod (* 2 (mod (sq (expmod 2 8 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (expmod 2 4 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (expmod 2 2 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (expmod 2 1 17)) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (* 2 (expmod 2 0 17)) 17)) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq (mod (* 2 1) 17)) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (sq 2) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod (* 2 2) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq (mod 4 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (sq 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod (* 4 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq (mod 16 17)) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (sq 16) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod (* 16 16) 17)) 17)) 17)
(mod (* 2 (mod (sq (mod 256 17)) 17)) 17)
(mod (* 2 (mod (sq 1) 17)) 17)
(mod (* 2 (mod (* 1 1) 17)) 17)
(mod (* 2 (mod 1 17)) 17)
(mod (* 2 1) 17)
(mod 2 17)
2

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (mod (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) m))
        (else (mod (* base (expmod base (- exp 1) m)) m))))

(expmod 2 17 17)
(mod (* 2 (expmod 2 16 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (expmod 2 8 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (expmod 2 4 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (expmod 2 2 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (expmod 2 1 17)) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod (* 2 (expmod 2 0 17)) 17)) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod (* 2 1) 17)) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod 2 17)) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) 2) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (mod (* 2 (expmod 2 0 17)) 17) 2) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (mod (* 2 1) 17) 2) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (mod 2 17) 2) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* 2 2) 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod 4 17)) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (mod (* (expmod 2 1 17) (expmod 2 1 17)) 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (mod (* (expmod 2 1 17) (mod (* 2 (expmod 2 0 17)) 17)) 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (mod (* (expmod 2 1 17) (mod (* 2 1) 17)) 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (mod (* (expmod 2 1 17) (mod 2 17)) 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (mod (* (expmod 2 1 17) 2) 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (mod (* (mod (* 2 (expmod 2 0 17)) 17) 2) 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (mod (* (mod (* 2 1) 17) 2) 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (mod (* (mod 2 17) 2) 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (mod (* 2 2) 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* (mod 4 17) 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod (* 4 4) 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) (mod 16 17)) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (expmod 2 4 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (expmod 2 2 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (expmod 2 1 17)) 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod (* 2 (expmod 2 0 17)) 17)) 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod (* 2 1) 17)) 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod 2 17)) 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) 2) 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (mod (* (mod (* 2 (expmod 2 0 17)) 17) 2) 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (mod (* (mod (* 2 1) 17) 2) 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (mod (* (mod 2 17) 2) 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (mod (* 2 2) 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) (mod 4 17)) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (expmod 2 2 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (mod (* (expmod 2 1 17) (expmod 2 1 17)) 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (mod (* (expmod 2 1 17) (mod (* 2 (expmod 2 0 17)) 17)) 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (mod (* (expmod 2 1 17) (mod (* 2 1) 17)) 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (mod (* (expmod 2 1 17) (mod 2 17)) 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (mod (* (expmod 2 1 17) 2) 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (mod (* (mod (* 2 (expmod 2 0 17)) 17) 2) 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (mod (* (mod (* 2 1) 17) 2) 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (mod (* (mod 2 17) 2) 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (mod (* 2 2) 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* (mod 4 17) 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod (* 4 4) 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* (mod 16 17) 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) (mod (* 16 16) 17)) 17)) 17)
(mod (* 2 (mod (* (expmod 2 8 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (expmod 2 4 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (expmod 2 2 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (expmod 2 1 17)) 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod (* 2 (expmod 2 0 17)) 17)) 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod (* 2 1) 17)) 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod 2 17)) 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) 2) 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (mod (* 2 (expmod 2 0 17)) 17) 2) 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (mod (* 2 1) 17) 2) 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* (mod 2 17) 2) 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod (* 2 2) 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) (mod 4 17)) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (expmod 2 2 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (mod (* (expmod 2 1 17) (expmod 2 1 17)) 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (mod (* (expmod 2 1 17) (mod (* 2 (expmod 2 0 17)) 17)) 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (mod (* (expmod 2 1 17) (mod (* 2 1) 17)) 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (mod (* (expmod 2 1 17) (mod 2 17)) 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (mod (* (expmod 2 1 17) 2) 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (mod (* (mod (* 2 (expmod 2 0 17)) 17) 2) 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (mod (* (mod (* 2 1) 17) 2) 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (mod (* (mod 2 17) 2) 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (mod (* 2 2) 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* (mod 4 17) 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod (* 4 4) 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) (mod 16 17)) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (expmod 2 4 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (expmod 2 2 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (expmod 2 1 17)) 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod (* 2 (expmod 2 0 17)) 17)) 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod (* 2 1) 17)) 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) (mod 2 17)) 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (mod (* (expmod 2 1 17) 2) 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (mod (* (mod (* 2 (expmod 2 0 17)) 17) 2) 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (mod (* (mod (* 2 1) 17) 2) 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (mod (* (mod 2 17) 2) 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (mod (* 2 2) 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) (mod 4 17)) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (expmod 2 2 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (mod (* (expmod 2 1 17) (expmod 2 1 17)) 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (mod (* (expmod 2 1 17) (mod (* 2 (expmod 2 0 17)) 17)) 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (mod (* (expmod 2 1 17) (mod (* 2 1) 17)) 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (mod (* (expmod 2 1 17) (mod 2 17)) 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (mod (* (expmod 2 1 17) 2) 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (mod (* (mod (* 2 (expmod 2 0 17)) 17) 2) 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (mod (* (mod (* 2 1) 17) 2) 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (mod (* (mod 2 17) 2) 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (mod (* 2 2) 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* (mod 4 17) 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod (* 4 4) 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* (mod 16 17) 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod (* 16 16) 17) 1) 17)) 17)
(mod (* 2 (mod (* (mod 256 17) 1) 17)) 17)
(mod (* 2 (mod (* 1 1) 17)) 17)
(mod (* 2 (mod 1 17)) 17)
(mod (* 2 1) 17)
(mod 2 17)
2


;; 27
(define (sq x) (* x x))
(define (even? n) (= (mod n 2) 0))
(define (divides? a b) (= (mod b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (sq test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (mod (sq (expmod base (/ exp 2) m)) m))
        (else (mod (* base (expmod base (- exp 1) m)) m))))

(define (fools-fermat-test? n)
  (define (congruent-mod-n? a) (= (expmod a n n) a))
  (define (iterate x)
    (cond ((= x 0) #t)
          ((congruent-mod-n? x) (iterate (- x 1)))
          (else #f)))
  (and (iterate (- n 1)) (not (prime? n))))

(fools-fermat-test?  561) ; => #t
(fools-fermat-test? 1105) ; => #t
(fools-fermat-test? 1729) ; => #t
(fools-fermat-test? 2465) ; => #t
(fools-fermat-test? 2821) ; => #t
(fools-fermat-test? 6601) ; => #t


;; 28 ;; TODO
(define (sq x) (* x x))
(define (even? n) (= (mod n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (mod (sq (expmod base (/ exp 2) m)) m))
        (else (mod (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try a) (= (expmod a n n) a))
  (try (+ 1 (random-integer (- n 1)))))

(define (miller-rabin-test n)
  (define (try a) (= (expmod a (- n 1) n) a))
  (try (+ 1 (random-integer (- n 1)))))
...


;; 29
(define (inc n) (+ n 1))
(define (even? n) (= (mod n 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k) (cond ((or (= k 0) (= k n)) (y k))
                         ((even? k) (* 2 (y k)))
                         (else (* 4 (y k)))))
  (* (/ h 3) (sum term 0 inc n)))

(define (integral f a b n)
  (simpsons-rule f a b n))

(define (cube x) (* x x x))

(integral cube 0 1 100) ; => 0.24999999999999992
(integral cube 0 1 1000) ; => 0.2500000000000003


;; 30
(define (identity x) x)
(define (inc x) (+ x 1))

(define (sum term a next b)
  (define (iterate a result)
    (if (> a b)
        result
        (iterate (next a) (+ result (term a)))))
  (iterate a 0))

(define (sum-ints from to)
  (sum identity from inc to))


;; 31

;; a
(define (identity x) x)
(define (inc x) (+ x 1))

(define (product term from next to)
  (if (> from to)
      1
      (* (term from) (product term (next from) next to))))

(define (factorial n)
  (product identity 1 inc n))

(factorial 0) ; => 1
(factorial 1) ; => 1
(factorial 2) ; => 2
(factorial 3) ; => 6
(factorial 4) ; => 24
(factorial 5) ; => 120

(define (pi-over-4 n)
  (define (term k)
    (if (even? k)
        (/ (+ 2 k) (+ 3 k))
        (/ (+ 3 k) (+ 2 k))))
  (product term 0 inc n))

(* 4 (pi-over-4 100)) ; => 3.1263793980429804
(* 4 (pi-over-4 1000)) ; => 3.1400269461050057
(* 4 (pi-over-4 10000)) ; => 3.1414356249916424

;; b
(define (product term from next to)
  (define (iterate result x)
    (if (> x to)
        result
        (iterate (* result (term x)) (next x))))
  (iterate 1 from))


;; 32

;; a
(define (identity x) x)
(define (inc x) (+ x 1))

(define (accumulate combiner null-value term from next to)
  (if (> from to)
      null-value
      (combiner (term from)
                (accumulate combiner null-value term (next from) next to))))


(define (sum term from next to)
  (accumulate + 0 term from next to))

(define (sum-ints from to)
  (sum identity from inc to))

(sum-ints 1 5)
(sum identity 1 inc 5)
(accumulate + 0 identity 1 inc 5)
(+ 1 (accumulate + 0 identity 2 inc 5))
(+ 1 (+ 2 (accumulate + 0 identity 3 inc 5)))
(+ 1 (+ 2 (+ 3 (accumulate + 0 identity 4 inc 5))))
(+ 1 (+ 2 (+ 3 (+ 4 (accumulate + 0 identity 5 inc 5)))))
(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (accumulate + 0 identity 6 inc 5))))))
(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 0)))))
(+ 1 (+ 2 (+ 3 (+ 4 5))))
(+ 1 (+ 2 (+ 3 9)))
(+ 1 (+ 2 12))
(+ 1 14)
15

(define (product term from next to)
  (accumulate * 1 term from next to))

(define (product-ints from to)
  (product identity from inc to))

(product-ints 1 5)
(product identity 1 inc 5)
(accumulate * 1 identity 1 inc 5)
(* 1 (accumulate * 1 identity 2 inc 5))
(* 1 (* 2 (accumulate * 1 identity 3 inc 5)))
(* 1 (* 2 (* 3 (accumulate * 1 identity 4 inc 5))))
(* 1 (* 2 (* 3 (* 4 (accumulate * 1 identity 5 inc 5)))))
(* 1 (* 2 (* 3 (* 4 (* 5 (accumulate * 1 identity 6 inc 5))))))
(* 1 (* 2 (* 3 (* 4 (* 5 1)))))
(* 1 (* 2 (* 3 (* 4 5))))
(* 1 (* 2 (* 3 20)))
(* 1 (* 2 60))
(* 1 120)
120

;; b
(define (accumulate combiner null-value term from next to)
  (define (iterate result x)
    (if (> x to)
        result
        (iterate (combiner result (term x)) (next x))))
  (iterate null-value from))


;; 33
(define (filtered-accumulate pred? op zero term from next to)
  (cond ((> from to) zero)
        ((pred? from) (op (term from)
                          (filtered-accumulate pred? op zero term (next from) next to)))
        (else (filtered-accumulate pred? op zero term (next from) next to))))

;; a
(define (identity x) x)
(define (inc x) (+ x 1))
(define (sq x) (* x x))
(define (even? n) (= (mod n 2) 0))
(define (divides? a b) (= (mod b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (sq test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (sum-of-squares-of-primes from to)
  (filtered-accumulate prime? + 0 sq from inc to))

(sum-of-squares-of-primes 1 10)
(filtered-accumulate prime? + 0 sq 1 inc 10)
(+ (sq 1) (filtered-accumulate prime? + 0 sq 2 inc 10))
(+ (sq 1) (+ (sq 2) (filtered-accumulate prime? + 0 sq 3 inc 10)))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (filtered-accumulate prime? + 0 sq 4 inc 10))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (filtered-accumulate prime? + 0 sq 5 inc 10))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (+ (sq 5) (filtered-accumulate prime? + 0 sq 6 inc 10)))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (+ (sq 5) (filtered-accumulate prime? + 0 sq 7 inc 10)))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (+ (sq 5) (+ (sq 7) (filtered-accumulate prime? + 0 sq 8 inc 10))))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (+ (sq 5) (+ (sq 7) (filtered-accumulate prime? + 0 sq 9 inc 10))))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (+ (sq 5) (+ (sq 7) (filtered-accumulate prime? + 0 sq 10 inc 10))))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (+ (sq 5) (+ (sq 7) (filtered-accumulate prime? + 0 sq 11 inc 10))))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (+ (sq 5) (+ (sq 7) 0)))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (+ (sq 5) (+ 49 0)))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (+ (sq 5) 49))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) (+ 25 49))))
(+ (sq 1) (+ (sq 2) (+ (sq 3) 74)))
(+ (sq 1) (+ (sq 2) (+ 9 74)))
(+ (sq 1) (+ (sq 2) 83))
(+ (sq 1) (+ 4 83))
(+ (sq 1) 87)
(+ 1 87)
88

;; b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

(define (relatively-prime? a b)
  (= (gcd a b) 1))

(relatively-prime? 10 1) ; => #t
(relatively-prime? 10 3) ; => #t
(relatively-prime? 10 7) ; => #t
(relatively-prime? 10 9) ; => #t

(define (product-of-relatively-prime-ints-up-to n)
  (define (relatively-prime-to-n? x) (= (gcd n x) 1))
  (filtered-accumulate relatively-prime-to-n? * 1 identity 1 inc n))

(product-of-relatively-prime-ints-up-to 10)
(filtered-accumulate relatively-prime-to-10? * 1 identity 1 inc 10)
(* 1 (filtered-accumulate relatively-prime-to-10? * 1 identity 2 inc 10))
(* 1 (filtered-accumulate relatively-prime-to-10? * 1 identity 3 inc 10))
(* 1 (* 3 (filtered-accumulate relatively-prime-to-10? * 1 identity 4 inc 10)))
(* 1 (* 3 (filtered-accumulate relatively-prime-to-10? * 1 identity 5 inc 10)))
(* 1 (* 3 (filtered-accumulate relatively-prime-to-10? * 1 identity 6 inc 10)))
(* 1 (* 3 (filtered-accumulate relatively-prime-to-10? * 1 identity 7 inc 10)))
(* 1 (* 3 (* 7 (filtered-accumulate relatively-prime-to-10? * 1 identity 8 inc 10))))
(* 1 (* 3 (* 7 (filtered-accumulate relatively-prime-to-10? * 1 identity 9 inc 10))))
(* 1 (* 3 (* 7 (* 9 (filtered-accumulate relatively-prime-to-10? * 1 identity 10 inc 10)))))
(* 1 (* 3 (* 7 (* 9 (filtered-accumulate relatively-prime-to-10? * 1 identity 11 inc 10)))))
(* 1 (* 3 (* 7 (* 9 1))))
(* 1 (* 3 (* 7 9)))
(* 1 (* 3 63))
(* 1 189)
189


;; 34
(define (square x) (* x x))
(define (f g) (g 2))

(f square)
(square 2)
(* 2 2)
4

(f (lambda (z) (* z (+ z 1))))
((lambda (z) (* z (+ z 1))) 2)
(* 2 (+ 2 1))
(* 2 3)
6

(f f)
(f 2)
(2 2) ; ~> 2 is not a function


;; 35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; phi ^ 2 = phi + 1
(fixed-point (lambda (phi) (+ 1 (/ 1 phi))) 1.0)


;; 36
(define tolerance 0.00001)

(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(fixed-point-print (lambda (x) (/ (log 1000) (log x))) 2.0) ; => 4.555532270803653
(fixed-point-print (lambda (x) (average x (/ (log 1000) (log x)))) 2.0) ; => 4.555537551999825
; w/o avg. damping     w/ avg. damping
; 9.965784284662087    5.9828921423310435
; 3.004472209841214    4.922168721308343
; 6.279195757507157    4.628224318195455
; 3.759850702401539    4.568346513136242
; 5.215843784925895    4.5577305909237005
; 4.182207192401397    4.555909809045131
; 4.8277650983445906   4.555599411610624
; 4.387593384662677    4.5555465521473675
; 4.671250085763899    4.555537551999825
; 4.481403616895052
; 4.6053657460929
; 4.5230849678718865
; 4.577114682047341
; 4.541382480151454
; 4.564903245230833
; 4.549372679303342
; 4.559606491913287
; 4.552853875788271
; 4.557305529748263
; 4.554369064436181
; 4.556305311532999
; 4.555028263573554
; 4.555870396702851
; 4.555315001192079
; 4.5556812635433275
; 4.555439715736846
; 4.555599009998291
; 4.555493957531389
; 4.555563237292884
; 4.555517548417651
; 4.555547679306398
; 4.555527808516254
; 4.555540912917957
; 4.555532270803653


;; 37

;; a
(define (cont-frac n d k)
  (define (step i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (step (+ i 1))))))
  (step 1))

(/ 1 (cont-frac (lambda (i) 1) (lambda (i) 1) 10)) ; => 1.6181818181818184
(/ 1 (cont-frac (lambda (i) 1) (lambda (i) 1) 11)) ; => 1.6179775280898876
(/ 1 (cont-frac (lambda (i) 1) (lambda (i) 1) 12)) ; => 1.6180555555555558

(/ 1 (cont-frac (lambda (i) 1) (lambda (i) 1) 12)) ; n = \ _ -> 1, d = \ _ -> 1, k = 12
(/ 1 (step 1))
(/ 1 (/ 1 (+ 1 (step 2))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 3))))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 4))))))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 5))))))))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 6))))))))))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 7))))))))))))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 8))))))))))))))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 9))))))))))))))))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 10))))))))))))))))))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 11))))))))))))))))))))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 12))))))))))))))))))))))))
(/ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 1 (step 13))))))))))))))))))))))))))

;; b
(define (cont-frac n d k)
  (define (step result i)
    (if (= i 0)
        result
        (step (/ (n i) (+ (d i) result)) (- i 1))))
  (step 0 k))


;; 38
(define (cont-frac n d k)
  (define (step result i)
    (if (= i 0)
        result
        (step (/ (n i) (+ (d i) result)) (- i 1))))
  (step 0 k))

(define (approx-e k)
  (+ 2 (cont-frac (lambda (i) 1)
                  (lambda (i) (if (= (mod i 3) 2)
                                  (- i (div i 3))
                                  1))
                  k)))

;                ~ 2.718281828459045...
(approx-e  1) ; => 3
(approx-e  2) ; => 2.6666666666666665
(approx-e  3) ; => 2.75
(approx-e  4) ; => 2.7142857142857144
(approx-e  5) ; => 2.71875
(approx-e  6) ; => 2.717948717948718
(approx-e  7) ; => 2.7183098591549295
(approx-e  8) ; => 2.718279569892473
(approx-e  9) ; => 2.718283582089552
(approx-e 10) ; => 2.7182817182817183
(approx-e 11) ; => 2.7182818352059925
(approx-e 12) ; => 2.7182818229439496
(approx-e 13) ; => 2.718281828735696


;; 39
(define (sq x) (* x x))

(define (cont-frac n d k)
  (define (step result i)
    (if (= i 0)
        result
        (step (/ (n i) (+ (d i) result)) (- i 1))))
  (step 0 k))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (sq x))))
             (lambda (i) (- (* 2 i) 1))
             k))

(define pi 3.14159265359)
(tan-cf (/ pi 4)  1) ; => 0.7853981633975
(tan-cf (/ pi 4)  2) ; => 0.9886892399343039
(tan-cf (/ pi 4)  3) ; => 0.9997876809150718
(tan-cf (/ pi 4)  4) ; => 0.9999978684157983
(tan-cf (/ pi 4)  5) ; => 0.9999999865264585
(tan-cf (/ pi 4)  6) ; => 0.9999999999414289
(tan-cf (/ pi 4)  7) ; => 0.9999999999999166
(tan-cf (/ pi 4)  8) ; => 1.0000000000001028
(tan-cf (/ pi 4)  9) ; => 1.0000000000001035
(tan-cf (/ pi 4) 10) ; => 1.0000000000001035


;; 40
(define (fixed-point f first-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 0 0 0) 1) ; => 0.000026531990291797187
(newtons-method (cubic 1 2 3) 1) ; => -1.2756822036498454


;; 41
(define (inc x) (+ x 1))

(define (double f) (lambda (x) (f (f x))))

((double inc) 1) ; => 3
(((double (double double)) inc) 5) ; => 21


;; 42
(define (inc x) (+ x 1))
(define (sq x) (* x x))

(define (compose f g) (lambda (x) (f (g x))))

((compose sq inc) 6) ; => 49


;; 43
(define (inc x) (+ x 1))
(define (sq x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated sq 2) 5) ; => 625


;; 44
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (smooth f)
  (define (avg a b c)
    (/ (+ a b c) 3))
  (let ((dx 0.00001))
    (lambda (x)
      (avg (f (- x dx))
           (f x)
           (f (+ x dx))))))

(define (n-fold-smoothed f n)
  (repeated (smooth f) n))


;; 45
(define (fixed-point f first-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
        ;(display next)
        ;(newline)
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (identity x) x)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

(define (sq x) (* x x))

(define (exp x n)
  (cond ((= n 0) 1)
        ((even? n) (sq (exp x (/ n 2))))
        (else (* x (exp x (- n 1))))))


(define (2nd-root x)
  (fixed-point ((repeated average-damp 1) (lambda (y) (/ x (exp y 1))))
               1.0))

(define (3rd-root x)
  (fixed-point ((repeated average-damp 1) (lambda (y) (/ x (exp y 2))))
               1.0))

(define (4th-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (exp y 3))))
               1.0))

(define (5th-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (exp y 4))))
               1.0))

(define (6th-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (exp y 5))))
               1.0))

(define (7th-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (exp y 6))))
               1.0))

(define (8th-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (exp y 7))))
               1.0))

(define (9th-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (exp y 8))))
               1.0))

(define (10th-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (exp y 9))))
               1.0))

(define (11th-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (exp y 10))))
               1.0))

(define (12th-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (exp y 11))))
               1.0))


(define (explore-average-damp-times-for-nth-root n x times)
  (fixed-point ((repeated average-damp times) (lambda (y) (/ x (exp y (- n 1)))))
               1.0))

; (explore-average-damp-times-for-nth-root 1  (exp 2 1)  0) ; => 2
; (explore-average-damp-times-for-nth-root 2  (exp 2 2)  1) ; => 2.000000000000002
; (explore-average-damp-times-for-nth-root 3  (exp 2 3)  1) ; => 1.9999981824788517
; (explore-average-damp-times-for-nth-root 4  (exp 2 4)  2) ; => 2.0000000000021965
; (explore-average-damp-times-for-nth-root 5  (exp 2 5)  2) ; => 2.000001512995761
; (explore-average-damp-times-for-nth-root 6  (exp 2 6)  2) ; => 2.0000029334662086
; (explore-average-damp-times-for-nth-root 7  (exp 2 7)  2) ; => 2.0000035538623377
; (explore-average-damp-times-for-nth-root 8  (exp 2 8)  3) ; => 2.000000000003967
; (explore-average-damp-times-for-nth-root 9  (exp 2 9)  3) ; => 1.9999997106840102
; (explore-average-damp-times-for-nth-root 10 (exp 2 10) 3) ; => 2.0000011830103324
; (explore-average-damp-times-for-nth-root 11 (exp 2 11) 3) ; => 1.9999976006547362
; (explore-average-damp-times-for-nth-root 12 (exp 2 12) 3) ; => 1.999997691470309
; (explore-average-damp-times-for-nth-root 13 (exp 2 13) 3) ; => 2.0000029085658984
; (explore-average-damp-times-for-nth-root 14 (exp 2 14) 3) ; => 1.9999963265447058
; (explore-average-damp-times-for-nth-root 15 (exp 2 15) 3) ; => 2.0000040951543023
; (explore-average-damp-times-for-nth-root 16 (exp 2 16) 4) ; => 2.000000000076957
; (explore-average-damp-times-for-nth-root 17 (exp 2 17) 4) ; => 2.0000000561635765
; (explore-average-damp-times-for-nth-root 18 (exp 2 18) 4) ; => 2.0000005848426476
; (explore-average-damp-times-for-nth-root 19 (exp 2 19) 4) ; => 2.0000003649180282
; (explore-average-damp-times-for-nth-root 20 (exp 2 20) 4) ; => 1.999999063225966
; (explore-average-damp-times-for-nth-root 21 (exp 2 21) 4) ; => 2.000001254054255
; (explore-average-damp-times-for-nth-root 22 (exp 2 22) 4) ; => 1.9999986334227027
; (explore-average-damp-times-for-nth-root 23 (exp 2 23) 4) ; => 1.999997131591442
; (explore-average-damp-times-for-nth-root 24 (exp 2 24) 4) ; => 1.999997814692085
; (explore-average-damp-times-for-nth-root 25 (exp 2 25) 4) ; => 1.9999977429539466
; (explore-average-damp-times-for-nth-root 26 (exp 2 26) 4) ; => 1.999997554120725
; (explore-average-damp-times-for-nth-root 27 (exp 2 27) 4) ; => 1.9999966641661142
; (explore-average-damp-times-for-nth-root 28 (exp 2 28) 4) ; => 1.9999957943905209
; (explore-average-damp-times-for-nth-root 29 (exp 2 29) 4) ; => 1.9999957104786468
; (explore-average-damp-times-for-nth-root 30 (exp 2 30) 4) ; => 2.000004490765405
; (explore-average-damp-times-for-nth-root 31 (exp 2 31) 4) ; => 1.9999951809750396
; (explore-average-damp-times-for-nth-root 32 (exp 2 32) 5) ; => 2.000000000000006
;          average-damp-times                          ~ integer part of log base 2 of n


(define (nth-root n x)
  (let ((times (div (log n) (log 2))))
    (fixed-point ((repeated average-damp times) (lambda (y) (/ x (exp y (- n 1)))))
                 1.0)))

(nth-root 1  (exp 2 1))  ; => 2
(nth-root 2  (exp 2 2))  ; => 2.000000000000002
(nth-root 3  (exp 2 3))  ; => 1.9999981824788517
(nth-root 4  (exp 2 4))  ; => 2.0000000000021965
(nth-root 5  (exp 2 5))  ; => 2.000001512995761
(nth-root 6  (exp 2 6))  ; => 2.0000029334662086
(nth-root 7  (exp 2 7))  ; => 2.0000035538623377
(nth-root 8  (exp 2 8))  ; => 2.000000000003967
(nth-root 9  (exp 2 9))  ; => 1.9999997106840102
(nth-root 10 (exp 2 10)) ; => 2.0000011830103324
(nth-root 11 (exp 2 11)) ; => 1.9999976006547362
(nth-root 12 (exp 2 12)) ; => 1.999997691470309
(nth-root 13 (exp 2 13)) ; => 2.0000029085658984
(nth-root 14 (exp 2 14)) ; => 1.9999963265447058
(nth-root 15 (exp 2 15)) ; => 2.0000040951543023
(nth-root 16 (exp 2 16)) ; => 2.000000000076957
(nth-root 17 (exp 2 17)) ; => 2.0000000561635765
(nth-root 18 (exp 2 18)) ; => 2.0000005848426476
(nth-root 19 (exp 2 19)) ; => 2.0000003649180282
(nth-root 20 (exp 2 20)) ; => 1.999999063225966
(nth-root 21 (exp 2 21)) ; => 2.000001254054255
(nth-root 22 (exp 2 22)) ; => 1.9999986334227027
(nth-root 23 (exp 2 23)) ; => 1.999997131591442
(nth-root 24 (exp 2 24)) ; => 1.999997814692085
(nth-root 25 (exp 2 25)) ; => 1.9999977429539466
(nth-root 26 (exp 2 26)) ; => 1.999997554120725
(nth-root 27 (exp 2 27)) ; => 1.9999966641661142
(nth-root 28 (exp 2 28)) ; => 1.9999957943905209
(nth-root 29 (exp 2 29)) ; => 1.9999957104786468
(nth-root 30 (exp 2 30)) ; => 2.000004490765405
(nth-root 31 (exp 2 31)) ; => 1.9999951809750396
(nth-root 32 (exp 2 32)) ; => 2.000000000000006



; (define (f y) (/ 16 (exp y 3)))

; ((average-damp (average-damp f)) 1.0)                ; => 4.75
; ((average-damp (average-damp f)) 4.75)               ; => 3.5998232249599065
; ((average-damp (average-damp f)) 3.5998232249599065) ; => 2.7856139316659103
; ((average-damp (average-damp f)) 2.7856139316659103) ; => 2.274263910561008
; ((average-damp (average-damp f)) 2.274263910561008)  ; => 2.045743730517053
; ((average-damp (average-damp f)) 2.045743730517053)  ; => 2.0015115314098866
; ((average-damp (average-damp f)) 2.0015115314098866) ; => 2.000001711389449
; ((average-damp (average-damp f)) 2.000001711389449)  ; => 2.0000000000021965

; (((repeated average-damp 2) f) 1.0)                  ; => 4.75
; (((repeated average-damp 2) f) 4.75)                 ; => 3.5998232249599065
; (((repeated average-damp 2) f) 3.5998232249599065)   ; => 2.7856139316659103
; (((repeated average-damp 2) f) 2.7856139316659103)   ; => 2.274263910561008
; (((repeated average-damp 2) f) 2.274263910561008)    ; => 2.045743730517053
; (((repeated average-damp 2) f) 2.045743730517053)    ; => 2.0015115314098866
; (((repeated average-damp 2) f) 2.0015115314098866)   ; => 2.000001711389449
; (((repeated average-damp 2) f) 2.000001711389449)    ; => 2.0000000000021965

; ((repeated (average-damp f) 2) 1.0)                  ; => 4.263026663952778
; ((repeated (average-damp f) 2) 4.263026663952778)    ; => 1.8341723103276433
; ((repeated (average-damp f) 2) 1.8341723103276433)   ; => 1.844363126744897
; ((repeated (average-damp f) 2) 1.844363126744897)    ; => 1.8527375467627856
; ((repeated (average-damp f) 2) 1.8527375467627856)   ; => 1.8597959434977196
; ((repeated (average-damp f) 2) 1.8597959434977196)   ; => 1.8658611343368858
; ((repeated (average-damp f) 2) 1.8658611343368858)   ; => 1.8711529512646927
; ((repeated (average-damp f) 2) 1.8711529512646927)   ; => 1.875827463108303
; ...


;; 46
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (iterative-improve good-enough? improve)
  (define (step guess)
    (if (good-enough? guess)
        guess
        (step (improve guess))))
  step)


(define tolerance 0.00001)

(define (sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) tolerance))
                      (lambda (guess) (average guess (/ x guess)))) 1.0))

(sqrt 1) ; => 1
(sqrt 2) ; => 1.4142156862745097
(sqrt 3) ; => 1.7320508100147274
(sqrt 4) ; => 2.0000000929222947

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) tolerance))
                      (lambda (guess) (f guess))) first-guess))

((lambda (x) (fixed-point (lambda (y) (average y (/ x y))) 1.0)) 1) ; => 1
((lambda (x) (fixed-point (lambda (y) (average y (/ x y))) 1.0)) 2) ; => 1.4142156862745097
((lambda (x) (fixed-point (lambda (y) (average y (/ x y))) 1.0)) 3) ; => 1.7320508100147274
((lambda (x) (fixed-point (lambda (y) (average y (/ x y))) 1.0)) 4) ; => 2.0000000929222947







