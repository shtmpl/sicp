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