;; 1
(define (pos? x) (> x 0))
(define (neg? x) (< x 0))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond ((or (and (pos? n) (pos? d)) (and (neg? n) (neg? d))) (cons (/ (abs n) g) (/ (abs d) g)))
          ((or (and (pos? n) (neg? d)) (and (neg? n) (pos? d))) (cons (/ (- (abs n)) g) (/ (abs d) g)))
          (else 0))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))


(numer (make-rat  1  2)) ; =>  1
(denom (make-rat  1  2)) ; =>  2

(numer (make-rat -1  2)) ; => -1
(denom (make-rat -1  2)) ; =>  2

(numer (make-rat  1 -2)) ; => -1
(denom (make-rat  1 -2)) ; =>  2

(numer (make-rat -1 -2)) ; =>  1
(denom (make-rat -1 -2)) ; =>  2


;; 2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (let ((x-start (x-point (start-segment s)))
        (y-start (y-point (start-segment s)))
        (x-end   (x-point (end-segment s)))
        (y-end   (y-point (end-segment s))))
    (make-point (/ (- x-end x-start) 2)
                (/ (- y-end y-start) 2))))

(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2)))) ; => (1, 1)


;; 3
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

; ----

; (define (make-rect bottom-left-point top-right-point)
;   (cons bottom-left-point top-right-point))
(define (make-rect start-point width height)
  (cons start-point
        (make-point (+ (x-point start-point) width)
                    (+ (y-point start-point) height))))

(define (bottom-left-point rect)
  (car rect))

(define (top-right-point rect)
  (cdr rect))

(define (width rect)
  (abs (- (x-point (top-right-point rect))
          (x-point (bottom-left-point rect)))))

(define (height rect)
  (abs (- (y-point (top-right-point rect))
          (y-point (bottom-left-point rect)))))

; ----

(define (area rect)
  (* (width rect) (height rect)))

(define (perim rect)
  (+ (* 2 (width rect)) (* 2 (height rect))))


; (area  (make-rect (make-point 0 0) (make-point 2 3))) ; => 6
; (perim (make-rect (make-point 0 0) (make-point 2 3))) ; => 10

(area  (make-rect (make-point 0 0) 2 3)) ; => 6
(perim (make-rect (make-point 0 0) 2 3)) ; => 10


;; 4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(car (cons x y))
((cons x y) (lambda (p q) p))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
x

(cdr (cons x y))
((cons x y) (lambda (p q) q))
((lambda (m) (m x y)) (lambda (p q) q))
((lambda (p q) q) x y)
y


;; 5
(define (encode-base base n)
  (if (= n 0)
      1
      (* base (encode-base base (- n 1)))))

(define (decode-base base n)
  (if (not (= (mod n base) 0))
      0
      (+ 1 (decode-base base (div n base)))))

(define (cons x y)
  (* (encode-base 2 x) (encode-base 3 y)))

(define (car z)
  (decode-base 2 z))

(define (cdr z)
  (decode-base 3 z))

(cons 2 3)
(* (encode-base 2 2) (encode-base 3 3))
(* (encode-base 2 2) (* 3 (encode-base 3 2)))
(* (encode-base 2 2) (* 3 (* 3 (encode-base 3 1))))
(* (encode-base 2 2) (* 3 (* 3 (* 3 (encode-base 3 0)))))
(* (encode-base 2 2) (* 3 (* 3 (* 3 1))))
(* (encode-base 2 2) (* 3 (* 3 3)))
(* (encode-base 2 2) (* 3 9))
(* (encode-base 2 2) 27)
(* (* 2 (encode-base 2 1)) 27)
(* (* 2 (* 2 (encode-base 2 0))) 27)
(* (* 2 (* 2 1)) 27)
(* (* 2 2) 27)
(* 4 27)
108

(car 108)
(decode-base 2 108)
(+ 1 (decode-base 2 54))
(+ 1 (+ 1 (decode-base 2 27)))
(+ 1 (+ 1 0))
(+ 1 1)
2

(cdr 108)
(decode-base 3 108)
(+ 1 (decode-base 3 36))
(+ 1 (+ 1 (decode-base 3 12)))
(+ 1 (+ 1 (+ 1 (decode-base 3 4))))
(+ 1 (+ 1 (+ 1 0)))
(+ 1 (+ 1 1))
(+ 1 2)
3


;; 6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(+ one two)
(lambda (f) (lambda (x) ((one f) ((two f) x))))
(lambda (f) (lambda (x) ((one f) (((lambda (f) (lambda (x) (f (f x)))) f) x))))
(lambda (f) (lambda (x) ((one f) ((lambda (x) (f (f x))) x))))
(lambda (f) (lambda (x) ((one f) (f (f x)))))
(lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f x))) f) (f (f x)))))
(lambda (f) (lambda (x) ((lambda (x) (f x)) (f (f x)))))
(lambda (f) (lambda (x) (f (f (f x)))))

(define three (lambda (f) (lambda (x) (f (f (f x))))))

(+ two two)
(lambda (f) (lambda (x) ((two f) ((two f) x))))
(lambda (f) (lambda (x) ((two f) (((lambda (f) (lambda (x) (f (f x)))) f) x))))
(lambda (f) (lambda (x) ((two f) ((lambda (x) (f (f x))) x))))
(lambda (f) (lambda (x) ((two f) (f (f x)))))
(lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f (f x)))) f) (f (f x)))))
(lambda (f) (lambda (x) ((lambda (x) (f (f x))) (f (f x)))))
(lambda (f) (lambda (x) (f (f (f (f x))))))

(define four (lambda (f) (lambda (x) (f (f (f (f x)))))))


;; 7 .. 16
(define (neg? x) (< x 0))
(define (pos? x) (> x 0))
(define (sign x) (cond ((< x 0) -1)
                       ((= x 0)  0)
                       ((> x 0)  1)))

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

(define (make-center-width center width)
  (make-interval (- center width) (+ center width)))

(define (make-center-percent center percent)
  (make-interval (* center (- 1 percent)) (* center (+ 1 percent))))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (center interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))

(define (percent interval)
  (/ (width interval) (center interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; (define (mul-interval x y)
;   (let ((p0 (* (lower-bound x) (lower-bound y)))
;         (p1 (* (lower-bound x) (upper-bound y)))
;         (p2 (* (upper-bound x) (lower-bound y)))
;         (p3 (* (upper-bound x) (upper-bound y))))
;     (make-interval (min p0 p1 p2 p3) (max p0 p1 p2 p3))))

(define (mul-interval x y)
  (let ((lo-x (lower-bound x))
        (up-x (upper-bound x))
        (lo-y (lower-bound y))
        (up-y (upper-bound y)))
    (cond ((and (neg? lo-x) (neg? up-x) (neg? lo-y) (neg? up-y)) (make-interval (* up-x up-y) (* lo-x lo-y)))
          ((and (neg? lo-x) (neg? up-x) (neg? lo-y) (pos? up-y)) (make-interval (* lo-x up-y) (* lo-x lo-y)))
          ((and (neg? lo-x) (neg? up-x) (pos? lo-y) (pos? up-y)) (make-interval (* lo-x up-y) (* up-x lo-y)))
          ((and (neg? lo-x) (pos? up-x) (neg? lo-y) (neg? up-y)) (make-interval (* up-x lo-y) (* lo-x lo-y)))
          ((and (neg? lo-x) (pos? up-x) (pos? lo-y) (pos? up-y)) (make-interval (* lo-x up-y) (* up-x up-y)))
          ((and (pos? lo-x) (pos? up-x) (neg? lo-y) (neg? up-y)) (make-interval (* up-x lo-y) (* lo-x up-y)))
          ((and (pos? lo-x) (pos? up-x) (neg? lo-y) (pos? up-y)) (make-interval (* up-x lo-y) (* up-x up-y)))
          ((and (pos? lo-x) (pos? up-x) (pos? lo-y) (pos? up-y)) (make-interval (* lo-x lo-y) (* up-x up-y)))
          (else (make-interval (min (* lo-x lo-y) (* lo-x up-y) (* up-x lo-y) (* up-x up-y))
                               (max (* lo-x lo-y) (* lo-x up-y) (* up-x lo-y) (* up-x up-y)))))))

(define (div-interval x y)
  (let ((lo-y (lower-bound y))
        (up-y (upper-bound y)))
    (cond ((= lo-y 0) (console-error "Interval lower bound should not be zero"))
          ((= up-y 0) (console-error "Interval upper bound should not be zero"))
          (else (mul-interval x (make-interval (/ 1.0 up-y) (/ 1.0 lo-y)))))))


; (width (add-interval x y))
; (width (make-interval (+ (lower-bound x) (lower-bound y))
;                       (+ (upper-bound x) (upper-bound y))))
; (/ (- (upper-bound (make-interval (+ (lower-bound x) (lower-bound y))
;                                   (+ (upper-bound x) (upper-bound y))))
;       (lower-bound (make-interval (+ (lower-bound x) (lower-bound y))
;                                   (+ (upper-bound x) (upper-bound y))))) 2)
; (/ (- (+ (upper-bound x) (upper-bound y))
;       (+ (lower-bound x) (lower-bound y))) 2)
; (/ (+ (upper-bound x) (upper-bound y) (- (lower-bound x)) (- (lower-bound y))) 2)
; (/ (+ (upper-bound x) (- (lower-bound x)) (upper-bound y) (- (lower-bound y))) 2)
; (/ (+ (- (upper-bound x) (lower-bound x)) (- (upper-bound y) (lower-bound y))) 2)
; (+ (/ (- (upper-bound x) (lower-bound x)) 2) (/ (- (upper-bound y) (lower-bound y)) 2))
; (+ (width x) (width y))

(width (make-interval 1 2)) ; => 0.5
(width (mul-interval (make-interval 1 2) (make-interval 1 2))) ; => 1.5

(width (make-interval 1 3)) ; => 1
(width (mul-interval (make-interval 1 3) (make-interval 1 3))) ; => 4

(width (make-interval 1 4)) ; => 1.5
(width (mul-interval (make-interval 1 4) (make-interval 1 4))) ; => 7.5

(width (make-interval 1 2)) ; => 0.5
(width (make-interval 3 4)) ; => 0.5
(width (mul-interval (make-interval 1 2) (make-interval 3 4))) ; => 2.5

(width (make-interval 1 3)) ; => 1
(width (make-interval 2 4)) ; => 1
(width (mul-interval (make-interval 1 3) (make-interval 2 4))) ; => 5

(div-interval (make-interval 1 2) (make-interval 0 0))


(mul-interval (make-interval  0  0) (make-interval -1 -1)) ; =>  0,  0
(mul-interval (make-interval  0  0) (make-interval -1  0)) ; =>  0,  0
(mul-interval (make-interval  0  0) (make-interval  0  0)) ; =>  0,  0
(mul-interval (make-interval  0  0) (make-interval  0  1)) ; =>  0,  0
(mul-interval (make-interval  0  0) (make-interval  1  1)) ; =>  0,  0

(mul-interval (make-interval -1 -1) (make-interval  0  0)) ; =>  0,  0
(mul-interval (make-interval -1  0) (make-interval  0  0)) ; =>  0,  0
(mul-interval (make-interval  0  0) (make-interval  0  0)) ; =>  0,  0
(mul-interval (make-interval  0  1) (make-interval  0  0)) ; =>  0,  0
(mul-interval (make-interval  1  1) (make-interval  0  0)) ; =>  0,  0

(mul-interval (make-interval -2 -1) (make-interval -4 -3)) ; =>  3,  8
(mul-interval (make-interval -2 -1) (make-interval -3  4)) ; => -8,  6
(mul-interval (make-interval -2 -1) (make-interval -4  3)) ; => -6,  8
(mul-interval (make-interval -2 -1) (make-interval  3  4)) ; => -8, -3
(mul-interval (make-interval -1  2) (make-interval -4 -3)) ; => -8,  4
(mul-interval (make-interval -1  2) (make-interval -3  4)) ; => -6,  8
(mul-interval (make-interval -1  2) (make-interval -4  3)) ; => -8,  6
(mul-interval (make-interval -1  2) (make-interval  3  4)) ; => -4,  8
(mul-interval (make-interval -2  1) (make-interval -4 -3)) ; => -4,  8
(mul-interval (make-interval -2  1) (make-interval -3  4)) ; => -8,  6
(mul-interval (make-interval -2  1) (make-interval -4  3)) ; => -6,  8
(mul-interval (make-interval -2  1) (make-interval  3  4)) ; => -8,  4
(mul-interval (make-interval  1  2) (make-interval -4 -3)) ; => -8, -3
(mul-interval (make-interval  1  2) (make-interval -3  4)) ; => -6,  8
(mul-interval (make-interval  1  2) (make-interval -4  3)) ; => -8,  6
(mul-interval (make-interval  1  2) (make-interval  3  4)) ; =>  3,  8

(percent (make-center-percent 42 0.01)) ; => 0.01000000000000004

; (let ((a  (make-interval (- x e) (+ x e)))
;       (ta (/ e x))
;       (b  (make-interval (- y d) (+ y d)))
;       (tb (/ d y)))
;   (mul-interval a b)
;   (make-interval (* (- x e) (- y d)) (* (+ x e) (+ y d)))
;   (make-interval (+ (* x y) (- (* x d)) (- (* y e)) (* e d)) (+ (* x y) (* x d) (* y e) (* e d)))
;   (let ((width  (/ (- upper-bound lower-bound) 2))
;         (width  (/ (- (+ (* x y) (* x d) (* y e) (* e d)) (+ (* x y) (- (* x d)) (- (* y e)) (* e d))) 2))
;         (width  (/ (+ (* x y) (* x d) (* y e) (* e d) (- (* x y)) (* x d) (* y e) (- (* e d))) 2))
;         (width  (/ (+ (* x y) (- (* x y)) (* x d) (* x d) (* y e) (* y e) (* e d) (- (* e d))) 2))
;         (width  (/ (+ (* 2 x d) (* 2 y e)) 2))
;         (width  (/ (* 2 (+ (* x d) (* y e))) 2))
;         (width  (+ (* x d) (* y e)))
;         (center (/ (+ lower-bound upper-bound) 2))
;         (center (/ (+ (+ (* x y) (- (* x d)) (- (* y e)) (* e d)) (+ (* x y) (* x d) (* y e) (* e d))) 2))
;         (center (/ (+ (* x y) (- (* x d)) (- (* y e)) (* e d) (* x y) (* x d) (* y e) (* e d)) 2))
;         (center (/ (+ (* x y) (* x y) (* x d) (- (* x d)) (* y e) (- (* y e)) (* e d) (* e d)) 2))
;         (center (/ (+ (* 2 x y) (* 2 e d)) 2))
;         (center (/ (* 2 (+ (* x y) (* e d))) 2))
;         (center (+ (* x y) (* e d))))
;     (/ width center)
;     (/ (+ (* x d) (* y e)) (+ (* x y) (* e d)))) ; (* e d) -> 0
;     (/ (+ (* x d) (* y e)) (* x y))
;     (+ (/ e x) (/ d y))
;     (+ ta tb)))

(let ((a (make-center-percent 1 0.01))
      (b (make-center-percent 2 0.02)))
 (percent (mul-interval a b))) ; => 0.029994001199760076

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(par1 (make-center-percent 1 0.01) (make-center-percent 2 0.02)) ; => 0.6361967213114754, 0.6984406779661017
(par2 (make-center-percent 1 0.01) (make-center-percent 2 0.02)) ; => 0.6577627118644067, 0.6755409836065573

(div-interval (make-interval 99 101)         (make-interval 99 101))         ; => 0.9801980198019802, 1.0202020202020203
(div-interval (make-center-width 100 1)      (make-center-width 100 1))      ; => 0.9801980198019802, 1.0202020202020203
(div-interval (make-center-percent 100 0.01) (make-center-percent 100 0.01)) ; => 0.9801980198019802, 1.0202020202020203

(let ((a (make-center-percent 1 0.01))
      (b (make-center-percent 2 0.02)))
  (div-interval a a) ; => 0.9801980198019802, 1.0202020202020203
  (div-interval a b) ; => 0.4852941176470588, 0.5153061224489796
  (div-interval (mul-interval a a) a) ; => 0.9703960396039604, 1.0304040404040404
  (div-interval (mul-interval (div-interval (mul-interval a a) a) a) a)) ; => 0.9511802764434859, 1.0512202836445261

(let ((x (make-center-percent 1 0.01))
      (x (make-interval (* 1 (- 1 0.01)) (* 1 (+ 1 0.01))))
      (x (make-interval 0.99 1.01)))
  (div-interval x (make-interval 2 2)) ; => 0.495, 0.505

  (par1 x x) ; => 0.4851980198019802, 0.5152020202020202
  (par1 (make-interval 0.99 1.01) (make-interval 0.99 1.01))
  (div-interval (mul-interval (make-interval 0.99 1.01) (make-interval 0.99 1.01))
                (add-interval (make-interval 0.99 1.01) (make-interval 0.99 1.01)))
  (div-interval (make-interval (* 0.99 0.99) (* 1.01 1.01))
                (make-interval (+ 0.99 0.99) (+ 1.01 1.01)))
  (div-interval (make-interval 0.9801 1.0201)
                (make-interval 1.98 2.02))
  (mul-interval (make-interval 0.9801 1.0201) (make-interval (/ 1.0 2.02) (/ 1.0 1.98)))
  (mul-interval (make-interval 0.9801 1.0201) (make-interval 0.49504950495049505 0.5050505050505051))
  (make-interval (* 0.9801 0.49504950495049505) (* 1.0201 0.5050505050505051))
  (make-interval 0.4851980198019802 0.5152020202020202)

  (par2 x x) ; => 0.495, 0.505
  (par2 (make-interval 0.99 1.01) (make-interval 0.99 1.01))
  (div-interval (make-interval 1 1)
                (add-interval (div-interval (make-interval 1 1) (make-interval 0.99 1.01))
                              (div-interval (make-interval 1 1) (make-interval 0.99 1.01))))
  (div-interval (make-interval 1 1)
                (add-interval (mul-interval (make-interval 1 1) (make-interval (/ 1.0 1.01) (/ 1.0 0.99)))
                              (mul-interval (make-interval 1 1) (make-interval (/ 1.0 1.01) (/ 1.0 0.99)))))
  (div-interval (make-interval 1 1)
                (add-interval (mul-interval (make-interval 1 1) (make-interval 0.9900990099009901 1.0101010101010102))
                              (mul-interval (make-interval 1 1) (make-interval 0.9900990099009901 1.0101010101010102))))
  (div-interval (make-interval 1 1)
                (add-interval (make-interval (* 1 0.9900990099009901) (* 1 1.0101010101010102))
                              (make-interval (* 1 0.9900990099009901) (* 1 1.0101010101010102))))
  (div-interval (make-interval 1 1)
                (add-interval (make-interval 0.9900990099009901 1.0101010101010102)
                              (make-interval 0.9900990099009901 1.0101010101010102)))
  (div-interval (make-interval 1 1)
                (make-interval (+ 0.9900990099009901 0.9900990099009901) (+ 1.0101010101010102 1.0101010101010102)))
  (div-interval (make-interval 1 1)
                (make-interval 1.9801980198019802 2.0202020202020203))
  (mul-interval (make-interval 1 1) (make-interval (/ 1 2.0202020202020203) (/ 1 1.9801980198019802)))
  (mul-interval (make-interval 1 1) (make-interval 0.495 0.505))
  (make-interval (* 1 0.495) (* 1 0.505))
  (make-interval 0.495 0.505))


;; 17
(define (last-pair xs)
  (if (null? (cdr xs))
      xs
      (last-pair (cdr xs))))

(last-pair (list 23 72 149 34)) ; => (34)


;; 18
(define (reverse xs)
  (if (null? xs)
      xs
      (append (reverse (cdr xs)) (list (car xs)))))

(reverse (list 1 4 9 16 25)) ; => (25 16 9 4 1)
(append (reverse (list 4 9 16 25)) (list 1))
(append (append (reverse (list 9 16 25)) (list 4)) (list 1))
(append (append (append (reverse (list 16 25)) (list 9)) (list 4)) (list 1))
(append (append (append (append (reverse (list 25)) (list 16)) (list 9)) (list 4)) (list 1))
(append (append (append (append (append (reverse (list)) (list 25)) (list 16)) (list 9)) (list 4)) (list 1))
(append (append (append (append (append (list) (list 25)) (list 16)) (list 9)) (list 4)) (list 1))
(append (append (append (append (list 25) (list 16)) (list 9)) (list 4)) (list 1))
(append (append (append (list 25 16) (list 9)) (list 4)) (list 1))
(append (append (list 25 16 9) (list 4)) (list 1))
(append (list 25 16 9 4) (list 1))
(list 25 16 9 4 1)

(define (reverse xs)
  (define (reverse-iterative xs result)
    (if (null? xs)
        result
        (reverse-iterative (cdr xs) (cons (car xs) result))))
  (reverse-iterative xs (list)))

(reverse (list 1 4 9 16 25)) ; => (25 16 9 4 1)
(reverse-iterative (list 1 4 9 16 25) (list))
(reverse-iterative (list 4 9 16 25) (list 1))
(reverse-iterative (list 9 16 25) (list 4 1))
(reverse-iterative (list 16 25) (list 9 4 1))
(reverse-iterative (list 25) (list 16 9 4 1))
(reverse-iterative (list) (list 25 16 9 4 1))
(list 25 16 9 4 1)


;; 19
(define (cc amount coin-values)
  (define (no-more? coin-values) (null? coin-values))
  (define (first-denomination coin-values) (car coin-values))
  (define (except-first-denomination coin-values) (cdr coin-values))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; (cc 100 us-coins) ; => 292
; (cc 100 uk-coins) ; => 104561

(cc 100 (list 50 25 10 5 1)) ; => 292
(cc 100 (list 5 25 50 10 1)) ; => 292
(cc 100 (list 1 5 10 25 50)) ; => 292
(cc 100 (list 1 10 50 25 5)) ; => 292


;; 20
(define (filter p? s)
  (if (null? s)
      (list)
      (let ((x  (car s))
            (xs (cdr s)))
        (if (p? x)
            (cons x (filter p? xs))
            (filter p? xs)))))

(define (same-parity x . xs)
  (if (even? x)
      (cons x (filter even? xs))
      (cons x (filter odd? xs))))

(same-parity 1 2 3 4 5 6 7) ; => (1 3 5 7)
(same-parity 2 3 4 5 6 7) ; => (2 4 6)


;; 21
(define (sq x) (* x x))

(define (square-list items)
  (if (null? items)
      (list)
      (cons (sq (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map sq items))

(square-list (list 1 2 3 4))


;; 22
(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items (list)))

(square-list (list 1 2 3 4))
(iter (list 1 2 3 4) (list))
(iter (list 2 3 4) (cons (square 1) (list)))
(iter (list 2 3 4) (cons 1 (list)))
(iter (list 2 3 4) (list 1))
(iter (list 3 4) (cons (square 2) (list 1)))
(iter (list 3 4) (cons 4 (list 1)))
(iter (list 3 4) (list 4 1))
(iter (list 4) (cons (square 3) (list 4 1)))
(iter (list 4) (cons 9 (list 4 1)))
(iter (list 4) (list 9 4 1))
(iter (list) (cons (square 4) (list 9 4 1)))
(iter (list) (list 16 9 4 1))
(list 16 9 4 1)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items (list)))

(square-list (list 1 2 3 4))
(iter (list 1 2 3 4) (list))
(iter (list 2 3 4) (cons (list) (square 1)))
(iter (list 2 3 4) (cons (list) 1))
(iter (list 3 4) (cons (cons (list) 1) (square 2)))
(iter (list 3 4) (cons (cons (list) 1) 4))
(iter (list 4) (cons (cons (cons (list) 1) 4) (square 3)))
(iter (list 4) (cons (cons (cons (list) 1) 4) 9))
(iter (list) (cons (cons (cons (cons (list) 1) 4) 9) (square 4)))
(iter (list) (cons (cons (cons (cons (list) 1) 4) 9) 16))
(cons (cons (cons (cons (list) 1) 4) 9) 16)


;; 23
(define (for-each f xs)
  (cond ((not (null? xs)) (f (car xs)) (for-each f (cdr xs)))))

(for-each (lambda (x) (display x) (newline))
          (list 57 321 88))

(define (for-each xs f)
  (cond ((not (null? xs)) (f (car xs)) (for-each (cdr xs) f))))

(for-each (list 57 321 88)
          (lambda (x) (display x) (newline)))


;; 24
(list 1 (list 2 (list 3 4))) ; => (1 (2 (3 4)))

o o > o x
v     v
1     o o > o x
      v     v
      2     o o > o x
            v     v
            3     4

    . (1 (2 (3 4)))
   / \
1 .   . (2 (3 4))
     / \
  2 .   . (3 4)
       / \
    3 .   . 4

;; 25
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))) ; => 7 (cadaddr)
(car (car (list (list 7)))) ; => 7 (caar)
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))) ; => 7 (cadadadadadadr)


;; 26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; => (1 2 3 4 5 6)
(cons x y)   ; => ((1 2 3) 4 5 6)
(list x y)   ; => ((1 2 3) (4 5 6))


;; 27
(define (reverse xs)
  (if (null? xs)
      xs
      (append (reverse (cdr xs)) (list (car xs)))))

(define (deep-reverse xs)
  (cond ((or (null? xs) (not (pair? xs))) xs)
        (else (reverse (map deep-reverse xs)))))

(define x (list (list 1 2) (list 3 4)))

x                ; => ((1 2) (3 4))
(reverse x)      ; => ((3 4) (1 2))
(deep-reverse x) ; => ((4 3) (2 1))

(define y (list (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8))))
y                ; => (((1 2) (3 4)) ((5 6) (7 8)))
(reverse y)      ; => (((5 6) (7 8)) ((1 2) (3 4)))
(deep-reverse y) ; => (((8 7) (6 5)) ((4 3) (2 1)))


;; 28
(define (fringe s)
  (cond ((null? s) s)
        ((not (pair? s)) (list s))
        (else (append (fringe (car s)) (fringe (cdr s))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x) ; => (1 2 3 4)
(fringe (list x x)) ; => (1 2 3 4 1 2 3 4)
(fringe (list (list x x) (list x x))) ; => (1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4)


;; 29
; (define (make-mobile left right)
;   (list left right))
(define (make-mobile left right)
  (cons left right))

(define (left-branch mobile)
  (car mobile))

; (define (right-branch mobile)
;   (car (cdr mobile)))
(define (right-branch mobile)
  (cdr mobile))


; (define (make-branch length structure)
;   (list length structure))
(define (make-branch length structure)
  (cons length structure))

(define (branch-length branch)
  (car branch))

; (define (branch-structure branch)
;   (car (cdr branch)))
(define (branch-structure branch)
  (cdr branch))


(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (if (not (pair? mobile))
      #t
      (let ((left  (left-branch mobile))
            (right (right-branch mobile)))
        (and (= (torque left) (torque right))
             (balanced? (branch-structure left))
             (balanced? (branch-structure right))))))


(total-weight 1)                                                                 ; => 1
(total-weight (make-mobile (make-branch 0.1 1)
                           (make-branch 0.1 1)))                                 ; => 2
(total-weight (make-mobile (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                         (make-branch 0.1 1)))
                           (make-branch 0.1 1)))                                 ; => 3
(total-weight (make-mobile (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                         (make-branch 0.1 1)))
                           (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                         (make-branch 0.1 1))))) ; => 4

(torque (make-branch 0.1 1))                                                                 ; => 0.1
(torque (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                      (make-branch 0.1 1))))                                 ; => 0.2
(torque (make-branch 0.1 (make-mobile (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                                    (make-branch 0.1 1)))
                                      (make-branch 0.1 1))))                                 ; => 0.3
(torque (make-branch 0.1 (make-mobile (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                                    (make-branch 0.1 1)))
                                      (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                                    (make-branch 0.1 1)))))) ; => 0.4

(balanced? 1)                                                                 ; => #t
(balanced? (make-mobile (make-branch 0.1 1)
                        (make-branch 0.1 1)))                                 ; => #t
(balanced? (make-mobile (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                      (make-branch 0.1 1)))
                        (make-branch 0.1 1)))                                 ; => #f
(balanced? (make-mobile (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                      (make-branch 0.1 1)))
                        (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                      (make-branch 0.1 1))))) ; => #t

(define x (make-mobile (make-branch 0.1 4)
                       (make-branch 0.1 (make-mobile (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                                                   (make-branch 0.1 1)))
                                                     (make-branch 0.1 (make-mobile (make-branch 0.1 1)
                                                                                   (make-branch 0.1 1)))))))

(balanced? x) ;=> #t


;; 30
(define (sq x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (sq tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree x)
             (sq x)))
       tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))) ; => (1 (4 (9 16) 25) (36 49))


;; 31
(define (sq x) (* x x))

(define (tree-map f tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map f x)
             (f x)))
       tree))

(define (square-tree tree)
  (tree-map sq tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))) ; => (1 (4 (9 16) 25) (36 49))


;; 32
(define (cons-x x)
  (lambda (xs)
    (cons x xs)))

(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (cons-x (car s)) rest)))))

(subsets (list 1 2 3)) ; => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(append (subsets (list 2 3))
        (map (cons-x 1) (subsets (list 2 3))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (subsets (list 3))
                                (map (cons-x 2) (subsets (list 3))))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (subsets (list 3))
                                (map (cons-x 2) (append (subsets (list))
                                                        (map (cons-x 3) (subsets (list))))))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (subsets (list 3))
                                (map (cons-x 2) (append (subsets (list))
                                                        (map (cons-x 3) (list (list))))))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (subsets (list 3))
                                (map (cons-x 2) (append (subsets (list))
                                                        (list (list 3)))))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (subsets (list 3))
                                (map (cons-x 2) (append (list (list))
                                                        (list (list 3)))))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (subsets (list 3))
                                (map (cons-x 2) (list (list) (list 3))))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (subsets (list 3))
                                (list (list 2) (list 2 3)))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (append (subsets (list))
                                        (map (cons-x 3) (subsets (list))))
                                (list (list 2) (list 2 3)))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (append (subsets (list))
                                        (map (cons-x 3) (list (list))))
                                (list (list 2) (list 2 3)))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (append (subsets (list))
                                        (list (list 3)))
                                (list (list 2) (list 2 3)))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (append (list (list))
                                        (list (list 3)))
                                (list (list 2) (list 2 3)))))
(append (subsets (list 2 3))
        (map (cons-x 1) (append (list (list) (list 3))
                                (list (list 2) (list 2 3)))))
(append (subsets (list 2 3))
        (map (cons-x 1) (list (list) (list 3) (list 2) (list 2 3))))
(append (subsets (list 2 3))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (subsets (list 3))
                (map (cons-x 2) (subsets (list 3))))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (subsets (list 3))
                (map (cons-x 2) (append (subsets (list))
                                        (map (cons-x 3) (subsets (list))))))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (subsets (list 3))
                (map (cons-x 2) (append (subsets (list))
                                        (map (cons-x 3) (list (list))))))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (subsets (list 3))
                (map (cons-x 2) (append (subsets (list))
                                        (list (list 3)))))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (subsets (list 3))
                (map (cons-x 2) (append (list (list))
                                        (list (list 3)))))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (subsets (list 3))
                (map (cons-x 2) (list (list) (list 3))))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (subsets (list 3))
                (list (list 2) (list 2 3)))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (append (subsets (list))
                        (map (cons-x 3) (subsets (list))))
                (list (list 2) (list 2 3)))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (append (subsets (list))
                        (map (cons-x 3) (list (list))))
                (list (list 2) (list 2 3)))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (append (subsets (list))
                        (list (list 3)))
                (list (list 2) (list 2 3)))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (append (list (list))
                        (list (list 3)))
                (list (list 2) (list 2 3)))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (append (list (list) (list 3))
                (list (list 2) (list 2 3)))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(append (list (list) (list 3) (list 2) (list 2 3))
        (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
(list (list) (list 3) (list 2) (list 2 3) (list 1) (list 1 3) (list 1 2) (list 1 2 3))


;; 33
(define (accumulate f z xs)
 (if (null? xs)
     z
     (f (car xs) (accumulate f z (cdr xs)))))

(define (inc x) (+ x 1))

(define (map p sequence)
  (accumulate (lambda (x r) (cons (p x) r)) (list) sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x r) (inc r)) 0 sequence))

(map inc (list 0 1 2 3 4 5)) ; => (1 2 3 4 5 6)
(append (list 0 1 2) (list 3 4 5)) ; => (0 1 2 3 4 5)
(length (list 0 1 2 3 4 5)) ; => 6


;; 34
(define (accumulate f z xs)
  (if (null? xs)
      z
      (f (car xs) (accumulate f z (cdr xs)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1)) ; => 79


;; 35
(define (accumulate f z xs)
  (if (null? xs)
      z
      (f (car xs) (accumulate f z (cdr xs)))))

; (define (count-leaves x)
;   (cond ((null? x) 0)
;         ((not (pair? x)) 1)
;         (else (+ (count-leaves (car x))
;                  (count-leaves (cdr x))))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (cond ((null? x) 0)
                               ((not (pair? x)) 1)
                               (else (count-leaves x))))
                       t)))

(define x (cons (list 1 2) (list 3 4)))

(count-leaves x) ; => 4
(count-leaves (list x x)) ; => 8


;; 36
(define (accumulate f z xs)
  (if (null? xs)
      z
      (f (car xs) (accumulate f z (cdr xs)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))) ; => (22 26 30)


;; 37
(define (accumulate f z xs)
  (if (null? xs)
      z
      (f (car xs) (accumulate f z (cdr xs)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))

(transpose (list (list 1))) ; => 1
(transpose (list (list 1 2 3)
                 (list 4 5 6)
                 (list 7 8 9))) ; => ((1 4 7) (2 5 8) (3 6 9))

(matrix-*-vector (list (list 1 2 3)) (list 1 2 3)) ; => (14)

(matrix-*-matrix (list (list 1 2 3)) (list (list 1) (list 2) (list 3))) ; => ((14))
(matrix-*-matrix (list (list 1 2 3)
                       (list 4 5 6)
                       (list 7 8 9))
                 (list (list 1 0 0)
                       (list 0 1 0)
                       (list 0 0 1))) ; => ((1 2 3) (4 5 6) (7 8 9))


;; 38
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
                         (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; => (/ 1 (/ 2 (/ 3 1))) => 1.5
(fold-left  / 1 (list 1 2 3)) ; => (/ (/ (/ 1 1) 2) 3) => 0.16666666666666666

(fold-right list (list) (list 1 2 3)) ; => (1 (2 (3 ())))
(fold-left  list (list) (list 1 2 3)) ; => (((() 1) 2) 3)

; Note: fold-left and fold-right will produce the same value given the same initial and sequence inputs,
;       when the binary op they're folding the sequence over is commutative.


;; 39
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) (list) sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))

(reverse (list 0 1 2 3 4)) ; => (4 3 2 1 0)


;; 40
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (identity x) x)

(flatmap identity
         (list (list 1 2) (list 3 4))) ; => (1 2 3 4)
(flatmap identity
         (flatmap identity
                  (list (list (list 1) (list 2)) (list (list 3) (list 4))))) ; => (1 2 3 4)

(define (enumerate-ints from to)
  (define (enumerate-ints-iteratively result from to)
    (if (> from to)
        result
        (enumerate-ints-iteratively (append result (list from)) (+ from 1) to)))
  (enumerate-ints-iteratively (list) from to))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-ints 1 (- i 1))))
           (enumerate-ints 1 n)))

(define (prime-sum-pairs n)
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  (define (make-pair-sum pair)
    (let ((i (car pair))
          (j (cadr pair)))
      (list i j (+ i j))))
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))


;; 41
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (enumerate-ints from to)
  (define (enumerate-ints-iteratively result from to)
    (if (> from to)
        result
        (enumerate-ints-iteratively (append result (list from)) (+ from 1) to)))
  (enumerate-ints-iteratively (list) from to))

(define (list-triples-with-sum-less-than-or-equal-to n)
  (define (sum-triple x)
    (+ (car x) (cadr x) (caddr x)))
 (define (triple-with-sum-less-than-or-equal-to n)
   (lambda (x)
     (<= (sum-triple x) n)))
 (define (unique-triples-for n)
   (flatmap (lambda (i)
              (flatmap (lambda (j)
                         (map (lambda (k) (list i j k))
                              (enumerate-ints 1 (- j 1))))
                       (enumerate-ints 1 (- i 1))))
            (enumerate-ints 1 n)))
  (filter (triple-with-sum-less-than-or-equal-to n)
          (unique-triples-for n)))


(list-triples-with-sum-less-than-or-equal-to 1) ; => ()
(list-triples-with-sum-less-than-or-equal-to 2) ; => ()
(list-triples-with-sum-less-than-or-equal-to 3) ; => ()
(list-triples-with-sum-less-than-or-equal-to 4) ; => ()
(list-triples-with-sum-less-than-or-equal-to 5) ; => ()
(list-triples-with-sum-less-than-or-equal-to 6) ; => ((3 2 1))
(list-triples-with-sum-less-than-or-equal-to 7) ; => ((3 2 1) (4 2 1))
(list-triples-with-sum-less-than-or-equal-to 8) ; => ((3 2 1) (4 2 1) (4 3 1)         (5 2 1))
(list-triples-with-sum-less-than-or-equal-to 9) ; => ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1)                                 (6 2 1))
;                                           triples: ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3) (6 2 1) (6 3 1) (6 3 2) ...)
;                                              sums: (6       7       8       9       8       9       10      10      11      12      9       10      11      ...)


;;42
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (enumerate-interval from to)
  (define (enumerate-interval-iteratively result from to)
    (if (> from to)
        result
        (enumerate-interval-iteratively (append result (list from)) (+ from 1) to)))
  (enumerate-interval-iteratively (list) from to))

(define (queens board-size)
  (define empty-board (list))
  (define (adjoin-position row col positions)
    (cons (list row col) positions))
  (define (find-at-col k positions)
    (car (filter (lambda (p) (= k (cadr p))) positions)))
  (define (remove x positions)
    (filter (lambda (p) (not (and (= (car x) (car p))
                                  (= (cadr x) (cadr p)))))
            positions))
  (define (on-the-same-row-as x)
    (lambda (position)
      (= (car x) (car position))))
  (define (on-the-same-diagonal-as x)
    (lambda (position)
      (= (abs (- (car x) (car position)))
         (abs (- (cadr x) (cadr position))))))
  (define (safe? k positions)
    (let ((queen (find-at-col k positions)))
      (and (null? (filter (on-the-same-row-as queen) (remove queen positions)))
           (null? (filter (on-the-same-diagonal-as queen) (remove queen positions))))))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                  (flatmap (lambda (rest-of-queens)
                             (map (lambda (new-row)
                                    (adjoin-position new-row k rest-of-queens))
                                  (enumerate-interval 1 board-size)))
                           (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8) ; => (((4 8) (2 7) (7 6) (3 5) (6 4) (8 3) (5 2) (1 1)) ((5 8) (2 7) (4 6) (7 5) (3 4) (8 3) (6 2) (1 1)) ((3 8) (5 7) (2 6) (8 5) (6 4) (4 3) (7 2) (1 1)) ((3 8) (6 7) (4 6) (2 5) (8 4) (5 3) (7 2) (1 1))
;                ((5 8) (7 7) (1 6) (3 5) (8 4) (6 3) (4 2) (2 1)) ((4 8) (6 7) (8 6) (3 5) (1 4) (7 3) (5 2) (2 1)) ((3 8) (6 7) (8 6) (1 5) (4 4) (7 3) (5 2) (2 1)) ((5 8) (3 7) (8 6) (4 5) (7 4) (1 3) (6 2) (2 1))
;                ((5 8) (7 7) (4 6) (1 5) (3 4) (8 3) (6 2) (2 1)) ((4 8) (1 7) (5 6) (8 5) (6 4) (3 3) (7 2) (2 1)) ((3 8) (6 7) (4 6) (1 5) (8 4) (5 3) (7 2) (2 1)) ((4 8) (7 7) (5 6) (3 5) (1 4) (6 3) (8 2) (2 1))
;                ((6 8) (4 7) (2 6) (8 5) (5 4) (7 3) (1 2) (3 1)) ((6 8) (4 7) (7 6) (1 5) (8 4) (2 3) (5 2) (3 1)) ((1 8) (7 7) (4 6) (6 5) (8 4) (2 3) (5 2) (3 1)) ((6 8) (8 7) (2 6) (4 5) (1 4) (7 3) (5 2) (3 1))
;                ((6 8) (2 7) (7 6) (1 5) (4 4) (8 3) (5 2) (3 1)) ((4 8) (7 7) (1 6) (8 5) (5 4) (2 3) (6 2) (3 1)) ((5 8) (8 7) (4 6) (1 5) (7 4) (2 3) (6 2) (3 1)) ((4 8) (8 7) (1 6) (5 5) (7 4) (2 3) (6 2) (3 1))
;                ((2 8) (7 7) (5 6) (8 5) (1 4) (4 3) (6 2) (3 1)) ((1 8) (7 7) (5 6) (8 5) (2 4) (4 3) (6 2) (3 1)) ((2 8) (5 7) (7 6) (4 5) (1 4) (8 3) (6 2) (3 1)) ((4 8) (2 7) (7 6) (5 5) (1 4) (8 3) (6 2) (3 1))
;                ((5 8) (7 7) (1 6) (4 5) (2 4) (8 3) (6 2) (3 1)) ((6 8) (4 7) (1 6) (5 5) (8 4) (2 3) (7 2) (3 1)) ((5 8) (1 7) (4 6) (6 5) (8 4) (2 3) (7 2) (3 1)) ((5 8) (2 7) (6 6) (1 5) (7 4) (4 3) (8 2) (3 1))
;                ((6 8) (3 7) (7 6) (2 5) (8 4) (5 3) (1 2) (4 1)) ((2 8) (7 7) (3 6) (6 5) (8 4) (5 3) (1 2) (4 1)) ((7 8) (3 7) (1 6) (6 5) (8 4) (5 3) (2 2) (4 1)) ((5 8) (1 7) (8 6) (6 5) (3 4) (7 3) (2 2) (4 1))
;                ((1 8) (5 7) (8 6) (6 5) (3 4) (7 3) (2 2) (4 1)) ((3 8) (6 7) (8 6) (1 5) (5 4) (7 3) (2 2) (4 1)) ((6 8) (3 7) (1 6) (7 5) (5 4) (8 3) (2 2) (4 1)) ((7 8) (5 7) (3 6) (1 5) (6 4) (8 3) (2 2) (4 1))
;                ((7 8) (3 7) (8 6) (2 5) (5 4) (1 3) (6 2) (4 1)) ((5 8) (3 7) (1 6) (7 5) (2 4) (8 3) (6 2) (4 1)) ((2 8) (5 7) (7 6) (1 5) (3 4) (8 3) (6 2) (4 1)) ((3 8) (6 7) (2 6) (5 5) (8 4) (1 3) (7 2) (4 1))
;                ((6 8) (1 7) (5 6) (2 5) (8 4) (3 3) (7 2) (4 1)) ((8 8) (3 7) (1 6) (6 5) (2 4) (5 3) (7 2) (4 1)) ((2 8) (8 7) (6 6) (1 5) (3 4) (5 3) (7 2) (4 1)) ((5 8) (7 7) (2 6) (6 5) (3 4) (1 3) (8 2) (4 1))
;                ((3 8) (6 7) (2 6) (7 5) (5 4) (1 3) (8 2) (4 1)) ((6 8) (2 7) (7 6) (1 5) (3 4) (5 3) (8 2) (4 1)) ((3 8) (7 7) (2 6) (8 5) (6 4) (4 3) (1 2) (5 1)) ((6 8) (3 7) (7 6) (2 5) (4 4) (8 3) (1 2) (5 1))
;                ((4 8) (2 7) (7 6) (3 5) (6 4) (8 3) (1 2) (5 1)) ((7 8) (1 7) (3 6) (8 5) (6 4) (4 3) (2 2) (5 1)) ((1 8) (6 7) (8 6) (3 5) (7 4) (4 3) (2 2) (5 1)) ((3 8) (8 7) (4 6) (7 5) (1 4) (6 3) (2 2) (5 1))
;                ((6 8) (3 7) (7 6) (4 5) (1 4) (8 3) (2 2) (5 1)) ((7 8) (4 7) (2 6) (8 5) (6 4) (1 3) (3 2) (5 1)) ((4 8) (6 7) (8 6) (2 5) (7 4) (1 3) (3 2) (5 1)) ((2 8) (6 7) (1 6) (7 5) (4 4) (8 3) (3 2) (5 1))
;                ((2 8) (4 7) (6 6) (8 5) (3 4) (1 3) (7 2) (5 1)) ((3 8) (6 7) (8 6) (2 5) (4 4) (1 3) (7 2) (5 1)) ((6 8) (3 7) (1 6) (8 5) (4 4) (2 3) (7 2) (5 1)) ((8 8) (4 7) (1 6) (3 5) (6 4) (2 3) (7 2) (5 1))
;                ((4 8) (8 7) (1 6) (3 5) (6 4) (2 3) (7 2) (5 1)) ((2 8) (6 7) (8 6) (3 5) (1 4) (4 3) (7 2) (5 1)) ((7 8) (2 7) (6 6) (3 5) (1 4) (4 3) (8 2) (5 1)) ((3 8) (6 7) (2 6) (7 5) (1 4) (4 3) (8 2) (5 1))
;                ((4 8) (7 7) (3 6) (8 5) (2 4) (5 3) (1 2) (6 1)) ((4 8) (8 7) (5 6) (3 5) (1 4) (7 3) (2 2) (6 1)) ((3 8) (5 7) (8 6) (4 5) (1 4) (7 3) (2 2) (6 1)) ((4 8) (2 7) (8 6) (5 5) (7 4) (1 3) (3 2) (6 1))
;                ((5 8) (7 7) (2 6) (4 5) (8 4) (1 3) (3 2) (6 1)) ((7 8) (4 7) (2 6) (5 5) (8 4) (1 3) (3 2) (6 1)) ((8 8) (2 7) (4 6) (1 5) (7 4) (5 3) (3 2) (6 1)) ((7 8) (2 7) (4 6) (1 5) (8 4) (5 3) (3 2) (6 1))
;                ((5 8) (1 7) (8 6) (4 5) (2 4) (7 3) (3 2) (6 1)) ((4 8) (1 7) (5 6) (8 5) (2 4) (7 3) (3 2) (6 1)) ((5 8) (2 7) (8 6) (1 5) (4 4) (7 3) (3 2) (6 1)) ((3 8) (7 7) (2 6) (8 5) (5 4) (1 3) (4 2) (6 1))
;                ((3 8) (1 7) (7 6) (5 5) (8 4) (2 3) (4 2) (6 1)) ((8 8) (2 7) (5 6) (3 5) (1 4) (7 3) (4 2) (6 1)) ((3 8) (5 7) (2 6) (8 5) (1 4) (7 3) (4 2) (6 1)) ((3 8) (5 7) (7 6) (1 5) (4 4) (2 3) (8 2) (6 1))
;                ((5 8) (2 7) (4 6) (6 5) (8 4) (3 3) (1 2) (7 1)) ((6 8) (3 7) (5 6) (8 5) (1 4) (4 3) (2 2) (7 1)) ((5 8) (8 7) (4 6) (1 5) (3 4) (6 3) (2 2) (7 1)) ((4 8) (2 7) (5 6) (8 5) (6 4) (1 3) (3 2) (7 1))
;                ((4 8) (6 7) (1 6) (5 5) (2 4) (8 3) (3 2) (7 1)) ((6 8) (3 7) (1 6) (8 5) (5 4) (2 3) (4 2) (7 1)) ((5 8) (3 7) (1 6) (6 5) (8 4) (2 3) (4 2) (7 1)) ((4 8) (2 7) (8 6) (6 5) (1 4) (3 3) (5 2) (7 1))
;                ((6 8) (3 7) (5 6) (7 5) (1 4) (4 3) (2 2) (8 1)) ((6 8) (4 7) (7 6) (1 5) (3 4) (5 3) (2 2) (8 1)) ((4 8) (7 7) (5 6) (2 5) (6 4) (1 3) (3 2) (8 1)) ((5 8) (7 7) (2 6) (6 5) (3 4) (1 3) (4 2) (8 1)))


;; 43
(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

; 1
(flatmap (lambda (rest-of-queens)
           (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                (enumerate-interval 1 board-size)))
         (queen-cols (- k 1)))
(accumulate append
            (list)
            (map (lambda (rest-of-queens)
                   (map (lambda (new-row) (adjoin-position new-row k rest-of-queens)) (list 1 2 ... k)))
                 (queen-cols (- k 1))))

; 2
(flatmap (lambda (new-row)
           (map (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens))
                (queen-cols (- k 1))))
         (enumerate-interval 1 board-size))
(accumulate append
            (list)
            (map (lambda (new-row)
                   (map (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens)) (queen-cols (- k 1))))
                 (list 1 2 ... k)))
(accumulate append
            (list)
            (list (map (lambda (rest-of-queens) (adjoin-position 1 k rest-of-queens)) (queen-cols (- k 1)))
                  (map (lambda (rest-of-queens) (adjoin-position 2 k rest-of-queens)) (queen-cols (- k 1)))
                  ...
                  (map (lambda (rest-of-queens) (adjoin-position k k rest-of-queens)) (queen-cols (- k 1)))))

; (bench (lambda () (queens 1)))      ; => 1.6
; (bench (lambda () (queens-slow 1))) ; => 1.6

; (bench (lambda () (queens 2)))      ; => 3.6
; (bench (lambda () (queens-slow 2))) ; => 4.8

; (bench (lambda () (queens 3)))      ; => 8.2
; (bench (lambda () (queens-slow 3))) ; => 17.7

; (bench (lambda () (queens 4)))      ; => 21.3
; (bench (lambda () (queens-slow 4))) ; => 132.3

; (bench (lambda () (queens 5)))      ; => 65.8
; (bench (lambda () (queens-slow 5))) ; => 1633.7

; (bench (lambda () (queens 6)))      ; => 280.1
; (bench (lambda () (queens-slow 6))) ; => 26626.9


;; 44
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


;; 45
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (split first second)
  (define (split-recursively painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-recursively painter (- n 1))))
          (first painter (second smaller smaller)))))
  split-recursively)

(define right-split (split beside below))
(define up-split    (split below beside))


;; 46
(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


;; 47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))


(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))


;; 48
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


;; 49
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
                (draw-line ((frame-coord-map frame) (start-segment segment))
                           ((frame-coord-map frame) (end-segment segment))))
              segment-list)))

(define (outline frame)
  (let ((origin (origin-frame frame))
        (edge1  (edge1-frame frame))
        (edge2  (edge2-frame frame)))
    (let ((p0 origin)
          (p1 (add-vect origin edge1))
          (p2 (add-vect origin (add-vect edge1 edge2)))
          (p3 (add-vect origin edge2)))
     (segments->painter (list (make-segment p0 p1
                              (make-segment p1 p2)
                              (make-segment p2 p3)
                              (make-segment p3 p0)))))))

(define (cross frame)
  (let ((origin (origin-frame frame))
        (edge1  (edge1-frame frame))
        (edge2  (edge2-frame frame)))
    (let ((p0 origin)
          (p1 (add-vect origin edge1))
          (p2 (add-vect origin (add-vect edge1 edge2)))
          (p3 (add-vect origin edge2)))
      (segments->painter (list (make-segment p0 p2)
                               (make-segment p1 p3))))))

(define (diamond frame)
  (let ((origin (origin-frame frame))
        (edge1  (edge1-frame frame))
        (edge2  (edge2-frame frame)))
    (let ((p0 (add-vect origin (scale-vect 0.5 edge1)))
          (p1 (add-vect origin (add-vect edge1 (scale-vect 0.5 edge2))))
          (p2 (add-vect origin (add-vect edge2 (scale-vect 0.5 edge1))))
          (p3 (add-vect origin (scale-vect 0.5 edge2))))
      (segments->painter (list (make-segment p0 p1)
                               (make-segment p1 p2)
                               (make-segment p2 p3)
                               (make-segment p3 p0))))))

(define (wave frame)
  (let ((origin (origin-frame frame))
        (edge1  (edge1-frame frame))
        (edge2  (edge2-frame frame)))
    (let ((p0  (add-vect origin (scale-vect 0.2 edge1)))
          (p1  (add-vect origin (scale-vect 0.4 edge1)))
          (p2  (add-vect origin (add-vect (scale-vect 0.5 edge1) (scale-vect 0.2 edge2))))
          (p3  (add-vect origin (scale-vect 0.6 edge1)))
          (p4  (add-vect origin (scale-vect 0.8 edge1)))
          (p5  (add-vect origin (add-vect (scale-vect 0.6 edge1) (scale-vect 0.4 edge2))))
          (p6  (add-vect origin (add-vect edge1 (scale-vect 0.2 edge2))))
          (p7  (add-vect origin (add-vect edge1 (scale-vect 0.5 edge2))))
          (p8  (add-vect origin (add-vect (scale-vect 0.8 edge1) (scale-vect 0.6 edge2))))
          (p9  (add-vect origin (add-vect (scale-vect 0.6 edge1) (scale-vect 0.6 edge2))))
          (p10 (add-vect origin (add-vect (scale-vect 0.7 edge1) (scale-vect 0.8 edge2))))
          (p11 (add-vect origin (add-vect (scale-vect 0.6 edge1) edge2)))
          (p12 (add-vect origin (add-vect (scale-vect 0.4 edge1) edge2)))
          (p13 (add-vect origin (add-vect (scale-vect 0.3 edge1) (scale-vect 0.8 edge2))))
          (p14 (add-vect origin (add-vect (scale-vect 0.4 edge1) (scale-vect 0.6 edge2))))
          (p15 (add-vect origin (add-vect (scale-vect 0.3 edge1) (scale-vect 0.6 edge2))))
          (p16 (add-vect origin (add-vect (scale-vect 0.2 edge1) (scale-vect 0.4 edge2))))
          (p17 (add-vect origin (scale-vect 0.8 edge2)))
          (p18 (add-vect origin (scale-vect 0.6 edge2)))
          (p19 (add-vect origin (add-vect (scale-vect 0.2 edge1) (scale-vect 0.2 edge2))))
          (p20 (add-vect origin (add-vect (scale-vect 0.4 edge1) (scale-vect 0.4 edge2)))))
      (segments->painter (list (make-segment  p1  p2)
                               (make-segment  p2  p3)
                               (make-segment  p4  p5)
                               (make-segment  p5  p6)
                               (make-segment  p7  p8)
                               (make-segment  p8  p9)
                               (make-segment  p9 p10)
                               (make-segment p10 p11)
                               (make-segment p12 p13)
                               (make-segment p13 p14)
                               (make-segment p14 p15)
                               (make-segment p15 p16)
                               (make-segment p16 p17)
                               (make-segment p18 p19)
                               (make-segment p19 p20)
                               (make-segment p20  p0))))))


;; 50
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


;; 51
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter painter1
                                          (make-vect 0.0 0.0)
                                          split-point
                                          (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter2
                                          split-point
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left  frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painter painter1
                                           (make-vect 0.0 0.0)
                                           (make-vect 0.0 1.0)
                                           split-point))
          (paint-top    (transform-painter painter2
                                           split-point
                                           (make-vect 1.0 0.5)
                                           (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top    frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))


;; 52
; a
(define (wave frame)
  (let ((origin (origin-frame frame))
        (edge1  (edge1-frame frame))
        (edge2  (edge2-frame frame)))
    (let ((p0  (add-vect origin (scale-vect 0.2 edge1)))
          (p1  (add-vect origin (scale-vect 0.4 edge1)))
          (p2  (add-vect origin (add-vect (scale-vect 0.5 edge1) (scale-vect 0.2 edge2))))
          (p3  (add-vect origin (scale-vect 0.6 edge1)))
          (p4  (add-vect origin (scale-vect 0.8 edge1)))
          (p5  (add-vect origin (add-vect (scale-vect 0.6 edge1) (scale-vect 0.4 edge2))))
          (p6  (add-vect origin (add-vect edge1 (scale-vect 0.2 edge2))))
          (p7  (add-vect origin (add-vect edge1 (scale-vect 0.5 edge2))))
          (p8  (add-vect origin (add-vect (scale-vect 0.8 edge1) (scale-vect 0.6 edge2))))
          (p9  (add-vect origin (add-vect (scale-vect 0.6 edge1) (scale-vect 0.6 edge2))))
          (p10 (add-vect origin (add-vect (scale-vect 0.7 edge1) (scale-vect 0.8 edge2))))
          (p11 (add-vect origin (add-vect (scale-vect 0.6 edge1) edge2)))
          (p12 (add-vect origin (add-vect (scale-vect 0.4 edge1) edge2)))
          (p13 (add-vect origin (add-vect (scale-vect 0.3 edge1) (scale-vect 0.8 edge2))))
          (p14 (add-vect origin (add-vect (scale-vect 0.4 edge1) (scale-vect 0.6 edge2))))
          (p15 (add-vect origin (add-vect (scale-vect 0.3 edge1) (scale-vect 0.6 edge2))))
          (p16 (add-vect origin (add-vect (scale-vect 0.2 edge1) (scale-vect 0.4 edge2))))
          (p17 (add-vect origin (scale-vect 0.8 edge2)))
          (p18 (add-vect origin (scale-vect 0.6 edge2)))
          (p19 (add-vect origin (add-vect (scale-vect 0.2 edge1) (scale-vect 0.2 edge2))))
          (p20 (add-vect origin (add-vect (scale-vect 0.4 edge1) (scale-vect 0.4 edge2))))
          (s0  (add-vect origin (add-vect (scale-vect 0.4 edge1) (scale-vect 0.8 edge2))))  ; added
          (s1  (add-vect origin (add-vect (scale-vect 0.5 edge1) (scale-vect 0.7 edge2))))  ; added
          (s2  (add-vect origin (add-vect (scale-vect 0.6 edge1) (scale-vect 0.8 edge2))))) ; added
      (segments->painter (list (make-segment  p1  p2)
                               (make-segment  p2  p3)
                               (make-segment  p4  p5)
                               (make-segment  p5  p6)
                               (make-segment  p7  p8)
                               (make-segment  p8  p9)
                               (make-segment  p9 p10)
                               (make-segment p10 p11)
                               (make-segment p12 p13)
                               (make-segment p13 p14)
                               (make-segment p14 p15)
                               (make-segment p15 p16)
                               (make-segment p16 p17)
                               (make-segment p18 p19)
                               (make-segment p19 p20)
                               (make-segment p20  p0)
                               (make-segment  s0  s1)      ; added
                               (make-segment  s1  s2)))))) ; added

; b
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up    (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left     up)    ; changed
              (bottom-right right) ; changed
              (corner       (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; c
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top    (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180     ; modified
                                  identity  flip-horiz))) ; modified
    (combine4 (corner-split painter n))))


;; 53
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c) ; => (a b c)

(list (list 'george)) ; => ((george))
(cdr '((x1 x2) (y1 y2))) ; => ((y1 y2))

(cadr '((x1 x2) (y1 y2))) ; => (y1 y2)
(pair? (car '(a short list))) ; => #f
(memq 'red '((red shoes) (blue socks))) ; => #f

(memq 'red '(red shoes blue socks)) ; => (red shoes blue socks)


;; 54
(define (equal? xs ys)
  (cond ((null? xs) (null? ys))
        ((null? ys) (null? xs))
        (else (let ((x (car xs))
                    (y (car ys)))
                (cond ((or (and (pair? x) (not (pair? y)))
                           (and (not (pair? x)) (pair? y))) #f)
                      ((and (pair? x) (pair? y)) (and (equal? x y) (eq? x y) (equal? (cdr xs) (cdr ys))))
                      (else (and (eq? x y) (equal? (cdr xs) (cdr ys)))))))))

(equal? '(this is a list) '(this is a list)) ; => #t
(equal? '(this is a list) '(this (is a) list)) ; => #f


;; 55
(car ''abracadabra) ; => quote
(car '(quote abracadabra))
(car (quote (quote abracadabra)))
;            |car| |   cdr   |


;; 56
(define (=number? x n)
  (and (number? x) (= x n)))

(define (variable? v)
  (symbol? v))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp))))
        ((exponentiation? exp) (make-product (make-product (exponent exp)
                                                           (make-exponentiation (base exp) (- (exponent exp) 1)))
                                             (deriv (base exp) var)))
        (else (error "unknown expression type -- DERIV" exp))))



