;; 1
(define (make-accumulator v)
  (lambda (x)
    (set! v (+ v x))
    v))

(define A (make-accumulator 5))
(A 10) ; => 15
(A 10) ; => 25


;; 2
(define (make-monitored f)
  (let ((invocation-count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) invocation-count)
            ((eq? x 'reset-count) (set! invocation-count 0) invocation-count)
            (else (set! invocation-count (+ invocation-count 1)) (f x))))))

(define s (make-monitored sqrt))
(s 100) ; => 10
(s 'how-many-calls?) ; => 1


;; 3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE_ACCOUNT" m)))
        (lambda (_) "Incorrect password")))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40) ; => 60
((acc 'some-other-password 'deposit) 50) ; => "Incorrect password"


;; 4
(define (make-account balance password)
  (let ((password-attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops)
      "Dispatching the police squad")
    (define (dispatch p m)
      (if (eq? p password)
          (begin (set! password-attempts 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request -- MAKE_ACCOUNT" m))))
          (if (< password-attempts 7)
              (begin (set! password-attempts (+ password-attempts 1))
                     (lambda (_) "Incorrect password"))
              call-the-cops)))
    dispatch))

(define acc (make-account 100 'secret-password))

((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'secret-password 'withdraw) 10) ; => 90

((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Incorrect password"
((acc 'some-other-password 'withdraw) 10) ; => "Dispatching the police squad"


(define (sq x) (* x x))

;; 5
(define (random-in-range from to)
  (let ((x (random-real)))
    (+ (* (- to from) x) from)))

(define (monte-carlo trials experiment)
  (define (monte-carlo-iteratively trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (monte-carlo-iteratively (- trials-remaining 1) (+ trials-passed 1)))
          (else (monte-carlo-iteratively (- trials-remaining 1) trials-passed))))
  (monte-carlo-iteratively trials 0))

(define (estimate-integral trials p x1 x2 y1 y2)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (p x y)))
  (* (* (- x2 x1) (- y2 y1)) (monte-carlo trials experiment)))

(define (make-circle x0 y0 r)
  (lambda (x y)
    (<= (+ (sq (- x x0)) (sq (- y y0))) (sq r))))

(estimate-integral 100 (make-circle 2 2 1) 0 4 0 4) ; => 4

(estimate-integral 10000 (make-circle 2 2 1) 1 3 1 3) ; => 3.1412
(estimate-integral 10000 (make-circle 2 2 1) 1 3 1 3) ; => 3.1368
(estimate-integral 10000 (make-circle 2 2 1) 1 3 1 3) ; => 3.1592
(estimate-integral 10000 (make-circle 2 2 1) 1 3 1 3) ; => 3.1244
(estimate-integral 10000 (make-circle 2 2 1) 1 3 1 3) ; => 3.1188


;; 6
(define (make-random)
  (let ((seed 1))
    (lambda (m)
      (cond ((eq? m 'generate) (begin (set! seed (mod (* 7 seed) 11)) seed))
            ((eq? m 'reset) (lambda (v) (set! seed v)))
            (else (error "Unknown request -- RAND" m))))))

(define rand (make-random))

((rand 'reset) 42)
(rand 'generate) ; => 8
(rand 'generate) ; => 1
(rand 'generate) ; => 7
(rand 'generate) ; => 5

((rand 'reset) 42)
(rand 'generate) ; => 8
(rand 'generate) ; => 1
(rand 'generate) ; => 7
(rand 'generate) ; => 5


;; 7
(define (contains? xs x)
  (cond ((null? xs) #f)
        ((eq? (car xs) x) #t)
        (else (contains? (cdr xs) x))))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
 (define (deposit amount)
   (set! balance (+ balance amount))
   balance)
 (let ((passwords (list password)))
   (define (add-password p)
     (set! passwords (cons p passwords)))
   (define (dispatch p m)
     (if (contains? passwords p)
         (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               ((eq? m 'add-password) add-password)
               (else (error "Unknown request -- MAKE_ACCOUNT" m)))
         (lambda _ "Incorrect password")))
   dispatch))

(define (make-joint account password new-password)
  ((account password 'add-password) new-password)
  account)

(define peter-acc
  (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 10) ; => 90
((paul-acc  'rosebud     'withdraw) 10) ; => 80

; Note: It is possible to interact w/ Peter's account using Paul's password, and vice versa.
((peter-acc 'rosebud     'withdraw) 10) ; => 70
((paul-acc  'open-sesame 'withdraw) 10) ; => 60


;; 8
(define (once f)
  (let ((invoked? #f))
    (lambda (x)
      (if invoked?
          0
          (begin (set! invoked? #t)
                 (f x))))))

(define f (once identity))

(+ (f 0) (f 1))

; Left -> Right
(f 0)
invoked? ; => #f
(set! invoked? #t)
(identity 0)
0

(f 1)
invoked? ; => #t
0

(+ 0 0)
0

; Left <- Right
(f 1)
invoked? ; => #f
(set! invoked? #t)
(identity 1)
1

(f 0)
invoked? ; => #t
0

(+ 0 1)
1


;; 12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
; x -> [.][.] -> [.][/]
;       |        |
;       v        v
;      [a]      [b]

(define y (list 'c 'd))
; y -> [.][.] -> [.][/]
;       |         |
;       v         v
;      [c]       [d]

(define z (append x y))
; x -> [.][.] -> [.][/]
;       |         |
;       v         v
;      [a]       [b]
;       ^         ^
;       |         |
; z -> [.][.] -> [.][.] -> [.][.] -> [.][/]
;                           |         |
;                           v         v
;                          [c]       [d]
;                           ^         ^
;                           |         |
;                     y -> [.][.] -> [.][/]

z ; => (a b c d)
(cdr x) ; => (b)

(define w (append! x y))
;       x                   y
;       |                   |
;       v                   v
; w -> [.][.] -> [.][.] -> [.][.] -> [.][.]
;       |         |         |         |
;       v         v         v         v
;      [a]       [b]       [c]       [d]

w ; => (a b c d)
(cdr x) ; => (b c d)


;; 13
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
;       +----------------------+
;       |                      |
;       v                      |
; z -> [.][.] -> [.][.] -> [.][.]
;       |         |         |
;       v         v         v
;      [a]       [b]       [c]

(last-pair '(a b c a b c a b c ...))
(last-pair '(b c a b c a b c a ...))
(last-pair '(c a b c a b c a b ...))
(last-pair '(a b c a b c a b c ...))
...


;; 14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
; v -> [.][.] -> [.][.] -> [.][.] -> [.][/]
;       |         |         |         |
;       v         v         v         v
;      [a]       [b]       [c]       [d]

(define w (mystery v))
; #0
; (loop x y)
;                 temp
;                 |
;                 v
; x -> [.][.] -> [.][.] -> [.][.] -> [.][/]    y -> [/]
;       |         |         |         |
;       v         v         v         v
;      [a]       [b]       [c]       [d]

; (set-cdr! x y)
; x -> [.][/]
;       |
;       v
;      [a]

; #1
; (loop x y)
;                 temp
;                 |
;                 v
; x -> [.][.] -> [.][.] -> [.][/]    y -> [.][/]
;       |         |         |              |
;       v         v         v              v
;      [b]       [c]       [d]            [a]

; (set-cdr! x y)
; x -> [.][.] -> [.][/]
;       |         |
;       v         v
;      [b]       [a]

; #2
; (loop x y)
;                 temp
;                 |
;                 v
; x -> [.][.] -> [.][/]    y -> [.][.] -> [.][/]
;       |         |              |         |
;       v         v              v         v
;      [c]       [d]            [b]       [a]

; (set-cdr! x y)
; x -> [.][.] -> [.][.] -> [.][/]
;       |         |         |
;       v         v         v
;      [c]       [b]       [a]

; #3
; (loop x y)
;          temp
;          |
;          v
; x -> [.][/]    y -> [.][.] -> [.][.] -> [.][/]
;       |              |         |         |
;       v              v         v         v
;      [d]            [c]       [b]       [a]

; (set-cdr! x y)
; x -> [.][.] -> [.][.] -> [.][.] -> [.][/]
;       |         |         |         |
;       v         v         v         v
;      [d]       [c]       [b]       [a]

; #4
; (loop x y)
; x -> [.][.] -> [.][.] -> [.][.] -> [.][/]
;       |         |         |         |
;       v         v         v         v
;      [d]       [c]       [b]       [a]

v ; => (a)
w ; => (d c b a)


;; 15
(define x (list 'a 'b))

(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1) ; => ((wow b) wow b)
; z1 -> [.][.]
;        |  |
;        v  v
;  x -> [.][.] -> [.][/]
;        |         |
;        v         v
;       [wow]     [b]

(set-to-wow! z2) ; => ((wow b) a b)
;           +---> [.][.] -> [.][/]
;           |      |         |
;           |      v         v
; z1 -> [.][.]    [a]       [b]
;        |                   ^
;        |                   |
;        +------> [.][.] -> [.][/]
;                  |
;                  v
;                 [wow]


;; 16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; 3 pairs
; x -> [.][.] -> [.][.] -> [.][/]
;       |         |         |
;       v         v         v
;      [0]       [1]       [2]

(define x (cons 0 (cons 1 (cons 2 '()))))
(count-pairs x) ; => 3

; 4 pairs
; x -> [.][.] -> [.][.] <- y
;       |         |  |
;       v         v  v
;      [0]       [.][.] <- z
;                 |  |
;                 v  v
;                [1][2]

(define z (cons 1 2))
(define y (cons z z))
(define x (cons 0 y))
(count-pairs x) ; => 4

; 7 pairs
; x -> [.][.]
;       |  |
;       v  v
; y -> [.][.]
;       |  |
;       v  v
; z -> [.][.]
;       |  |
;       v  v
;      [0][0]

(define z (cons 0 0))
(define y (cons z z))
(define x (cons y y))
(count-pairs x) ; => 7

; inf pairs
;       +----------------------+
;       |                      |
;       v                      |
; x -> [.][.] -> [.][.] -> [.][.]
;       |         |         |
;       v         v         v
;      [0]       [1]       [2]


;; 17
(define (contains-pointer? ps p)
  (cond ((null? ps) #f)
        ((eq? (car ps) p) #t)
        (else (contains-pointer? (cdr ps) p))))

(define (count-pairs x)
  (let ((visited '()))
    (define (count-pairs-recursively x)
      (cond ((not (pair? x)) 0)
            ((contains-pointer? visited x) 0)
            (else (set! visited (cons x visited))
                  (+ 1
                     (count-pairs-recursively (car x))
                     (count-pairs-recursively (cdr x))))))
    (count-pairs-recursively x)))

(let ((z (cons 1 2)))
  (let ((y (cons z z)))
    (let ((x (cons 0 y)))
      (count-pairs x)))) ; => 3 (was 4)

(let ((z (cons 0 0)))
  (let ((y (cons z z)))
    (let ((x (cons y y)))
      (count-pairs x)))) ; => 3 (was 7)


;; 18
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (contains-pointer? ps p)
  (cond ((null? ps) #f)
        ((eq? (car ps) p) #t)
        (else (contains-pointer? (cdr ps) p))))

(define p '(0 1 2))
(define x (make-cycle p))

(eq? p x) ; => #t
(eq? p (cdr x)) ; => #f
(eq? p (cddr x)) ; => #f
(eq? p (cdddr x)) ; => #t

(define visited '())
(set! visited (cons x visited))

(contains-pointer? visited x) ; => #t
(contains-pointer? visited (cdr x)) ; => #f
(contains-pointer? visited (cddr x)) ; => #f
(contains-pointer? visited (cdddr x)) ; => #t

(define (has-cycles? x)
  (let ((visited '()))
    (define (has-cycles-recursively? x)
      (cond ((not (pair? x)) #f)
            ((contains-pointer? visited x) #t)
            (else (set! visited (cons x visited))
                  (or (has-cycles-recursively? (car x)))
                      (has-cycles-recursively? (cdr x)))))
    (has-cycles-recursively? x)))

(has-cycles? '(0 1 2)) ; => #f
(has-cycles? x) ; => #t
(has-cycles? p) ; => #t (p was modified to create x)

(define c (cons 'c0 'c1))
(define left (cons 'a0 (cons 'a1 (cons 'a2 c))))
(define right (cons 'b0 (cons 'b1 (cons 'b2 c))))
(define x (cons left right))

(has-cycles? x) ; => #t


(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue) (cons '() '()))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called w/ an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called w/ an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;; 21
(define q1 (make-queue))
(insert-queue! q1 'a) ; => ((a) a)
(insert-queue! q1 'b) ; => ((a b) b)
(delete-queue! q1)    ; => ((b) b)
(delete-queue! q1)    ; => (() b)

; The standard Lisp printer prints the queue as a pair of lists that the front-ptr and the rear-ptr point to.

; (insert-queue! q1 'a) creates the structure as follows:
; q1 -> [.][.]
;        |  |
;        v  v
;       [.][/]
;        |
;        v
;       [a]
; Both front-ptr and rear-ptr point to the same (shared) item pair.
; Printing the queue pair causes the item pair to be printed twice: first as the car of the queue pair and then as the cdr of the queue pair.

; (insert-queue! q1 'b) mutates the structure as follows:
; q1 -> [.][.] ----+
;        |         |
;        v         v
;       [.][.] -> [.][/]
;        |         |
;        v         v
;       [a]       [b]
; The front-ptr points to the previously created pair. The rear-ptr points to the newly created pair.
; Printing the queue pair results in printing the list to which front-ptr points followed by the list to which the rear-ptr points.

; (delete-queue! q1) mutates the structure as follows:
; q1 -> [.][.]--------+
;        +---------+  |
;                  v  v
;       [.][.] -> [.][/]
;        |         |
;        v         v
;       [a]       [b]
; Both front-ptr and rear-ptr point to the same item pair.
; Printing the queue pair causes the item pair to be printed twice: first as the car of the queue pair and then as the cdr of the queue pair.

; (delete-queue! q1) mutates the structure as follows:
; q1 -> [.][.]------+
;        |          |
;        v          v
;       [/]        [.][/]
;                   |
;                   v
;                  [b]
; The front-ptr points to the empty list. The rear-ptr points to the deleted pair (this behaviour is the implementation detail of delete-queue! procedure).
; Printing the queue pair results in printing the empty list to which front-ptr points followed by the list to which the rear-ptr happened to point before the addition.

(define (print-queue queue)
  (display (front-ptr queue))
  (newline))

(define q2 (make-queue))
(print-queue (insert-queue! q2 'a)) ; prints (a)
(print-queue (insert-queue! q2 'b)) ; prints (a b)
(print-queue (delete-queue! q2))    ; prints (b)
(print-queue (delete-queue! q2))    ; prints ()


;; 22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called w/ an empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
        (else
         (set-cdr! rear-ptr new-pair)
         (set! rear-ptr new-pair)
         front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called w/ an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (error "Undefined operation -- MAKE-QUEUE" m))))
    dispatch))

(define (empty-queue? queue) ((queue 'empty-queue?)))
(define (front-queue queue) ((queue 'front-queue)))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) ((queue 'delete-queue!)))

(define q (make-queue))
(empty-queue? q)     ; => #t

(insert-queue! q 'a) ; => (a)
(empty-queue? q)     ; => #f
(front-queue q)      ; => a

(insert-queue! q 'b) ; => (a b)
(empty-queue? q)     ; => #f
(front-queue q)      ; => a

(delete-queue! q)    ; => (b)
(empty-queue? q)     ; => #f
(front-queue q)      ; => b

(delete-queue! q)    ; => ()
(empty-queue? q)     ; => #t


;; 23
(define (prev-ptr item) (cadr item))
(define (set-prev-ptr! item v) (set-car! (cdr item) v))

(define (next-ptr item) (cddr item))
(define (set-next-ptr! item v) (set-cdr! (cdr item) v))

(define (make-item x) (cons x (cons '() '())))

(define 0th (make-item 0))
0th ; => (0 ())
(prev-ptr 0th) ; => ()
(next-ptr 0th) ; => ()

(define 1st (make-item 1))
1st ; => (1 ())
(prev-ptr 1st) ; => ()
(next-ptr 1st) ; => ()

(set-next-ptr! 0th 1st)
0th ; => (0 () 1 ())
(prev-ptr 0th) ; => ()
(next-ptr 0th) ; => (1 ())

(set-next-ptr! 0th '())
0th ; => (0 ())
(prev-ptr 0th) ; => ()
(next-ptr 0th) ; => ()

(set-prev-ptr! 1st 0th)
1st ; => (1 (0 ()))
(next-ptr 1st) ; => ()
(prev-ptr 1st) ; => (0 ())

(define (front-ptr deque) (car deque))
(define (set-front-ptr! deque item) (set-car! deque item))

(define (rear-ptr deque) (cdr deque))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (make-deque) (cons '() '()))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called w/ an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called w/ an empty deque" deque)
      (car (rear-ptr deque))))

(define (deque->list deque)
  (define (traverse-items item)
    (if (null? item)
        '()
        (cons (car item) (traverse-items (next-ptr item)))))
  (traverse-items (front-ptr deque)))

(define dm (make-deque))
(deque->list dm) ; => ()

(define (front-insert-deque! deque x)
  (let ((item (make-item x)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque item)
           (set-rear-ptr! deque item)
           deque)
          (else
           (set-prev-ptr! (front-ptr deque) item)
           (set-next-ptr! item (front-ptr deque))
           (set-front-ptr! deque item)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called w/ an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque ))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         deque)
        (else
         (set-prev-ptr! (next-ptr (front-ptr deque)) '())
         (set-front-ptr! deque (next-ptr (front-ptr deque)))
         deque)))

(define df (make-deque))
(deque->list df)                          ; => ()
(deque->list (front-insert-deque! df 'a)) ; => (a)
(deque->list (front-insert-deque! df 'b)) ; => (b a)
(deque->list (front-delete-deque! df))    ; => (a)
(deque->list (front-delete-deque! df))    ; => ()

(define (rear-insert-deque! deque x)
  (let ((item (make-item x)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque item)
           (set-rear-ptr! deque item)
           deque)
          (else
           (set-prev-ptr! item (rear-ptr deque))
           (set-next-ptr! (rear-ptr deque) item)
           (set-rear-ptr! deque item)
           deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called w/ an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         deque)
        (else
         (set-next-ptr! (prev-ptr (rear-ptr deque)) '())
         (set-rear-ptr! deque (prev-ptr (rear-ptr deque)))
         deque)))

(define dr (make-deque))
(deque->list dr)                         ; => ()
(deque->list (rear-insert-deque! dr 'a)) ; => (a)
(deque->list (rear-insert-deque! dr 'b)) ; => (a b)
(deque->list (rear-delete-deque! dr))    ; => (a)
(deque->list (rear-delete-deque! dr))    ; => ()


;; 24
(define (assoc same-key? key records)
  (cond ((null? records) #f)
        ((same-key? key (caar records)) (car records))
        (else (assoc same-key? key (cdr records)))))

(define (make-table same-key?)
  (let ((table (list '*table*)))
    (define (lookup k1 k2)
      (let ((subtable (assoc same-key? k1 (cdr table))))
        (if subtable
            (let ((record (assoc same-key? k2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! k1 k2 v)
      (let ((subtable (assoc same-key? k1 (cdr table))))
        (if subtable
            (let ((record (assoc same-key? k2 (cdr subtable))))
              (if record
                  (set-cdr! record v)
                  (set-cdr! subtable (cons (cons k2 v) (cdr subtable)))))
            (set-cdr! table (cons (list k1 (cons k2 v)) (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (within-tolerance tolerance)
  (lambda (x y)
    (cond ((and (number? x) (number? y)) (<= (abs (- x y)) tolerance))
          (else (equal? x y)))))

(define t (make-table (within-tolerance 0.1)))

((t 'insert!) 'numbers 0 '0th)
((t 'insert!) 'numbers 1 '1st)
((t 'insert!) 'numbers 2 '2nd)

((t 'lookup) 'numbers 0) ; => 0th
((t 'lookup) 'numbers 0.01) ; => 0th
((t 'lookup) 'numbers 1) ; => 1st
((t 'lookup) 'numbers 1.01) ; => 1st
((t 'lookup) 'numbers 2) ; => 2nd
((t 'lookup) 'numbers 2.01) ; => 2nd


;; 25
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table) (cons '*table* '()))

(define (lookup table keys)
  (if (null? keys)
      (cdr table)
      (let ((subtable (assoc (car keys) (cdr table))))
        (if subtable
            (lookup subtable (cdr keys))
            #f))))

(define (insert! table keys v)
  (if (null? keys)
      (set-cdr! table v)
      (let ((subtable (assoc (car keys) (cdr table))))
        (if subtable
            (insert! subtable (cdr keys) v)
            (let ((created (cons (car keys) '())))
              (set-cdr! table (cons created (cdr table)))
              (insert! created (cdr keys) v)))))
  table)

(define t0 (make-table))
(insert! t0 '() 0) ; => (*table* . 0)
(lookup t0 '())    ; => 0

(define t1 (make-table))
(insert! t1 '(x) 0) ; => (*table* (x . 0))
(lookup t1 '())     ; =>         ((x . 0))
(lookup t1 '(x))    ; =>               0

(define t2 (make-table))
(insert! t2 '(x y) 0) ; => (*table* (x (y . 0)))
(lookup t2 '())       ; =>         ((x (y . 0)))
(lookup t2 '(x))      ; =>            ((y . 0))
(lookup t2 '(x y))    ; =>                  0

(define t3 (make-table))
(insert! t3 '(x y z) 0) ; => (*table* (x (y (z . 0))))
(lookup t3 '())         ; =>         ((x (y (z . 0))))
(lookup t3 '(x))        ; =>            ((y (z . 0)))
(lookup t3 '(x y))      ; =>               ((z . 0))
(lookup t3 '(x y z))    ; =>                     0


(define (get-signal wire) ...)
(define (set-signal! wire x) ...)
(define (add-action! wire f) ...)

(define (after-delay delay f) ...)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signal" (list s1 s2)))))

(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signal" (list s1 s2)))))

(define (inverter in out)
  (define (on-change)
    (let ((x (logical-not (get-signal in))))
      (after-delay inverter-delay (lambda () (set-signal! out x)))))
  (add-action! in on-change)
  'ok)

(define (and-gate in1 in2 out)
  (define (on-change)
    (let ((x (logical-and (get-signal in1) (get-signal in2))))
      (after-delay and-gate-delay (lambda () (set-signal! out x)))))
  (add-action! in1 on-change)
  (add-action! in2 on-change)
  'ok)

;; 28
(define (or-gate in1 in2 out)
  (define (on-change)
    (let ((x (logical-or (get-signal in1) (get-signal in2))))
      (after-delay or-gate-delay (lambda () (set-signal! out x)))))
  (add-action! in1 on-change)
  (add-action! in2 on-change)
  'ok)


;; 29
;      +--------------------------------------------------------+
;      |   +----------+                                         |
; A ---+---+ inverter +---+                                     |
;      |   +----------+   +---+----------+       +----------+   |
;      |                      | and-gate +-------+ inverter +---|--- C
;      |   +----------+   +---+----------+       +----------+   |
; B ---+---+ inverter +---+                                     |
;      |   +----------+                                         |
;      +--------------------------------------------------------+
;      |<---------------->|<---------------->|<---------------->|
;         inverter-delay     and-gate-delay     inverter-delay

; total-delay
; (+ (* 2 inverter-delay) and-gate-delay)

(define (or-gate in1 in2 out)
  (let ((a1 (make-wire))
        (a2 (make-wire))
        (i3 (make-wire)))
    (inverter in1 a1)
    (inverter in2 a2)
    (and-gate a1 a2 i3)
    (inverter i3 out))
    'ok)


(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 out)
    'ok))

;; 30
(define (ripple-carry-adder as bs ss c)
  (if (or (null? as) (null? bs) (null? ss))
      'ok
      (let ((c-out (make-wire)))
        (full-adder (car as) (car bs) c (car ss) c-out)
        (ripple-carry-adder (cdr as) (cdr bs) (cdr ss) c-out))))


(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


(define inverter-delay 2)

(define (inverter in out)
  (define (on-change)
    (let ((x (logical-not (get-signal in))))
      (after-delay inverter-delay (lambda () (set-signal! out x)))))
  (add-action! in on-change)
  'ok)

(define and-gate-delay 3)

(define (and-gate in1 in2 out)
  (define (on-change)
    (let ((x (logical-and (get-signal in1) (get-signal in2))))
      (after-delay and-gate-delay (lambda () (set-signal! out x)))))
  (add-action! in1 on-change)
  (add-action! in2 on-change)
  'ok)

(define or-gate-delay 5)

(define (or-gate in1 in2 out)
  (define (on-change)
    (let ((x (logical-or (get-signal in1) (get-signal in2))))
      (after-delay or-gate-delay (lambda () (set-signal! out x)))))
  (add-action! in1 on-change)
  (add-action! in2 on-change)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (probe name wire)
  (add-action! wire (lambda ()
                      (newline)
                      (display name)
                      (display " ")
                      (display (current-time the-agenda))
                      (display " New-value = ")
                      (display (get-signal wire)))))


(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)

;; 31
; (half-adder ...)
; -> (or-gate ...)
; -> (add-action! ...)
; -> (set! action-procedures ...)
;    (proc)
; -> (on-change ...)
; -> (after-delay ...)
; -> (add-to-agenda! ...)

; The action procedure call in accept-action-procedure! (local to the make-wire definition) is necessary for the signal modification procedure to be added to the-agenda at an appropriate time offset.
; The action procedures for function boxes (in their definition) are responsible for scheduling themselves to the-agenga by means of a call to the after-delay.
; FIXME


(define (for-each-except exception f xs)
  (define (iterate items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (iterate (cdr items)))
          (else (f (car items))
                (iterate (cdr items)))))
  (iterate xs))

(define (inform-about-value constraint) (constraint 'I-have-a-value))
(define (inform-about-no-value constraint) (constraint 'I-lost-my-value))

(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter inform-about-value constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor inform-about-no-value constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?) (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    me))

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant) ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor) ((connector 'forget) retractor))
(define (connect connector new-constraint) ((connector 'connect) new-constraint))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum (+ (get-value a1) (get-value a2)) me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2 (- (get-value sum) (get-value a1)) me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1 (- (get-value sum) (get-value a2)) me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product (* (get-value m1) (get-value m2)) me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2 (/ (get-value product) (get-value m1)) me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1 (/ (get-value product) (get-value m2)) me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

;; 33
; FIXME


;; 38
; Peter: (set! balance (+ balance 10))
; Paul:  (set! balance (- balance 20))
; Mary:  (set! balance (- balance (/ balance 2)))

; Before: 100

;; a
; (1) Peter -> Paul -> Mary
(+ 100 10) ; => 110
(- 110 20) ; => 90
(- 90 (/ 90 2)) ; => 45
; After: 45

; (2) Peter -> Mary -> Paul
(+ 100 10) ; => 110
(- 110 (/ 110 2)) ; => 55
(- 55 20) ; => 35
; After: 35

; (3) Paul -> Peter -> Mary
(- 100 20) ; => 80
(+ 80 10) ; => 90
(- 90 (/ 90 2)) ; => 45
; After: 45

; (4) Paul -> Mary -> Peter
(- 100 20) ; => 80
(- 80 (/ 80 2)) ; => 40
(+ 40 10) ; => 50
; After: 50

; (5) Mary -> Peter -> Paul
(- 100 (/ 100 2)) ; => 50
(+ 50 10) ; => 60
(- 60 20) ; => 40
; After: 40

; (6) Mary -> Paul -> Peter
(- 100 (/ 100 2)) ; => 50
(- 50 20) ; => 30
(+ 30 10) ; => 40
; After: 40

;; b
; FIXME


;; 39
(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

; Possible events:
; (1) P1 accesses and calculates x times x.
; (2) P1 sets x to x times x.
; (3) P2 accesses and increments x.

; Note:
; (1) happens before (2).

; (1) -> (2) -> (3): 101
; (1) -> (3) -> (2): 100
; (3) -> (1) -> (2): 121


;; 40
(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

; Possible events:
; (1) P1 accesses x the first time during the evaluation of (* x x).
; (2) P1 accesses x the second time during the evaluation of (* x x).
; (3) P1 sets the value of x to the square of x.
; (4) P2 accesses x the first time during the evaluation of (* x x x).
; (5) P2 accesses x the second time during the evaluation of (* x x x).
; (6) P2 accesses x the third time during the evaluation of (* x x x).
; (7) P2 sets the value of x to the cube of x.

; Notes:
; (1) -> (2) is equivalent to (2) -> (1).
; (3) happens after (1) and (2).
; (4) -> (5) is equivalent to (5) -> (4), (5) -> (6) is equivalent to (6) -> (5), (4) -> (6) is equivalent to (6) -> (4).
; (7) happens after (4), (5), and (6).

; (1) -> (2) -> (3) -> (4) -> (5) -> (6) -> (7):
(* (* 10 10) (* 10 10) (* 10 10)) ; => 1000000

; (1) -> (2) -> (4) -> (3) -> (5) -> (6) -> (7):
(* 10 (* 10 10) (* 10 10)) ; => 100000

; (1) -> (2) -> (4) -> (5) -> (3) -> (6) -> (7):
(* 10 10 (* 10 10)) ; => 10000

; (1) -> (2) -> (4) -> (5) -> (6) -> (3) -> (7):
(* 10 10 10) ; => 1000

; (1) -> (2) -> (4) -> (5) -> (6) -> (7) -> (3):
(* 10 10) ; => 100

; (1) -> (4) -> (2) -> (3) -> (5) -> (6) -> (7):
(* 10 (* 10 10) (* 10 10)) ; => 100000

; (1) -> (4) -> (2) -> (5) -> (3) -> (6) -> (7):
(* 10 10 (* 10 10)) ; => 10000

; (1) -> (4) -> (2) -> (5) -> (6) -> (3) -> (7):
(* 10 10 10) ; => 1000

; (1) -> (4) -> (2) -> (5) -> (6) -> (7) -> (3):
(* 10 10) ; => 100

; (1) -> (4) -> (5) -> (2) -> (3) -> (6) -> (7):
(* 10 10 (* 10 10)) ; => 10000

; (1) -> (4) -> (5) -> (2) -> (6) -> (3) -> (7):
(* 10 10 10) ; => 1000

; (1) -> (4) -> (5) -> (2) -> (6) -> (7) -> (3):
(* 10 10) ; => 100

; (1) -> (4) -> (5) -> (6) -> (2) -> (3) -> (7):
(* 10 10 10) ; => 1000

; (1) -> (4) -> (5) -> (6) -> (2) -> (7) -> (3):
(* 10 10) ; => 100

; (1) -> (4) -> (5) -> (6) -> (7) -> (2) -> (3):
(* 10 (* 10 10 10)) ; => 10000

; (4) -> (1) -> (2) -> (3) -> (5) -> (6) -> (7):
(* 10 (* 10 10) (* 10 10)) ; => 100000

; (4) -> (1) -> (2) -> (5) -> (3) -> (6) -> (7):
(* 10 10 (* 10 10)) ; => 10000

; (4) -> (1) -> (2) -> (5) -> (6) -> (3) -> (7):
(* 10 10 10) ; => 1000

; (4) -> (1) -> (2) -> (5) -> (6) -> (7) -> (3):
(* 10 10) ; => 100

; (4) -> (1) -> (5) -> (2) -> (3) -> (6) -> (7):
(* 10 10 (* 10 10)) ; => 10000

; (4) -> (1) -> (5) -> (2) -> (6) -> (3) -> (7):
(* 10 10 10) ; => 1000

; (4) -> (1) -> (5) -> (2) -> (6) -> (7) -> (3):
(* 10 10) ; => 100

; (4) -> (1) -> (5) -> (6) -> (2) -> (3) -> (7):
(* 10 10 10) ; => 1000

; (4) -> (1) -> (5) -> (6) -> (2) -> (7) -> (3):
(* 10 10) ; => 100

; (4) -> (1) -> (5) -> (6) -> (7) -> (2) -> (3):
(* 10 (* 10 10 10)) ; => 10000

; (4) -> (5) -> (1) -> (2) -> (3) -> (6) -> (7):
(* 10 10 (* 10 10)) ; => 10000

; (4) -> (5) -> (1) -> (2) -> (6) -> (3) -> (7):
(* 10 10 10) ; => 1000

; (4) -> (5) -> (1) -> (2) -> (6) -> (7) -> (3):
(* 10 10) ; => 100

; (4) -> (5) -> (1) -> (6) -> (2) -> (3) -> (7):
(* 10 10 10) ; => 1000

; (4) -> (5) -> (1) -> (6) -> (2) -> (7) -> (3):
(* 10 10) ; => 100

; (4) -> (5) -> (1) -> (6) -> (7) -> (2) -> (3):
(* 10 (* 10 10 10)) ; => 10000

; (4) -> (5) -> (6) -> (1) -> (2) -> (3) -> (7):
(* 10 10 10) ; => 1000

; (4) -> (5) -> (6) -> (1) -> (2) -> (7) -> (3):
(* 10 10) ; => 100

; (4) -> (5) -> (6) -> (1) -> (7) -> (2) -> (3):
(* 10 (* 10 10 10)) ; => 10000

; (4) -> (5) -> (6) -> (7) -> (1) -> (2) -> (3):
(* (* 10 10 10) (* 10 10 10)) ; => 1000000

; Possible values of x: 100, 1000, 10000, 100000, 1000000


(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

; Possible events
; (1) P1 accesses x two times and sets the value of x to the square of x.
; (2) P2 accesses x three times and sets the value of x to the cube of x.

; (1) -> (2):
(* (* 10 10) (* 10 10) (* 10 10)) ; => 1000000

; (2) -> (1):
(* (* 10 10 10) (* 10 10 10)) ; => 1000000

; Possible values of x: 1000000


;; 41
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) ((protected (lambda () balance)))) ; serialized
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

; Performing unserialized access to the variable could potentially result in an unexpected value being read.
; The (potentially anomalous) behaviour could be observed when accessing the variable while it's being set as part of the deposit/withdraw operation.
; This is heavily dependent on the internal implementation of the set! operation.
; If the set! operation is guaranteed to be performed "atomically" w/ respect to external observers, then it is safe to leave access to the variable unserialized.


;; 42
; FIXME


;; 50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map (cons proc (map stream-cdr argstreams))))))


;; 52
(define sum 0)
; sum = 0

(define (accum x) (set! sum (+ x sum)) sum)
; sum = 0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(stream-map accum (stream-enumerate-interval 1 20))
(stream-map accum (cons-stream 1 (stream-enumerate-interval 2 20)))
(cons-stream (accum 1) (stream-map accum (stream-cdr (stream-enumerate-interval 2 20))))
(cons-stream 1 (stream-map accum (stream-cdr (stream-enumerate-interval 2 20))))
; sum = 1

(define y (stream-filter even? seq))


(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
(stream-ref y 7)
(display-stream z)
; FIXME


;;