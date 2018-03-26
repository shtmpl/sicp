(define milliseconds-in-a-year   31556952000)
(define milliseconds-in-a-month  2629746000)
(define milliseconds-in-a-day    86400000)
(define milliseconds-in-an-hour  3600000)
(define milliseconds-in-a-minute 60000)
(define milliseconds-in-a-second 1000)

(define (date->ms date)
  (+ (* milliseconds-in-a-year   (date-year date))
     (* milliseconds-in-a-month  (date-month date))
     (* milliseconds-in-a-day    (date-day date))
     (* milliseconds-in-an-hour  (date-hour date))
     (* milliseconds-in-a-minute (date-minute date))
     (* milliseconds-in-a-second (date-second date))
     (date-millisecond date)))

(define (date-diff-in-ms to-date from-date)
  (- (date->ms to-date)
     (date->ms from-date)))

(define (time f)
  (let ((start  (current-date))
        (result (f)))
    (let ((elapsed (date-diff-in-ms (current-date) start)))
      (cons elapsed result))))

(define (times count f)
  (define (time-iteratively count f result)
    (if (= count 0)
        result
      (let ((timed (time f)))
        (time-iteratively (- count 1) f (cons timed result)))))
  (time-iteratively count f (list)))

(define (avg xs)
  (/ (accumulate + 0 xs) (length xs)))

(define (bench f)
  (avg (map car (times 10 f))))