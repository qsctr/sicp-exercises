; 1

(define (make-rat n d)
  (cond ((zero? d) (error "denominator is zero"))
        ((zero? n) (cons 0 1))
        (else (let ((g (gcd n d)))
                (let ((nr (/ n g))
                      (dr (/ d g)))
                  (if (positive? dr)
                      (cons nr dr)
                      (cons (- nr) (- dr))))))))

; 2

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(define (average a b)
  (/ (+ a b) 2))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

; 3

(define (make-rectangle point-1 point-2)
  (define (corner compare)
    (make-point (compare (x-point point-1) (x-point point-2))
                (compare (y-point point-1) (y-point point-2))))
  (cons (corner min) (corner max)))

(define (lower-left-rectangle rectangle)
  (car rectangle))

(define (upper-right-rectangle rectangle)
  (cdr rectangle))

(define (width-rectangle rectangle)
  (dimension-rectangle x-point rectangle))

(define (height-rectangle rectangle)
  (dimension-rectangle y-point rectangle))

(define (dimension-rectangle selector rectangle)
  (- (selector (upper-right-rectangle rectangle))
     (selector (lower-left-rectangle rectangle))))

(define (perimeter-rectangle rectangle)
  (* 2 (+ (width-rectangle rectangle) (height-rectangle rectangle))))

(define (area-rectangle rectangle)
  (* (width-rectangle rectangle) (height-rectangle rectangle)))

(define (make-rectangle-alt lower-left width height)
  (cons lower-left (cons width height)))

(define (width-rectangle-alt rectangle)
  (car (cdr rectangle)))

(define (height-rectangle-alt rectangle)
  (cdr (cdr rectangle)))

(define (perimeter-rectangle-alt rectangle)
  (* 2 (+ (width-rectangle-alt rectangle) (height-rectangle-alt rectangle))))

(define (area-rectangle-alt rectangle)
  (* (width-rectangle-alt rectangle) (height-rectangle-alt rectangle)))

; 4

(define (cdr-alt z)
  (z (lambda (p q) q)))

; 5

(define (cons-num a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-num c)
  (select-num 2 3 c))

(define (cdr-num c)
  (select-num 3 2 c))

(define (select-num select remove c)
  (let ((qr (integer-divide c remove)))
    (if (zero? (integer-divide-remainder qr))
        (select-num select remove (integer-divide-quotient qr))
        (round (/ (log c) (log select))))))

; 6

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+-church a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

; 7

(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

; 8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; 9

"Addition/subtraction:

let x = a +- c
let y = b +- d
max x = a + c
min x = a - c
max y = b + d
min y = b - d
max (x + y) = (a + c) + (b + d)
            = (a + b) + (c + d)
min (x + y) = (a - c) + (b - d)
            = (a + b) - (c + d)
width of (x + y) = c + d

Multiplication/division:

let x = 5 +- 1
let y = 8 +- 2
max (x * y) = 6 * 10 = 60
min (x * y) = 4 * 6 = 24
width of (x * y) = 60 - 24 = 36

let x = 2 +- 1
let y = 3 +- 2
max (x * y) = 3 * 5 = 15
min (x * y) = 1 * 1 = 1
width of (x * y) = 15 - 1 = 14"

; 10

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (>= (upper-bound y) 0) (<= (lower-bound y) 0))
      (error "Division by interval that spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; 11

(define (mul-interval-alt x y)
  (let ((ux (upper-bound x))
        (lx (lower-bound x))
        (uy (upper-bound y))
        (ly (lower-bound y)))
    (if (positive? ux)
        (if (positive? lx)
            (if (positive? uy)
                (if (positive? ly)
                    (make-interval (* lx ly) (* ux uy))
                    (make-interval (* ux ly) (* ux uy)))
                (make-interval (* ux ly) (* lx uy)))
            (if (positive? uy)
                (if (positive? ly)
                    (make-interval (* lx uy) (* ux uy))
                    (make-interval (min (* ux ly) (* lx uy))
                                   (max (* ux uy) (* lx ly))))
                (make-interval (* ux ly) (* lx ly))))
        (if (positive? uy)
            (if (positive? ly)
                (make-interval (* lx uy) (* ux ly))
                (make-interval (* lx uy) (* lx ly)))
            (make-interval (* ux uy) (* lx ly))))))

; 12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* (/ (width i) (center i)) 100))

; 13

"(a +- x%) * (b +- y%)
max:
  (a + x%) * (b + y%)
  = (a + a * x%) * (b + b * y%)
  = a * b + a * b * y% + a * b * x% + a * b * x% * y%
  = (a * b)(1 + x% * y%) + (a * b)(x% * y%)
min:
  (a - x%) * (b - y%)
  = (a - a * x%) * (b - b * y%)
  = a * b - a * b * y% - a * b * x% + a * b * x% * y%
  = (a * b)(1 + x% * y%) - (a * b)(x% + y%)
tolerance:
  (a * b)(x% + y%) / (a * b)(1 + x% * y%)
  = (x% + y%) / (1 + x% * y%)
"

; 14

(define (e14)
  (let ((a (make-center-percent 10 1))
        (b (make-center-percent 20 2)))
    (display (percent (div-interval a a))) (newline)
    (display (percent (div-interval a b))) (newline)))

"The percent tolerance of the quotient is the sum of the percent tolerances of
its two arguments."

; 15

"Yes, because when interval variables are repeated, the error bounds of the
result increases, even if the variables are identical. This is why the width of
the result of par2 is smaller than that of par1."

; 16

"Equivalent algebraic expressions may lead to different answers because
variables may be repeated in some expressions and the order and amount of
arithmetic operations can also differ. To solve this problem, you would need to
implement a symbolic algebra system which reduces any expression to some normal
form which minimizes the error, then computes the result of the normalized
expression."

; TODO

; 17

(define (last-pair xs)
  (if (null? (cdr xs))
      xs
      (last-pair (cdr xs))))

; 18

(define (reverse xs)
  (define (go xs ys)
    (if (null? xs)
        ys
        (go (cdr xs) (cons (car xs) ys))))
  (go xs '()))

; 19

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

"The order of the list coin-values does not matter, because the results of the
recursive calls are added together and addition is commutative."

; 20

(define (same-parity x . xs)
  (define (parity n)
    (remainder n 2))
  (let ((x-parity (parity x)))
    (define (go xs)
      (cond ((null? xs) xs)
            ((= (parity (car xs)) x-parity) (cons (car xs) (go (cdr xs))))
            (else (go (cdr xs)))))
    (cons x (go xs))))

; 21

(define (square-list-direct items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-direct (cdr items)))))

(define (square-list-map items)
  (map square items))

; 22

"The item at the front of the list gets consed onto the result list first, and
the item at the end is added last."

"The result is not a list because the tail of the list is in the car part of
each pair, not the cdr part."

; 23

(define (for-each f xs)
  (define (run xs)
    (f (car xs))
    (for-each f (cdr xs)))
  (if (not (null? xs))
    (run xs)))

; 24

"(1 (2 (3 4)))"

"
---------    ---------
| o | o-+--->| o | / |
--|------    --|------
  V            V
-----        ---------    ---------
| 1 |        | o | o-+--->| o | / |
-----        --|------    --|------
               V            V
             -----        ---------    ---------
             | 2 |        | o | o-+--->| 4 | / |
             -----        --|------    ---------
                            V
                          -----
                          | 3 |
                          -----
"

"
  o
 / \
1   o
   / \
  2   o
     / \
    3   4
"

; 25

(define (e25)
  (display (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))) (newline)
  (display (car (car (list (list 7))))) (newline)
  (display
   (car
    (cdr
     (car
      (cdr
       (car
        (cdr
         (car
          (cdr
           (car
            (cdr
             (car
              (cdr (list 1
                         (list 2
                               (list 3
                                     (list 4
                                           (list 5
                                                 (list 6 7)))))))))))))))))))
  (newline))

; 26

"(1 2 3 4 5 6)"
"((1 2 3) 4 5 6)"
"((1 2 3) (4 5 6))"

; 27

(define (deep-reverse x)
  (define (go xs ys)
    (if (null? xs)
        ys
        (go (cdr xs)
            (cons (deep-reverse (car xs))
                  ys))))
  (if (pair? x)
      (go x '())
      x))

; 28

(define (fringe x)
  (cond ((null? x) '())
        ((pair? x) (append (fringe (car x))
                           (fringe (cdr x))))
        (else (list x))))

; 29

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight x)
  (if (pair? x)
      (+ (total-weight (branch-structure (left-branch x)))
         (total-weight (branch-structure (right-branch x))))
      x))

(define (balanced? mobile)
  (define (balanced-weight x)
    (define (branch-weight side callback)
      (let ((branch (side x)))
        (callback branch (balanced-weight (branch-structure branch)))))
    (if (pair? x)
        (branch-weight left-branch
          (lambda (left left-weight)
            (if left-weight
                (branch-weight right-branch
                  (lambda (right right-weight)
                    (if (and right-weight
                             (= (* (branch-length left) left-weight)
                                (* (branch-length right) right-weight)))
                        (+ left-weight right-weight)
                        false)))
                false)))
        x))
  (if (balanced-weight mobile) true false))

"Only the selectors right-branch and branch-structure need to be changed."
