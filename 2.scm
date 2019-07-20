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

(define (mul-interval x y)
  (let ((ux (upper-bound x))
        (lx (lower-bound x))
        (uy (upper-bound y))
        (ly (lower-bound y)))
    (cond ((and (positive? ux) (positive? lx) (positive? uy) (positive? ly))
           (make-interval (* lx ly) (* ux uy))))))
          ; TODO
