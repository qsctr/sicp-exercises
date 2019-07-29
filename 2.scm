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

(define nil '())

(define (reverse xs)
  (define (go xs ys)
    (if (null? xs)
        ys
        (go (cdr xs) (cons (car xs) ys))))
  (go xs nil))

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
      nil
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
      (go x nil)
      x))

; 28

(define (fringe x)
  (cond ((null? x) nil)
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

; 30

(define (square-tree-direct tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (square-tree-direct (car tree))
                            (square-tree-direct (cdr tree))))
        (else (square tree))))

(define (square-tree-map tree)
  (map (lambda (sub-tree) (if (pair? sub-tree)
                              (square-tree-map sub-tree)
                              (square sub-tree)))
       tree))

; 31

(define (tree-map f tree)
  (map (lambda (sub-tree) (if (pair? sub-tree)
                              (tree-map f sub-tree)
                              (f sub-tree)))
       tree))

; 32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))

"P({}) = {}.
(P(S) where S /= {}) =
  P(S \ {e}) U { T U {e} | T <- P(S \ {e}) } for any e in S."

; 33

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-accumulate p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append-accumulate seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-accumulate sequence)
  (accumulate (lambda (x y) (1+ y)) 0 sequence))

; 34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

; 35

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (fringe t))))

; 36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; 37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

; 38

3/2
1/6
'(1 (2 (3 ())))
'(((() 1) 2) 3)

"If op is associative then fold-right and fold-left will produce the same values
for any sequence."

; 39

(define (reverse-fold-right sequence)
  (fold-right
    (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-fold-left sequence)
  (fold-left
    (lambda (x y) (append (list y) x)) nil sequence))

; 40

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1)
                                high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

; 41

(define (remove-item item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (triples-sum n s)
  (filter (lambda (triple)
            (= (accumulate + 0 triple) s))
          (let ((one-to-n (enumerate-interval 1 n)))
            (flatmap
              (lambda (i)
                (let ((one-to-n-without-i (remove-item i one-to-n)))
                  (flatmap
                    (lambda (j)
                      (map (lambda (k) (list i j k))
                           (remove-item j one-to-n-without-i)))
                    one-to-n-without-i)))
              one-to-n))))

; 42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions)
            (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

(define (safe? k positions)
  (define (find-row ps)
    (cond ((null? ps) (error "safe?: Invalid column"))
          ((= (cdar ps) k) (caar ps))
          (else (find-row (cdr ps)))))
  (let ((row (find-row positions)))
    (define (check? ps)
      (if (null? ps)
          false
          (or (and (not (= (cdar ps) k)) 
                   (or (= (caar ps) row)
                       (= (abs (- row (caar ps)))
                          (abs (- k (cdar ps))))))
              (check? (cdr ps)))))
    (not (check? positions))))

; 43

"He is recalculating (queen-cols (- k 1)) each time he tests a new row for the
k-th column."

"T^8"

; 44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter
               (beside smaller smaller)))))

; 45

(define (split first second)
  (define (go painter n)
    (if (= n 0)
        painter
        (let ((smaller (go painter (- n 1))))
          (first painter
                 (second smaller smaller)))))
  go)

; 46

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (binop-vect op)
  (lambda (v1 v2)
    (make-vect (op (xcor-vect v1) (xcor-vect v2))
               (op (ycor-vect v1) (ycor-vect v2)))))

(define add-vect (binop-vect +))
(define sub-vect (binop-vect -))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; 47

(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame-1 frame)
  (car frame))

(define (edge1-frame-1 frame)
  (cadr frame))

(define (edge2-frame-1 frame)
  (caddr frame))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2 frame)
  (car frame))

(define (edge1-frame-2 frame)
  (cadr frame))

(define (edge2-frame-2 frame)
  (cddr frame))

; 48

; Same as exercise 2

; 49

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (path vect1 vect2 . vects)
  (define (go v1 v2 vs)
    (if (null? vs)
        (list (make-segment v1 v2))
        (cons (make-segment v1 v2)
              (go v2 (car vs) (cdr vs)))))
  (go vect1 vect2 vects))

(define outline
  (segments->painter
    (path (make-vect 0 0)
          (make-vect 0 1)
          (make-vect 1 1)
          (make-vect 1 0)
          (make-vect 0 0))))

(define x-shape
  (segments->painter
    (list (make-segment (make-vect 0 0)
                        (make-vect 1 1))
          (make-segment (make-vect 0 1)
                        (make-vect 1 0)))))

(define diamond
  (segments->painter
    (path (make-vect 0 0.5)
          (make-vect 0.5 1)
          (make-vect 1 0.5)
          (make-vect 0.5 0)
          (make-vect 0 0.5))))

; TODO: wave
