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

; 50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

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

; 51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painter painter1
                                           (make-vect 0.0 0.0)
                                           (make-vect 1.0 0.0)
                                           split-point))
          (paint-top (transform-painter painter2
                                        split-point
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below-alt painter1 painter2)
  (rotate270 (beside (rotate90 painter2)
                     (rotate90 painter1))))

; 52

; TODO: wave-modified

(define (corner-split-modified painter n)
  (if (= n 0)
      painter
      (beside (below painter
                     (up-split painter (- n 1)))
              (below (right-split painter (- n 1))
                     (corner-split painter (- n 1))))))

(define (square-limit-modified painter n)
  ((square-of-four flip-vert rotate180 identity flip-horiz)
   (corner-split painter n)))

; 53

"(a b c)"
"((george))"
"((y1 y2))"
"(y1 y2)"
"#f"
"#f"
"(red shoes blue socks)"

; 54

(define (equal-reimpl? a b)
  (if (and (pair? a) (pair? b))
      (and (equal-reimpl? (car a) (car b))
           (equal-reimpl? (cdr a) (cdr b)))
      (eq? a b)))

; 55

"The expression is evaluated as:
(car ''abracadabra)
(car '(quote abracadabra))
quote"

; 56

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (deriv-56 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv-56 (addend exp) var)
                              (deriv-56 (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv-56 (multiplicand exp) var))
                   (make-product (deriv-56 (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (- (exponent exp) 1)))
                       (deriv-56 (base exp) var)))
        (else (error "unknown expression type: DERIV-56" exp))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) b)
        ((and (number? b) (number? n)) (expt b n))
        (else (list '** b n))))

; 57

(define (augend-57 s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand-57 p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (deriv-57 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv-57 (addend exp) var)
                              (deriv-57 (augend-57 exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv-57 (multiplicand-57 exp) var))
                   (make-product (deriv-57 (multiplier exp) var)
                                 (multiplicand-57 exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (- (exponent exp) 1)))
                       (deriv-57 (base exp) var)))
        (else (error "unknown expression type: DERIV-57" exp))))

; 58a

(define (make-sum-58 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product-58 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation-58 b n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) b)
        ((and (number? b) (number? n)) (expt b n))
        (else (list b '** n))))

(define (sum-58a? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend-58a s) (car s))

(define (augend-58a s) (caddr s))

(define (product-58a? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier-58a p) (car p))

(define (multiplicand-58a p) (caddr p))

(define (exponentiation-58a? x) (and (pair? x) (eq? (cadr x) '**)))

(define (base-58a e) (car e))

(define (exponent-58a e) (caddr e))

(define (deriv-58a exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum-58a? exp) (make-sum-58 (deriv-58a (addend-58a exp) var)
                                     (deriv-58a (augend-58a exp) var)))
        ((product-58a? exp)
         (make-sum-58 (make-product-58 (multiplier-58a exp)
                                       (deriv-58a (multiplicand-58a exp) var))
                      (make-product-58 (deriv-58a (multiplier-58a exp) var)
                                       (multiplicand-58a exp))))
        ((exponentiation-58a? exp)
         (make-product-58
           (make-product-58 (exponent-58a exp)
                            (make-exponentiation-58 (base-58a exp)
                                                    (- (exponent-58a exp) 1)))
           (deriv-58a (base-58a exp) var)))
        (else (error "unknown expression type: DERIV-58A" exp))))

; 58b

(define order-of-operations
  '(** * +))

(define (lower-prec op1 op2)
  (define (go ooo)
    (cond ((eq? (car ooo) op1) op2)
          ((eq? (car ooo) op2) op1)
          (else (go (cdr ooo)))))
  (if (eq? op1 op2)
      op1
      (go order-of-operations)))

(define (min-prec-op exp)
  (if (null? (cdddr exp))
      (cadr exp)
      (lower-prec (cadr exp) (min-prec-op (cddr exp)))))

(define (operation-predicate op)
  (lambda (x)
    (and (pair? x) (eq? (min-prec-op x) op))))

(define sum-58b? (operation-predicate '+))

(define product-58b? (operation-predicate '*))

(define exponentiation-58b? (operation-predicate '**))

(define (left-arg op)
  (define (go x single)
    (if (eq? (cadr x) op)
        (if single
            (car x)
            (list (car x)))
        (cons (car x)
              (cons (cadr x)
                    (go (cddr x) false)))))
  (lambda (x)
    (go x true)))

(define (right-arg op)
  (define (go x)
    (if (eq? (cadr x) op)
        (if (null? (cdddr x))
            (caddr x)
            (cddr x))
        (go (cddr x))))
  go)

(define addend-58b (left-arg '+))

(define augend-58b (right-arg '+))

(define multiplier-58b (left-arg '*))

(define multiplicand-58b (right-arg '*))

(define base-58b (left-arg '**))

(define exponent-58b (right-arg '**))

(define (deriv-58b exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum-58b? exp) (make-sum-58 (deriv-58b (addend-58b exp) var)
                                     (deriv-58b (augend-58b exp) var)))
        ((product-58b? exp)
         (make-sum-58 (make-product-58 (multiplier-58b exp)
                                       (deriv-58b (multiplicand-58b exp) var))
                      (make-product-58 (deriv-58b (multiplier-58b exp) var)
                                       (multiplicand-58b exp))))
        ((exponentiation-58b? exp)
         (make-product-58
           (make-product-58 (exponent-58b exp)
                            (make-exponentiation-58 (base-58b exp)
                                                    (- (exponent-58b exp) 1)))
           (deriv-58b (base-58b exp) var)))
        (else (error "unknown expression type: DERIV-58B" exp))))

; 59

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

; 60

(define (element-of-set-non-unique? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set-non-unique? x (cdr set)))))

(define (adjoin-set-non-unique x set)
  (cons x set))

(define (union-set-non-unique set1 set2)
  (append set1 set2))

(define (intersection-set-non-unique set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set-non-unique? (car set1) set2)
         (cons (car set1) (intersection-set-non-unique (cdr set1) set2)))
        (else (intersection-set-non-unique (cdr set1) set2))))

"This representation is less space efficient compared to the non-duplicate
representation, but it has the same asymptotic time complexity for the
element-of-set operation (O(n)) and the intersection-set operation (O(n^2)),
and lower time complexity for the adjoin-set operation (O(1)) and the union-set
operation (O(n)). In reality, adjoin-set and union-set for the duplicate
representation perform better than their non-duplicate counterparts, and
element-of-set and intersection-set perform worse, or in the best case (when
there are no duplicates) identical, since the presence of duplicates increases
the number of elements to be iterated through."

"This representation should be used when there are not a lot of duplicate
elements (thus space usage will not be increased by much compared to the
non-duplicate representation) but the operations of adjoin-set and union-set
are used often."

; 61

(define (adjoin-set-ordered x set)
  (cond ((null? set) (list x)) 
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set-ordered x (cdr set))))))

; 62

(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set-ordered (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set-ordered (cdr set1) set2)))
                 ((< x2 x1) (cons x2 (union-set-ordered set1 (cdr set2)))))))))

; 63a

"Yes"

'(1 3 5 7 9 11)

; 63b

"No"

"tree->list-2"

; 64a

"First, partial-tree calls itself to turn the first n/2 elements (the first half
of the list) into the left subtree. Therefore, the first remaining element is
the current entry. Everything after that (n/2 to n) is turned into the right
subtree. These are then assembled together with make-tree. In the case that
n = 0 it simply returns the empty tree."

"    5
    / \
   /   \
  1     9
   \   / \
    3 7   11
"

; 64b

"O(n)"

; 65

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (union-set-tree set1 set2)
  (list->tree (union-set-ordered (tree->list-2 set1)
                                 (tree->list-2 set2))))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-set-ordered (cdr set1)
                                                            (cdr set2))))
              ((< x1 x2) (intersection-set-ordered (cdr set1) set2))
              ((< x2 x1) (intersection-set-ordered set1 (cdr set2)))))))

(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set-ordered (tree->list-2 set1)
                                        (tree->list-2 set2))))
