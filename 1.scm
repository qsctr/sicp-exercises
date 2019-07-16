(define e1
  '(10
    12
    8
    3
    6
    a
    b
    19
    #f
    4
    16
    6
    16))

(define e2
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

(define (e3 a b c)
  (cond ((first-smallest a b c) (sum-of-squares b c))
        ((first-smallest b a c) (sum-of-squares a c))
        ((first-smallest c a b) (sum-of-squares a b))))

(define (first-smallest a b c)
  (and (< a b) (< a c)))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define e4
  "if b > 0 then a + b else a - b")

(define e5
  "If the interpreter is using applicative-order evaluation, then the evaluation
  of the expression will not terminate, because the interpreter will try to
  evaluate (p) before test. If the interpreter is using normal-order evaluation,
  then the result of the expression will be 0, because the interpreter will
  evaluate the if first and will never evaluate (p) since the condition is true.
  ")

(define e6
  "The program will run forever since the interpreter must evaluate all
  arguments to new-if, including the recursive call to sqrt-iter, before
  evaluating new-if.")

(define (e7 x)
  (sqrt-iter-improve 1.0 x))

(define (sqrt-iter guess prev-guess x)
  (if (sqrt-good-enough? guess prev-guess)
      guess
      (sqrt-iter-improve guess x)))

(define (sqrt-iter-improve guess x)
  (sqrt-iter (sqrt-improve guess x) guess x))

(define (sqrt-good-enough? guess prev-guess)
  (< (/ (abs (- guess prev-guess)) guess) 0.001))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (e8 x)
  (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
  (if (cbrt-good-enough? guess x)
      guess
      (cbrt-iter (cbrt-improve guess x)
                 x)))

(define (cbrt-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(define (cbrt-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube x)
  (* x x x))

(define e9
  '(((+ 4 5)
     (inc (+ 3 5))
     (inc (inc (+ 2 5)))
     (inc (inc (inc (+ 1 5))))
     (inc (inc (inc (inc (+ 0 5)))))
     (inc (inc (inc (inc 5))))
     (inc (inc (inc 6)))
     (inc (inc 7))
     (inc 8)
     9
     "recursive")
    ((+ 4 5)
     (+ 3 6)
     (+ 2 7)
     (+ 1 8)
     (+ 0 9)
     9
     "iterative")))

(define e10
  '((1024
     65536
     65536)
    ("f(n) = 2n"
     "g(n) = 2^n"
     "h(n) = 2^^n")))

(define (e11-recursive n)
  (if (< n 3)
      n
      (+ (e11-recursive (- n 1))
         (* 2 (e11-recursive (- n 2)))
         (* 3 (e11-recursive (- n 3))))))

(define (e11-iterative n)
  (define (f-iter a b c count)
    (if (= count 0)
        c
        (f-iter b
                c
                (+ c (* 2 b) (* 3 a))
                (- count 1))))
  (if (< n 3)
      n
      (f-iter 0 1 2 (- n 2))))

(define (e12 r c)
  (if (or (= c 0) (= c r))
      1
      (+ (e12 (- r 1) (- c 1))
         (e12 (- r 1) c))))

(define e13
  "Proof by induction:
    let P(n) = (Fib(n) = (phi^n - psi^n) / sqrt(5))
    Base case (n = 0):
      Fib(0) = 0.
      (phi^0 - psi^0) / sqrt(5)
      = (1 - 1) / sqrt(5)
      = 0.
      P(0) holds.
    Base case (n = 1):
      Fib(1) = 1.
      (phi^1 - psi^1) / sqrt(5)
      = ((1 + sqrt(5)) / 2 - (1 - sqrt(5)) / 2) / sqrt(5)
      = (1/2 + sqrt(5)/2 - 1/2 + sqrt(5)/2) / sqrt(5)
      = sqrt(5) / sqrt(5)
      = 1.
      P(1) holds.
    Recursive case:
      Assume P(n - 1) and P(n - 2) hold.
      Fib(n)
      = Fib(n - 1) + Fib(n - 2)
      = (phi^(n - 1) - psi^(n - 1)) / sqrt(5)
        + (phi^(n - 2) - psi^(n - 2)) / sqrt(5)
      = (phi^(n - 1) + phi^(n - 2) - psi^(n - 1) - psi^(n - 2)) / sqrt(5)
      = (((1 + sqrt(5)) / 2)^(n - 1) + ((1 + sqrt(5)) / 2)^(n - 2)
        - ((1 - sqrt(5)) / 2)^(n - 1) - ((1 - sqrt(5)) / 2)^(n - 2)) / sqrt(5)
      = (((1 + sqrt(5))^(n - 1) - (1 - sqrt(5))^(n - 1)) / 2^(n - 1)
        + ((1 + sqrt(5))^(n - 2) - (1 - sqrt(5))^(n - 2)) / 2^(n - 2)) / sqrt(5)
      = ((1 + sqrt(5))^(n - 1) - (1 - sqrt(5))^(n - 1)
        + 2(1 + sqrt(5))^(n - 2) - 2(1 - sqrt(5))^(n - 2)) / (2^(n - 1) sqrt(5))
      = ((1 + sqrt(5))^(n - 2) (1 + sqrt(5) + 2)
        - (1 - sqrt(5))^(n - 2) (1 - sqrt(5) + 2)) / (2^(n - 1) sqrt(5))
      = (((1 + sqrt(5))^(n - 2) / 2^(n - 1)) (3 + sqrt(5))
        - ((1 - sqrt(5))^(n - 2) / 2^(n - 1)) (3 - sqrt(5))) / sqrt(5)
      = (((1 + sqrt(5)) / 2)^(n - 2) ((3 + sqrt(5)) / 2)
        - ((1 - sqrt(5)) / 2)^(n - 2) ((3 - sqrt(5)) / 2)) / sqrt(5)
      = (phi^(n - 2) ((3 + sqrt(5)) / 2)
        - psi^(n - 2) ((3 - sqrt(5)) / 2)) / sqrt(5)
      | phi^2
      | = ((1 + sqrt(5)) / 2)^2
      | = (1 + 2 sqrt(5) + 5) / 4
      | = (6 + 2 sqrt(5)) / 4
      | = (3 + sqrt(5)) / 2.
      | psi^2
      | = ((1 - sqrt(5)) / 2)^2
      | = (1 - 2 sqrt(5) + 5) / 4
      | = (6 - 2 sqrt(5)) / 4
      | = (3 - sqrt(5)) / 2.
      = (phi^(n - 2) phi^2 - psi^(n - 2) psi^2) / sqrt(5)
      = (phi^n - psi^n) / sqrt(5).
      P(n) holds.
    Forall n : Nat, P(n) holds.
  Proof by induction:
    let P(n) = (Fib(n) is the closest integer to phi^n / sqrt(5))
    = ((phi^n - psi^n) / sqrt(5) is the closest integer to phi^n / sqrt(5))
    = (abs((phi^n - psi^n) / sqrt(5) - phi^n / sqrt(5)) < 1/2)
    = (abs((phi^n - psi^n - phi^n) / sqrt(5)) < 1/2)
    = (abs(-psi^n / sqrt(5)) < 1/2)
    = (abs(psi^n) < sqrt(5) / 2)
    Base case (n = 0):
      abs(psi^0) < sqrt(5) / 2
      1 < sqrt(5) / 2.
      P(0) holds.
    Recursive case:
      Assume P(n) holds.
      psi^(n + 1) < psi^n
      psi^n psi < psi^n
      psi < 1
      (1 - sqrt(5)) / 2 < 1.
      psi^(n + 1) < sqrt(5) / 2.
      P(n) holds.
    Forall n : Nat, P(n) holds.")

(define e14
  '(((cc 11 5)
     ((cc 11 4)
      ((cc 11 3)
       ((cc 11 2)
        ((cc 11 1)
         ((cc 11 0)
          0)
         ((cc 10 1)
          ((cc 10 0)
           0)
          ((cc 9 1)
           ((cc 9 0)
            0)
           ((cc 8 1)
            ((cc 8 0)
             0)
            ((cc 7 1)
             ((cc 7 0)
              0)
             ((cc 6 1)
              ((cc 6 0)
               0)
              ((cc 5 1)
               ((cc 5 0)
                0)
               ((cc 4 1)
                ((cc 4 0)
                 0)
                ((cc 3 1)
                 ((cc 3 0)
                  0)
                 ((cc 2 1)
                  ((cc 2 0)
                   0)
                  ((cc 1 1)
                   ((cc 1 0)
                    0)
                   ((cc 0 1)
                    1))))))))))))
        ((cc 6 2)
         ((cc 6 1)
          ((cc 6 0)
           0)
          ((cc 5 1)
           ((cc 5 0)
            0)
           ((cc 4 1)
            ((cc 4 0)
             0)
            ((cc 3 1)
             ((cc 3 0)
              0)
             ((cc 2 1)
              ((cc 2 0)
               0)
              ((cc 1 1)
               ((cc 1 0)
                0)
               ((cc 0 1)
                1)))))))
         ((cc 1 2)
          ((cc 1 1)
           ((cc 1 0)
            0)
           ((cc 0 1)
            1))
          ((cc -1 2)
           0))))
       ((cc 1 3)
        ((cc 1 2)
         ((cc 1 1)
          ((cc 1 0)
           0)
          ((cc 0 1)
           1))
         ((cc -1 2)
          0))
        ((cc -9 3)
         0)))
      ((cc -14 4)
       0))
     ((cc -39 5)
      0))
    (space "O(n)")
    (steps "O(n^5)")))

(define e15
  '((a 5)
    (b ((space "O(log(a))")
        (steps "O(log(a))")))))

(define (e16 b n)
  (define (expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1)))))
  (expt-iter 1 b n))

(define (e17 a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (e17 (double a) (halve b)))
        (else (+ a (e17 a (- b 1))))))

(define (e18 a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (define (*-iter x a b)
    (cond ((= b 0) x)
          ((even? b) (*-iter x (double a) (halve b)))
          (else (*-iter (+ x a) a (- b 1)))))
  (*-iter 0 a b))

(define e19
  '("T_pq(T_pq(a, b)):
    a' <- bq + aq + ap
    b' <- bp + aq
    a'' <- b'q + a'q + a'p
      = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
      = bpq + aqq + bqq + aqq + apq + bqp + aqp + app
      = b(pq + qq + qp) + a(pq + qq + qp) + a(qq + pp)
      = b(q(2p+q)) + a(q(2p+q)) + a(pp + qq)
    b'' <- b'p + a'q
      = (bp + aq)p + (bq + aq + ap)q
      = bpp + aqp + bqq + aqq + apq
      = b(pp + qq) + a(qp + qq + pq)
      = b(pp + qq) + a(q(2p+q))
    p' = pp + qq
    q' = q(2p+q)"
    fib))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (* q (+ (* 2 p) q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


(define e20
  '(((gcd 206 40)
     (gcd 40 (remainder 206 40))
     (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
     (gcd (remainder 40 (remainder 206 40))
          (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
     (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
          (remainder (remainder 40 (remainder 206 40))
                     (remainder (remainder 206 40)
                                (remainder 40 (remainder 206 40)))))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
    18
    4))

(define e21
  '(199
    1999
    7))

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

(define e22
  '(search-for-primes
    (1000 ((1009 0)
           (1013 0)
           (1019 0)))
    (10000 ((10007 1)
            (10009 1)
            (10037 1)))
    (100000 ((100003 3)
             (100019 3)
             (100043 3)))
    (1000000 ((1000003 10)
              (1000033 7)
              (1000037 7)))
    "Yes, each increase of 10x in n corresponds to an increase of roughtly
    sqrt(10)x in time."))

(define (search-for-primes a b)
  (define (check-range n)
    (if (<= n b)
        (test-prime n)))
  (define (test-prime n)
    (timed-prime-test n)
    (check-range (+ n 2)))
  (check-range (if (even? a)
                   (+ a 1)
                   a)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (real-time-clock) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define e23
  '((1009 1)
    (1013 0)
    (1019 0)
    (10007 0)
    (10009 1)
    (10037 1)
    (100003 2)
    (100019 1)
    (100043 2)
    (1000003 4)
    (1000033 6)
    (1000037 4)
    "Yes, it is about twice as fast as in exercise 22."))

(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (smallest-divisor-e23 n)
  (find-divisor-e23 n 2))

(define (find-divisor-e23 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-e23 n (next test-divisor)))))

(define (prime-e23? n)
  (= n (smallest-divisor-e23 n)))

(define (timed-prime-test-e23 n)
  (newline)
  (display n)
  (start-prime-test-e23 n (real-time-clock)))

(define (start-prime-test-e23 n start-time)
  (if (prime-e23? n)
      (report-prime (- (real-time-clock) start-time))))

(define e24
  '((1009 0)
    (1013 0)
    (1019 0)
    (10007 0)
    (10009 0)
    (10037 0)
    (100003 1)
    (100019 0)
    (100043 0)
    (1000003 0)
    (1000033 0)
    (1000037 0)
    "The time needed to test primes near 1,000,000 should be around twice the
    time needed to test primes near 1000."
    "The data does not demonstrate this because fast-prime? is too fast to be
    measured even for n = 1,000,000."))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test-e24 n)
  (newline)
  (display n)
  (start-prime-test-e24 n (real-time-clock)))

(define (start-prime-test-e24 n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (real-time-clock) start-time))))

(define e25
  "This definition of expmod returns the correct answer but is more inefficient
  than the original one because it must compute the result of the exponential
  before finding the remainder.")

(define e26
  "The call (expmod base (/ exp 2) m) is duplicated so each time expmod is
  called with an even exponent it has to call itself twice. This makes the
  runtime O(2^(original time)) = O(2^log(n)) = O(n).")

(define (e27 n)
  (define (test a)
    (or (= a n)
        (and (= (expmod a n n) a)
             (test (+ a 1)))))
  (test 1))

(define (e28 n)
  (= (expmod-e28 (+ 1 (random (- n 1)))
                 (- n 1)
                 n)
     1))

(define (expmod-e28 base exp m)
  (define (check-trivial root)
    (check-square-mod (remainder (square root) m)
                      (or (= root 1) (= root (- m 1)))))
  (define (check-square-mod rem trivial)
    (if (and (not trivial) (= rem 1))
        0
        rem))
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-trivial (expmod base (/ exp 2) m)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (e29 f a b n)
  (define (y k)
    (f (+ a (* k h))))
  (define (add k)
    (+ (* 4 (y k))
       (if (= (+ k 1) n)
           (y n)
           (+ (* 2 (y (+ k 1)))
              (add (+ k 2))))))
  (define h (/ (- b a) n))
  (* (/ h 3) (+ (y 0) (add 1))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define e30 sum)

(define e31
  '((product
     factorial
     approx-pi/4)
    product-iter))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (identity x) x)

(define (inc x)
  (+ x 1))

(define (approx-pi/4 n)
  (define (inc-2 x)
    (+ x 2))
  (define (*-inc-2 x)
    (* x (inc-2 x)))
  (/ (product *-inc-2 2.0 inc-2 (* n 2))
     (product square 3.0 inc-2 (inc (* n 2)))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define e32
  '((accumulate
     sum-accumulate
     product-accumulate)
    accumulate-iter))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-accumulate term a next b)
  (accumulate + 0 term a next b))

(define (product-accumulate term a next b)
  (accumulate * 1 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define e33
  '(filtered-accumulate
    sum-of-squares-of-primes
    product-of-relative-primes-to))

(define (filtered-accumulate combiner null-value predicate? term a next b)
  (if (> a b)
      null-value
      (combiner (if (predicate? a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner
                                     null-value
                                     predicate?
                                     term
                                     (next a)
                                     next
                                     b))))

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

(define (product-of-relative-primes-to n)
  (define (relatively-prime-to-n? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 relatively-prime-to-n? identity 1 inc (- n 1)))

(define e34
  '(((f f)
     (f 2)
     (2 2))
    "There will be an error because 2 cannot be applied to 2."))

(define e35
  '("phi^2 = phi + 1
     phi = 1 + 1/phi"
    (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define e36
  '(fixed-point-e36
    (fixed-point-e36 (lambda (x) (/ (log 1000) (log x))) 4)
    (fixed-point-e36 (lambda (x) (average x (/ (log 1000) (log x)))) 4)
    (steps-with-average-damping 7)
    (steps-without-average-damping 29)))

(define (fixed-point-e36 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define e37
  '((cont-frac
     10)
    cont-frac-iter))

(define (cont-frac n d k)
  (define (term i)
    (/ (n i)
       (if (= i k)
           (d k)
           (+ (d i) (term (+ i 1))))))
  (term 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) result)))))
  (iter k 0))

(define (e38 k)
  (+ 2 (cont-frac-iter (lambda (i) 1.0)
                       (lambda (i) (if (= (remainder i 3) 2)
                                       (* 2 (/ (+ 1 i) 3))
                                       1))
                       k)))

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1) x (- (square x))))
                  (lambda (i) (- (* i 2) 1))
                  k))

(define e39 tan-cf)

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define e40 cubic)

(define e41
  '(double
    21))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define e42 compose)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define e43 repeated)

(define e44
  '(smooth
    n-fold-smooth))

(define (smooth f)
  (let ((dx 0.01))
    (lambda (x) (/ (+ (f (- x dx))
                      (f x)
                      (f (+ x dx)))
                   3))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (e45 n x)
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2))))
                (lambda (y) (/ x (e16 y (- n 1)))))
               1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define e46
  '(iterative-improve
    sqrt-iterative-improve
    fixed-point-iterative-improve))

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt-iterative-improve x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess))))
   1.0))

(define (fixed-point-iterative-improve f first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- (f guess) guess)) tolerance))
                      f)
   first-guess))
