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

(define (square x)
  (* x x))

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
  (cond ((= b 0) 0)
        ((even? b) (e17 (double a) (halve b)))
        (else (+ a (e17 a (- b 1))))))

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (e18 a b)
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
