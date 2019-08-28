;;; 1

(define (make-accumulator sum)
  (lambda (x)
    (set! sum (+ sum x))
    sum))

;;; 2

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) count)
            ((eq? x 'reset-count) (set! count 0))
            (else (set! count (1+ count))
                  (f x))))))

;;; 3

(define (make-account-3 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? pw password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m)))
        (lambda x "Incorrect password")))
  dispatch)

;;; 4

(define (make-account-4 balance password)
  (let ((tries 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pw m)
      (if (eq? pw password)
          (begin (set! tries 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT" m))))
          (begin (set! tries (1+ tries))
                 (if (> tries 7)
                     (call-the-cops))
                 (lambda x "Incorrect password"))))
    dispatch))

;;; 5

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (monte-carlo trials (lambda () (P (random-in-range x1 x2)
                                       (random-in-range y1 y2))))
     (* (- x2 x1) (- y2 y1))))

(define (pi-estimate)
  (estimate-integral (lambda (x y) (< (+ (square x) (square y)) 1))
                     -1. 1. -1. 1. 100000))

;;; 6

;; NOTE: rand-6 is thunk-ified because random-init is not defined

(define (rand-6)
  (let ((x random-init))
    (lambda (op)
      (cond ((eq? op 'generate)
             (set! x (rand-update x))
             x)
            ((eq? op 'reset)
             (lambda (new-value)
               (set! x new-value)))))))

;;; 7

(define (make-account-7 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (cond ((eq? m 'verify-password) (eq? pw password))
          ((eq? pw password) (cond ((eq? m 'withdraw) withdraw)
                                   ((eq? m 'deposit) deposit)
                                   (else (error "Unknown request: MAKE-ACCOUNT"
                                                m))))
          (else (lambda x "Incorrect password"))))
  dispatch)

(define (make-joint acc old-password new-password)
  (if (acc old-password 'verify-password)
      (lambda (pw m)
        (if (eq? pw new-password)
            (acc old-password m)
            "Incorrect password"))
      "Incorrect password"))

;;; 8

(define f
  (let ((z false))
    (lambda (x)
      (cond (z 0)
            ((zero? x) (set! z true)
                       0)
            (else x)))))

;;; 9

"
Recursive
               _____________________________________________
              |                                             |
global env -->|                                             |
              |_____________________________________________|
                 ^       ^       ^       ^       ^       ^
(factorial 6)    |       |       |       |       |       |
               __|__   __|__   __|__   __|__   __|__   __|__
              | n:6 | | n:5 | | n:4 | | n:3 | | n:2 | | n:1 |
              |_____| |_____| |_____| |_____| |_____| |_____|

                 (if (= n 1) 1 (* n (factorial (- n 1))))

Iterative
              _________________________________________________________________
             |                                                                 |
   global -->|                                                                 |
   env       |_________________________________________________________________|
               ^         ^       ^       ^       ^       ^       ^       ^
(factorial 6)  |         |       |       |       |       |       |       |
              _|_   _____|_____  |  _____|_____  |  _____|_____  |  _____|_____
             |n:6| |product:1  | | |product:2  | | |product:24 | | |product:720|
             |   | |counter:1  | | |counter:3  | | |counter:5  | | |counter:7  |
             |   | |max-count:6| | |max-count:6| | |max-count:6| | |max-count:6|
             |___| |___________| | |___________| | |___________| | |___________|
                            _____|_____     _____|_____     _____|_____
      (fact-iter 1         |product:1  |   |product:6  |   |product:120|
                 1         |counter:2  |   |counter:4  |   |counter:6  |
                 n)        |max-count:6|   |max-count:6|   |max-count:6|
                           |___________|   |___________|   |___________|

                    (if (> counter max-count)
                        product
                        (fact-iter (* counter product) (+ counter 1) max-count))
"

;;; 10

"
(define W1 (make-withdraw 100))

           ____________________________________________________________
          | make-withdraw:-------------------------------+             |
global -->| W1:-+                                        |             |
env       |_____|________________________________________|_____________|
                |                   ^                    |        ^
                |         __________|__________          V        |
                |   E1 ->| initial-amount: 100 |     ---------    |
                |        |_____________________|     | o | o-+----+
                |                   ^                --+------
                |             ______|_______           |
                |   E2 ----->| balance: 100 |          |
                V            |______________|          |
            ---------               ^                  |
            | o | o-+---------------+                  |
            --+------                                  |
              |                                        |
              V                                        |
parameters: amount                                     |
body: (if (>= balance amount)                          |
          (begin (set! balance (- balance amount))     |
                 balance)                              |
          \"Insufficient funds\")                      |
                                                       V
                          parameters: initial-amount
                          body: (let ((balance initial-amount))
                                  (lambda (amount)
                                    (if (>= balance amount)
                                        (begin (set! balance (- balance amount))
                                        balance)
                                        \"Insufficient funds\")))

(W1 50)

           ______________________________________________
          | make-withdraw: ...                           |
global -->| W1:-+                                        |
env       |_____|________________________________________|
                |                   ^
                |         __________|__________
                |   E1 ->| initial-amount: 100 |
                |        |_____________________|
                |                   ^
                |             ______|______
                |   E2 ----->| balance: 50 |
                V            |_____________|
            ---------           ^       ^
            | o | o-+-----------+    ___|________
            --+------               | amount: 50 |
              |                     |____________|
              V
parameters: amount          (if (>= balance amount))
body: ...                       (begin (set! balance
                                             (- balance amount))
                                       balance)
                                \"Insufficient funds\")

(define W2 (make-withdraw 100))

           ____________________________________________________________________
          |     make-withdraw: ...                                             |
global -->|     W2:-----------------------------+                              |
env       |   +-W1:                             |                              |
          |___|_________________________________|______________________________|
              |                   ^             |                  ^
              |         __________|__________   |        __________|__________
              |   E1 ->| initial-amount: 100 |  |  E3 ->| initial-amount: 100 |
              |        |_____________________|  |       |_____________________|
              |                   ^             |                  ^
              |             ______|______       |            ______|_______
              |   E2 ----->| balance: 50 |      |  E4 ----->| balance: 100 |
              V            |_____________|      V           |______________|
          ---------               ^         ---------              ^
          | o | o-+---------------+         | o | o-+--------------|
          --+------                         --+------
            |                                 |
            |    +----------------------------+
            |    |
            V    V
      parameters: amount
      body: ...

Compared to the version without `let`, there is an extra environment containing
the initial amount for each account object. The environment containing the
balance and the procedure that withdraws from the balance are within this
environment, so they can access the initial amount.
"

;;; 11

"
                                                       +-----------------+
                                                       |                 |
                                                       V                 |
           ____________________________________________________    ------+--
          | make-account:--------------------------------------+-->| o | o |
global -->| acc:---------------------------------------+       |   --+------
env       |____________________________________________|_______|     |
                                                       |             |
                    +----------------------------------+-+           |
                    |                                  | |           |
                    V                                  | |           |
     ______________________________                    V |           |
    | balance: 30                  |               ------+--         |
    | dispatch:--------------------+-------------->| o | o |         |
E1->| withdraw:--------------------+-----------+   --+------         |
    | deposit:---------------------+-------+   |     |               |
    |______________________________|       |   |     |               |
          ^        ^ ^           ^         |   |     |               |
     _____|______  | |      _____|______   |   |     |               |
E2->| amount: 40 | | | E3->| amount: 60 |  |   |     |               |
    |____________| | |     |____________|  |   |     |               |
                   | |                     |   |     |               |
    +--------------+-+---------------------+   |     |               |
    |              | |                         |     |               |
    |   +----------+-+-------------------------+     |               |
    |   |          | |                               |               |
    |   |          | |                               V               +---------+
    |   |          | | parameters: m                                           |
    |   |          | | body: (cond ((eq? m 'withdraw) withdraw)                |
    |   |          | |             ((eq? m 'deposit) deposit)                  |
    |   |          | |             (else                                       |
    |   |          | +---+          (error \"Unknown request: MAKE-ACCOUNT\"   |
    |   |          |     |                 m)))                                |
    |   |    ------+--   |                                                     |
    |   +--->| o | o |   |                                                     |
    |        --+------   +-----------------------------------+                 |
    |          |                                             |                 |
    |          V                                             |                 |
    |   parameters: amount                                   |                 |
    |   body: (if (>= balance amount)                        |                 |
    |             (begin (set! balance (- balance amount))   |                 |
    |                    balance)                            |                 |
    |             \"Insufficient funds\")                    |                 |
    |                                                        |                 |
    |   ---------                                            |                 |
    +-->| o | o-+--------------------------------------------+                 |
        --+------                                                              |
          |                                                                    |
          V                                                                    |
    parameters: amount                                                         |
    body: (set! balance (+ balance amount))                   +----------------+
          balance                                             |
                                                              V
                                                    parameters: balance
                                                    body: (define withdraw ...)
                                                    (define deposit ...)
                                                    (define dispatch ...)
                                                    dispatch

The local state for `acc` is kept in E1.

The local state for `acc2` is distinct from that of `acc` because it is stored
in a separate environment created when `make-account` is invoked in the
definition of `acc2`.

The code for the local procedure definitions is shared between `acc` and `acc2`.
"

;;; 12

"
(b)

      ---------     ---------
x --->| o | o-+---->| o | / |
      --+------     --+------
        |             |
        V             V
      -----         -----
      | a |         | b |
      -----         -----

(b c d)

      ---------     ---------     ---------     ---------
x --->| o | o-+---->| o | o-+---->| o | o-+---->| o | / |
      --+------     --+------     --+------     --+------
        |             |             |             |
        V             V             V             V
      -----         -----         -----         -----
      | a |         | b |         | c |         | d |
      -----         -----         -----         -----
"

;;; 13

"
          +-----------------------------+
          |                             |
          V                             |
      ---------     ---------     ------+--
z --->| o | o-+---->| o | o-+---->| o | o |
      --+------     --+------     --+------
        |             |             |
        V             V             V
      -----         -----         -----
      | a |         | b |         | c |
      -----         -----         -----

(last-pair z) will not terminate.
"

;;; 14

"
`mystery` reverses the list and returns its new head.

      ---------     ---------     ---------     ---------
v --->| o | o-+---->| o | o-+---->| o | o-+---->| o | / |
      --+------     --+------     --+------     --+------
        |             |             |             |
        V             V             V             V
      -----         -----         -----         -----
      | a |         | b |         | c |         | d |
      -----         -----         -----         -----

                                                  v
                                                  |
                                                  V
      ---------     ---------     ---------     ---------
w --->| o | o-+---->| o | o-+---->| o | o-+---->| o | / |
      --+------     --+------     --+------     --+------
        |             |             |             |
        V             V             V             V
      -----         -----         -----         -----
      | d |         | c |         | b |         | a |
      -----         -----         -----         -----

v ==> (a)
w ==> (d c b a)
"

;;; 15

"
      ---------
z1 -->| o | o |
      --+---+--
        |   |
        V   V
      ---------     ---------
x --->| o | o-+---->| o | / |
      --+------     --+------
        |             |
        V             V
     -------        -----
     | wow |        | b |
     -------        -----

      ---------     ---------     ---------
z2 -->| o | o-+---->| o | o-+---->| o | / |
      --+------     --+------     --+------
        |             |             |
        |             V             V
        |           -----         -----
        |           | a |         | b |
        |           -----         -----
        |                           ^
        |                           |
        |           ---------     --+------
        +---------->| o | o-+---->| o | / |
                    --+------     ---------
                      |
                      V
                   -------
                   | wow |
                   -------
"

;;; 16

(define (count-pairs-16 x)
  (if (not (pair? x))
      0
      (+ (count-pairs-16 (car x))
         (count-pairs-16 (cdr x))
         1)))

(define count-pairs-test-3 '(1 2 3))
(count-pairs-16 count-pairs-test-3) ; ==> 3

"
---------     ---------     ---------
| o | o-+---->| o | o-+---->| o | / |
--+------     --+------     --+------
  |             |             |
  V             V             V
-----         -----         -----
| 1 |         | 2 |         | 3 |
-----         -----         -----
"

(define count-pairs-test-4
  (let ((x '(1)))
    (list x x)))
(count-pairs-16 count-pairs-test-4) ; ==> 4

"
---------     ---------
| o | o-+---->| o | / |
--+------     --+------
  |             |
  V             |
---------       |
| o | / |<------+
--+------
  |
  V
-----
| 1 |
-----
"

(define count-pairs-test-7
  (let ((x '(1)))
    (let ((y (cons x x)))
      (cons y y))))
(count-pairs-16 count-pairs-test-7) ; ==> 7

"
---------
| o | o |
--+---+--
  |   |
  V   V
---------
| o | o |
--+---+--
  |   |
  V   V
---------
| o | / |
--+------
  |
  V
-----
| 1 |
-----
"

(define count-pairs-test-infinite
  (let ((x '(1 2 3)))
    (set-cdr! (last-pair x) x)
    x))
; (count-pairs-16 count-pairs-test-infinite) ; ==> does not return

"
    +-----------------------------+
    |                             |
    V                             |
---------     ---------     ------+--
| o | o-+---->| o | o-+---->| o | o |
--+------     --+------     --+------
  |             |             |
  V             V             V
-----         -----         -----
| 1 |         | 2 |         | 3 |
-----         -----         -----
"
