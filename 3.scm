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

;;; 17

(define (count-pairs-17 x)
  (define (go x visited)
    (if (or (not (pair? x)) (memq x visited))
        (cons 0 visited)
        (let ((new-visited (cons x visited)))
          (let ((car-result (go (car x) new-visited)))
            (let ((car-count (car car-result))
                  (car-visited (cdr car-result)))
              (let ((cdr-result (go (cdr x) car-visited)))
                (let ((cdr-count (car cdr-result))
                      (cdr-visited (cdr cdr-result)))
                  (cons (+ car-count cdr-count 1) cdr-visited))))))))
  (car (go x '())))

;;; 18

(define (cyclical-18? x)
  (define (go x visited)
    (cond ((null? x) false)
          ((memq x visited) true)
          (else (go (cdr x) (cons x visited)))))
  (go x '()))

;;; 19

(define (cyclical-19? x)
  (define (go a b)
    (if (null? a)
        false
        (let ((a1 (cdr a)))
          (if (null? a1)
              false
              (let ((a2 (cdr a1))
                    (b1 (cdr b)))
                (if (eq? a2 b1)
                    true
                    (go a2 b1)))))))
  (go x x))

;;; 20

"
               _________________________________
              | cons: ...                       |
global env -->| x:------------------+           |
              | z:------------------+-------+   |
              |_____________________|_______|___|
                                    |       |
               _____________        |       |
              | x:17        |       |       |
              | y:2         |<------+-+     |
          E1->| set-x!: ... |       V |     |
              | set-y!: ... |   ------+--   |
              | dispatch:---+-->| o | o |   |
              |_____________|   --+------   |
                    ^  ^          |         |
               _____|__|____      |         |
              | x:--+  |    |     |         |
              | y:-----+    |<----+---------+-+
          E2->| set-x!: ... |     |         V |
              | set-y!: ... |     |     ------+--
              | dispatch:---+-----+---->| o | o |
              |_____________|     |     --+------
                                  |       |
                                  V       V
                                parameters: m
                                body: (cond ...)
"

;;; 21

(define (front-ptr-21 queue) (car queue))
(define (rear-ptr-21  queue) (cdr queue))
(define (set-front-ptr-21! queue item)
  (set-car! queue item))
(define (set-rear-ptr-21!  queue item)
  (set-cdr! queue item))

(define (empty-queue-21? queue)
  (null? (front-ptr-21 queue)))

(define (make-queue-21) (cons '() '()))

(define (front-queue-21 queue)
  (if (empty-queue-21? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr-21 queue))))

(define (insert-queue-21! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue-21? queue)
           (set-front-ptr-21! queue new-pair)
           (set-rear-ptr-21! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr-21 queue) new-pair)
           (set-rear-ptr-21! queue new-pair)
           queue))))

(define (delete-queue-21! queue)
  (cond ((empty-queue-21? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr-21! queue (cdr (front-ptr-21 queue)))
              queue)))

"The interpreter interprets the queue as a standard cons-list so it seems like
there are duplicates in the queue because the front and rear pointers point to
shared data (the data at the end of the queue).

After 'a is inserted, q1 is (cons (cons a '()) (cons a '())) which is displayed
as ((a) a).
After 'b is inserted, q1 is (cons (cons a (cons b '())) (cons b '())) which is
displayed as ((a b) b)."

(define (print-queue-21 queue)
  (display (front-ptr-21 queue)))

;;; 22

(define (make-queue-22)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" dispatch)
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" dispatch))
            (else (set! front-ptr (cdr front-ptr))
                  dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)))
    dispatch))

(define (empty-queue-22? queue) ((queue 'empty-queue?)))
(define (front-queue-22 queue) ((queue 'front-queue)))
(define (insert-queue-22! queue item) ((queue 'insert-queue!) item))
(define (delete-queue-22! queue) ((queue 'delete-queue!)))

;;; 23

;; deque-cell

(define (make-deque-cell item next prev)
  (cons item (cons next prev)))

(define (item-deque-cell cell) (car cell))
(define (next-deque-cell cell) (cadr cell))
(define (prev-deque-cell cell) (cddr cell))

(define (set-next-deque-cell! cell next)
  (set-car! (cdr cell) next))
(define (set-prev-deque-cell! cell prev)
  (set-cdr! (cdr cell) prev))

;; deque

(define (make-deque)
  (let ((deque (make-deque-cell '() '() '())))
    (set-next-deque-cell! deque deque)
    (set-prev-deque-cell! deque deque)
    deque))

(define (empty-deque? deque)
  (eq? (next-deque-cell deque) deque))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (item-deque-cell (prev-deque-cell deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (item-deque-cell (next-deque-cell deque))))

(define (front-insert-deque! deque item)
  (let ((prev-front (prev-deque-cell deque)))
    (let ((new-front (make-deque-cell item deque prev-front)))
      (set-next-deque-cell! prev-front new-front)
      (set-prev-deque-cell! deque new-front)
      deque)))

(define (rear-insert-deque! deque item)
  (let ((prev-rear (next-deque-cell deque)))
    (let ((new-rear (make-deque-cell item prev-rear deque)))
      (set-prev-deque-cell! prev-rear new-rear)
      (set-next-deque-cell! deque new-rear)
      deque)))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "FRONT-DELETE! called with an empty deque" deque)
      (let ((new-front (prev-deque-cell (prev-deque-cell deque))))
        (set-next-deque-cell! new-front deque)
        (set-prev-deque-cell! deque new-front)
        deque)))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "REAR-DELETE! called with an empty deque" deque)
      (let ((new-rear (next-deque-cell (next-deque-cell deque))))
        (set-prev-deque-cell! new-rear deque)
        (set-next-deque-cell! deque new-rear)
        deque)))

(define (print-deque deque)
  (define (go cell)
    (if (not (eq? cell deque))
        (begin (display (item-deque-cell cell))
               (display " ")
               (go (prev-deque-cell cell)))))
  (go (prev-deque-cell deque)))

;;; 24

(define (make-table-with-predicate same-key?)
  (define (custom-assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (custom-assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (custom-assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (custom-assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (custom-assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (custom-assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;;; 25

(define (make-nd-table)
  (list '*table*))

(define (lookup-nd keys table)
  (cond ((not table) false)
        ((null? keys) (cdr table))
        (else (lookup-nd (cdr keys)
                         (assoc (car keys) (cdr table))))))

(define (insert-nd! keys value table)
  (if (null? keys)
      (set-cdr! table value)
      (let ((subtable (assoc (car keys) (cdr table))))
        (define (make-subtables ks)
          (cons (car ks)
                (if (null? (cdr ks))
                    value
                    (list (make-subtables (cdr ks))))))
        (if subtable
            (insert-nd! (cdr keys) value subtable)
            (set-cdr! table (cons (make-subtables keys)
                                  (cdr table))))))
  'ok)

;;; 26

;; tree

(define (make-tree key value)
  (cons (cons key value) (cons '() '())))

(define (key-tree tree) (caar tree))
(define (value-tree tree) (cdar tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (cddr tree))

(define (set-value! tree value) (set-cdr! (car tree) value))
(define (set-left-branch! tree left) (set-car! (cdr tree) left))
(define (set-right-branch! tree right) (set-cdr! (cdr tree) right))

;; tree-table

(define (make-tree-table)
  (list '*table*))

(define (lookup-tree key table)
  (define (go tree)
    (if (null? tree)
        false
        (let ((k (key-tree tree)))
          (cond ((< key k) (go (left-branch tree)))
                ((> key k) (go (right-branch tree)))
                (else (value-tree tree))))))
  (go (cdr table)))

(define (insert-tree! key value table)
  (define (go tree)
    (let ((k (key-tree tree)))
      (cond ((< key k) (if (null? (left-branch tree))
                           (set-left-branch! tree (make-tree key value))
                           (go (left-branch tree))))
            ((> key k) (if (null? (right-branch tree))
                           (set-right-branch! tree (make-tree key value))
                           (go (right-branch tree))))
            (else (set-value! tree value)))))
  (if (null? (cdr table))
      (set-cdr! table (make-tree key value))
      (go (cdr table)))
  'ok)

;;; 27

"
environments:
  global:
    memoize: procedure memoize
    memo-fib: procedure memoized-fib
  E1: -> global ; (memoize (lambda (n) ...))
    f: procedure memo-fib-body
  E2: -> E1 ; (let ((table (make-table))))
    table: (*table* (2 . 1) (0 . 0) (1 . 1))
  E3: -> E2 ; (memo-fib 3)
    x: 3
  E4: -> E3 ; (let ((previously-computed-result (lookup 3 table))))
    previously-computed-result: false
  E5: -> E4 ; (let ((result (f 3))))
    result: 2
  E6: -> global ; ((lambda (n) ...) 3)
    n: 3
  E7: -> E2 ; (memo-fib 2)
    x: 2
  E8: -> E7 ; (let ((previously-computed-result (lookup 2 table))))
    previously-computed-result: false
  E9: -> E8 ; (let ((result (f 2))))
    result: 1
  E10: -> global ; ((lambda (n) ...) 2)
    n: 2
  E11: -> E2 ; (memo-fib 1)
    x: 1
  E12: -> E11 ; (let ((previously-computed-result (lookup 1 table))))
    previously-computed-result: false
  E13: -> E12 ; (let ((result (f 1))))
    result: 1
  E14: -> global ; ((lambda (n) ...) 1)
    n: 1
  E15: -> E2 ; (memo-fib 0)
    x: 0
  E16: -> E15 ; (let ((previously-computed-result (lookup 0 table))))
    previously-computed-result: false
  E17: -> E16 ; (let ((result (f 0))))
    result: 0
  E18: -> global ; ((lambda (n) ...) 0)
    n: 0
  E19: -> global ; (memo-fib 1)
    x: 1
  E20: -> E19 ; (let ((previously-computed-result (lookup 2 table))))
    previously-computed-result: 1

procedures:
  memoize:
    environment: global
    parameters: f
    body: (let ((table (make-table)))
            (lambda (x) ...))
  memoized-fib:
    environment: E2
    parameters: x
    body: (let ((previously-computed-result (lookup x table)))
            (or previously-computed-result ...))
  memo-fib-body:
    environment: global
    parameters: n
    body: (cond ((= n 0) 0)
                ...)
"

"memo-fib has O(n) complexity because for any k it only needs to compute f(k)
once. To compute f(n), f(k) needs to be computed for every k in [0, n].
Therefore, there are n + 1 calls to f in the computation of f(n) for any n,
which is proportional to n.
Each call to memo-fib, including the table operations, takes a constant number
of steps. If x = 1, the table will be empty and f(1) can be computed in constant
time. If x = 0, the table will have 1 record (1) and f(0) can also be computed
in constant time. If x >= 2, f(n - 1) and f(n - 2) will have already been
computed, and since records are added to the table with keys in ascending order
(except for 1 and 0), the records for n - 1 and n - 2 will be at most 2 cdrs
away from the head of the table (except for when n = 3, where 1 is 3 cdrs away).
Therefore, the lookup operation always takes constant time. The insert operation
is also O(1) because it adds the new record at the head of the list.
"

"(define memo-fib (memoize fib)) would not work, because when fib makes
recursive calls, it calls fib, which is the non-memoized version, instead of
memo-fib, so the results cannot be saved in the table."

;;; 28

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;;; 29

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate-compound a1 a2 output)
  (let ((w1 (make-wire)) (w2 (make-wire)) (w3 (make-wire)))
    (inverter a1 w1)
    (inverter a2 w2)
    (and-gate w1 w2 w3)
    (inverter w3 output)
    'ok))

"The delay time is 2 * inverter-delay + and-gate-delay."

;;; 30

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder as bs ss c)
  (if (or (null? as) (null? bs) (null? ss))
      'ok
      (let ((c-out (make-wire)))
        (full-adder (car as) (car bs) c (car ss) c-out)
        (ripple-carry-adder (cdr as) (cdr bs) (cdr ss) c-out))))

;; TODO: calculate delay
