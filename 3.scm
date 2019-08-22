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
