#lang sicp

; define variables
(define size 2)
(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))

; procedure definitions

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;(define (f a)
 ; (sum-of-squares (+ a 1) (* a 2)))

; conditional expressions

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; another way to define the abs function

(define (abs-2 x)
  (cond ((< x 0) (- x))
        (else x)))

; yet another way to write the absolute value function

(define (abs-3 x )
  (if (< x 0)
      (- x)
      x))

; logical operators:
; and: (and <e1> ... <en>)
; or: (or <e1> ... <en>)
; not: (not <e>)


; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
(* 3 (- 6 2) (- 2 7)))

; Exercise 1.3:
(define (sum-of-square-of-two-largest x y z)
  (cond ( (and (< x y) (< x z)) (sum-of-squares y z))
        ( (< y z) (sum-of-squares x z))
        (else (sum-of-squares x y))))

; Exercise 1.5: under applicative-order evaluation, the interpreter evaluates
; inputs first, namely 0 and (p). Since (p) evaluates to itself, it ends up
; in an infinite loop. In a normal-order evaluation this wouldn't happen.
; After expanding the definitions, it would check the condition (= x 0) and
; never evaluates (p).


; Newton's method for computing square roots. The idea is the following: given
; a guess, if the guess is not good enough, we improve it via
; new_guess = (average guess (/ x guess)), where x is the number for which
; we want to take the square root.

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) (average guess (/ x guess)))
;
; (define (good-enough? guess x)
;   (< (abs (- (square guess) x)) 0.001))
;
; (define (sqrt-iter guess x)
;   (if (good-enough? guess x)
;     guess
;     (sqrt-iter (improve guess x) x)))
;
; (define (sqrt x) (sqrt-iter 1.0 x))


; Exercise 1.6: The new-if fails, because cond evaluates both subexpressions,
; creating an infinite loop. On the other hand, if only evaluates the second
; condition when is needed.


; Exercise 1.7:
(define (sqrt-iter-2 guess x)
  (define new-guess (improve guess x))
    (if (< (abs (- new-guess guess)) (* 0.001 guess))
      guess
    (sqrt-iter-2 new-guess x)))
(define (sqrt-2 x) (sqrt-iter-2 1.0 x))


; Writing the sqrt function using block structures:

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
    (sqrt-iter 1.0))

; recursive definition of factorial
(define (factorial-rec n)
  (if (= n 1) 1 (* n (factorial-rec (- n 1)))))

; iterative definition of factorial
(define (factorial-iter n)
  (define (iter product counter)
      (if (> counter n)
          product
          (iter (* counter product)
                (+ counter 1))))
  (iter 1 1))

; recursive definition of Fibonacci

(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

; iterative definition of Fibonacci

(define (fib-iter n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))


; Exercise 1.11:

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
          (* 2 (f-rec (- n 2)))
          (* 3 (f-rec (- n 3))))))

; similar as fibonacci, but now with three parameters

(define (f-iter n)
  (define (iter a b c count)
      (if (= count 0)
          c
          (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (iter 2 1 0 n))

; Exercise 1.12: this procedure essentially computes the binomial coefficient
; n over k.

(define (pascal-triangle n k)
  (cond ((= n k) 1)
        ((= k 0) 1)
        ((= k 1) n)
        (else (+ (pascal-triangle (- n 1) (- k 1))
            (pascal-triangle (- n 1) k)))))

(define (pascal-triangle-iter n k)
  (/ (factorial-iter n) (* (factorial-iter k) (factorial-iter (- n k)))))

; Exercise 1.15: Without assuming that we can use the relation sin(360+x)= sin(x),
; the number of steps grows logarithmically in the size of the input x, and
; the amount of space grows linearly on x. Intuitively, the number of steps
; can be approximated by solving the equation 12 = (0.1)* 3^n, giving
; n = log_3(10*x). On the other hand, we need roughly 2^(log n) space, which
; is proportional to n.


; Exponentiation

; recursive version:  O(n) space and time
(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))
; iterative version: O(1) space and O(n) time.

(define (expt-iter b n)
  (define (iter power counter product)
    (if (= counter 0)
        product
        (iter power (- counter 1) (* b product))))
  (iter b n 1))


; fast-recursive version: O(log(n)) space and time

(define (fast-expt b n)
  (cond ((= n 0) 1)
         ((even? n) (square (fast-expt b (/ n 2))))
         (else (* b (fast-expt b (- n 1))))))

; Exercise 1.16: as suggested in the hint, the idea is that the expression
; a*(power^counter) remains invariant, while we divide the counter when it
; is possible (namely, when it is even)

(define (fast-expt-iter b n)
  (define (iter power counter a)
    (cond ((= counter 0) a)
           ((even? counter) (iter (square power) (/ counter 2) a))
           (else (iter power (- counter 1) (* a power)))))
  (iter b n 1))

; Exercise 1.17:

(define (mult a b)
  (define (double n) (+ n n))
  (define (halve n) (/ n 2)) ;assume even n
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double(mult a (halve b))))
        (else (+ a (mult a (- b 1))))))

; Exercise 1.18:

(define (mult-iter a b)
  (define (double n) (+ n n))
  (define (halve n) (/ n 2)) ;assume even n
  (define (iter x y a)
    (cond ((= y 0) a)
          ((even? y) (iter (double x) (halve y) a))
          (else (iter x (- y 1) (+ a x) ))))
  (iter a b 0))

; Euclid's algorithm

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; finding smallest divisor greater than one of an integer

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (+ 1 test-divisor)))))
  (find-divisor 2))

; primality test
(define (prime? n)
  (and (> n 1) (= n (smallest-divisor n))))

; computes (base^exp) (mod m)
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder
          (square (expmod base (/ exp 2) m))
          m))
          (else
           (remainder
            (* base (expmod base (- exp 1) m))
            m))))

; Fermat test: by Fermat's little theorem, we know that if for some a<n
; we have a^n=a (mod n), then a is not prime. Thus, the test checks for random
; numbers whether a^n=a(mod n). Beware: the converse is not true! see the
; Carmichael numbers

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a ))
  ; (random n) returns a random integer between 0 and (n-1)
  (try-it (+ 1 (random (- n 1))))) ; here I am using a racket package for large random numbers

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((= n 1) #f)
        ((fermat-test n) (fast-prime? n (- times 1))) (else #f)))



; Exercise 1.21:

(display (smallest-divisor 199))
(newline)
(display (smallest-divisor 1999))
(newline)
(display (smallest-divisor 19999))
(newline)

; Exercise 1.22:

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n ) 
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end) ; checks primality of odd integers in range 1..n (including n)
  (cond ((< start 3) (search-for-primes 3 end))
        ((even? start) (search-for-primes (+ start 1) end))
        (else
         (cond ((< start end)  
                 (timed-prime-test start) (search-for-primes (+ start 1) end))))))

; The tests (for numbers of order 10^9 and larger) show that growth of the algorithm is of the correct order.


; Higher-order abstractions


; Abstract sum of a sequence from a to b, 

(define (sum term a  next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (integral f a b n); instead of dx from sicp use dx=(b-a)/n
  (define (dx x) (+ x (/ (- b a) n)))
  (/ (sum f a dx b) n))
    
(define (cube x) (* x x x))
(define (double x) (* 2 x))
(define (identity x) x)

; Exercise 1.30:

(define (sum-iter term a next b)
  (define (iter a result)
    (if  (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31:

(define (product term a next b )
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if  (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


(define (wallis-term n)
   (/ (* (double n) (double (inc n)))
         (square (inc (double n)))))
(define (pi-wallis n)
  (* 4.0 (product wallis-term 1 inc n)))

; Exercise 1.32:

(define (accumulate combiner null-value term a next b )
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b )
  (define (iter a result)
    (if  (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; Exercise 1.33:

(define (filtered-accumulate predicate combiner null-value term a next b )
  (cond ((> a b) 
      null-value)
        ((predicate a) (combiner (term a) (filtered-accumulate predicate combiner null-value term (next a) next b)))
        (else (filtered-accumulate predicate combiner null-value term (next a) next b))))

; lambda and let

; Examples of lambda:

;(lambda (x) (+ x 4))
;(lambda (xy) (+ x y))

; Examples of let

; (+ (let ( (x 3) (+ x (* x 10)))) x) if x=5, then the output is 38
; (let ((x 3)(y (+ x 2)))(* x y))  if x=2, then the output is 12. Reason: the x in  (y (+ x 2)) is evaluated to 2.
; In general, the local variables in a let only affects the body. In this case, the x=3 only affects the product (* x y)

; Exercise 1.34:
; If we define (define (f g) (g 2)), then (f f) would reduce to (f 2), but "2" is not a procedure, so it's illegal.
; This is analogous to what happens in the lambda calculus.


; Finding roots of continuous functions on an interval

(define (close-enough? x y tolerance) (< (abs (- x y)) tolerance))

(define (search f neg-point pos-point tolerance)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point tolerance)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint tolerance))
                ((negative? test-value)
                 (search f midpoint pos-point tolerance))
                (else midpoint))))))

(define (half-interval-method f a b tolerance)
  (let ((x (f a)) (y (f b)))
    (cond ( (and (negative? x) (positive? y) ) (search f a b tolerance))
          ( (and (negative? y (positive? x) ) (search f b a tolerance)))
          (else
           (error "Values are not of opposite sign" a b tolerance)))))

; Example: finding a root of x^3-2x-3 in the interval [1,2]
(display (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0
2.0 0.001))
(newline)

(define (fixed-point f first-guess tolerance)
  (define (try guess)
    (let ((next (f guess)))
          (if (close-enough? next guess tolerance)
              next
              (try next))))
  (try first-guess))

; Example: finding fixed-point of cos(x):
(display (fixed-point cos 1.0 0.001))
(newline)

; Newton's method for finding roots: when f is smooth enough and for most guesses, one can find a root
; of f by finding a fixed-point of the function g(x)=x- f(x)/f'(x)

(define (deriv f dx)
  (lambda (x) (/  (- (f (+ x dx)) (f x)) dx)))

(define (newton-transform g dx)
(lambda (x) (- x (/ (g x) ((deriv g dx) x)))))

(define (newtons-method g guess dx tolerance)
  (fixed-point (newton-transform g dx) guess tolerance))

; Exercise 1.40:

(define (cubic-poly a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c )))

; Exercise 1.41:

(define (double-proc f)
  (lambda (x) (f (f x))))

; Exercise 1.42:
(define (compose f g)
  (lambda (x) (f (g x))))

; Exercise 1.43:

(define (repeated f n)
  (cond ((= n 0) identity)
        ((= n 1) f)
        (else (compose f (repeated f (- n 1))))))