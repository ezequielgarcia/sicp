#lang sicp

; The sqrt implementation is very beatiful,
; so I wanted to write it here.
; It only needs a conditional (to decide when to stop)
; and primitive procedures for addition and division.

; ** The original
(define (sqrt-iter guess x)
  (if (almost? guess x)
      guess
      (sqrt-iter (improve-sqrt guess x) x)))

(define (improve-sqrt guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

; Note that this is close to the assertish
; proposed in mit/6.037/p1 solution
(define (almost? guess x)
  (< (abs (- (square guess) x)) 1e-9))

(define (sqrt1 x)
  (sqrt-iter 1.0 x))

; ** Using a named let
(define (sqrt2 x)
  (let __sqrt-iter ((guess 1)) 
    (if (almost? guess x)
      guess
      (__sqrt-iter (average guess (/ x guess))))))

(sqrt2 9)

;(if test if-true if-false)

;Ex. 1.6

(define (test-eq a b)
  (cond ((= a b) #t)
        ((not (= a b)) #f)))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt3 x)
  (let __sqrt-iter ((guess 1))
    (new-if (almost? guess x)
      guess
      (__sqrt-iter (average guess (/ x guess))))))

;infinite recursion, due to order of evaluation.
;(sqrt3 9)