#lang sicp

; Integration and stuff.

; Area of unit circle is Pi,
; so if we can do square roots and integration,
; we can also calculate Pi.

(define (unit-circle x)
  (sqrt (- 1 (* x x))))

; Use iterative sum from exercise 1.30
; but this time using a named let,
; which binds symbols to initial values
; and also binds the body itself.

(define (sum f a next b)
  (let iter ((x a) (result 0))
    (if (> x b)
        result
        (iter (next x) (+ (f x) result)))))

(define (integral1 f a b dx)
  (* (sum f (+ a (/ dx 2.0))
            (lambda (x) (+ x dx)) b)
     dx))

(* 4 (integral1 unit-circle 0 1 0.001))

; Simpson's integration rule.
; My distate for conditionals explains
; the slight unrolling.
(define (integral2 f a b n)
  (let ((h (/ (- b a) n)))
    (let ((y (lambda (k) (f (+ a (* k h)))))
          (inc2 (lambda (i) (+ i 2))))
      (/ (* h (+ (y 0) (y n) (* 4 (sum y 1 inc2 (- n 1))) (* 2 (sum y 2 inc2 (- n 2))))) 3))))

(* 4 (integral2 unit-circle 0 1 10000))

; Not too readable, so maybe let's use
; some conditionals

(define (integral3 f a b n)
  (define (h) (/ (- b a) n))
  (define (m k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((odd? k) 2)
          (else 4)))
  (define (y k)
    (* (m k) (f (+ a (* k h)))))
  (/ (* h (sum y 1 inc n)) 3))

; 3.14159265359

(* 4 (integral2 unit-circle 0 1 10000))