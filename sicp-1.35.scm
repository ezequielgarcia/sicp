#lang sicp

(define tolerance 0.0000001)

(define(average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

; 1.35
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(sqrt 9)
(sqrt 16)
(sqrt 2)
