#lang sicp

(define (sumr term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sumr term (next a) next b))))

; why can't they just name the application
; with f?

(define (sum f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (f a) result))))
  (iter a 0))

(define (sum-integers a b)
  (sum identity a inc b))

; pi = 8 * (1 / 1 * 3 + 1 / 5 *  7 + 1 / 9 * 11 + ... )

(define (pi N)
  (* 8 (sum
        (lambda (x) (/ 1.0 (* x (+ x 2))))
        1
        (lambda (n) (+ n 4))
        N)))

(pi 3333)
