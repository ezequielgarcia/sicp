#lang sicp

; Procedural compound data
(define (_cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          (else (= m 1) y)))
  dispatch)

(define (_car z) (z 0))
(define (_cdr z) (z 1))

; Define Church numerals
(define _zero (lambda (f) (lambda (x) x)))
(define (_add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; This is equivalent to (list 1 2 3 4) 
(cons 1
  (cons 2
    (cons 3
      (cons 4 '()))))

(define nil '())

; Section 2.2

; Exercise 2.17
(define (last-pair items)
  (if (null? (cdr items))
    (car items)
    (last-pair (cdr items))))

; Exercise 2.18
; (1 2) 3 -> (1 2 3)
(define (push items n)
  (if (null? items)
    (list n)
      (if (null? (cdr items))
        (list (car items) n)
        (cons (car items) (push (cdr items) n)))))

; (1 2 3) -> (3 2 1)
(define (reverse items)
  (if (null? items)
    nil
    (push (reverse (cdr items)) (car items))))

; testing!
;(reverse (list 1))
;(reverse (list 1 2 3))