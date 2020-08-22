#lang sicp

(define (push lst n)
  (if (null? lst)
    (list n)
    (if (null? (cdr lst))
      (list (car lst) n)
      (cons (car lst) (push (cdr lst) n)))))

(define (reverse lst)
  (if (null? lst)
    nil
    (push (reverse (cdr lst)) (car lst))))

(define (append lst1 lst2)
  (if (null? lst2)
    lst1
    (append (push lst1 (car lst2)) (cdr lst2))))

(define (remove-element lst at)
  (define (iter new old i)
    (if (or (null? old) (= i at))
      (append (reverse new) (cdr old))
      (iter (cons (car old) new) (cdr old) (+ i 1))))
  (iter nil lst 0))

; Now we finally have a remove-element thing
;(remove-element (list 1 2 3 4 5 6) 2)

; Find the smallest
; Return: pair of (position . smallest value)
(define (find-smallest lst)
  (define (iter lst i smallest pos)
    (if (null? lst)
      (cons pos smallest)
      (if (< (car lst) smallest)
        (iter (cdr lst) (+ i 1) (car lst) (+ i 1))
        (iter (cdr lst) (+ i 1) smallest pos))))
  (iter (cdr lst) 0 (car lst) 0))

(define (order-list lst)
  (define (iter old new)
    (if (null? old)
      new
      (iter (remove-element old (car (find-smallest old))) (push new (cdr (find-smallest old))))))
  (iter lst nil))

(define unordered-list (list 0 0 0 0 100 1000 5 9 1 56 43 99 1 2 3 4 5 6 -1 -3 -4 4 3 5 8 9 7 6 3))

(order-list unordered-list)