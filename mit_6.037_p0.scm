#lang sicp

; identity function
(define (foo x) x)
; passing multiple args
(define (bar x y z)
  (+ x y z))

; composing
(foo (foo foo))

; calling them
(foo 1)
(bar 1 2 3)

; fun fact, comparing booleans and values
; is not the same. i guess because true
; and false are actually objects
(eq? (= 0 0) #t)

; use drracket "error"
; (error "bad")

; -------------------------------------
; Get started with 6.037: Project 0

; tests with conditionales
(define (__test func val expected)
  (if (not (= (func val) expected))
    (error "failed test")
    #t))

; cond is more expressive than if
; if is clearly such a sequentialism!
(define (test val expected)
  (cond
    ((not (= val expected)) (error "failed test"))))

(define (testish val expected)
  (cond
    ((> (abs (- val expected)) 1e-4)
     (error "failed testish"))))

(test 0 0)
(testish 0 1e-10)

; ** Problem 1
; y=f(x)
(define (bitfunc x)
  (+ (expt x 4) (* -5 (expt x 2)) 4))

; test the function
(test (bitfunc 0) 4)
(test (bitfunc 1) 0)
(test (bitfunc -1) 0)
(test (bitfunc 2) 0)
(test (bitfunc -2) 0)

; Area is (x2 - x1) * (f at x1)
(define (rect func x1 x2)
  (* (- x2 x1) (func x1)))

; ** Problem 2
(define (bitfunc-rect x1 x2)
  (rect bitfunc x1 x2))

; ** Problem 3
;(define (bitfunc-integral-iter num-steps x1 x2)
;  (for )
;  (rect bitfunc x1 x2))

; ** Problem 3
; Recursive - using tail recursion properly, I hope
(define (bitfunc-integral num-steps x1 x2)
  (let ((step (/ (- x2 x1) num-steps)))
    (let loop ((x x1) (area 0))
      (if (>= x x2)
        area
        (loop
          (+ x step)
          (+ area (bitfunc-rect x (+ x step))))))))

; We know the function is symmetric
(testish
  (bitfunc-integral 100 -10 -9)
  (bitfunc-integral 100 9 10))
(testish
  (bitfunc-integral 100 -2 -1)
  (bitfunc-integral 100 1 2))

; This one works particularly bad
(-
  (bitfunc-integral 1000 0 1)
  (bitfunc-integral 1000 -1 0))
