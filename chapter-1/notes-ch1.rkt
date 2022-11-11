;; These notes are not complete as I am only taking notes on things that are new
;; to me or very important. I already have a good understanding of scheme so
;; I will be leaving out a lot of chapter 1.

;; 1.1.3
;; Procedure used to evaluate a combination
;; 1. Evaluate subexpressions
;; 2. When no more subexpressions call function on values
(* (+ 2 (* 4 6))
   (+ 3 5 7))

(* (+ 2 24)
   (+ 3 5 7))

(* 26
   (+ 3 5 7))

(* 26 15)

;; 1.1.4
;; Procedure definitions
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x)) (square y))
(define (f x) (sum-of-squares (+ x 1) (* x 2)))

;; 1.1.5 Applicative Order vs. Normal Order
;; The substitution model for procedure application:
;; Evaluate args then apply procedure call
;; this is applicative order (how the interpreter actually does it)
(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square 6) (square 10))
(+ (* 6 6) (* 10 10))
(+ 36 100)
136

;; Normal order exands all procedures until only left with primitive ops
(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square (+ 5 1)) (square (* 5 2)))
(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
(+ (* 6 6) (* 10 10))
(+ 36 100)
136

;; 1.1.6
;; Conditionals
;; Conditionals are special forms
;; if evals the predicate and then evaluates first expression if true and second if false
;; cond evals each predicate until it finds a true one and then evals the corresponding expression
