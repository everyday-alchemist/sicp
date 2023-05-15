;; 2.2 - Hierarchical Data and the Closure Property

;; Closure property - when an operation that combines things produces a result
;;   that can be combined with the same operation. i.e. cons produces a pair of
;;   cons cells which themselves can be combined with cons

;; 2.2.1 - Representing Sequences

;;   In lisp languages sequences are represented as a chain of pairs. The car of
;; each pair is an item in the chain. The cdr is the next pair, with the
;; last cdr being nil. Sequences of cons cells combined in this manner are
;; referred to as lists.
(cons 1 (cons 2 (cons 3) (cons 4 nil)))
(list 1 2 3 4)

;; Common list algorithm patterns:
(define (length items)
	(if (null? items)
			0
			(+ 1 (length (cdr items)))))

(define (append list1 list2)
	(if (null? list1)
			list2
			(cons (car list1) (append (cdr list1) list2))))

(define (map op items)
	(if (null? items)
			#nil
			(cons (op (car items))
						(map op (cdr items)))))

;; 2.2.2 - Hierarchical Structures
;; We can think of a sequence of sequences as a tree
;; ((1 2) 3 4)
;;         .
;;      /    / \
;;     / \  3  4
;;    1   2

;; algorithm to count the number of leaves in a tree
(define (count-leaves tree)
	(cond ((null? tree) 0)
				((not (pair? tree)) 1)
				(else (+ (count-leaves (car tree))
								 (count-leaves (cdr tree))))))
