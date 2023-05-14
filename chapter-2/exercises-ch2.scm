;; Exercise 2.1: Define a better version of `make-rat' that handles
;; both positive and negative arguments.  `Make-rat' should normalize
;; the sign so that if the rational number is positive, both the
;; numerator and denominator are positive, and if the rational number
;; is negative, only the numerator is negative.

(define (make-rat n d)
  (let ((g (gcd n d))
        (coef (if (< d 0)
                  -1
                  1)))
    (cons (* (/ n g) coef)
          (* (/ d g) coef))))

;; *Exercise 2.2:* Consider the problem of representing line segments
;; in a plane.  Each segment is represented as a pair of points: a
;; starting point and an ending point.  Define a constructor
;; `make-segment' and selectors `start-segment' and `end-segment'
;; that define the representation of segments in terms of points.
;; Furthermore, a point can be represented as a pair of numbers: the
;; x coordinate and the y coordinate.  Accordingly, specify a
;; constructor `make-point' and selectors `x-point' and `y-point'
;; that define this representation.  Finally, using your selectors
;; and constructors, define a procedure `midpoint-segment' that takes
;; a line segment as argument and returns its midpoint (the point
;; whose coordinates are the average of the coordinates of the
;; endpoints).  To try your procedures, you'll need a way to print
;; points:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (define (average n m) (/ (+ n m) 2))
  (let ((start (start-segment seg))
        (end (end-segment seg)))
        (make-point (average (x-point start)
                             (x-point end))
                    (average (y-point start)
                             (y-point end)))))

(define test-seg
  (make-segment (make-point 16 8)
                (make-point 8 -8)))
(print-point (midpoint-segment test-seg))

;; Exercise 2.3: Implement a representation for rectangles in a
;; plane.  (Hint: You may want to make use of *Note Exercise 2-2::.)
;; In terms of your constructors and selectors, create procedures
;; that compute the perimeter and the area of a given rectangle.  Now
;; implement a different representation for rectangles.  Can you
;; design your system with suitable abstraction barriers, so that the
;; same perimeter and area procedures will work using either
;; representation?

(define (rectangle pt1 pt2 pt3 pt4)
  (cons (cons (cons pt1 pt2) pt3) pt4))

(define (rect-seg rect n)
  (let ((pt1 (caaar rect))
        (pt2 (cdaar rect))
        (pt3 (cdar rect))
        (pt4 (cdr rect)))
    (cond ((= n 0) (make-segment pt1 pt2))
          ((= n 1) (make-segment pt2 pt3))
          ((= n 2) (make-segment pt3 pt4))
          (else (make-segment pt4 pt1)))))

;; assumes lines either vertical or horizontal
(define (segment-length seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (+ (abs (- (x-point start)
               (x-point end)))
       (abs (- (y-point start)
               (y-point end))))))


(define my-rect (rectangle (make-point 1 2)
                           (make-point 3 2)
                           (make-point 1 3)
                           (make-point 3 3)))

;; Have to stop because Alex is yelling at me. This is sufficiently abstract to work with other representations.
(define (rect-perimeter rect get-seg)
  (+ (segment-length (get-seg rect 0))
     (segment-length (get-seg rect 1))
     (segment-length (get-seg rect 2))
     (segment-length (get-seg rect 3))))
(rect-perimeter my-rect rect-seg)

;; Exercise 2.4: Here is an alternative procedural representation
;; of pairs.  For this representation, verify that `(car (cons x y))'
;; yields `x' for any objects `x' and `y'.
;;
      ;; (define (cons x y)
      ;;   (lambda (m) (m x y)))

      ;; (define (car z)
      ;;   (z (lambda (p q) p)))

;; What is the corresponding definition of `cdr'? (Hint: To verify
;; that this works, make use of the substitution model of section
;; *Note 1-1-5::.)

;; (define (cdr z)
;;   (z (lambda (p q) q)))
;; (cdr (cons 1 2))

;; substitution model
;; (cdr (cons 1 2))
;;
;; (cdr (lambda (m) (m 1 2)))
;;
;; ((lambda (m) (m 1 2))
;;    (lambda (p q) q))
;;
;; ((lambda (p q) q) 1 2)
;;
;; 2
;; Did they just smuggle closures in 4 lectures into an intro to CS class?

;; Exercise 2.5: Show that we can represent pairs of nonnegative
;; integers using only numbers and arithmetic operations if we
;; represent the pair a and b as the integer that is the product 2^a
;; 3^b.  Give the corresponding definitions of the procedures `cons',
;; `car', and `cdr'.

;; This is probably a naive and bad implementation.
;; (define (cons-num-pair a b)
;;   (* (expt 2 a) (expt 3 b)))

;; (define (num-divs n d)
;;   (define (rec N i)
;;     (if (= (modulo N d) 0)
;;         (rec (/ N d) (+ i 1))
;;         i))
;;   (rec n 0))

;; (define (car p)
;;   (num-divs p 2))

;; (define (cdr p)
;;   (num-divs p 3))

;; Exercise 2.6: In case representing pairs as procedures wasn't
;; mind-boggling enough, consider that, in a language that can
;; manipulate procedures, we can get by without numbers (at least
;; insofar as nonnegative integers are concerned) by implementing 0
;; and the operation of adding 1 as
;;
      (define zero (lambda (F) (lambda (x) x)))
;;
      (define (add-1 n)
        (lambda (f) (lambda (x) (f ((n f) x)))))
;;
;; This representation is known as "Church numerals", after its
;; inventor, Alonzo Church, the logician who invented the [lambda]
;; calculus.
;;
;; Define `one' and `two' directly (not in terms of `zero' and
;; `add-1').  (Hint: Use substitution to evaluate `(add-1 zero)').
;; Give a direct definition of the addition procedure `+' (not in
;; terms of repeated application of `add-1').

;; proves we can replace the inner lambda with identity
(define my-zero (lambda (F) identity))

;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f (identity x))))
(define one (lambda (f) (lambda (x) (f x))))

;; (add-1 one)
;; (lambda (f) (lambda (x) (f ((one f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (a) (lambda (b) (a b))) f) x)))
;; (lambda (f) (lambda (x) (f ((lambda (b) (f b)) x))))
;; (lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;; if we keep going we just get repeated calls to f
(define three (lambda (f) (lambda (x) (f (f (f x))))))
;; demo with inc function
(define (inc n) (+ n 1))
((one inc) 0)
((two inc) 0)
((three inc) 0)

;; so (((+ one two) inc) 0) should also return 3
;; I guess I accidentally figured out *...
(define (my-* a b)
  (lambda (f)
    (a (b f))))
(((my-* two three) inc) 0)

(define (my-+ a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; Exercise 2.7: Alyssa's program is incomplete because she has not
;; specified the implementation of the interval abstraction.  Here is
;; a definition of the interval constructor:
;;
;;      (define (make-interval a b) (cons a b))
;;
;; Define selectors `upper-bound' and `lower-bound' to complete the
;; implementation.
(define (make-interval x y)
  (cons x y))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                  (max p1 p2 p3 p4))))


;; Exercise 2.8: Using reasoning analogous to Alyssa's, describe
;; how the difference of two intervals may be computed.  Define a
;; corresponding subtraction procedure, called `sub-interval'.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Skipping 2.9 and 2.10 - seem boring
(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(+ (width (make-interval 2 4))
   (width (make-interval 8 16)))

(width (add-interval (make-interval 2 4)
                     (make-interval 8 16)))
;; both of the above eval to 5
(* (width (make-interval 2 4))
   (width (make-interval 8 16)))
;; evals to 4

(width (mul-interval (make-interval 2 4)
                     (make-interval 8 16)))
;; evals to 24




(define (div-interval x y)
  (if (and (>= (upper-bound y) 0)
           (<= (lower-bound y) 0))
      (error "divisor spans 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; this is disgusting and shameful, but correct.
(define (new-mul-interval x y)
  (let ((pos-l-x? (> (lower-bound x) 0))
        (pos-u-x? (> (upper-bound x) 0))
        (pos-l-y? (> (lower-bound y) 0))
        (pos-u-y? (> (upper-bound y) 0)))
          ;; all nums positive
    (cond ((and pos-l-x?
                pos-l-y?)
           (make-interval (* (lower-bound x)
                             (lower-bound y))
                          (* (upper-bound x)
                             (upper-bound y))))
          ;; all nums negative
          ((and (not pos-u-x?)
                (not pos-u-y?))
           (make-interval (* (upper-bound x)
                             (upper-bound y))
                          (* (lower-bound x)
                             (lower-bound y))))
          ;; both x negative, both y positive
          ((and (not pos-l-x?)
                (not pos-u-x?)
                pos-l-y?
                pos-u-y?)
           (make-interval (* (lower-bound x)
                             (upper-bound y))
                          (* (upper-bound x)
                             (lower-bound y))))
          ;; both x positive, both y negative
          ((and pos-l-x?
                pos-u-x?
                (not pos-l-y?)
                (not pos-u-y?))
           (make-interval (* (upper-bound x)
                             (lower-bound y))
                          (* (lower-bound x)
                             (upper-bound y))))
          ;; only lower-bound x negative
          ((and (not pos-l-x?)
                pos-u-x?
                pos-l-y?
                pos-u-y?)
           (make-interval (* (lower-bound x)
                             (upper-bound y))
                          (* (upper-bound x)
                             (upper-bound y))))
           ;; only lower-bound y negative
          ((and pos-l-x?
                pos-u-x?
                (not pos-l-y?)
                pos-u-y?)
           (make-interval (* (upper-bound x)
                             (lower-bound y))
                          (* (upper-bound x)
                             (upper-bound y))))
          ;; only upper-bound x positive
          ((and (not pos-l-x?)
                pos-u-x?
                (not pos-l-y?)
                (not pos-u-y?))
           (make-interval (* (upper-bound x)
                             (lower-bound y))
                          (* (lower-bound x)
                             (lower-bound y))))
          ;; only upper-bound y positive
          ((and (not pos-l-x?)
                (not pos-u-x?)
                (not pos-l-y?)
                pos-u-y?)
           (make-interval (* (lower-bound x)
                             (upper-bound y))
                          (* (lower-bound x)
                             (lower-bound y))))
          (else
           (let ((p0 (* (lower-bound x) (lower-bound y)))
                 (p1 (* (upper-bound x) (lower-bound y)))
                 (p2 (* (lower-bound x) (upper-bound y)))
                 (p3 (* (upper-bound x) (upper-bound y))))
             (make-interval (min p0 p1 p2 p3)
                            (max p0 p1 p2 p3)))))))

(define (rand-sign x)
  (if (= (random 2) 0)
      x
      (* x -1)))

(define (rand-interval)
  (let ((x1 (rand-sign (random 300)))
        (x2 (rand-sign (random 300))))
    (make-interval (min x1 x2) (max x1 x2))))

(define (test)
  (let ((i1 (rand-interval))
        (i2 (rand-interval)))
    (let ((ans (mul-interval i1 i2))
         (new-ans (new-mul-interval i1 i2)))
      (println ans)
      (println new-ans)
      (if (and (= (car ans)
                  (car new-ans))
               (= (cdr ans)
                  (cdr new-ans)))
          '(#t)
          (error "fuck you")))))

(define (rec-test n)
  (if (= n 0)
      #t
      (let ((res (test)))
        (if (car res)
            (rec-test (- n 1))
            (cdr res)))))

(let ((x (rand-interval))
      (y (rand-interval)))
  (let ((ans (mul-interval x y))
        (new-ans (new-mul-interval x y)))
    (println ans)
    (println new-ans)))
         
            
;; Exercise 2.12: Define a constructor `make-center-percent' that
;; takes a center and a percentage tolerance and produces the desired
;; interval.  You must also define a selector `percent' that produces
;; the percentage tolerance for a given interval.  The `center'
;; selector is the same as the one shown above.
(define (make-center-percent center tolerance)
  (let ((delta (* center tolerance)))
    (make-interval (- center delta)
                   (+ center delta))))

;; Exercise 2.14: Demonstrate that Lem is right.  Investigate the
;; behavior of the system on a variety of arithmetic expressions.
;; Make some intervals A and B, and use them in computing the
;; expressions A/A and A/B.  You will get the most insight by using
;; intervals whose width is a small percentage of the center value.
;; Examine the results of the computation in center-percent form (see
;; *Note Exercise 2-12::).

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define (reverse l)
  (define (iter L R)
    (if (null? R)
        L
        (iter (cons (car R) L) (cdr R))))
  (iter '() l))


(append (append (list 1) 2) (list 3))

(reverse (list 3 2 1))

(define (same-parity n . remaining)
  (let ((n-odd? #t))
    (define (rec l  r)
      (if (null? r)
          l
          (if (and (odd? (car r))
                   n-odd?)
              (rec (cons (car r) l) (cdr r))
              (rec l (cdr r)))))
    (define (foo l . r)
      (cdar r))
    (rec (list n) remaining)))

(same-parity 3 1 2 3 5 7 8 13)
(cdr (car (list (list 1 2 3))))
(cdar (list (list 1 2 3)))
              
(define (bar n . remaining)
  (car remaining))

(bar 1 2 3 4)
