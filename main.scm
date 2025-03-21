;; Esse arquivo é o meu caderno de anotações do livro Concrete Abstractions,
;; sem muita ordem. Tem alguns rascunhos, experimentos e respostas de
;; exercícios.

(define power
  (lambda (base exponent)
    (if (= exponent 0)
	1
	(* base (power base (- exponent 1))))))

(define factorial
  (lambda (n)
    (if (= n 1)
	1
	(* n (factorial (- n 1))))))

;; Base case: (factorial 1) terminates with the value 1 because of the evaluation
;; rule for if. Because 1! = 1, (factorial 1) computes the correct value.

;; Induction hyphotesis: Assume that (factorial k) terminates with the value
;; k! for all k in the range 1 <= k <= n

;; Inductive step: Consider evaluating (factorial n), with n > 0. This will
;; terminate if the evaluation of (factorial (- n 1)) does and will have the
;; same value as (* n (factorial (- n 1))). If (factorial (- n 1)) returns (n-1)!,
;; then (factorial n) will return the correct result n!.
;; 
;; (factorial (- n 1)) will reduce until we reach n = 1, our base case, which
;; will return 1. We can be sure it will reach 1 because we always subtract one,
;; until we reach 1. As stated previously, we can see that some
;; (factorial (- k 1)), with k < n, will be correctly solved, which means that
;; (factorial k) will also be correctly solved, all the way to (factorial n).

;; Conclusion: Therefore, by mathematical induction on n, (factorial n) terminates
;; with the value n! for any integer n > 0.

(define square ; doesn't work
  (lambda (n)
    (if (= n 0)
	0
	(+ (square (- n 2))
	   (- (* 4 n) 4)))))

;; Base case: n = 0, evaluates to 0 because of the if-statement, which is correct
;; as 0^2 = 0.

;; Induction hypothesis: Assume that (square k) terminates with the value k^2
;; for all k in the range 0 <= k < n.

;; Inductive step: Consider evaluating (square n), with n > 0. This will terminate
;; if the evaluation of (square (- n 2)) does and will have the same value as
;; (+ (square (- n 2)) (- (* 4 n) 4)) but it may never finish, as we subtract by
;; 2, and may miss 0 by 1 (5, 3, 1, -1...). Ops.


(define square
  (lambda (n)
    (if (= n 0)
	0
	(if (even? n)
	    (* (square (/ n 2))
	       4)
	    (+ (square (- n 1))
	       (- (+ n n) 1))))))

(define quot
  (lambda (n d)
    (if (< n 0)
	(- (quot (- n) d))
	(if (< d 0)
	    (- (quot n (- d)))
	    (if (< n d)
		0
		(+ 1 (quot (- n d) d)))))))

(define quot
  (lambda (n d)
    (cond ((< d 0) (- (quot n (- d))))
	  ((< n 0) (- (quot (- n) d)))
	  ((< n d) 0)
	  (else    (+ 1 (quot (- n d) d))))))

(define multiply
  (lambda (a b)
    (if (= 0 b)
	0
	(+ a (multiply a (- b 1))))))

(define multiply
  (lambda (a b)
    (cond ((< a 0) (- (multiply (- a) b)))
	  ((< b 0) (- (multiply a (- b))))
	  ((= 0 b) 0)
	  (else    (+ a (multiply a (- b 1)))))))
	   

;; The subtract-the-first procedure computes the negative of the sum of all
;; integers from 0 to n inclusive, where n is a non-negative integer. In other
;; words, it calculates (- 0 + 1 + 2 + ... + n).

(define square
  (lambda (n)
    (* n n)))

(define sum-of
  (lambda (n)
    (if (= n 0)
	0
	(+ n (sum-of (- n 1))))))

;; sum the average payoff
(define square-of-average
  (lambda (n)
    (square (/ (sum-of n) n))))

(define sum-of-squares
  (lambda (n)
    (if (= n 0)
	(square 0)
	(+ (square n) (sum-of-squares (- n 1))))))

;; average of the square of payoffs
(define average-of-square
  (lambda (n)
    (/ (sum-of-squares n) n)))


(define subtract-the-first
  (lambda (n)
    (if (= n 0)
	0
	(- n (subtract-the-first (- n 1))))))

;; The result is different because it changes the order of subtraction.
;; It now returns n/2 when n is even, and (n/2)+1 when n
;; is odd, where division yields an integer. So, the same as (ceil (/ n 2))

(define sum-integers-from-to
  (lambda (low high)
    (if (> low high)
	0
	(+ (sum-integers-from-to (+ low 1) high)
	   low))))

(define sum-of-cubes
  (lambda (n)
    (if (= n 0)
	0
	(+ (* n n n) (sum-of-cubes (- n 1))))))

(define power
  (lambda (n p)
    (if (= p 0)
	1
	(* n (power n (- p 1))))))

(define sum-of-powers
  (lambda (n p)
    (if (= n 0)
	0
	(+ (power n p) (sum-of-powers (- n 1) p)))))

(define num-digits
  (lambda (n)
    (cond ((< n 0) (num-digits (- n)))
	  ((< n 10) 1)
	  (else    (+ 1 (num-digits (quotient n 10)))))))

(define num-sixes
  (lambda (n)
    (cond ((= n 0) 0)
	  ((= (remainder n 10) 6) (+ 1 (num-sixes (quotient n 10))))
	  (else (num-sixes (quotient n 10))))))

;; only significant digits, so (num-d 0 0) = 0
(define num-d
  (lambda (n d)
    (cond ((= n 0) 0)
	  ((= (remainder n 10) d) (+ 1 (num-d (quotient n 10) d)))
	  (else (num-d (quotient n 10) d)))))

(define num-odd
  (lambda (n)
    (cond ((= n 0) 0)
	  ((odd? n) (+ 1 (num-odd (quotient n 10))))
	  (else    (num-odd (quotient n 10))))))

(define sum-of-digits
  (lambda (n)
    (if (= n 0)
	0
	(+ (remainder n 10) (sum-of-digits (quotient n 10))))))

(define exponent-of-two
  (lambda (n)
    (if (odd? n)
	0
	(+ 1 (exponent-of-two (/ n 2))))))
	  

(define foo
  (lambda (x n)
    (if (= n 0)
	1
	(+ (expt x n) (foo x (- n 1))))))

;; Hypothesis: foo terminates with (x^(n+1) - 1)/(x-1)
;; Base case: n=0 terminates with 1, (x^(0+1)-1)/x-1
;;                                   = (x^1-1)/x-1
;;                                   = (x-1)/(x-1)
;;                                   = 1
;; so base case holds for any value of 1
;;
;; Inductive step: assume it holds for some integer k, and prove
;; it holds for k+1
;;
;; We expect (x^(k+2)-1)/(x-1)
;;
;; (+ (expt x k+1) (foo x k))
;; = (+ {x^(k+1)} {(x^(k+1)-1)/(x-1)})
;; = x^(k+1) + (x^(k+1)-1)/(x-1)
;; = ... to much algebra to type!
;; = (x^(k+2)-1)/(x-1)

(define presents-on-day
  (lambda (n)
    (if (= n 1)
	1
	(+ n (presents-on-day (- n 1))))))

(define presents-through-day
  (lambda (n)
    (if (= n 1)
	(presents-on-day 1)
	(+ (presents-on-day n) (presents-through-day (- n 1))))))

(define f
  (lambda (n)
    (if (= n 0)
	0
	(+ 2 (f (- n 1))))))

;; hypothesis: for every nonnegative integer n, the procedure
;;   f computes 2n
;; base case: n = 0, computes 0, which is correct
;; inductive step: assume (f k) computes 2k, demonstrate that
;;   (f (+ k 1)) computes 2(k+1)
;;
;; (f (+ k 1)) translates to (+ 2 (f k))
;; = (+ 2 (* k 2))
;; = 2 + 2k
;; by distributing the two, we get
;; 2(k+1)

(define foo
  (lambda (n)
    (if (= n 0)
	2
	(expt (foo (- n 1)) 2))))

;; hypothesis: for all nonnegative integers n, the procedure foo
;;   computes the value 2^(2^n)
;; base case: n = 0, computes 2, 2^(2^0) = 2^1 = 2, which is
;;   the expected value
;; inductive step: assume (foo k) works as intended, attempt to
;;   prove that (foo (+ k 1)) computes 2^(2^(k+1))
;;
;; (foo (- k 1)) evaluates to (expt (foo k) 2)
;; = (2^(2^k))^2
;; = 2^(2 (2^k))
;; = 2^(2^(k+1))

(define f
  (lambda (n)
    (if (= n 0)
	0
	(+ (f (- n 1))
	   (/ 1 (* n (+ n 1)))))))

;; hypothesis: f computes n/(n+1) for any nonnegative integer n.
;; base case: n = 0, computes 0, which is 0/0+1 = 0
;; inductive step: assume (f k) computes k/k+1, attempt to prove
;;   that (f (+ k 1)) computes k+1/k+2
;;
;; (f (+ k 1)) evaluates to
;; (+ (f k) (/ 1 (* (+ k 1) (+ (+ k 1) 1))))
;; = (k/k+1) + 1/((k+1)(k+2))
;; ... multiply left numerator and denominator by (k+2),
;;     sum fractions,
;;     apply perfect square,
;;     divide numerator and denominator by k+1...
;; = k+1/k+2

(define f
  (lambda (image n)
    (if (= n 0)
	image
	(stack-on-itself (f image (- n 1))))))

;; hypothesis: the value of (f image n) is an image 2^n times as high as image,
;;   provided n is a nonnegative integer
;; base case: for n = 0, return the image unchanged, and as 2^0 = 1, it checks
;;   out (is an image 1 time the size of the original)
;; inductive step: assume (f image k) gives an image 2^k times as high as
;;   the original image, demonstrate that (f image (+ k 1)) gives an image
;;   2^(k+1) times as high
;;
;; (f image (+ k 1)) evaluates to (stack-on-itself (f image k))
;; = (stack-on-itself k-image)  with height(k-image) = 2^k height(image)
;; as stack-on-itself gives an image with double the height, we can say that
;; height(stack-on-itself k-image) = 2 height(k-image)
;; = 2 . 2^k height(image)
;; = 2^(k+1) height(image)
;; which is the expected result

(define foo
  (lambda (n)
    (if (= n 0)
	0
	(+ (foo (- n 1))
	   (/ 1 (- (* 4 (square n)) 1))))))

;; hypothesis: for every nonnegative integer n, (foo n) computes n/(2n+1)
;; base case: for n = 0, computes 0, which checks out 0/1 = 0
;; inductive step: assume (foo k) computes k/2k+1, show that (foo (+ k 1)
;;   computes k+1/2(k+1)+1
;;
;; (foo (+ k 1)) evaluates to (k/2k+1) + (1/(4((k+1)^2)-1))
;; =

(define image-of-digit
  (lambda (d)
    (cond ((= d 0) (zero-bb))
	  ((= d 1) (one-bb))
	  ((= d 2) (two-bb))
	  ((= d 3) (three-bb))
	  ...
	  ((= d 9) (nine-bb)))))

(define image-of-number
  (lambda (n)
    (if (<= n 9)
	(image-of-digit n)
	(side-by-side
	 (image-of-number (quotient n 10))
	 (image-of-digit (remainder n 10))))))

(define factorial-product
  (lambda (a b)
    (if (= b 0)
	a
	(factorial-product (* a b) (- b 1)))))

(define factorial
  (lambda (n)
    (factorial-product 1 n)))

;; para o fatorial da direita para a esquerda, não consegui pensar em uma
;; maneira de não precisar carregar o resultado parcial, tenho a impressão
;; de que não existe, a não ser que eu faça recursivo:
;;   (* n (factorial-product f (+ n 1)))
(define factorial-product
  (lambda (f n p)
    (if (= n f)
	p
	(factorial-product f (+ n 1) (* p (+ n 1))))))

(define factorial
  (lambda (n)
    (factorial-product n 1 1)))

	
(define exponent-of-two
  (lambda (n p)
    (if (odd? n)
	p
	(exponent-of-two (/ n 2) (+ 1 p)))))
