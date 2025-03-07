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


