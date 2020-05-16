;;;; Chapter 4

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define sign-get?
  (lambda (m n)
    (or (and (< m 0) (< n 0))
	(and (>= m 0) (< n 0)))))

(define plus
  (lambda (x y)
    (cond
     ((sign-get? x y)
      (cond
       ((zero? y) x)
       (else (plus (sub1 x)
		   (add1 y)))))
     (else
      (cond
       ((zero? y) x)
       (else (plus (add1 x)
		   (sub1 y))))))))

(define minus
  (lambda (x y)
    (cond
     ((zero? y) x)
     (else
      (cond
       ((and (< x 0) (< y 0))
	(plus x (- y)))
       ((and (>= x 0) (< y 0))
	(minus (add1 x)
	       (add1 y)))
       (else
	(minus (sub1 x)
	       (sub1 y))))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (plus (car tup)
		 (addtup (cdr tup)))))))

(define mult
  (lambda (m n)
    (cond
     ((zero? m) 0)
     (else (plus n
		 (mult (sub1 m) n))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (plus (car tup1)
		       (car tup2))
		 (tup+ (cdr tup1)
		       (cdr tup2)))))))

(define pick
  (lambda (n lat)
    (cond
     ((null? lat) ())
     ((= n 1) (car lat))
     (else (pick (sub1 n)
		 (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((= n 1) (cdr lat))
     (else (cons (car lat)
		 (rempick (sub1 n)
			  (cdr lat)))))))

(define no-numbers
  (lambda (lat)
    (cond
     ((null? lat) ())
     ((number? (car lat))
      (no-numbers (cdr lat)))
     (else (cons (car lat)
		 (no-numbers (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) ())
     ((number? (car lat))
      (cons (car lat)
	    (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (atom1 atom2)
    (cond
     ((and (number? atom1) (number? atom2))
      (= atom1 atom2))
     ((or (number? atom1) (number? atom2))
      #f)
     (else (eq? atom1 atom2)))))

(define occur
  (lambda (atom lat)
    (cond
     ((null? lat) 0)
     ((eqan? (car lat) atom)
      (add1 (occur atom (cdr lat))))
     (else (occur atom (cdr lat))))))
