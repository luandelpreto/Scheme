;;;; Chapter 3

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a)
      (cdr lat))
     (else (cons (car lat)
		 (rember a (cdr lat)))))))

(define firsts
  (lambda (lst)
    "Takes one list as argument, which is either a null
   list or contains only  non-empty lists. It  builds
   another list composed of the first S-expression of
   each internal list"
    (cond
     ((null? lst) (quote ()))
     (else (cons (caar lst)
		 (firsts (cdr lst)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons old
	    (cons new
		  (insertR new old (cdr lat)))))
     (else (cons (car lat)
		 (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons new lat))
     (else (cons (car lat)
		 (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    "Replaces the first occurrence of old in lat with
     new"
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons new (cdr lat)))
     (else (cons (car lat)
		 (subst new old (cdr lat)))))))

(define subst2
  (lambda (new old1 old2 lat)
  "Replaces either the first occurrence of old1
   or the first occurrence of old2 by new"
  (cond
   ((null? lat) (quote ()))
   ((or (eq? (car lat) old1) (eq? (car lat) old2))
    (cons new (cdr lat)))
   (else (cons (car lat)
	       (subst2 new old1 old2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    "Removes all occurrences of a in lat"
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a)
      (multirember a (cdr lat)))
     (else (cons (car lat)
		 (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons old
	    (cons new
		  (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? (car lat) old)
      (cons new
	    (cons old
		  (multiinsertL new old (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? (car lat) old)
      (cons new
	    (multisubst new old (cdr lat))))
     (else (cons (car lat)
		 (multisubst new old (cdr lat)))))))
