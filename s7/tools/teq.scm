;;; cyclic/shared timing tests

;(set! (*s7* 'heap-size) (* 2 1024000))

;;; equal? write/object->string/format cyclic-sequences

(define* (make-circular-list n init)
  (let ((lst (make-list n init)))
    (set-cdr! (list-tail lst (- n 1)) lst)))

(define list-0 (list 1 2 3 4))
(define vect-0 (vector 1 2 3 4))
(define let-0 (inlet :a 1 :b 2))
(define hash-0 (hash-table :a 1 :b 2))

(define list-1 (make-circular-list 1))
(set-car! list-1 #t)
(define hash-1 (hash-table :a list-1))
(define vect-1 (vector list-1))
(define let-1 (inlet :a list-1))
(define list-2 (list list-1 list-1))
(define list-3 (make-circular-list 3))
(define vect-2 (let* ((z (vector 1 2))
		      (y (list 1 z 2))
		      (x (hash-table 'x y)))
		 (set! (z 1) x)
		 z))
(define vect-3 (let ((x '(1 2)))
		 (vector x (list x x))))
(define vect-4 (let ((v (vector 1 2 3 4))
		     (lst (list 1 2)))
		 (set-cdr! (cdr lst) lst)
		 (set! (v 0) v)
		 (set! (v 3) lst)))
(define hash-2 (let ((h1 (make-hash-table 11)))
		 (hash-table-set! h1 "hi" h1)))
(define list-4 (let ()
		 (define make-node list)
		 (define prev (dilambda (lambda (node) (node 0)) (lambda (node val) (set! (node 0) val))))
		 (define next (dilambda (lambda (node) (node 2)) (lambda (node val) (set! (node 2) val))))
		 ;(define data (dilambda (lambda (node) (node 1)) (lambda (node val) (set! (node 1) val))))
		 (let* ((head (make-node () 0 ()))
			(cur head))
		   (do ((i 1 (+ i 1)))
		       ((= i 8))
		     (let ((next-node (make-node cur i ())))
		       (set! (next cur) next-node)
		       (set! cur (next cur))))
		   (set! (next cur) head)
		   (set! (prev head) cur)
		   head)))

(define let-2 (let ((hi 3))
		(let ((e (curlet)))
		  (set! hi (curlet)) 
		  e)))
(define let-3 (let* ((e (inlet 'a 0 'b 1))
		     (e1 (inlet 'a e)))
		(set! (e 'b) e1)
		e))
(define let-4 (inlet :a vect-0 :b list-0))
(define hash-3 (hash-table :a vect-0 :b list-0))
(define hash-4 (hash-table :a hash-1))

(define-constant teq-vars (list list-0 list-1 list-2 list-3 list-4
			      vect-0 vect-1 vect-2 vect-3 vect-4
			      hash-0 hash-1 hash-2 hash-3 hash-4
			      let-0 let-1 let-2 let-3 let-4))

;(format *stderr* "~A ~A ~A ~A ~A~%" (length hash-0) (length hash-1) (length hash-2) (length hash-3) (length hash-4))

;(set! (*s7* 'initial-string-port-length) 64)

(define (equal-tests size)
  (let ((str #f)
	(vj #f)
	(p (open-output-string)))
    (do ((i 0 (+ i 1)))                 ; nested for-each is almost as fast
	((= i size))
      (do ((a teq-vars (cdr a)))
	  ((null? a))
	(set! vj (car a))
	(do ((b teq-vars (cdr b)))
	    ((null? b))
	  (if (equal? vj (car b))
	      (if (not (eq? a b))
		  (format *stderr* "oops!: ~A ~A~%" a b))))
	(write vj p)
	(get-output-string p #t)
	(object->string vj)
	(set! str (format #f "~A~%" vj)) ; set! to cancel the optimization to format_nr
	(cyclic-sequences vj)))
    (close-output-port p)))

(equal-tests 20000)

(define (equivalent-tests size)
  (let ((vj #f))
  (do ((i 0 (+ i 1)))
      ((= i size))
    (do ((a teq-vars (cdr a)))
	((null? a))
      (set! vj (car a))
      (do ((b teq-vars (cdr b)))
	  ((null? b))
	(if (equivalent? vj (car b))
	    (if (not (eq? a b)) ; (and ...) would look nicer, but this way is much faster
		(format *stderr* "oops!: ~A ~A~%" a b))))))))

(equivalent-tests 20000)

(when (> (*s7* 'profile) 0)
  (show-profile 200))
(exit)
