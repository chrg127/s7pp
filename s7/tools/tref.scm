(define tries 10)

(define (ft3)
  (let ((size 100000))
    (let ((v (make-float-vector size)))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (set! (v j) (+ (v j) 1.0))))
      v)))

(let ((v (ft3)))
  (if (not (equal? v (make-float-vector 100000 tries)))
      (format *stderr* "ft3: ~A~%" v)))

(define (ft31)
  (let ((size 100000))
    (let ((v (make-float-vector size)))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (float-vector-set! v j (+ (float-vector-ref v j) 1.0))))
      v)))

(let ((v (ft31)))
  (if (not (equal? v (make-float-vector 100000 tries)))
      (format *stderr* "ft31: ~A~%" v)))


(define (ft4)
  (let ((size 100000))
    (let ((v (make-int-vector size)))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (set! (v j) (+ (v j) 1))))
      v)))

(let ((v (ft4)))
  (if (not (equal? v (make-int-vector 100000 tries)))
      (format *stderr* "ft4: ~A~%" v)))

(define (ft41)
  (let ((size 100000))
    (let ((v (make-int-vector size)))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (int-vector-set! v j (+ (int-vector-ref v j) 1))))
      v)))

(let ((v (ft41)))
  (if (not (equal? v (make-int-vector 100000 tries)))
      (format *stderr* "ft41: ~A~%" v)))


(define (ft5)
  (let ((size 100000))
    (let ((v (make-string size #\a)))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (set! (v j) (v j))))
      v)))

(let ((v (ft5)))
  (if (not (equal? v (make-string 100000 #\a)))
      (format *stderr* "ft5: ~A~%" v)))

(define (ft51)
  (let ((size 100000))
    (let ((v (make-string size #\a)))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (string-set! v j (string-ref v j))))
      v)))

(let ((v (ft51)))
  (if (not (equal? v (make-string 100000 #\a)))
      (format *stderr* "ft51: ~A~%" v)))


(define (ft6)
  (let ((size 100000))
    (let ((v (make-vector size #\a)))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (set! (v j) (integer->char (+ (char->integer (v j)) 1)))))
      v)))

(let ((v (ft6)))
  (if (not (equal? v (make-vector 100000 (integer->char (+ (char->integer #\a) tries)))))
      (format *stderr* "ft6: ~A~%" v)))

(define (ft61)
  (let ((size 100000))
    (let ((v (make-vector size #\a)))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (vector-set! v j (integer->char (+ (char->integer (vector-ref v j)) 1)))))
      v)))

(let ((v (ft61)))
  (if (not (equal? v (make-vector 100000 (integer->char (+ (char->integer #\a) tries)))))
      (format *stderr* "ft61: ~A~%" v)))

(define (ft62)
  (let ((size 100000))
    (let ((v (make-int-vector size 1)))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1))) ; -> copy via opt_do_very_simple
	    ((= j size))
	  (vector-set! v j (vector-ref v j))))
      v)))

(let ((v (ft62)))
  (if (not (equal? v (make-int-vector 100000 1)))
      (format *stderr* "ft62: ~A~%" v)))

(define (ft63)
  (let ((size 100000))
    (let ((v (make-float-vector size 1.0)))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1))) ; same as above
	    ((= j size))
	  (vector-set! v j (vector-ref v j))))
      v)))

(let ((v (ft63)))
  (if (not (equal? v (make-float-vector 100000 1.0)))
      (format *stderr* "ft63: ~A~%" v)))


(define (ft7)
  (let ((size 400))
    (let ((v (make-list size 0)))
      (do ((i 0 (+ i 1)))
	  ((= i (* 10 tries)))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (set! (v j) (+ (v j) 1))))
      v)))

(let ((v (ft7)))
  (if (not (equal? v (make-list 400 (* 10 tries))))
      (format *stderr* "ft7: ~A~%" v)))

(define (ft71)
  (let ((size 400))
    (let ((v (make-list size 0)))
      (do ((i 0 (+ i 1)))
	  ((= i (* 10 tries)))
	(do ((p v (cdr p)))
	    ((null? p))
	  (set-car! p (+ (car p) 1))))
      v)))

(let ((v (ft71)))
  (if (not (equal? v (make-list 400 (* 10 tries))))
      (format *stderr* "ft71: ~A~%" v)))


(define (ft8)
  (let ((size 100000))
    (let ((v (make-hash-table))
	  (c #\a))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (set! (v j) c)))
      v)))

(ft8)

(define (ft81)
  (let ((size 100000))
    (let ((v (make-hash-table))
	  (c #\a))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (hash-table-set! v j c)))
      v)))

(ft81)


(define (ft9)
  (let ((size 100000))
    (let ((v (inlet 'a 1))
	  (c #\a))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (set! (v 'a) c)))
      v)))

(ft9)

(define (ft91)
  (let ((size 100000))
    (let ((v (inlet 'a 1))
	  (c #\a))
      (do ((i 0 (+ i 1)))
	  ((= i tries))
	(do ((j 0 (+ j 1)))
	    ((= j size))
	  (let-set! v 'a c)))
      v)))

(ft91)

(when (> (*s7* 'profile) 0)
  (show-profile 200))
(exit)

