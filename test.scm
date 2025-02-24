(define* (range a (b '()))
  (cond ((null? b) (range 0 a))
        ((>= a b) '())
        (else (cons a (range (+ a 1) b)))))

(define (fac x)
  (if (< x 2)
    1
    (* x (fac (- x 1)))))

(define (zip . args)
  (if (null? (car args))
    '()
    (cons (map car args) (apply zip (map cdr args)))))

(define (printf . args)
  (apply format #t args))

(printf "nums = ~a\n" (map fac (range 10)))
(printf "with zip: ~a\n" (zip (range 10) (map fac (range 10))))

