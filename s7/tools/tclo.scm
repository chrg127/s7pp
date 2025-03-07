(set! (*s7* 'heap-size) (* 8 1024000))

(define d1-size 200000)
(define g-size 1000000)
(define kf-size 30)
(define k100-size 10000)

(define* (f0 a b)
  (display b #f))

(define* (f1 a . b)
  (display b #f))

(define* (f2 a)
  (display 2 #f))

(define* (f3)
  (display 3 #f))

(define* (f4)
  (apply + (list 1 2)))

(define* (f5 (a 1))
  (apply + (list a 2)))

(define* (f6 a . b)          ; unsafe 
  (apply values (cons a b)))

(define* (f7 (a 1) (b 2))
  (apply + (list a b)))

(define* (f8 a b c)
  (list a b c))

(define* (f9 a :rest b)
  (list a b))

(define* (f10 a :allow-other-keys)
  (display a #f))

(define* (f11 . a)
  (apply list a))

(define* (tfib n (a 1) (b 1))
  (if (= n 0)
      a
      (if (= n 1)
	  b
	  (tfib (- n 1) b (+ a b)))))

(define (d1)
  (tfib 35)
  (let ((x 1) (y 2))
    (do ((i 0 (+ i 1)))
	((= i d1-size))
      (f0 1 2)
      (f0 x y)
      (f0 :a x)
      (f0 1)
      (f0)
      (f0 :a 1 2)
      (f0 :b 1)
      (f1 1 2 3)
      (f1 1 2)
      (f1 y x)
      (f1 1)
      (f1 :a 1)
      (f1)
      (f1 (- y 1))
      (f2) (f2)
      (f2 1)
      (f2 :a 1)
      (f3) (f3) (f3)
      (f4) (f4) (f4)
      (f5) (f5)
      (f5 1)
      (f5 (- x 1))
      (f5 :a 1)
      (f6 1 2 3)
      (f6 1 2)
      (f6 1)
      (f6 :a 1)
      (f6)
      (f7 1 2)
      (f7 1)
      (f7)
      (f8)
      (f8 :b 2)
      (f8 :c 3 :b 2 :a 1)
      (f9)
      (f9 1 x y)
      (f10)
      (f10 :a 2 :b 2)
      (f11)
      (f11 x)
      (f11 x y))))

(d1)

;;; -------- comparison with non-key case: --------

(define no-key-fib
   (lambda (n)
     (if (<= n 2) 1 (+ (no-key-fib (- n 2))
                       (no-key-fib (- n 1))))))

(define key-fib
  (lambda* (n)
    (if (<= n 2) 1 
	(+ (key-fib :n (- n 2))
           (key-fib :n (- n 1))))))

(define (f12 a b)
  (when (> a b)
    (+ a b)))

(define* (f13 a b)
  (when (> a b)
    (+ a b)))

(define* (f14 (a 1) (b 0))
  (when (> a b)
    (+ a b)))


(define size g-size)

(define (g1)
  (do ((i 0 (+ i 1)))
      ((= i g-size))
    (f12 i i)))

(define (g2)
  (do ((i 0 (+ i 1)))
      ((= i g-size))
    (f13 i i)))

(define (g3)
  (do ((i 0 (+ i 1)))
      ((= i g-size))
    (f13 :a i :b i)))

(define (g4)
  (do ((i 0 (+ i 1)))
      ((= i g-size))
    (f13 :b i :a i)))

(define (g5)
  (do ((i 0 (+ i 1)))
      ((= i g-size))
    (f14)))

(define (g6)
  (do ((i 0 (+ i 1)))
      ((= i g-size))
    (f14 i i)))

(define (g7)
  (do ((i 0 (+ i 1)))
      ((= i g-size))
    (f14 :a i)))


;;; -------- 100 key args --------

(define* (k100 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9
	      a10 a11 a12 a13 a14 a15 a16 a17 a18 a19
	      a20 a21 a22 a23 a24 a25 a26 a27 a28 a29
	      a30 a31 a32 a33 a34 a35 a36 a37 a38 a39
	      a40 a41 a42 a43 a44 a45 a46 a47 a48 a49
	      a50 a51 a52 a53 a54 a55 a56 a57 a58 a59
	      a60 a61 a62 a63 a64 a65 a66 a67 a68 a69
	      a70 a71 a72 a73 a74 a75 a76 a77 a78 a79
	      a80 a81 a82 a83 a84 a85 a86 a87 a88 a89
	      a90 a91 a92 a93 a94 a95 a96 a97 a98 a99)
  (+ a0 a1))

(define (g100)
  (let ((args (list :a0 1 :a1 1 :a2 1 :a3 1 :a4 1 :a5 1 :a6 1 :a7 1 :a8 1 :a9 1
		    :a10 1 :a11 1 :a12 1 :a13 1 :a14 1 :a15 1 :a16 1 :a17 1 :a18 1 :a19 1
		    :a20 1 :a21 1 :a22 1 :a23 1 :a24 1 :a25 1 :a26 1 :a27 1 :a28 1 :a29 1
		    :a30 1 :a31 1 :a32 1 :a33 1 :a34 1 :a35 1 :a36 1 :a37 1 :a38 1 :a39 1
		    :a40 1 :a41 1 :a42 1 :a43 1 :a44 1 :a45 1 :a46 1 :a47 1 :a48 1 :a49 1
		    :a50 1 :a51 1 :a52 1 :a53 1 :a54 1 :a55 1 :a56 1 :a57 1 :a58 1 :a59 1
		    :a60 1 :a61 1 :a62 1 :a63 1 :a64 1 :a65 1 :a66 1 :a67 1 :a68 1 :a69 1
		    :a70 1 :a71 1 :a72 1 :a73 1 :a74 1 :a75 1 :a76 1 :a77 1 :a78 1 :a79 1
		    :a80 1 :a81 1 :a82 1 :a83 1 :a84 1 :a85 1 :a86 1 :a87 1 :a88 1 :a89 1
		    :a90 1 :a91 1 :a92 1 :a93 1 :a94 1 :a95 1 :a96 1 :a97 1 :a98 1 :a99 1)))
  (do ((i 0 (+ i 1)))
      ((= i k100-size))
    (apply k100 args))))

;;; --------------------------------
(define (kcall)
  (no-key-fib kf-size)
  (key-fib kf-size))
(kcall)

(g1)
(g2)
(g3)
(g4)
(g5)
(g6)
(g7)

(g100)


;;; --------------------------------
(define* (ff2 x y)
  (or (null? x)
      (and (symbol? (car x))
	   (ff2 (cdr x) (+ y 1)))))

(define (testff2 lst)
  (do ((i 0 (+ i 1)))
      ((= i 100000))
    (ff2 lst 0)))

(testff2 (make-list 10 'a))

;;; --------------------------------


(define xlen 1000000)

(define (xf size)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (xf1 i)))

(define (xf1 x)
  (unless (integer? x)
    (error 'wrong-type-arg "f1: ~S is ~A but should be an integer" x (type-of x)))
  (+ x 1))

;(xf xlen)
;; 197 eval+g_add_x1+lookup op_safe_closure_s


(define (xf2 size)
  (define (xf3 x)
    (unless (integer? x)
      (error 'wrong-type-arg "f3: ~S is ~A but should be an integer" x (type-of x)))
    (+ x 1))
  (do ((i 0 (+ i 1)))
      ((= i size))
    (xf3 i)))

;(xf2 xlen)
;; 197 exactly like xf+xf1


(define (xf4 func size)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (func i)))

;(xf4 xf1 xlen)
;; 196

;(xf4 (lambda (x) (+ x 1)) xlen)
;; 167

;(xf4 (macro (x) `(+ ,x 1)) xlen)
;; 15! opt_dotimes+opt_i_ii_sc_add!!

;(xf4 (macro (x) `(display (+ ,x 1))) 10)  ; this form does not collapse
;(xf4 (macro (x) `(* 2 (+ ,x 1))) 10)      ; this does collapse (it stays in i_ii_ok?)

(define-macro (xm1 x) `(+ ,x 1))
;(xf4 xm1 xlen)
;; 15! as above

;; so macro is ca 10 times faster than function -- need to evert func?
;;   optimizer func->body tieing arg to outer
;; macros are expanded at opt time in int|float_optimize

;(xf4 (lambda (x) (display (+ x 1) #f)) xlen)
;; 192!

(define (xf5 size)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (+ i 1))) ; the everted form

;(xf5 xlen)
;; 15 as above


(define (xg size)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (xg1 i)))

(define (xg1 x)
  (+ x 1))

;(xg xlen)
;; 88 eval+fx_safe_closure_s_to_add1 via op_safe_closure_s_to_sc

;(xf4 xg1 xlen)
;; 253 which looks about right


(define xg2 (let ((xg3 (lambda (x) 
		       (+ x 1))))
	     (lambda (size)
	       (do ((i 0 (+ i 1)))
		   ((= i size))
		 (xg3 i)))))

;(xg2 xlen)
;; 167 eval+add_x1+gc+lookup op_closure_s_o


(define (xh size)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (xh1 i)))

(define (xh1 x)
  (eval '(+ x 1) (curlet)))

;(xh xlen)
;; 457 eval+gc+add_p_pp+g_eval+reverse_in_place_unchecked!+g_add, hop_c=curlet, op_c_ap=g_eval op_closure_s_o + eval_args



(define (xg3 x)
  (+ x x))

(define (xg4 size)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (xg3 i)))

;(xg4 xlen)
;; 168 eval+closure_is_ok_1 op_safe_closure_s_a


(define (xtest)
  (xf xlen)
  (xf2 xlen)
  (xf4 xf1 xlen)
  (xf4 (lambda (x) (+ x 1)) xlen)
  (xf4 (macro (x) `(+ ,x 1)) xlen)
  (xf4 xm1 xlen)
  (xf4 (lambda (x) (display (+ x 1) #f)) xlen)
  (xf5 xlen)
  (xg xlen)
  (xf4 xg1 xlen)
  (xg2 xlen)
  (xg4 xlen)
  (xh xlen))

(xtest)


;;; --------------------------------
(when (> (*s7* 'profile) 0)
  (show-profile 200))

(exit)
