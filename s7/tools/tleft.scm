(define tc-size 1500000)
(define rc-size 10000)
(define case-size 1000000)


(set! (*s7* 'heap-size) 512000)

;;; why no fx_tree? or tentative? sub_t1|s1, fx_lt|geq|leq_si, fb|fx_num_eq_s0, fx_s
;;; no other fb's
;;; cond/let cases


;;; --------------------------------------------------------------------------------
;OP_TC_WHEN_LA and unless cases

(define (f1 x)     ; now op_tc_when_la
  (when (> x 0)    ; fx_gt_ti
    (f1 (- x 1)))) ; fx_subtract_t1

(f1 tc-size)  ; safe_closure_a_o[checked] op_if_a_p

(define (f2 x)
  (unless (<= x 0) ; fx_leq_ti
    (f2 (- x 1)))) ; fx_subtract_t1

(f2 tc-size)  ; safe_closure_a_o[checked] op_if_a_r

;;; --------------------------------------------------------------------------------
;OP_TC_WHEN_L3A and unless cases

(define (f11 x y z)                 ; now op_tc_when_l3a
  (when (> x 0)                     ; fx_gt_ti
    (f11 (- x 1) (- y 2) (+ z 1)))) ; op_safe_closure_3a fx_add_v1 [subtract]

(f11 tc-size 2 3)

(define (f12 x y z)
  (unless (<= x 0)                  ; fx_leq_ti
    (f12 (- x 1) y z)))             ; op_safe_closure_agg fx_subtract_t1 fx_u fx_v

(f12 tc-size 1 2)

;;; --------------------------------------------------------------------------------
;OP_TC_IF_A_Z_IF_A_Z_L3A and reverse [23-Jun-24]

(define (f3 x y z)
  (if (<= x 0)                      ; fx_leq_ti
      y
      (if (= y 0)                   ; fx_num_eq_u0
	  x
	  (f3 (- x 1) (- y 1) z)))) ; fx_subtract_t|u1 fx_v

(unless (= tc-size (f3 tc-size (* 2 tc-size) 0)) ; safe_closure_3a[checked] if_a_a_p[leq] if_b_a_p[=] -- b_a_p leq case?
  (display "f3 if_a_z_if_a_z_l3a: ~S~%" (f3 tc-size (*2 tc-size) 0)))

(define (f4 x y z)
  (if (<= x 0)
      y
      (if (> y 0)
	  (f4 (- x 1) (- y 1) z)
	  x)))

(unless (= tc-size (f4 tc-size (* 2 tc-size) 0)) ; safe_closure_3a[checked] if_a_a_p[leq] if_a_p_a[gt]
  (display "f4 if_a_z_if_a_l3a_z: ~S~%" (f4 tc-size (*2 tc-size) 0)))

;;; --------------------------------------------------------------------------------
;OP_TC_IF_A_LA_IF_A_Z[_Z] -- already handled

(define (f5 x)        ; op_tc_if_a_la_z
  (if (>= x 0)        ; opt_b_ii_sc_geq_0
      (f5 (- x 1))    ; opt_i_ii_sc_sub
      (if (< x 0)
	  x
	  'oops!)))

(f5 tc-size)

;;; --------------------------------------------------------------------------------
;OP_TC_AND_A_OR_A_L3A and OP_TC_OR_A_AND_A_L3A

(define (f6 x y z)                   ; now op_tc_and_a_or_a_l3a (also op_tc_or_a_and_a_l3a)
  (and (> x 0)                       ; fx_gt_ti
       (or (= y 0)                   ; fx_num_eq_u0
	   (f6 (- x 1) (- y 1) 0)))) ; fx_subtract_t|u1

(f6 tc-size (* 2 tc-size) 0)

(define (f7 x y z)
  (or (<= x 0)
       (and (> y 0)
	    (f7 (- x 1) (- y 1) 0))))

(f7 tc-size (* 2 tc-size) 0)

;;; --------------------------------------------------------------------------------
;OP_TC_IF_A_Z_IF_A_Z_IF_A_Z_LA

(define (f7 x)
  (if (= x 0)
      x
      (if (< x 0)
	  'oops
	  (if (= x 0)
	      'oops
	      (f7 (- x 1))))))

(f7 tc-size) ; op_safe_closure_3a[checked] if_a_a_p[fx_lt_si] if_b_a_p[fb_num_eq_s0]

;;; --------------------------------------------------------------------------------
;OP_TC_COND_A_Z_L3A [24-June-24]

(define (f8 x y z)
  (cond ((= x 0) 0)                     ; fx_num_eq_t0
	(else (f8 (- x 1) (- y 1) z)))) ; fx_subtract_t|u1 fx_v

(f8 tc-size tc-size 0) ; op_safe_closure_3a

;;; --------------------------------------------------------------------------------
;OP_TC_COND_N

(define (tc-cond-n3 x) 
  (cond ((= x 1) 1)
	((= x 2) 2)
	((= x 3) 3)
	(else (tc-cond-n3 (- x 1)))))
(tc-cond-n3 tc-size)

;;; --------------------------------------------------------------------------------
;OP_TC_CASE_LAA

(define (f9 x y) ; 276 -> 175 op_tc_case_la 30-Jun-24
  (case (if (= x 0) 'a 'b)
    ((a) 1)
    ((c) (* x y))
    (else (f9 (- x 1) (+ y 1)))))

(f9 tc-size 0)

;;; --------------------------------------------------------------------------------
;OP_TC_LET_IF_A_Z_L3A

(define (f10 x y z)
  (let ((a (+ x y z)))       ; fx_c_tuv_direct  let_a_p_old
    (if (= a 0)              ; fx_num_eq_t0  if_a_a_p
	x                    ; fx_o?
	(f10 (- x 1) y z)))) ; op_safe_closure_agg  fx_subtract_s1 + fx_o

(f10 tc-size (/ tc-size 10) (/ tc-size 10))

;;; --------------------------------------------------------------------------------
;OP_RECUR_IF_A_A_opLAA_LAAq OP_RECUR_IF_A_opLAA_LAAq_A ; handled now

(define (rc1 x y)
  (if (<= y 0)
      x
      (+ (rc1 (+ x 1) (- y 1))
	 (rc1 (- x 1) (- y 1)))))

(define (trc1)
  (do ((i 0 (+ i 1)))
      ((= i rc-size))
    (rc1 0 6)))

(trc1)

(define (rc1-rev x y)
  (if (> y 0)
      (+ (rc1-rev (+ x 1) (- y 1))
	 (rc1-rev (- x 1) (- y 1)))
      x))

(define (trc1r)
  (do ((i 0 (+ i 1)))
      ((= i rc-size))
    (rc1-rev 0 6)))

(trc1r)

;;; --------------------------------------------------------------------------------
;OP_RECUR_IF_A_A_opL3A_L3Aq OP_RECUR_IF_A_opL3A_L3Aq_A ; handled now

(define (rc2 x y z)
  (if (<= z 0)
      (+ x y)
      (+ (rc2 (+ x 1) (+ y 1) (- z 1))
	 (rc2 (- x 1) (- y 1) (- z 1)))))

(define (trc2)
  (do ((i 0 (+ i 1)))
      ((= i rc-size))
    (rc2 0 0 5)))

(trc2)

(define (rc2-rev x y z)
  (if (> z 0)
      (+ (rc2-rev (+ x 1) (+ y 1) (- z 1))
	 (rc2-rev (- x 1) (- y 1) (- z 1)))
      (+ x y)))

(define (trc2r)
  (do ((i 0 (+ i 1)))
      ((= i rc-size))
    (rc2-rev 0 0 5)))

(trc2r)

;;; --------------------------------------------------------------------------------
;;; OP_RECUR_IF_A_A_IF_A_A_opL3A_L3Aq?,

;OP_RECUR_OR_A_AND_A_opLA(AA)q?

(define (rc3 lst) ; does this pattern ever happen? maybe opA_LAq is more likely
  (or (null? lst)
      (and (symbol? (car lst))
	   (null? (rc3 (cdr lst))))))

(define (trc3)
  (let ((lst (make-list 100 'a)))
    (do ((i 0 (+ i 1)))
	((= i rc-size))
      (rc3 lst))))

(trc3)


;;; --------------------------------------------------------------------------------

(define (tcase1)       ; opt_case, numbers_are_eqv
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case i
      ((0) 0)
      ((1) 1)
      ((2) 2)
      (else 3))))

(tcase1)

(define (tcase2)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case i
      ((0) 0)
      ((1) 1)
      ((2) 2)
      (else 3))))

(tcase2)

(define (tcase3 lst)
  (do ((i 0 (+ i 1)))        ; op_simple_do_step -> safe_closure_aa_o
      ((= i case-size))
    (case i                    ;   t_lookup
      ((0) (pair? lst))        ; fx_is_pair_u
      ((1) (pair? (cdr lst)))  ; fx_is_pair_cdr_u
      (else (null? lst)))))    ; fx_is_null_u


(tcase3 '(1 2))

(define (tcase3-1 lst)
  (do ((i 0 (+ i 1)))                ; op_simple_do_step -> safe_closure_aa_o, g_add_x1
      ((= i case-size))              ; g_num_eq_2
    ;(case3 (remainder i 3) '(1 2)))) ; modulo_p_pi via fx_c_ti_direct(80400 check_do), fx_q
    (case i                    ;   t_lookup
      ((0) (pair? lst))        ; fx_is_pair_u
      ((1) (pair? (cdr lst)))  ; fx_is_pair_cdr_u
      (else (null? lst)))))    ; fx_is_null_u

(tcase3-1 '(1 2))

(define (tcase4)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (remainder i 5)
      ((a) 0)
      ((b) 1)
      ((c) 2)
      ((d) 3)
      (else 4))))

(tcase4)

(define-constant sym-selector  ; currently not handled by opt* (also int|any-selector, but they're fxable??)
  (let ((syms '(a b c d e)))
    (lambda (x)
      (syms (remainder x 5))))) ; list-ref is slower
(define (tcase5)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (sym-selector i)
      ((a) 0)
      ((b) 1)
      ((c) 2)
      ((d) 3)
      (else 4))))

(tcase5)

(define-constant (int-selector x)
  (remainder x 5))
(define (tcase6)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (int-selector i)
      ((1) 0)
      ((2) 1)
      ((3) 2)
      ((4) 3)
      (else 4))))

(tcase6)

(define-constant (any-selector x)
  (if (zero? (remainder x 3)) #\a))

(define (tcase7)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (any-selector i)
      ((1/2) 0)
      ((#\a) 1)
      ((#<unspecified>) 2)
      ((#f) 3)
      (else 4))))

(tcase7)

(define (tcase8)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (even? i)
      ((1/2) 0)
      ((#\a) 1)
      ((#<unspecified>) 2)
      ((#f) 3)
      (else 4))))

(tcase8)

(define (tcase9)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (even? i)
      ((#t) (display i #f) (not i))
      ((#f) (write i #f) (integer? i))
      (else 'oops))))

(tcase9)

(define (tcase10)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (even? i)
      ((#t) (display i #f) (not i))
      ((1/2) (write i #f) (integer? i))
      ((1 2 3) 0)
      (else 'oops))))

(tcase10)

(define (tcase11)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (remainder i 5)
      ((a) 0)
      ((b) (display i #f) 1)
      ((c) (+ 1 2))
      ((d) 3)
      (else 4))))

(tcase11)

(define (tcase12)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (any-selector i)
      ((#t) (display i #f) (not i))
      ((1/2) (write i #f) (integer? i))
      ((1 2 3) 0)
      (else 'oops))))

(tcase12)

(define (tcase13)
  (do ((i 0 (+ i 1)))
      ((= i case-size))

    (case (sym-selector i)
      ((a) 0)
      ((b) (display i #f) 32)
      ((c) 2)
      ((d) 3)
      (else 4))))

(tcase13)

(define (tcase14)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case i
      ((1 2 3) 0)
      ((4 5) 1)
      ((6) 2)
      ((7 8 9) 3)
      ((10) 4))))

(tcase14)

(define (tcase15)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (remainder i 5)
      ((a) 0)
      ((b) 1)
      ((c) 2)
      ((d) 3))))

(tcase15)

(define (tcase16)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case (remainder i 5)
      ((a) 0)
      ((b) (display i #f) 1)
      ((c) (+ 1 2))
      ((d) 3)
      (else 4))))

(tcase16)

(exit)
