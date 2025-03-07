;; find where two callgrind runs differ, also combine n callgrind runs

(define (compare-calls f1 f2)
  (let ((h1 (with-input-from-file f1 read-calls))
	(total-diff 0)
	(diffs ())
	(scl 1e-6))

    (let ((h2 (with-input-from-file f2 read-calls)))
      (for-each 
       (lambda (kv1)
	 (let ((kv2 (h2 (car kv1))))
	   (let ((diff (if kv2 (- kv2 (cdr kv1)) (- (cdr kv1)))))
	     (if (> (abs diff) 3e6)
		 (begin
		   (set! diffs (cons (list diff (car kv1) (cdr kv1) (or kv2 0)) diffs))
		   (set! total-diff (+ total-diff diff)))))))
       h1)
      (for-each
       (lambda (kv2)
	 (let ((kv1 (h1 (car kv2))))
	   (if (not kv1)
	       (let ((diff (cdr kv2)))
		 (if (> (abs diff) 3e6)
		     (begin
		       (set! diffs (cons (list diff (car kv2) 0 (cdr kv2)) diffs))
		       (set! total-diff (+ total-diff diff))))))))
       h2))
		 
    (let ((vals (sort! diffs (lambda (a b) (> (car a) (car b))))))
      (format *stderr* "total: ~,3F~%" (* scl total-diff))
      (for-each
       (lambda (entry)
	 (format *stderr* "~A~,3F~12T(~,3F~24T~,3F)~40T~A~%" 
		 (if (negative? (entry 0)) "" " ")
		 (* scl (entry 0)) 
		 (* scl (entry 2)) 
		 (* scl (entry 3)) 
		 (entry 1)))
       vals)))
  (exit))

(define (string->number-ignoring-commas str)
  (let ((num 0)
	(tens 1)
	(len (length str)))
    (do ((i (- len 1) (- i 1)))
	((< i 0) num)
      (if (char-numeric? (str i))
	  (begin
	    (set! num (+ num (* tens (- (char->integer (str i)) 48))))
	    (set! tens (* 10 tens)))))))

(define (read-calls)
  ;; throw away the header
  (do ((i 0 (+ i 1)))
      ((= i 25))
    (read-line))
  ;; read about 500 lines and store in a hash table as (func . timing)
  ;; names can match!
  (let ((h (make-hash-table)))
    (call-with-exit
     (lambda (quit)
       (do ((i 0 (+ i 1)))
	   ((= i 500))
	 (let ((line (read-line)))
	   (if (eof-object? line)
	       (quit))
	   (let ((len (length line)))
	     (do ((k 0 (+ k 1)))
		 ((or (= k len)
		      (not (char-whitespace? (line k))))
		  (if (< k len)
		      (let ((end (char-position #\space line k)))
			(if end
			    (let ((num (string->number-ignoring-commas (substring line k end))))
			      (when num
				(let ((func-end (char-position #\space line (+ end 2))))
				  (when (and (integer? func-end)
					     (> func-end (+ end 2)))
				    (let ((func (substring line (+ end 2) func-end)))
				      (let ((colon-pos (char-position #\: func)))
					(if (integer? colon-pos)
					    (let ((isra-pos (char-position #\. func colon-pos)))
					      (if (integer? isra-pos)
						  (set! func (substring func 0 isra-pos))))))
				      (let ((sym (string->symbol func)))
					(let ((curval (h sym)))
					  (set! (h sym) (+ (or curval 0) num)))))))))))))))))))
    h))
    

(define (get-overheads file)
  (with-input-from-file file
    (lambda ()
      (let ((overheads ())
	    (total 0))

	(define (get-overheads-1 file line)
	  (let ((len (min 20 (length line))))
	    (do ((i 0 (+ i 1)))
		((or (= i len)
		     (not (char-whitespace? (line i))))
		 (if (and (< i (- len 4))
			  (char=? (line i) #\.)
			  (char=? (line (+ i 1)) #\space)
			  (char=? (line (+ i 2)) #\space)
			  (char-alphabetic? (line (+ i 3))))
		     (let ((next-line (read-line)))
		       (let ((nlen (length next-line)))
			 (if (char=? (next-line (- nlen 1)) #\{)
			     (do ((j 0 (+ j 1)))
				 ((or (= j nlen)
				      (and (char-numeric? (next-line j))
					   (let ((cost (string->number-ignoring-commas (substring next-line j (- nlen 3)))))
					     (set! total (+ total cost))
					     (set! overheads (cons (list cost (substring line (+ i 3) (min 80 (length line)))) overheads)))))))
			     (get-overheads-1 file next-line)))))))))
	
	(do ((line (read-line) (read-line)))
	    ((eof-object? line) overheads)
	  (get-overheads-1 file line))
			     
	(set! overheads (sort! overheads (lambda (a b) (< (car a) (car b)))))
	(format *stderr* "~{~^~A~%~}" (list-tail overheads (max 10 (- (length overheads) 20))))
	(format *stderr* "total: ~A~%" total)))))


(define (read-all-calls)
  ;; throw away the header
  (do ((i 0 (+ i 1)))
      ((= i 25))
    (read-line))
  (let ((h (make-hash-table)))
    (call-with-exit
     (lambda (quit)
       (do () ()
	 (let ((line (read-line)))
	   (if (or (eof-object? line)
		   (and (= (length line) 80)
			(char=? (line 0) #\-)))
	       (quit))
	   (let ((len (length line)))
	     (do ((k 0 (+ k 1)))
		 ((or (= k len)
		      (not (char-whitespace? (line k))))
		  (if (< k len)
		      (let ((end (char-position #\space line k)))
			(if end
			    (let ((num (string->number-ignoring-commas (substring line k end))))
			      (when num
				(let ((func-end (char-position #\space line (+ end 2))))
				  (when (and (number? func-end)
					     (> func-end (+ end 2)))
				    (let* ((name (substring line (+ end 2) func-end))
					   (len (length name)))
				      (if (and ;(not (char=? (name 0) #\?))
					       (not (char=? (name 0) #\/))
					       (or (< len 3)
						   (not (char=? (name (- len 2)) #\')))
					       (or (< len 8)
						   (not (string=? "libgsl_" (substring name 0 7)))))
					  (set! (h (string->symbol name)) num)))))))))))))))))
    h))

#|
(define (combine . files)
  (let ((tables (map (lambda (file)
		       (with-input-from-file file
			 read-all-calls))
		     files)))
    (let ((h (make-hash-table)))
      (for-each
       (lambda (file table)
	 (for-each
	  (lambda (entry)
	    (let ((current-entry (h (car entry))))
	      (if current-entry
		  (set! (h (car entry)) 
			(cons (max (cdr entry)
				   (car current-entry))
			      (cons (list file (cdr entry))
				    (cdr current-entry))))
		  (set! (h (car entry))
			(cons (cdr entry)
			      (list (list file (cdr entry))))))))
	  table))
       files tables)

      (let ((v (copy h (make-vector (hash-table-entries h)))))
	(set! v (sort! v (lambda (a b) (> (cadr a) (cadr b)))))
	(call-with-output-file "test.table"
	  (lambda (p)
	    (for-each
	     (lambda (entry)
	       (format p "~A: ~A ~{~%~16T~{~A~32T ~A~}~}~%" (car entry) (cadr entry) (cddr entry)))
	     v)))))))

(define (combine-latest)
  (let ((file-names (list 
		     "v-index"
		     "v-mac"
		     "v-peak"
		     "v-vect"
		     "v-eq"
		     "v-fft"
		     "v-ref"
		     "v-auto"
		     "v-test"
		     "v-cop"
		     "v-lt"
		     "v-form"
		     "v-read"
		     "v-map"
		     "v-mat"
		     "v-misc"
		     "v-iter"
		     "v-sort"
		     "v-let"
		     "v-hash"
		     "v-gen"
		     "v-all"
		     "v-call"
		     "v-sg"
		     "v-dup"
		     "v-set"
		     "v-rec"
		     "v-clo"
		     "v-big"
		     "v-shoot"
		     "v-fb"
		     "v-rclo"
		     "v-case"
		     ;"v-b"
		     "v-io"
		     "v-gc"
		     "v-num"
		     "v-mock"
		     "v-str"
		     "v-gsl"
		     "v-list"
		     "v-load"
		     "v-cb"
		     "v-ari"
		     "v-exit"
		     "v-left"
		     "v-obj"
		     "v-imp"
		     "v-lamb"
		     "v-hook"
		     "v-complex"
		     "v-star")))
    (define (next-file f)
      (let ((name (system (format #f "ls -t ~A*" f) #t)))
	(let ((len (length name)))
	  (do ((i 0 (+ i 1)))
	      ((or (= i len)
		   (and (char-numeric? (name i))
			(char-numeric? (name (+ i 1)))))
	       (string-append f (substring name i (+ i 2))))))))

    (apply combine (map next-file file-names))))


;;; show all timing test overheads

(define (get-overheads file)
  (with-input-from-file file
    (lambda ()
      (let ((overheads ())
	    (total 0))

	(define (get-overheads-1 file line)
	  (let ((len (min 20 (length line))))
	    (do ((i 0 (+ i 1)))
		((or (= i len)
		     (not (char-whitespace? (line i))))
		 (if (and (< i (- len 4))
			  (char=? (line i) #\.)
			  (char=? (line (+ i 1)) #\space)
			  (char=? (line (+ i 2)) #\space)
			  (char-alphabetic? (line (+ i 3))))
		     (let ((next-line (read-line)))
		       (let ((nlen (length next-line)))
			 (if (char=? (next-line (- nlen 1)) #\{)
			     (do ((j 0 (+ j 1)))
				 ((or (= j nlen)
				      (and (char-numeric? (next-line j))
					   (let ((cost (string->number-ignoring-commas (substring next-line j (- nlen 3)))))
					     (set! total (+ total cost))
					     (set! overheads (cons (list cost (substring line (+ i 3) (min 80 (length line)))) overheads)))))))
			     (get-overheads-1 file next-line)))))))))
	
	(do ((line (read-line) (read-line)))
	    ((eof-object? line))
	  (get-overheads-1 file line))
			     
	(set! overheads (sort! overheads (lambda (a b) (< (car a) (car b)))))
	(list-tail overheads (max 10 (- (length overheads) 20)))))))


(define file-names '(
		     ("concordance.scm" . "/home/bil/motif-snd/v-str85")
		     ("dup.scm" . "/home/bil/motif-snd/v-dup85")
		     ("fbench.scm" . "/home/bil/motif-snd/v-fb85")
		     ("full-snd-test.scm" . "/home/bil/motif-snd/v-sg85")
		     ("lt.scm" . "/home/bil/motif-snd/v-lt85")
		     ("s7test.scm" . "/home/bil/motif-snd/v-test85")
		     ("snd-test.scm" . "/home/bil/motif-snd/v-call85")
		     ("tall.scm" . "/home/bil/motif-snd/v-all85")
		     ("tari.scm" . "/home/bil/motif-snd/v-ari85")
		     ("tauto.scm" . "/home/bil/motif-snd/v-auto85")
		     ("tbig.scm" . "/home/bil/motif-snd/v-big85")
		     ("tcase.scm" . "/home/bil/motif-snd/v-case85")
		     ("tclo.scm" . "/home/bil/motif-snd/v-clo85")
		     ("tcomplex.scm" . "/home/bil/motif-snd/v-complex85")
		     ("tcopy.scm" . "/home/bil/motif-snd/v-cop85")
		     ("teq.scm" . "/home/bil/motif-snd/v-eq85")
		     ("texit.scm" . "/home/bil/motif-snd/v-exit85")
		     ("tfft.scm" . "/home/bil/motif-snd/v-fft85")
		     ("tform.scm" . "/home/bil/motif-snd/v-form85")
		     ("tgc.scm" . "/home/bil/motif-snd/v-gc85")
		     ("tgen.scm" . "/home/bil/motif-snd/v-gen85")
                     ("tgsl.scm" . "/home/bil/motif-snd/v-gsl85")
		     ("thash.scm" . "/home/bil/motif-snd/v-hash85")
		     ("thook.scm" . "/home/bil/motif-snd/v-hook85")
		     ("timp.scm" . "/home/bil/motif-snd/v-imp85")
		     ("tio.scm" . "/home/bil/motif-snd/v-io85")
		     ("titer.scm" . "/home/bil/motif-snd/v-iter85")
		     ("tlamb.scm" . "/home/bil/motif-snd/v-lamb85")
		     ("tleft.scm" . "/home/bil/motif-snd/v-left85")
		     ("tlet.scm" . "/home/bil/motif-snd/v-let85")
		     ("tlimit.scm" . "/home/bil/motif-snd/v-limit85")
		     ("tlist.scm" . "/home/bil/motif-snd/v-list85")
		     ("tload.scm" . "/home/bil/motif-snd/v-load85")
		     ("tmac.scm" . "/home/bil/motif-snd/v-mac85")
		     ("tmap-hash.scm" . "/home/bil/motif-snd/v-map-hash85")
		     ("tmap.scm" . "/home/bil/motif-snd/v-map85")
		     ("tmat.scm" . "/home/bil/motif-snd/v-mat85")
		     ("tmisc.scm" . "/home/bil/motif-snd/v-misc85")
		     ("tmock.scm" . "/home/bil/motif-snd/v-mock85")
		     ("tmv.scm" . "/home/bil/motif-snd/v-mv85")
		     ("tnum.scm" . "/home/bil/motif-snd/v-num85")
		     ("tpeak.scm" . "/home/bil/motif-snd/v-peak85")
		     ("trclo.scm" . "/home/bil/motif-snd/v-rclo85")
		     ("tread.scm" . "/home/bil/motif-snd/v-read85")
		     ("trec.scm" . "/home/bil/motif-snd/v-rec85")
		     ("tref.scm" . "/home/bil/motif-snd/v-ref85")
		     ("tset.scm" . "/home/bil/motif-snd/v-set85")
		     ("tshoot.scm" . "/home/bil/motif-snd/v-shoot85")
		     ("tsort.scm" . "/home/bil/motif-snd/v-sort85")
		     ("tstar.scm" . "/home/bil/motif-snd/v-sort85")
		     ("tvect.scm" . "/home/bil/motif-snd/v-star85")
                     ("make-index.scm" . "/home/bil/motif-snd/v-index85")
		     ))

(for-each (lambda (file)
	    (format *stderr* "-------- ~S:\n~{~S~%~^~}~%" (car file) (reverse (get-overheads (cdr file)))))
	  file-names)
|#


