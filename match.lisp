
(defun assertion_check (pattern assertion assertion_dup)
	; (print 20)
	; (print pattern)
	; (print assertion)
	; (print assertion_dup)
	(cond
		((endp assertion) (match pattern assertion_dup))
		; ((eql (determine_if_asterisks (first pattern)) t) 
		; 	(if (asterisk (string (first pattern)) (string (first assertion)))
		; 		(assertion_check pattern (cdr assertion) assertion)
		; 		nil
		; 	))
		((string= (first pattern) (nth 1 pattern)) (assertion_check (cdr pattern) assertion assertion_dup))
		((string/= (first pattern) (first assertion)) (assertion_check pattern (cdr assertion) assertion_dup))
		((string= (first pattern) (first assertion)) (assertion_check pattern (cdr assertion) assertion))
	)
)

(defun assertion_check_more (pattern pattern_dup assertion)
	; (print 70)
	; (print pattern)
	; (print pattern_dup)
	; (print assertion)
	(cond
		((and (null assertion) (string/= (first pattern) "!")) nil)
		((string= (first pattern) "!") (match pattern assertion))
		((string= (first pattern) (first assertion)) (assertion_check_more (cdr pattern) pattern_dup (cdr assertion)))
		((string/= (first pattern) (first assertion)) (assertion_check_more pattern_dup pattern_dup (cdr assertion)))
	)
)

; add optional substring index
(defun determine_if_asterisks (test_string &optional (substring_index 0))
	; (print 4)
	(if (find #\* (subseq (string test_string) substring_index))
		t
		nil
	)
)

(defun more_exclamations (pattern)
	(cond
		((null pattern) nil)
		((string= (first pattern) "!") t)
		((string/= (first pattern) "!") (more_exclamations (cdr pattern)))
	)
)

;asterisk string comes from pattern, test_string comes from assertion
(defun asterisk (pattern assertion &optional (index 0) (assert_index 0))
	; (print 50)
	; (print pattern)
	; (print assertion)
	; (print index)
	; (print assert_index)
	(cond
		; pattern string ends with * i.e. x*, return t since they match
		((eql assertion "NIL") nil)
		((and (eql index (length pattern)) (eql assert_index (length assertion))) t)
		((and (eql index (length pattern)) (not (eql assert_index (length assertion)))) nil)
		((and (char= (char pattern index) #\*) (eql index (- (length pattern) 1))) t)
		; two subsequent *s so go next NEED TWO INDEXES?
		((and (char= (char pattern index) #\*) (char= (char pattern (+ index 1)) #\*)) (asterisk pattern assertion (+ index 1) assert_index))
		((eql assert_index (length assertion)) nil)
		
		; equivalent chars so go next
		((char= (char pattern index) (char assertion assert_index)) (asterisk pattern assertion (+ index 1) (+ assert_index 1)))
		
		; char is * so helper function? //FIX, use determine asterisks
		((char= (char pattern index) #\*) (if (determine_if_asterisks pattern (+ index 1)) 
			(asterisk_helper_more pattern assertion (+ index 1) (+ index 1) assert_index assert_index)
			(asterisk_helper pattern assertion (+ index 1) (+ index 1) assert_index assert_index)
		))
		((char/= (char pattern index) (char assertion assert_index)) nil)
	)
)

(defun asterisk_helper (pattern assertion index index_dup assert_index assert_index_dup)
	; (print 60)
	; (print pattern)
	; (print assertion)
	; (print index)
	; (print index_dup)
	; (print assert_index)
	; (print assert_index_dup)
	(cond
		((and (eql index (length pattern)) (eql assert_index (length assertion))) (asterisk pattern assertion index assert_index))
		((and (not (eql index (length pattern))) (eql assert_index (length assertion))) (asterisk pattern assertion index assert_index))

		; ((char= (char pattern index) (char pattern (+ index 1))) (asterisk_helper pattern assertion (+ index 1) assert_index))
		((and (and (eql (+ index 1) (length pattern)) (not (eql (+ assert_index 1) (length assertion)))) (char= (char pattern index) (char assertion assert_index))) 
		(asterisk_helper pattern assertion index index_dup (+ assert_index 1) assert_index_dup))
		((char= (char pattern index) (char assertion assert_index)) (asterisk_helper pattern assertion (+ index 1) index_dup (+ assert_index 1) assert_index_dup))
		((char/= (char pattern index) (char assertion assert_index)) (asterisk_helper pattern assertion index_dup index_dup (+ assert_index_dup 1) (+ assert_index_dup 1)))
	)
)

(defun asterisk_helper_more (pattern assertion index index_dup assert_index assert_index_dup)
	; (print 80)
	; (print pattern)
	; (print assertion)
	; (print index)
	; (print index_dup)
	; (print assert_index)
	; (print assert_index_dup)
	(cond
		((and (eql assert_index (length assertion)) (char/= (char pattern index) #\*)) nil)
		((char= (char pattern index) #\*) (asterisk pattern assertion index assert_index))
		((char= (char pattern index) (char assertion assert_index)) (asterisk_helper_more pattern assertion (+ index 1) index_dup (+ assert_index 1) assert_index_dup))
		((char/= (char pattern index) (char assertion assert_index)) (asterisk_helper_more pattern assertion index_dup index_dup (+ assert_index_dup 1) (+ assert_index_dup 1)))
	)
)


(defun match (pattern assertion)
	; (print 10)
	; (print pattern)
	; (print assertion)
	(cond
		; got successfully through whole thing
		((and (endp pattern) (endp assertion)) t)
		; ! at end of pattern so return t
		((and (string= (first pattern) "!") (= (length pattern) 1)) t)
		; Two subsequent !s, in which the first one will not matter
		((and (string= (first pattern) "!") (string= (nth 1 pattern) "!")) (match (cdr pattern) assertion))
		; Asterisk checking if ! near, might get rid of this
		((and (string= (first pattern) "!") (string= (nth 1 pattern) "*")) (match (cdr pattern) assertion))
		; Asterisk checking if standing alone
		((and (string= (first pattern) "*") (not (null assertion))) (match (cdr pattern) (cdr assertion)))
		((and (string= (first pattern) "*") (null assertion)) nil)
		; Normal asterisk checking
		((eql (determine_if_asterisks (first pattern)) t) 
			(if (asterisk (string (first pattern)) (string (first assertion)))
				(match (cdr pattern) (cdr assertion))
				nil
			))
		; Normal ! checking
		((string= (first pattern) "!") (if (more_exclamations (cdr pattern)) 
										(assertion_check_more (cdr pattern) (cdr pattern) assertion)
										(assertion_check (cdr pattern) assertion assertion)
										))
		; Normal checking of strings, recurses to next place in both lists
		((string= (first pattern) (first assertion)) (match (cdr pattern) (cdr assertion)))
		; If strings not equal in normal check, returns nil
		((string/= (first pattern) (first assertion)) nil)
	)

)
; (print (match '(apple2 banana) '(apple2 banana)))

; Test cases
;;;Passing
; (print (match '(color apple red) '(color apple red))) ;t
; (print (match '(color apple red) '(color apple green))) ;nil
; (print (match '(! table !) '(this table supports a bloc))) ;t
; (print (match '(this table !) '(this table supports a bloc))) ;t
; (print (match '(! brown) '(green red brown yellow))) ;nil
; (print (match '(! brown) '(green red brown brown))) ;t
; (print (match '(red green ! blue) '(red green blue))) ;t
; (print (match '(! table orange !) '(this table is table orange rock))) ;t
; (print (match '(! table orange) '(this table is table orange table orange))) ;t
; (print (match '(! table orange) '(this table is table orange table))) ;nil
; (print (match '(color ! * red) '(color apple red))) ;t
; (print (match '(color apple *) '(color apple red))) ;t
; (print (match '(color * red) '(color apple red))) ;t
; (print (match '(color * red) '(color apple green))) ;nil
; (print (match '(! ! ! apple ! ! ! hotdog ! ! ! bruh) '(apple hotdog))) ;nil
; (print (match '(! ! ! apple ! ! ! hotdog ! ! ! bruh) '(apple hotdog bruh))) ;t
; (print (match '(! ! ! apple ! ! ! hotdog ! ! ! bruh) '(some random stuff apple oh no hotdog pineapple bruh))) ;t
; (print (match '(! ! ! apple ! ! ! hotdog ! ! ! bruh) '(some random stuff apple oh no hotdog pineapple bruh burple nurples))) ;nil
; (print (match '(! ! * benson * ! ! *) '(hamburger delta there benson is))) ;nil
; (print (match '() '())) ;t
; (print (match '(*) '())) ;nil
; (print (match '(!) '())) ;t
; (print (match '(! ! *) '())) ;nil
; (print (match '(! ! * ! * ! ! *) '())) ;nil
; (print (match '(apple2 ! bananas !) '(apple2 apple plum bananas))) ;t 

; (print (match '(color*red) '(color apple red))) ;nil
; (print (match '(red gr*n blue) '(red green blue))) ; t
; (print (match '(t* table is *n) '(this table is blue))) ; nil
; (print (match '(** ****** * *) '(i am a pirate))) ;t
; (print (match '(apple * red) '(apple red))) ;nil

; (print (match '(color ! apple ! ! apple ! * red) '(color apple apple red))) ;nil
; (print (match '(! bear bear) '(bear bear bear bear bear))) ;t
; (print (match '(cone old lock odor rock ! rock rock egg dead) '(cone old lock odor rock rock rock rock egg dead))) ;t
; (print (match '(cone old lock odor rock ! ! ! rock egg ! ! ! dead rock egg) '(cone old lock odor rock rock egg big big rock egg big big king egg big big cone dead rock egg))) ;t
; (print (match '(! rock ! table) '(bark roar rock table table table))) ;t
; (print (match '(color ! **a*ple red) '(color apple red))) ;t

;;;Failing
; (print (match '(! ! * benson * ! ! *) '(hamburger delta there benson is is))) ;t
; (print (match '(* **) '(c))) ;nil ----

;;;* cases
; (print (match '(color*rred) '(colorrrred))) ;t
; (print (match '(color*d) '(colord))) ;t 
; (print (match '(color***re***d) '(colorred))) ;t
; (print (match '(color***re***d) '(colorrebbbcd))) ;t
; (print (match '(color***re***dre) '(colorrebbbcdre))) ;t
; (print (match '(color***re***d) '(colorred))) ;t
; (print (match '(color***re***d) '(colorrrrreddd))) ;t
; (print (match '(color***re***d) '(colorrererereddd))) ;t 
; (print (match '(color***re***dre) '(colorrebbrebbkebbcdre))) ;t
; (print (match '(color*re) '(colorredre))) ;t 
; (print (match '(bl*ue*ueueue) '(blueueueue))) ;t
; (print (match '(bl*ue*ueueue) '(blapppppueueueue))) ;t
; (print (match '(bl*ue*ueueue) '(bluemmmueueue))) ;t

; (print (match '(color**red) '(colorred))) ;t 
; (print (match '(color**) '(color))) ;t 
; (print (match '(***color**) '(colorr))) ;t 
; (print (match '(***color**) '(colorabceere))) ;t
; (print (match '(bl*ue*) '(blccueecueu))) ;t
; (print (match '(***bl*ue*) '(blccueecueu))) ;t
; (print (match '(*****) '(a))) ;t
; (print (match '(*** **) '(a c))) ;t

; (print (match '(color*rred) '(colorcrrrcd))) ;nil
; (print (match '(color***re***d) '(colorbbbcdre))) ;nil
; (print (match '(color***re***dre) '(colorbbbcdre))) ;nil
; (print (match '(color*re) '(colorredbe))) ;nil 
; (print (match '(color***re***d) '(colorredre))) ;nil 
; (print (match '(color***re**re*d) '(colorredre))) ;nil 
; (print (match '(color***re***dre) '(colorebbbcdre))) ;nil
; (print (match '(color*rred) '(colorrrrede))) ;nil
; (print (match '(color*rrd) '(colorrrred))) ;nil
; (print (match '(color*eeerrrd) '(colorerrrd))) ;nil
; (print (match '(color*red) '(colored))) ;nil
; (print (match '(bl*ue*ueueue) '(blueueu))) ;nil
; (print (match '(bl*ue*ueueue) '(blccueecueu))) ;nil

; (print (match '(color**red) '(color))) ;nil
; (print (match '(**pp**color**) '(colorr))) ;nil
