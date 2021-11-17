
(defun assertion_check (pattern assertion assertion_dup)
	; (print 2)
	; (print pattern)
	; (print assertion)
	; (print assertion_dup)
	(cond
		((endp assertion) (match pattern assertion_dup))
		((string/= (first pattern) (first assertion)) (assertion_check pattern (cdr assertion) assertion_dup))
		((string= (first pattern) (first assertion)) (assertion_check pattern (cdr assertion) assertion))
		; ((and (<= (length pattern) (length assertion))(string= (first pattern) (first assertion))) (assertion_check pattern (cdr assertion) assertion))
	)
)

(defun rev (lis)
	(rev_helper lis NIL)
)

(defun rev_helper (lis1 lis2)
	(if lis1
		(rev_helper (cdr lis1) (cons (car lis1) lis2))
		lis2
	)
)

(defun determine_if_asterisks (test_string)
	; (print 4)
	(if (find #\* (string test_string))
		t
		nil
	)
)

;asterisk string comes from pattern, test_string comes from assertion
(defun asterisk (pattern assertion &optional (index 0) (assert_index 0))
	; (print 5)
	(cond
		; pattern string ends with * i.e. x*, return t since they match
		((eql index (length pattern)) t)
		((eql assert_index (length assertion)) nil)
		((and (char= (char pattern index) #\*) (eql index (- (length pattern) 1))) t)
		; equivalent chars so go next
		((char= (char pattern index) (char assertion assert_index)) (asterisk pattern assertion (+ index 1) (+ assert_index 1)))
		; two subsequent *s so go next NEED TWO INDEXES?
		((and (char= (char pattern index) #\*) (char= (char pattern (+ index 1)) #\*)) (asterisk pattern assertion (+ index 1) assert_index))
		; char is * so helper function?
		((char= (char pattern index) #\*) (asterisk_helper pattern (char pattern (+ index 1)) assertion (+ index 1) assert_index))
		((char/= (char pattern index) (char assertion assert_index)) nil)
	)
)

(defun asterisk_helper (pattern pattern_char assertion index assert_index)
	; (print 6)
	(cond
		((eql assert_index (length assertion)) nil)
		((char= pattern_char (char assertion assert_index)) (asterisk pattern assertion (+ index 1) (+ assert_index 1)))
		((char/= pattern_char (char assertion assert_index)) (asterisk_helper pattern pattern_char assertion index (+ assert_index 1)))
	)
)


(defun match (pattern assertion)
	; This is just to show that it works when there's no quotations
	; (print 1)
	; (print pattern)
	; (print assertion)
	(cond
		; got successfully through whole thing
		((and (endp pattern) (endp assertion)) t)
		; ! at end of pattern so return t
		((and (string= (first pattern) "!") (= (length pattern) 1)) t)
		; end of pattern = ! string/!/*, change to last being not ! and not *
		; ((and (string= (first pattern) "!") (= (length pattern) 2)) (exclamation (cdr pattern) (rev assertion) t))
		; Two subsequent !s, in which the first one will not matter
		((and (string= (first pattern) "!") (string= (nth 1 pattern) "!")) (match (cdr pattern) assertion))
		; Asterisk checking if ! near, might get rid of this
		((and (string= (first pattern) "!") (string= (nth 1 pattern) "*")) (match (cdr pattern) assertion))
		; Asterisk checking if standing alone
		((and (string= (first pattern) "*") (not (null assertion))) (match (cdr pattern) (cdr assertion)))
		; Normal asterisk checking
		((eql (determine_if_asterisks (first pattern)) t) 
			(if (asterisk (string (first pattern)) (string (first assertion)))
				(match (cdr pattern) (cdr assertion))
				nil
			))
		; Normal ! checking
		((string= (first pattern) "!") (assertion_check (cdr pattern) assertion assertion))
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


;;;Failing
; (print (match '(! bear bear) '(bear bear bear bear bear))) ;t
; (print (match '(! ! * benson * ! ! *) '(hamburger delta there benson is is))) ;t



