(defun assertion_check (pattern assertion assertion_dup)
	(cond
		((endp assertion) (match pattern assertion_dup))
		((pure_asterisks (string (first pattern))) (assertion_check (cdr pattern) (cdr assertion) (cdr assertion)))
		((eql (determine_if_asterisks (first pattern)) t) 
			(if (asterisk (string (first pattern)) (string (first assertion)))
				(assertion_check (cdr pattern) (cdr assertion) assertion)
				nil
		))
		((string= (first pattern) (nth 1 pattern)) (assertion_check (cdr pattern) assertion assertion_dup))
		((string/= (first pattern) (first assertion)) (assertion_check pattern (cdr assertion) assertion_dup))
		((string= (first pattern) (first assertion)) (assertion_check pattern (cdr assertion) assertion))
	)
)

(defun assertion_check_more (pattern pattern_dup assertion)
	(cond
		((and (null assertion) (string/= (first pattern) "!")) nil)
		((string= (first pattern) "!") (match pattern assertion))
		((pure_asterisks (string (first pattern))) (assertion_check_more (cdr pattern) pattern_dup (cdr assertion)))
		((eql (determine_if_asterisks (first pattern)) t) 
			(if (asterisk (string (first pattern)) (string (first assertion)))
				(assertion_check_more (cdr pattern) pattern_dup (cdr assertion))
				nil
		))
		((string= (first pattern) (first assertion)) (assertion_check_more (cdr pattern) pattern_dup (cdr assertion)))
		((string/= (first pattern) (first assertion)) (assertion_check_more pattern_dup pattern_dup (cdr assertion)))
	)
)

(defun determine_if_asterisks (test_string &optional (substring_index 0))
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

(defun pure_asterisks (pattern &optional (index 0))
	(cond
		((eql index (length pattern)) t)
		((char= (char pattern index) #\*) (pure_asterisks pattern (+ index 1)))
		((char/= (char pattern index) #\*) nil)
	)
)

(defun asterisk (pattern assertion &optional (index 0) (assert_index 0))
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
	(cond
		((and (eql assert_index (length assertion)) (char/= (char pattern index) #\*)) nil)
		((char= (char pattern index) #\*) (asterisk pattern assertion index assert_index))
		((char= (char pattern index) (char assertion assert_index)) (asterisk_helper_more pattern assertion (+ index 1) index_dup (+ assert_index 1) assert_index_dup))
		((char/= (char pattern index) (char assertion assert_index)) (asterisk_helper_more pattern assertion index_dup index_dup (+ assert_index_dup 1) (+ assert_index_dup 1)))
	)
)


(defun match (pattern assertion)
	(cond
		; Got successfully through whole thing
		((and (endp pattern) (endp assertion)) t)
		; ! at end of pattern so return t
		((and (string= (first pattern) "!") (= (length pattern) 1)) t)
		; Two subsequent !s, in which the first one will not matter
		((and (string= (first pattern) "!") (string= (nth 1 pattern) "!")) (match (cdr pattern) assertion))
		; Asterisk checking if standing alone
		((and (pure_asterisks (string (first pattern))) (not (null assertion))) (match (cdr pattern) (cdr assertion)))
		((and (pure_asterisks (string (first pattern))) (null assertion)) nil)
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