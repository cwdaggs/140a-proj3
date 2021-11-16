#|use reverse list?
car of list = !, then recurse using element after ! and cdr rest of list
Make helper function
Pattern can contain the special chars, match is the actual list of all strings

Two cases for *?
all by itself, in which it replaces one word
in a word, in which need to look at the characters

Cases for !
End of pattern
Randomly in pattern with one word after
Randomly in pattern with multiple words after
Replaces no, 1, many words
Case: If last two items are ! and a word, look for latest placement of the word
Otherwise, find the first placement
Keep length in mind
|#
(defun exclamation (pattern assertion &optional (reverse nil))
	; (print (car pattern))
	; (print (car assertion))
	; add boolean with default value, change if inputting reverse list, should only be one check to see if last elements are equal
	(cond
		((and (string= (first pattern) (first assertion)) (equal reverse t)) t)
		((and (string/= (first pattern) (first assertion)) (equal reverse t)) nil)
		((string= (first pattern) (first assertion)) (match (cdr pattern) (cdr assertion)))
		((string/= (first pattern) (first assertion)) (exclamation pattern (cdr assertion)))
	
	)
)

(defun get_list_between_excl (pattern partial_pattern)
	(cond
		((string= (first pattern) "!") partial_pattern)
		((string/= (first pattern) "!") (get_list_between_excl ((cdr pattern) (cons (car pattern) partial_pattern))))
	)
)
; maybe have pattern doing the same thing just so match can be called afterwards again
(defun check_list (partial_pattern assertion)
	(cond
		; Successfully got through whole partial pattern, match?
		((endp partial_pattern) t)
		; If assertion < partial pattern, never found a match
		((< (length assertion) (length partial_pattern)) nil)
		; First strings of both match so go to next of both
		((string= (first partial_pattern) (first assertion)) (check_list (cdr partial_pattern) (cdr assertion)))
		; First strings not equal so escape back to either end or restart with statement below, moving assertion ahead
		((string/= (first partial_pattern) (first assertion)) nil)
		((and (>= (length assertion) (length partial_pattern)) nil) (check_list partial_pattern (cdr assertion)))
	)

)

(defun rev (lis)
;;; REV
;;; inputs: lis, a list
;;; outputs: a list with elements in reverse order
;;;
	(rev_helper lis NIL)
)



(defun rev_helper (lis1 lis2)
;;; REV_HELPER
;;; inputs: lis1, list; lis2, list
;;; output: the result of consing the elements of lis1 onto lis2 in reverse order
;;;
	(if lis1
		(rev_helper (cdr lis1) (cons (car lis1) lis2))
		lis2
	)
)

(defun determine_if_asterisks (test_string (index 0))
	(cond
		; Reached end of string and no asterisks
		((char= (char test_string index) nil) nil)
		; Proceeds to next char
		((char/= (char test_string index) '*) (determine_if_asterisks test_string (+ index 1)))
		; Contains asterisk and returns true
		((char= (char test_string index) '*) t)
	)
)

;asterisk string comes from pattern, test_string comes from match
(defun asterisk (asterisk_string test_string (index 0))
	(cond
		; Reached end of string and no asterisks
		((char= (char test_string index) nil) nil)
		; Proceeds to next char
		((char/= (char test_string index) '*) (determine_if_asterisks test_string (+ index 1)))
		; Contains asterisk and returns true
		((char= (char test_string index) '*) t)
	)
)


(defun match (pattern assertion)
	; This is just to show that it works when there's no quotations
	; (print (car pattern))
	; (print (car assertion))
	(cond
		; got successfully through whole thing
		((and (endp pattern) (endp assertion)) t)
		; ! at end of pattern so return t
		((and (string= (first pattern) "!") (= (length pattern) 1)) t)
		; end of pattern = ! string/!/*, change to last being not ! and not *
		((and (string= (first pattern) "!") (= (length pattern) 2)) (exclamation (cdr pattern) (rev assertion) t))
		; Two subsequent !s, in which the first one will not matter
		((and (string= (first pattern) "!") (string= (nth 1 pattern) "!")) (match (cdr pattern) assertion))
		; Normal asterisk checking

		; Asterisk checking if standing alone

		; Asterisk checking if ! near

		; Normal ! checking
		((string= (first pattern) "!") (exclamation (cdr pattern) assertion))
		; Normal checking of strings, recurses to next place in both lists
		((string= (first pattern) (first assertion)) (match (cdr pattern) (cdr assertion)))
		; If strings not equal in normal check, returns nil
		((string/= (first pattern) (first assertion)) nil)
	)

)
; (print (match '(apple2 banana) '(apple2 banana)))

; Test cases
(print (match '(color apple red) '(color apple red))) ;t
(print (match '(color apple red) '(color apple green))) ;nil
(print (match '(! table !) '(this table supports a bloc))) ;t
(print (match '(this table !) '(this table supports a bloc))) ;t
(print (match '(! brown) '(green red brown yellow))) ;nil
(print (match '(! brown) '(green red brown brown))) ;t
(print (match '(red green ! blue) '(red green blue))) ;t
; (print (match '(red gr*n blue) '(red green blue))) ; t
; (print (match '(t* table is *n) '(this table is blue))) ; nil
; (print (match '(color apple *) '(color apple red))) ;t
; (print (match '(color * red) '(color apple red))) ;t
; (print (match '(color * red) '(color apple green))) ;nil
; (print (match '(color*red) '(color apple red)) ;nil
; (print (match '(color ! * red) '(color apple red))) ;t
(print (match '(! table orange !) '(this table is table orange rock))) ;t

; (print (find 'table '(this table is table orange rock)))
; Have helper for asterisk, so like if statement in match
; Have helper for exclamation


