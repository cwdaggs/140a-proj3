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

(defun match (pattern match)
	; This is just to show that it works when there's no quotations
	; (print (car pattern))
	; (print (car match))
	(cond
		; got successfully through whole thing
		((and (endp pattern) (endp match)) t)
		; ! at end of pattern so return t
		((and (string= (first pattern) "!") (= (length pattern) 1)) t)
		; end of pattern = ! string/!/*
		((and (string= (first pattern) "!") (= (length pattern) 2)) (exclamation (nth 1 (pattern)) (rev (cdr match))))
		; Two subsequent !s, in which the first one will not matter
		((and (string= (first pattern) "!") (string= (nth 1 (pattern)) "!")) (match (cdr pattern) match))
		; Normal asterisk checking

		; Asterisk checking if standing alone

		; Asterisk checking if ! near

		; Normal ! checking
		((string= (first pattern) "!") (exclamation (nth 1 (pattern)) (cdr match)))
		; Normal checking of strings, recurses to next place in both lists
		((string= (first pattern) (first match)) (match (cdr pattern) (cdr match)))
		; If strings not equal in normal check, returns nil
		((not (string= (first pattern) (first match))) nil)
	)

)
; (print (match '(apple2 banana) '(apple2 banana)))

; Test cases
(print (match '(color apple red) '(color apple red))) ;t
(print (match '(color apple red) '(color apple green))) ;nil

(print (match '(this table !) '(this table supports a bloc))) ;t


; Have helper for asterisk, so like if statement in match
; Have helper for exclamation
(defun exclamation (second_word match)

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
