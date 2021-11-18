Running Test Cases:
I used VSCode along with a Common Lisp extension in order to code this. 
In order to run the code, I installed clisp and ran using 'clisp <filename>' 
through the terminal. In the test cases file, there are many test cases for 
match.lisp that I would run by copying and pasting them below my match function.

If using portacle/emacs, compile all the functions individually by clicking on
them and pressing Ctrl-C Ctrl-C. Then run using whatever the main function's name
is by typing the case and pressing Ctrl-Enter.

Ex: 
(match '(color apple red) '(color apple red))
(nth-fib 8)

Since I have no idea how this is being graded, please let me know if there are 
issues running it as I've passed all the cases in my test cases file which has 
quite an extensive amount of cases.

Design of fibonacci:
nth-fib uses a classic recursive approach. The other two fibonacci functions
utilize a helper function in order to recursively solve the problem without
altering the original function. They recursively add to a list with cons until
the base case is met.

Design of match:
Match works on two levels: the list/assertion level and the word level. It will
recursively parse and examine the lists and determine the path it takes based on
what's in the pattern. There are essentially two cases for both ! and *: there are
either more !s or *s afterward or there are not any more. This is why there are 
two asterisk helper functions and two list-level checking functions as well. They 
utilize a sliding window approach, trying to find either the first time the pattern
matches the assertion or the last time using the two cases above respectively. 
Other helper functions have functionalities like checking if there are more !s
in a pattern, if a word has asterisks in it, and if a word is only composed of 
asterisks. No imperative functions are used.

Sources:
I learned of some Common Lisp functions by just looking at the documentation. In
the case of errors, I used StackOverflow posts to try and guess the problem since 
Lisp does not provide much information. Many test cases were outsourced from 
other students in the Discord. 