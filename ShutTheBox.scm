; CS 201b

;**************************************************************
; Name: Feridun Mert Celebi
; Email address: feridun.celebi@yale.edu
;**************************************************************

; Topics: Scheme, the game of shut-the-box

;**************************************************************
; ** problem 0 ** (1 easy point)
; Please change the following definition of the variable hours to
; reflect how long you spent on the assignment.

(define hours 8)

;**************************************************************
; ** problem 1 ** (10 points)
; Write a procedure:

; (keep predicate? lst)

; to take a procedure predicate? and a list lst
; and return (in order) a list of all the elements of lst
; for which predicate? returns a non-#f value.

; Examples
; (keep odd? '(3 2 14 5 9 11 3)) => (3 5 9 11 3)
; (keep (lambda (x) (> x 0)) '(-12 3 4 0 -11)) => (3 4)
; (keep even? '(3 1 21)) => ()
;**************************************************************

(define keep
    (lambda (predicate? lst)
      (cond
        ((null? lst) '())
        ((predicate? (car lst)) (append (list (car lst)) (keep predicate? (cdr lst))))
        (else (keep predicate? (cdr lst))))))

;**************************************************************
; ** problem 2 ** (10 points)
; Write a procedure:

; (combine op identity lst)

; where op is a procedure of two arguments
; identity is the value to return if lst is empty
; and lst is a list of values.
; What is returned is the result of applying
; op to the first element of the list and
; the result of combining the rest of the elements of
; the list using op and identity.

; Examples:
; (combine + 0 '(3 4 5)) => 12
; (combine * 1 '(3 4 5)) => 60
; (combine - 0 '(3 4 5)) => 4
; (combine cons '() '(3 4 5)) => (3 4 5)
; (combine cons '(a) '(3 4 5)) => (3 4 5 a)
; (combine append '() '((a b) (c d e) (f g))) => (a b c d e f g)
;**************************************************************

(define combine
  (lambda (op identity lst)
    (cond
      ((equal? lst '()) identity)
      ((number? lst) lst)
      ((symbol? lst) (list lst))
      (else
       (op (combine op identity (car lst))
           (combine op identity (cdr lst)))))))
                    
;**************************************************************
; ** problem 3 ** (10 points)
; Write two procedures:

; (subset? lst1 lst2)
; (set-equal? lst1 lst2)

; each of which takes two lists as arguments.
; (subset? lst1 lst2) returns #t if every top-level
; element of lst1 is equal to a top-level element of
; lst2, and #f otherwise.
; (set-equal? lst1 lst2) returns #t if every top-level
; element of lst1 is equal to a top-level element of
; lst2 and every top-level element of lst2 is equal
; to a top-level element of lst1, and #f otherwise.

; Examples:
; (subset? '() '(a b a d)) => #t
; (subset? '(b a b) '(a b a d)) => #t
; (subset? '(b a c) '(a b a d)) => #f
; (subset? '(a) '()) => #f
; (set-equal? '() '()) => #t
; (set-equal? '() '(a)) => #f
; (set-equal? '(a b a d) '(d d d a b)) => #t
; (set-equal? '(b c) '(a b c)) => #f
; (set-equal? '((1 2) (3 4)) '((3 4) (1 2))) => #t
; (set-equal? '((1 2) (3 4)) '((2 1) (3 4))) => #f
;**************************************************************

; This procedure basically does the same thing as symbol-in? procedure
; But it does it for non-permitted expressions as well. 
(define member?
    (lambda (e lst)
      (cond
        ((equal? lst '()) #f)
        ((equal? e (car lst)) #t )
        (else (member? e (cdr lst))))))

; This remove procedure receives two arguments: one element and one list.
; This procedure scans the list and tries to find the element. If it can find
; the element it removes from the list.
(define remove
  (lambda (element lst)
    (cond
      ((equal? '() lst) '())
      ((equal? (car lst) element) (remove element (cdr lst)))
      (else (cons (car lst) (remove element (cdr lst)))))))

(define subset?
  (lambda (lst1 lst2)
    (cond
      ((null? lst1) #t)
      ((not (member? (car lst1) lst2)) #f)
      (else
       (subset? (cdr lst1) lst2)))))

(define set-equal?
  (lambda (lst1 lst2)
    (cond
      ((and (null? lst1) (null? lst2)) #t)
      ((or (null? lst1) (null? lst2)) #f)
      ((or (not (member? (car lst1) lst2)) (not (member? (car lst2) lst1))) #f)
      (else
       (set-equal? (remove (car lst2) (remove (car lst1) lst1)) (remove (car lst1) (remove (car lst2) lst2)))))))

;**************************************************************
; For the remaining problems,
; we consider the game of "shut the box,"
; which will be (or was) demonstrated in Lecture.
; (Or try a web search with "shut the box game".)

; The traditional version of the game has 9 flappers 
; numbered 1 to 9, each of which may be up or down.
; Initially they are all up.
; The player rolls 2 dice and notes the sum, S.
; The player then flips down any combination
; of up flappers whose sum is S, and rolls
; the dice again.
; This process repeats until there is no combination
; of up flappers whose sum is S, at which point
; the player's score is the sum of the remaining
; up flappers.

; Play of the game consists of player 1 playing
; to find his or her score, and then player 2
; playing to find his or her score -- the
; player with the *lower* score wins.

; For example, player 1 might
; roll 2 and 6, and decide to flip down 8
; roll 6 and 6, and decide to flip down 3 and 9
; roll 1 and 3, and decide to flip down 4
; roll 6 and 6, and decide to flip down 5 and 7
; roll 5 and 6, and finish with a score of 9 = 1+2+6

;**************************************************************
; The following accesses an implementation
; of a procedure (random-integer n)
; to return a random integer
; between 0 and n-1 inclusive
; in the R5RS language of DrRacket.
; For example,
; (random-integer 20) => 17
; (random-integer 20) => 3

; (Note that the numbers returned
; are "pseudo-random" -- every
; time you re-enter Scheme, a
; sequence of calls to random-integer
; will return the same sequence of values.)

(#%require srfi/27)

;**************************************************************
; The following defines the symbol standard-flappers in the
; top-level environment to be the list of numbers on the flappers
; in the standard game.

; Some of your procedures will be parameterized with
; a list of flappers and should be able to
; deal with variants of the game in which
; the number of flappers and/or 
; the numbers on the flappers are different.

(define standard-flappers (list 1 2 3 4 5 6 7 8 9))

; We assume that the flappers are an *increasing*
; sequence of distinct positive integers, so another
; list of initial flappers could be (2 3 5 7 11).

; The following defines the standard number of dice to
; be rolled each time a player rolls the dice,
; and the standard numbers on the faces of each die.
; Each die has the same collection of numbers
; on its faces.
; (Die is the singular of dice.)

; Some of your procedures will be parameterized with
; these values as well, and 
; should be able to deal with a different
; number of dice and dice with different
; numbers of faces and/or different numbers on the faces.

; We assume that the number of dice and the numbers
; on the faces of each die are (not necessarily distinct)
; positive integers.  The dice have at least one face.

(define standard-number-of-dice 2)

(define standard-die-values (list 1 2 3 4 5 6))

; A state of the game is a list containing
; the currently up flappers in *increasing order*.
; The initial state in the standard game is 
; (1 2 3 4 5 6 7 8 9).
; In the example above, the state at the end
; of the game is (1 2 6).

;**************************************************************
; ** problem 4 ** (10 points)
; Write four procedures:
; (from-to m n)
; (sum-of lst)
; (pick-random lst)
; (roll-dice number-of-dice die-values)

; (from-to m n) takes two integers m and n
; (where m is less than or equal to n)
; and returns the list of integers (m m+1 ... n-1 n).

; (sum-of lst) takes a list lst of
; integers, and returns their sum.
; (Note that the sum of an empty
; list of integers is 0.)

; (pick-random lst)
; takes a *non-empty* list lst
; and returns a uniformly randomly
; chosen element of lst.
; (Please use the procedure random-integer,
; accessed as shown above.)

; (roll-dice number-of-dice dies-values) 
; returns a list of number-of-dice integers
; each chosen randomly and independently
; from the list die-values.

; Examples:
; (Note that for the ones that involve
; randomness, your results may differ.)
; (from-to 3 7) => (3 4 5 6 7)
; (from-to 4 4) => (4)
; (from-to -3 3) => (-3 -2 -1 0 1 2 3)
; (sum-of '()) => 0
; (sum-of '(1 2 6)) => 9
; (sum-of '(4 5 6 9)) => 24
; (pick-random '(a b c)) => c
; (pick-random '(a b c)) => a
; (pick-random '(1 2 3 4 5 6)) => 2
; (roll-dice 2 '(1 2 3 4 5 6)) => (4 2)
; (roll-dice 2 '(3 4 5 6)) => (3 5)
; (roll-dice 2 '(3 3 3 4)) => (3 3)
;**************************************************************

(define from-to
  (lambda (m n)
    (if (= m n)
        (list n)
        (append (list m) (from-to (+ m 1) n)))))

(define sum-of
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum-of (cdr lst))))))

(define pick-random
  (lambda (lst)
    (list-ref lst (random-integer (length lst)))))

(define roll-dice
  (lambda (number-of-dice dies-values)
    (cond
      ((= number-of-dice 0) '())
      (else
       (append (list (list-ref dies-values (random-integer (length dies-values)))) 
               (roll-dice (- number-of-dice 1) dies-values))))))


;**************************************************************
; ** problem 5 ** (10 points)
; A move consists of a list of positive integers
; given in *increasing order*
; indicating that flappers with those numbers 
; should be flipped down.

; Write two procedures
; (ok-move? move state)
; (make-move move state)

; where state is a possible
; state of the flappers and
; move is a move as indicated above.

; (ok-move? move state)
; given a move and a state
; returns #t if the move is possible in the state
; and #f otherwise.
; A move is possible if we can flip down a flapper
; in state corresponding to each of the numbers
; in move.

; (make-move move state)
; given a move and a state
; returns the state that results from making the given move
; (ie, flipping down the indicated flappers.)
; You may assume that the move is possible in the
; given state.


; Examples
; (ok-move? '(1 2 6) '(1 2 3 5 6 9)) => #t
; (ok-move? '(3 7) '(1 2 3 5 6 9)) => #f
; (ok-move? '(6) '(4 5 6)) => #t
; (make-move '(1 2 6) '(1 2 3 5 6 9)) => (3 5 9)
; (make-move '() '(3 4 5)) => (3 4 5)
; (make-move '(3 4 5) '(3 4 5)) => ()
; (make-move '(4) '(1 2 3 4 5 6 7 8 9)) => (1 2 3 5 6 7 8 9)
;**************************************************************

(define ok-move?
  (lambda (move state)
    (cond
      ((null? move) #t)
      ((not (member? (car move) state)) #f)
      (else
       (ok-move? (cdr move) state)))))

(define make-move
  (lambda (move state)
    (cond
      ((or (null? move) (not (list? move))) state)
      (else
        (make-move (cdr move) (remove (car move) state))))))
      
;**************************************************************
; ** problem 6 ** (10 points)
; Write a procedure:
; (all-subsequences lst)
; that returns a list of all the *distinct* subsequences
; of the list lst.
; A subsequence of a list is a list that can
; be obtained by deleting zero or more (not necessarily contiguous)
; elements from the list.
; The output should not contain duplicates, but may list the 
; possible subsequences in a *different* order from the examples.

; Examples:
; (all-subsequences '()) => (())
; (all-subsequences '(a)) => (() (a))
; (all-subsequences '(a b c)) => (() (c) (b) (b c) (a) (a c) (a b) (a b c))
; (all-subsequences '(a b a)) => (() (a) (b) (b a) (a a) (a b) (a b a))
; (all-subsequences '(x (x))) => (() ((x)) (x) (x (x)))

; Hint: think "executively" -- if you have the
; solution for (all-subsequences '(b c)), what must
; you do to get the solution for (all-subsequences '(a b c))?
; The remove-duplicates procedure from assignment #1 might be helpful.
;**************************************************************

; This procedure is from hw1. The procedure's definition from hw1 is:
; It takes a list of Scheme values and returns
; a list of the top-level elements of lst with
; duplicates removed.
(define remove-duplicates
  (lambda (lst)
    (if (null? lst)
     '()
     (cons (car lst) (remove-duplicates(remove (car lst) (cdr lst)))))))

; This attach procedure receives an element and a list.
; It basically adds the element to every member of the given list.
; This attach was also implemented in the CPSC TA section on Monday.
(define attach
  (lambda (ele lst)
    (cond
      ((equal? lst '()) '())
      (else
       (cons (cons ele (car lst)) (attach ele (cdr lst)))))))

(define all-subsequences
  (lambda (lst)
    (if (null? lst)
        (list '())
        (remove-duplicates (append (all-subsequences (cdr lst)) 
                (attach (car lst) (all-subsequences (cdr lst))))))))
;**************************************************************
; ** problem 7 ** (10 points)
; Write two procedures:

; (possible-moves sum state)
; (next-states sum state)

; where sum is the sum of a dice roll
; and state is a possible game state.

; (possible-moves sum state)
; returns the list of all possible moves
; whose elements sum to the value sum in the given state.
; The moves may be listed in *any order*, but
; there should not be duplicates.

; (next-states sum state)
; returns a list of all the states reachable
; from the given state by making a possible
; move with value equal to sum.
; The states may be listed in *any order*, but
; there should be no duplicates.

; Examples
; (possible-moves 12 '(3 4 5 7 9)) => ((5 7) (3 9) (3 4 5))
; (possible-moves 6 '(1 2 3 4 5 6)) => ((6) (2 4) (1 5) (1 2 3))
; (possible-moves 11 '(1 2 6)) => ()
; (next-states 12 '(3 4 5 7 9)) => ((3 4 9) (4 5 7) (7 9))
; (next-states 11 '(1 2 6)) => ()
;**************************************************************

; This is a modified keep method. It receives an additional argument, sum.
; The procedure checks whether the predicate? is true for the sum and the list.
; If the conditional statements return true, the element of the list is kept, otherwise it is thrown away.
(define keep2
    (lambda (predicate? sum lst)
      (cond
        ((null? lst) '())
        ((predicate? sum (sum-of (car lst))) (append (list (car lst)) (keep2 predicate? sum (cdr lst))))
        (else (keep2 predicate? sum (cdr lst))))))

(define possible-moves
  (lambda (sum state)
    (keep2 equal? sum (all-subsequences state))))

; This is a helper method for the next-states procedure. 
; The procedure receives the list of the possible moves and the state of the flappers.
; Then it basically removes the possible moves from the original state by using the
; make-move procedure.
(define remove-states
  (lambda (pos state)
    (cond
      ((or (null? pos) (null? state)) '())
      (else 
       (cons (make-move (car pos) state) (remove-states (cdr pos) state))))))

(define next-states
  (lambda (sum state)
    (if (null? (possible-moves sum state))
        '()
        (remove-states (possible-moves sum state) state))))

;**************************************************************
; ** problem 8 ** (10 points)
; Write a procedure:

; (choose-random-move sum state)

; where sum is the sum of a dice roll
; and state is a possible game state,
; returns a randomly selected one
; of the possible moves in the
; given state when the dice roll sum
; is as indicated.
; If there is no legal move, it
; returns the symbol none.

; Examples:
; (choose-random-move 6 '(1 2 3 4 5 6 7 8 9)) => (2 4)
; (choose-random-move 6 '(1 2 3 4 5 6 7 8 9)) => (2 4)
; (choose-random-move 6 '(1 2 3 4 5 6 7 8 9)) => (6)
; (choose-random-move 11 '(1 2 4 6)) => (1 4 6)
; (choose-random-move 2 '(3 4 5)) => none
;**************************************************************

(define choose-random-move
  (lambda (sum state)
    (cond
      ((null? (possible-moves sum state)) 'none)
      (else (pick-random (possible-moves sum state))))))

;**************************************************************
; ** problem 9 ** (10 points)
; Write a procedure:

; (play move-chooser flappers number-of-dice die-values)

; that takes a move-chooser
; a list flappers of initial flappers
; a number of dice
; a list of values on the faces of a die
; and simulates playing shut the box
; using the move-chooser to choose
; moves until no further moves
; are possible, at which point
; it returns the sum of the remaining
; up flappers in the state.

; Like choose-random-move, 
; a move-chooser takes as input
; a sum of dice and a game state
; and returns either
; 1) a move that is possible in the
; given state and sums to the given sum,
; or 
; 2) the symbol none if there is
; no move possible in the current state
; that sums to the given sum.
; The procedure choose-random-move is a
; move-chooser.

; You may assume the move-chooser is not "cheating."

;**************************************************************

(define play
  (lambda (move-chooser flappers number-of-dice die-values)
    (let ((x (move-chooser (sum-of (roll-dice number-of-dice die-values)) flappers)))
    (if (equal? x 'none)
        (sum-of flappers)
        (play move-chooser (make-move x flappers) number-of-dice die-values)))))
    

;**************************************************************
; ** problem 10 ** (10 points)
; Write a procedure

; (choose-better-move sum state)

; that makes a better than random choice of
; one of the possible moves in 
; the given state when the dice roll
; sum is as indicated.

; Please explain CLEARLY in comments how 
; your procedure chooses moves.
; To get credit for this problem, your procedure
; must win a "reasonably convincing" number of games
; against choose-random-move.

; Include (as **comments**) some data from
; playing your choose-better-move against
; choose-random-move using the match procedure
; defined below.

; For example,
;>  (match choose-random-move choose-better-move 100 
;          standard-flappers 2 '(1 2 3 4 5 6))
;(22 75 3)
; is reasonably convincing: in 100 matches,
; choose-random-move won 22 times, choose-better-move
; won 75 times and they tied 3 times.

;**************************************************************
; Once you have your play procedure (problem #9)
; working, the following code may be used to
; play two players against each other
; for a number of games, returning the number
; of games won by player1, the number of games
; won by player2, and the number of tie games.

; Note that in this setting, player2 does not
; learn player1's score.

(define match
  (lambda (player1 player2 games flappers number-of-dice die-values)
    (match-helper 
     player1 player2 games 0 0 0 flappers number-of-dice die-values)))

(define match-helper
  (lambda (player1 player2 games won1 won2 ties flappers number-of-dice die-values)
    (if (<= games 0)
  (list won1 won2 ties)
	(let ((score1 (play player1 flappers number-of-dice die-values))
	      (score2 (play player2 flappers number-of-dice die-values)))
	  (match-helper player1 player2 (- games 1)
			(+ won1 (if (< score1 score2) 1 0))
			(+ won2 (if (< score2 score1) 1 0))
			(+ ties (if (= score1 score2) 1 0))
			flappers number-of-dice die-values)))))

;************************************************************

; MY ALGORITHM:
; It is common knowledge that when you roll two dice the probability of getting numbers around 5 and 6 is much higher than getting
; lower or higher sums. So, in my algorithm I firstly try to eliminate the sums which are higher or lower than 5 and 6.
; And because in the shut the box game, we are trying to get the smaller sum at the end, I thought it would be smarter to shut the
; tiles with the higher numbers. So, basically that is why I defined my priority list with numbers 9, 8, 7 (in descending order).
; On the other hand, getting lower numbers are tougher since they almost always be combined with other numbers to eqaul a certain sum.
; That is why I then continue my priority list with the numbers 1, 2, 3 (in ascending order).
; If I cannot find any of these priority numbers, I basically turn to a random move selection, since for me they would all be equally 
; bad choices for me. 
;---------------------------------------------------------------------------------------------------------------
; SOME DATA:
; Basically, the wins (out of 100 trials) of the choose-better-move procedure ranged mostly from 64 to 78. 
; However, this does not change the fact that most of the wins (out of 100 trials) were around 74. 
; At some point, my code consistently gave the results around the outcome (23 - 74 - 3).
;
;---------------------------------------------------------------------------------------------------------------
; HOW TO IMPROVE:
; In my opinion my code might be improved in two aspects. First of all, it might be much much more efficient. Second of all,
; I probably would be able to cover more cases to increase the win-loss ratio of the choose-better-move procedure.
;
;---------------------------------------------------------------------------------------------------------------
; PROCEDURE COMMENT:
; This procedure receives three arguments, two of them (pos) are the list of all possible moves,
; and the other is the priority list that I defined. The second pos is for the purpose of storing
; the initial state of all possible moves. Basically, this procedure checks whether any member of the priority
; list is in any of the pos lists. If it finds anything it returns that element of the possible moves list.
(define get-priority
  (lambda (pos priority old-pos)
    (cond
      ((null? pos) (get-priority old-pos (cdr priority) old-pos)) 
      ((or (null? priority) (member? (car priority) (car pos)) (car pos)))
      (else (get-priority (cdr pos) priority old-pos)))))


(define choose-better-move
  (lambda (sum state)
    (let ((priority '(9 8 7 1 2 3)))
    (cond
      ((null? (possible-moves sum state)) 'none)
      ((list? (get-priority (possible-moves sum state) priority (possible-moves sum state))) (get-priority (possible-moves sum state) priority (possible-moves sum state)))
      (else (choose-random-move sum state))))))

;******************  end of hw #2  *****************************
