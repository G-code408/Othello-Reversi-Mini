#| 
Project Description: Reversi Game implemented in LISP 
Assignment: HW05C
Team Members: 
Omar Abdulaziz
Brian Huynh
Davin Benson


General Notes: 
User is Black and the Program is White. 
Player makes the first move
|#



( defstruct state
    board   ; this is a list of lists, Just using all C++ ideas 
    player  ;keeps track of the turn ( B | W )
    moves   ; depending on who's turn it is, "moves" stores all possible moves 
    byMove ; The position ( x y ) of the move that created the state
) 

( defun NEW-GAME ()

    ( let ; necessary keyword to manipulate structs 
        (
            ( board  
                '( ( nil nil nil nil )
                   ( nil  B   W  nil )
                   ( nil  W   B  nil )
                  ( nil nil nil nil ) )
            )
            ( player 'B ) ; Rule of the game is black goes first 
        )
        ( make-state ; a temporary instance to track the game. 
            :board board
            :player player 
            :moves (moves_list board player) ; call to actions fn 
            :byMove nil ; this is nil because initial state
        )
    )
)


#|(defun PRINT-BOARD (state) 
(princ"________")
(terpri)
(loop for i from 0 to 15 
  do( when ( OR (eq i 4) (eq i 8) (eq i 12) ) (terpri) ) ; This ensure the board prints a line every 4 spaces to simulate a 4x4 grid
    (write (aref state i) )
    (princ " ")
    (when (eq i 3) (princ "|1"))
    (when (eq i 7) (princ "|2"))
    (when (eq i 11) (princ "|3"))
    (when (eq i 15) (princ "|4"))
);end loop
(terpri)
(princ "^ ^ ^ ^  
A B C D ")
(terpri)
)|#
(defun PRINT-BOARD(board) ; Print function with GAME BOARD parameter
	    ( let 
			( ( i 1 ) )
			; Format column numbers
			( format t "  1 2 3 4~%" )
			( dolist ( x board )
            ; Print row number
            ( format t "~a " i )
            ( dolist ( y x )
                ; If null print - else print what is there
                ( if ( null y )
                    ( format t "- " )
                    ( format t "~a " y )
                )
            )
            ; Increment row number
            ( setf i ( 1+ i ) )
            ( format t "~%" )
        )

        ( format t "~%" )
    )
)

(defun PRINT-BOARD1(state) ; Print function with GAME STATE parameter
	(setf board (state-board state))
	    ( let 
			( ( i 1 ) )
			; Format column numbers
			( format t "  1 2 3 4~%" )
			( dolist ( x board )
            ; Print row number
            ( format t "~a " i )
            ( dolist ( y x )
                ; If null print - else print what is there
                ( if ( null y )
                    ( format t "- " )
                    ( format t "~a " y )
                )
            )
            ; Increment row number
            ( setf i ( 1+ i ) )
            ( format t "~%" )
        )

        ( format t "~%" )
    )
)


(defun TERMINAL-TEST (state)
	(setq state_2 (copy-state state) )
	(setf (state-player state_2) (if (eq (state-player state) 'B) 'W 'B))
  ;(eql (count '0 state ) 0 ) 
 	(if (eq 0 (length (ACTIONS(state)))) 
 		(if ( eq 0 (length (ACTIONS(state_2)))) 
 			(return-from TERMINAL-TEST nil) ) )
 	(return-from TERMINAL-TEST t)
 )


#|(defun ACTIONS (state) 

(setq ACTION-LIST (make-array 12 :fill-pointer 0)) ; an empty list to store actions
(loop for i from 0 to 15 ; loop through the whole array
    do( 
    
         when (eq (aref state i) 'W)  ; if a W is found then ....
          
          ; each if statement is checking a direction for two things : 1. if the space it borders contains a B. & 2. If the space one unit over 
          ;-in whichever direction that if is handling is empty( eq 0), then we have a valid move. 
          (if (< 0 (- i 8) ) (if (AND (eq (aref state (- i 4) ) 'B) (eq (aref state (- i 8)) 0)) (vector-push (- i 8) ACTION-LIST) ) ) ;up
          (if (< 0 (+ i 8) ) (if (AND (eq (aref state (+ i 4) ) 'B) (eq (aref state (+ i 8)) 0)) (vector-push (+ i 8) ACTION-LIST) ) ) ;down
          (if (< 0 (- i 2) ) (if (AND (eq (aref state (- i 1) ) 'B) (eq (aref state (- i 2)) 0)) (vector-push (- i 2) ACTION-LIST) ) ) ;left
          (if (< 0 (+ i 2) ) (if (AND (eq (aref state (+ i 1) ) 'B) (eq (aref state (+ i 2)) 0)) (vector-push (+ i 2) ACTION-LIST) ) ) ;right
          (if (< 0 (- i 10) ) (if (AND (eq (aref state (- i 5) ) 'B) (eq (aref state (- i 10)) 0)) (vector-push (- i 10) ACTION-LIST) ) );upleft
          (if (< 0 (- i 6) ) (if (AND (eq (aref state (- i 3) ) 'B) (eq (aref state (- i 6)) 0)) (vector-push (- i 6) ACTION-LIST) ) );upright
          (if (< 0 (+ i 6) ) (if (AND (eq (aref state (+ i 3) ) 'B) (eq (aref state (+ i 6)) 0)) (vector-push (+ i 6) ACTION-LIST) ) );downleft
          (if (< 0 (+ i 10) ) (if (AND (eq (aref state (+ i 5) ) 'B) (eq (aref state (+ i 10)) 0)) (vector-push (+ i 10) ACTION-LIST) ) );downright
         ) ;end do 
         ) ; end loop  
    ( return-from ACTIONS ACTION-LIST) ; ACTION-LIST array is returned after the loop is over
) ;end fn
|#

;Function to find all possible moves for player, use GAME STATE parameter
(defun ACTIONS (state)
	(do 
		(	;Block for setting up variables
			;Setting up the y coordinate for board
			(y 0 (+ 1 y))
			;List to contain all possible moves
			(moves_list nil)
			;Get other player's color, if current player = w, other player = b. Vice versa
			(other_player (if (eq (state-player state) 'B) 'W 'B))
			;Variable to hold individual move
			individual_move
			;Get board
			(board (state-board state))
			;Get current player
			(player (state-player state))
		)
		(	
			(>= y 4) ;After searching entire board
			(merge-moves moves_list) ;call function merge_moves to get rid of redundant moves
		)
		(do	
				(
					( x ;Setting up the x coordinate for the board
						(position player (nth y board)) ;find the position of the current player's first coin
						(position player (nth y board) :start (+ 1 x)) ;find remaining coins.
					)
				)
				((not x) nil) ;Stops searching when there are no more coins to be found
				
				;for each neighbor board[x][y]
				(do ((m -1 (+ 1 m))) ((>= m 2) nil) ;x
					(do ((n -1 (+ 1 n))) ((>= n 2) nil);y
					
					(when
						(and
							(>= (+ x m) 0) ;Skip if out of bounds
							(>= (+ y n) 0) ;Skip if out of bounds
							(eq (at board (+ x m) (+ y n)) other_player) ;if neighbor's piece is other player's piece
						)
						;Call check_path function to determine path from player's starting coin to chosen spot
						(setf move (check_path board (+ x m) (+ y n) m n))
						;If move is found and not nil, add it to move list
						(if move (setf moves_list (cons move moves_list)))
					)
					)
				)
		)
	)
)

; Function similar above but use to create moves list when initiating/creating State struct.
(defun moves_list (board player)
	(do 
		(	;Block for setting up variables
			;Setting up the y coordinate for board
			(y 0 (+ 1 y))
			;List to contain all possible moves
			(moves_list nil)
			;Get other player's color, if current player = w, other player = b. Vice versa
			(other_player (if (eq player 'B) 'W 'B))
			;Variable to hold individual move
			individual_move
		)
		(	
			(>= y 4) ;After searching entire board
			(merge-moves moves_list) ;call function merge_moves to get rid of redundant moves
		)
		(do	
				(
					( x ;Setting up the x coordinate for the board
						(position player (nth y board)) ;find the position of the current player's first coin
						(position player (nth y board) :start (+ 1 x)) ;find remaining coins.
					)
				)
				((not x) nil) ;Stops searching when there are no more coins to be found
				
				;for each neighbor board[x][y]
				(do ((m -1 (+ 1 m))) ((>= m 2) nil) ;x
					(do ((n -1 (+ 1 n))) ((>= n 2) nil);y
					
					(when
						(and
							(>= (+ x m) 0) ;Skip if out of bounds
							(>= (+ y n) 0) ;Skip if out of bounds
							(eq (at board (+ x m) (+ y n)) other_player) ;if neighbor's piece is other player's piece
						)
						;Call check_path function to determine path from player's starting coin to chosen spot
						(setf move (check_path board (+ x m) (+ y n) m n))
						;If move is found and not nil, add it to move list
						(if move (setf moves_list (cons move moves_list)))
					)
					)
				)
		)
	)
)

;Helper function for ACTIONS function
;Function to check for redundant moves and reduce list.
;Returns a new list with no redundant moves
(defun merge-moves (moves_list)
	(let
		(
			(return_list nil) ;List to return with no redundant moves
			(temp_list (copy-list moves_list)) ;Make a local copy of moves list
			in_list ;List of redundant moves or moves already on return_list
		)
		
		;For each move in temp_list
		(dolist (move temp_list return_list)
			;Checks if move is already in the return_list
			( setf in_list ( car ( member move return_list :test
                #'( lambda ( m1 m2 ) ( equal ( car m1 ) ( car m2 ) ))
				) 
				)				
			)
			; If move is already in return_list
			(if in_list
				;add to in_list
				(setf (nth 1 in_list) (append (nth 1 move) (nth 1 in_list)))
				;Else, add the move to return_list
				(setf return_list(cons move return_list))
			)
		)
	)
)

;Helper function for ACTIONS function
;function to check of move by player is legal or not
;Returns true for legal and false for illegal move
(defun check_path (board x y m n)
	(do
		(	;Block for setting up
			(player (at board x y)) ;get player's color
			(stop nil) ; boolean for stop check
			(move nil) ; 
			(path nil) ; path from beginning spot to designated spot
		)
		(stop move)
		
		(setf path (cons (list x y) path)) ;add current position to path
		
		;Move current position towards designated spot/path
		(setf x (+ x m)) 
		(setf y (+ y n))
		
		(cond
			(	;check for out of bounds
				(or (< x 0) (>= x 4) (< y 0) (>= y 4)) ;condition
				(setf stop T) ;Change stop from nil to True
			)
			
			(	;check if on player's piece
				(eq (at board x y) player) ;condition
			)
			
			(	;empty spot
				(not (at board x y)) ;condition
				(setf stop T)
				(setf move (list (list x y) path))
			)
			
			(	;check if on other player's piece
				t ;condition
				(setf stop T)
			)
		)
	)
)


; macro for accessing board positions 
; positions on board using Row and Col number
( defmacro at ( board R C )

    `( nth ,R ( nth ,C ,board ) )
)

( defun flip_tiles ( board path )
    ( let
        (
            player ; The color marker for the current player
            ( newBoard ( mapcar #'copy-list board ) ) ; this is how to make a local 
        )

        ; For each position in the path
        ( dolist ( tile path newBoard )
            ; Get the player's color at this position
            ( setf player ( at newBoard ( car tile ) ( cadr tile ) ) )

            ; Toggle the player's color
            ( setf player ( if ( eq player 'W ) 'B 'W ) )

            ; Set the new player's color at this position
            ( setf ( at newBoard ( car tile ) ( cadr tile ) ) player )
        )
    )
)

; Places a coin for the given player at the given position
( defun place_coin ( board player pos )
"Places a coin for the given player at the given position."
    ( setf
        ( at
            board
            ( car pos )
            ( cadr pos )
        )
        player
    )
)

( defun move_to_state ( curState move )
    ( let
        (
            ( path ( cadr move ) ) ; list of coin positions that will be changed by this move
            ( pos ( car move ) ) ; position of player move 
            ; Remembering other player
            ( other-player ( if ( eq ( state-player curState ) 'B ) 'W 'B ) )
            ; We use newBoard to put out a new  gamestate after move has been made 
            newBoard
        )
		; making the newboard change based on the move made
        ( setf newBoard ( flip_tiles ( state-board curState ) path ) )

        ; Place the coin that is indicative of the move
        ( if pos
            ( place_coin newBoard ( state-player curState ) pos )
        )

        ; Generate a new game state
        ( make-STATE
            :board newBoard
            :player other-player
            :moves ( moves_list newBoard other-player )
            :creationMove pos
        )
    )
)


;helper fn to make player move
( defun xyToOutput ( pos )

    ; Add one to row and column
    ( list ( 1+ ( cadr pos ) ) ( 1+ ( car pos ) ) )

)


; Asksplayer for  move and checks if valid
; helps player if wrong move used by printing possible moves
; Makes move and returns new state
( defun player-move ( curState )

    ( let
        (
            ( invalid t ) ; bool for if the entered move is invalid
            row ;The entered row of a move
            col ;The entered column of a move
            posMoves ;List of possible moves
            newState ;the game state created by the new move
        )

        ; ask  user for a move only if there are moves to be made
        ( when ( setf posMoves ( state-moves curState ) )

            ; check until valid move
            ( do ()
                ; leave when valid move is entered
                ( ( null invalid ) newState )
					 ( format t "What is your move [row col]? " )
                ; read in te row and column
                ( setf row ( 1- ( read ) ) )  ( setf col ( 1- ( read ) ) )  ( format t "~%" )

                ; Loop through  all possible moves
                ( dolist ( x posMoves )
                    ; Check if row and col match acceptable move
                    ( when
                        ( and  ( = row ( cadr ( first x ) ) ) ( = col ( car ( first x ) ) ) )
                        ( setf invalid nil )
                        ( setf newState ( move_to_state curState x ) )
                    )
                )
                ; Prints possible moves if invalid move was entered
                ( when invalid
                    ( format t "Possible Moves:~%~{~{~a ~}~%~}~%"
                        ( sort
                            ; Convert the moves to output format
                            ( mapcar #'( lambda ( move )
                                ( xyToOutput ( first move ) )
                            ) posMoves )
                            ; Sort the moves by row then column
                            #'( lambda ( x y )
                                ( if ( eq ( car x ) ( car y ) )  ( < ( cadr x ) ( cadr y ) )  ( < ( car x ) ( car y ) ) )
 )
 )
 )
 )
)
 )
)
)



;Function to get utility score
( defun utility ( state )
    ; For each row on the board
    ( do
        (
            ; Set up i for loop
            ( i 0 (+ 1 i) )

            ; Get other player's color
            ( other-player ( if ( eq ( state-player state ) 'B ) 'W 'B ) )

            ( player-count 0 ) ; The count for the current player
            ( other-player-count 0 ) ; The count for the other player
        )
        ; Stop when all rows are looped
        ( ( >= i ( length ( state-board state ) ) )
            ; Return the utility value
            (- player-count other-player-count)
        )

        ; Counts the number of coins the current player has on the board
        ( setf player-count
            ( + player-count ( count
                ( state-player state )
                ( nth i ( state-board state ) )
            ) )
        )

        ; Counts the number of coins the other player has on the board
        ( setf other-player-count
            ( + other-player-count ( count
                other-player
                ( nth i ( state-board state ) )
            ) )
        )
    )
)

(defun MAX-VALUE (state) 
	(if (TERMINAL-TEST state) (return-from MAX-VALUE (UTILITY state) ) )
	(setq v (most-negative-fixnum) )
	( loop for a in (ACTIONS state) 
		do( setq v (MAX v (MIN-VALUE ( RESULT state a )) ) )
	)
	(return-from MAX-VALUE v)
) ;Helper fn for minimax-decision

(defun MIN-VALUE (state) 
	(if (TERMINAL-TEST state) (return-from MIN-VALUE (UTILITY state) ) )
	(setq v (most-positive-fixnum) )
	( loop for a in (ACTIONS state) 
		do( setq v (MIN v (MAX-VALUE ( RESULT state a )) ) )
	)
	(return-from MIN-VALUE v)
) ;Helper fn for minimax-decision

(defun MINIMAX-DECISION (state) 
	; returns that action in ACTIONS(state) that has the maximum value of MIN-VALUE(RESULT(state, a))
	(setq return_state nil) ; will hold the state to return
	(setq return_state_value 0) ; will hold the util value of the state to return
	(loop for a in (ACTIONS state)
		do( setq a_value (MIN-VALUE ( RESULT state a ) ) ) ; get the util val of action a
		if (<= return_state_value a_value ) ; if the prior state to return has a lower util value, swap it with a
		do (setq return_state (a) )
		if (<= return_state_value a_value ) ; if the prior state to return has a lower util value, swap it with a
		do (setq return_state_value (a_value) ) 
	)
	(return-from MINIMAX-DECISION return_state)
) 

#|_______________________________________________________________________________________|#



 



