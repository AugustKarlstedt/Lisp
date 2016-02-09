(defparameter player-black 'X)
(defparameter player-white 'O)
(defparameter blank-tile '_)

(defparameter player-black-score 0)
(defparameter player-white-score 0)

(defparameter rows-count 9)
(defparameter columns-count 9)

(defparameter moves-count (- (* rows-count columns-count) 1))

(defparameter positive-infinity most-positive-fixnum)
(defparameter negative-infinity most-negative-fixnum)

; pairs of moves the AI will search
(defparameter num-plys 3)

; wonky generate board function
; returns a board with specified dimensions
(defun generate-board (rows columns)
  (let ((board nil))
    (dotimes (i rows board)
      (setf board (append board (list (copy-list (make-list columns :initial-element blank-tile))))))))

; representation of the board
; players are 'X' and 'O', black and white, respectively
; blank tile is '_'
(defparameter initial-board (generate-board rows-count columns-count))

; prints the board in a friendly format
(defun print-board (board)
  (dolist (x board) (print x)))

; check a specific row, column is free
; returns the player that occupies it or blank-tile 
(defun check-space-occupied (board row column)
  (nth column (car (nthcdr row board))))

; checks a column recusively if a piece can be placed here
(defun check (board row column)
  (cond
   ((< row 0) nil)
   ((not (equal (check-space-occupied board row column) blank-tile)) (check board (1- row) column))
   (t row)))

; returns the row in which the piece should be place
(defun valid-move (board column)
  (let ((current-row (- rows-count 1)))
    (if (not (or (< column 0)
                 (> column (- rows-count 1))))
        (check board current-row column)
      nil)))

; drops a piece into the board, returns nil if unable to
(defun drop-piece (board piece column)
  (let ((row (valid-move board column)))
    (if (not (null row))
        (setf (nth column (car (nthcdr row board))) piece)
      nil)))

; gets the current score for a particular player
; probably can be optimized a lot (scratch that, DEFINITELY can)
(defun get-score (board player)
  (let ((score 0))
    (dotimes (i columns-count score) ; iterate through each column
      (dotimes (j rows-count score) ; iterate through each row
        (progn ; evalaute all of these ifs
          (if (and (< i (- columns-count 1))
                   (and (equal (check-space-occupied board j i) player) ; check if current tile is equal
                        (equal (check-space-occupied board j (+ i 1)) player))) ; to tile next to it
              (setf score (+ score 2))) ; if so, score + 2
              
          (if (and (< j (- rows-count 1))
                   (and (equal (check-space-occupied board j i) player) ;check if the current tile is equal
                        (equal (check-space-occupied board (+ j 1) i) player))) ; to the tile below it
              (setf score (+ score 2))) ; if so, score + 2
          
          (if (and (> i 0)
                   (< j (- rows-count 1))
                   (and (equal (check-space-occupied board j i) player) ; check if the current tile is equal
                        (equal (check-space-occupied board (+ j 1) (- i 1)) player))) ; to the tile to the bottom left
              (setf score (+ score 1))) ; if so, score + 1
          
          (if (and (< i (- columns-count 1))
                   (< j (- rows-count 1))
                   (and (equal (check-space-occupied board j i) player) ; check if the current tile is equal
                        (equal (check-space-occupied board (+ j 1) (+ i 1)) player))) ; to the tile to the bottom right
              (setf score (+ score 1)))))))) ; if so, score + 1

; returns if a board is a terminal board
; simply checks the first row for a single blank-tile
(defun terminal? (board)
  (equal (length (member blank-tile (car board))) 1))

; returns a list of boards (crazy, I know. I need to learn object-oriented lisp!)
(defun generate-children (board piece)
  (let ((children (make-hash-table))) ; start with an empty list (well, and empty list would pass nil into dotimes and fail. so we use a placeholder to prevent that
    (dotimes (i columns-count children) ; repeat over all columns
      (let ((tmp-board (copy-tree board))) ; create a temporary board
        (if (not (null (drop-piece tmp-board piece i))) ; drop a piece
            (setf (gethash i children) tmp-board)))))) ; if it worked, add to children
                        
; the glorious minimax
; returns a list of (BEST-COLUMN BEST-VALUE)
(defun minimax (board depth maximizingPlayer)
    (cond
     ((or (zerop depth)
          (terminal? board))
      (list nil (get-score board player-black)))
   
     ((equal maximizingPlayer t)
      (let ((best-column nil) (best-value negative-infinity))
        (progn
          (maphash #'(lambda (key value) ; key is the column and value is the board
                       (let ((v (second (minimax value (- depth 1) nil))))
                         (setf best-value (max best-value v))
                         (if (equal best-value v)
                             (setf best-column key))))
                   (generate-children board player-black))
          (list best-column best-value))))
   
     (t ; minimizing player
      (let ((best-column nil) (best-value positive-infinity))
        (progn
          (maphash #'(lambda (key value) ; key is the column and value is the board
                       (let ((v (second (minimax value (- depth 1) t))))
                         (setf best-value (min best-value v))
                         (if (equal best-value v)
                             (setf best-column key))))
                   (generate-children board player-white))
          (list best-column best-value))))))

; receives user input to drop a piece on the board
(defun player-turn (board player)
  (format t "Enter a column to drop a piece (1 - ~A): " columns-count)
  (let ((input-column (read)))
    (if (equal (drop-piece board player (- input-column 1)) nil)
        (player-turn board player))))

; main game loop repeats until all moves are up
; then prints the winner
(defun game-loop (board)
  (let ((current-player player-white))
    (dotimes (current-move moves-count)
      (format t "~%Move: ~A/~A Player: ~A~%" (+ current-move 1) moves-count current-player)
      (format t "Black score: ~A White score: ~A" (get-score board player-black) (get-score board player-white))
      (print-board board)
      (if (equal current-player player-white)
          (progn
            (player-turn board current-player)
            (setf current-player player-black))
        (progn ; current player is player-black
          (format t "AI's turn~%")
          (drop-piece board player-black (first (minimax board num-plys t)))
          (setf current-player player-white))))
    (let ((final-score-black (get-score board player-black)) (final-score-white (get-score board player-white)))
      (format t "~%~%_____FINAL SCORES_____~%~%")
      (format t "Black: ~a ~%White: ~a~%~%" final-score-black final-score-white)
      (print-board board)
      (cond
       ((> final-score-black final-score-white) (format t "!!! BLACK WINS !!!~%"))
       ((< final-score-black final-score-white) (format t "!!! WHITE WINS !!!~%"))
       (t (format t "!!! TIE !!!~%"))))))
          
; start a game!
(defun start ()
  (princ "Number of plys: ")
  (setf num-plys (read))
  (game-loop (copy-tree initial-board))) ; pass a copy of the board so we don't wreck it
   
   
   
   
   