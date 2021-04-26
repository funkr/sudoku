(require 'cl-lib)
(require 'easymenu)

;; This has some compatibility things built in, like propertize...
(when (featurep 'xemacs)
  (require 'easy-mmode))

(defgroup sudoku nil
  "Sudoku - web-enabled puzzle game"
  :group  'games
  :prefix "sudoku-")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sudoku-level "easy"
  "*Level of difficulty for sudoku."
  :group 'sudoku
  :type '(radio (const "easy")
		(const "medium")
		(const "hard")
		(const "evil")))

;; This is just a compatibility thing from the old setup, when the
;; difficulty level was a symbol instead of a string
(when (symbolp sudoku-level)
  (setq sudoku-level (symbol-name sudoku-level)))

(defcustom sudoku-download "Preconfigured Puzzle"
  "*Should sudoku download puzzles from the web?"
  :type  '(radio
	   (const "Preconfigured Puzzled")
	   (const "Generated Puzzle")
	   (const "Download"))
  :group 'sudoku)

(defcustom sudoku-download-method "lynx"
  "*Method for downloading new puzzles."
  :group 'sudoku
  :type '(radio (const "native-url-lib")
		(const "lynx")
		(const "wget")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sudoku-mode 'nil)
(make-variable-buffer-local 'sudoku-mode)

(defvar sudoku-mode-hooks 'nil)
(make-variable-buffer-local 'sudoku-mode-hooks)

(defconst blank-cell "_")

(defconst sudoku-buffer-name "*sudoku*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic mode functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku ()
  "Used to start a new game and change to sudoku-mode"
  (interactive)
  (sudoku-new)
  (sudoku-mode))

(defun sudoku-mode ()
  "A mode for playing `sudoku' The key bindings for sudoku-mode
are: \\{sudoku-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map sudoku-mode-map)
  (setq major-mode 'sudoku-mode
        mode-name  "sudoku")	
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defvar sudoku-mode-hook nil)

(defvar sudoku-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-n" 'sudoku-new)

    (define-key map "\C-c\C-r" 'sudoku-show-row-singularities)
    (define-key map "\C-c\C-c" 'sudoku-show-column-singularities)
    (define-key map "\C-c\C-s" 'sudoku-show-square-singularities)
    (define-key map "\C-c\C-i" 'sudoku-insert-all-single-possibility)
    
    (define-key map "\M-<" 'sudoku-move-point-leftsquare)
    (define-key map "\M->" 'sudoku-move-point-rightsquare)
    (define-key map "\M-a" 'sudoku-move-point-beginsquare)
    (define-key map "\M-e" 'sudoku-move-point-endsquare)
    
    (define-key map [up] 'sudoku-move-point-up)
    (define-key map "\C-p" 'sudoku-move-point-up)
    (define-key map "k" 'sudoku-move-point-up)
    (define-key map [down] 'sudoku-move-point-down)
    (define-key map "\C-n" 'sudoku-move-point-down)
    (define-key map "j" 'sudoku-move-point-down)
    (define-key map [return] 'sudoku-move-point-down)
    (define-key map [left] 'sudoku-move-point-left)
    (define-key map "\C-b" 'sudoku-move-point-left)
    (define-key map "h" 'sudoku-move-point-left)
    (define-key map [right] 'sudoku-move-point-right)
    (define-key map "\C-f" 'sudoku-move-point-right)
    (define-key map "l" 'sudoku-move-point-right)
    (define-key map [tab] 'sudoku-move-point-right)

    (define-key map "\C-a" 'sudoku-move-point-leftmost)
    (define-key map "\C-e" 'sudoku-move-point-rightmost)
    (define-key map [prior] 'sudoku-move-point-upmost)
    (define-key map [next] 'sudoku-move-point-downmost)
    (define-key map [home] '(lambda () (interactive) 
			      (sudoku-move-point "upmost")
			      (sudoku-move-point-leftmost)))
    (define-key map [end] '(lambda () (interactive) 
			     (sudoku-move-point "downmost")
			     (sudoku-move-point-rightmost)))

    (define-key map "\C-d" 'sudoku-cell-erase)
    (define-key map "_" 'sudoku-cell-erase)
    (define-key map " " 'sudoku-cell-erase)
    (define-key map "0" 'sudoku-cell-erase)
    (define-key map [backspace] 'sudoku-cell-erase)

    (define-key map "\C-c\C-h" 'sudoku-hint)
    
    ;; Disabled in sudoku mode
    
    (define-key map "\C-v" 'sudoku-disabled-key)
    (define-key map "\M-v" 'sudoku-disabled-key)
    (define-key map [mouse-1] 'sudoku-disabled-key)
    (define-key map [down-mouse-1] 'sudoku-disabled-key)
    (define-key map [drag-mouse-1] 'sudoku-disabled-key)
    (define-key map [double-mouse-1] 'sudoku-disabled-key)
    
    (define-key map "\C-k" 'sudoku-disabled-key)
    (define-key map "\C-xh" 'sudoku-disabled-key)
    (define-key map "\M-h" 'sudoku-disabled-key)


    ;;I want to figure out how to make it only go to valid cells, but
    ;;for the time being...

    (define-key map "1" '(lambda () (interactive) (sudoku-change-point 1)))
    (define-key map "2" '(lambda () (interactive) (sudoku-change-point 2)))
    (define-key map "3" '(lambda () (interactive) (sudoku-change-point 3)))
    (define-key map "4" '(lambda () (interactive) (sudoku-change-point 4)))
    (define-key map "5" '(lambda () (interactive) (sudoku-change-point 5)))
    (define-key map "6" '(lambda () (interactive) (sudoku-change-point 6)))
    (define-key map "7" '(lambda () (interactive) (sudoku-change-point 7)))
    (define-key map "8" '(lambda () (interactive) (sudoku-change-point 8)))
    (define-key map "9" '(lambda () (interactive) (sudoku-change-point 9)))
    map)
  "Keymap for sudoku mode")

(easy-menu-add-item nil '("tools" "games") ["Sudoku" sudoku t])

(easy-menu-define sudoku-mode-menu sudoku-mode-map "sudoku menu."
  '("Sudoku"
    ["New game"               sudoku-new t]
    ["Reset game"            sudoku-restart t]
    ["Quit game"              sudoku-quit t]
    "---"
    ("Set level"
     ["Easy"  (setq sudoku-level "easy") 
      :style radio :selected (string= sudoku-level "easy")]

     ["Medium" (setq sudoku-level "medium") 
      :style radio :selected (string= sudoku-level "medium")]
     ["Hard"  (setq sudoku-level "hard") 
      :style radio :selected (string= sudoku-level "hard")]
     ["Evil" (setq sudoku-level "evil") 
      :style radio :selected (string= sudoku-level "evil")])
    ("Puzzle Source"
     ["Preconfigured Puzzle" (setq sudoku-download "Preconfigured Puzzle") 
      :style radio :selected (string= sudoku-download  "Preconfigured Puzzle")]
     ["Generated Puzzle"     (setq sudoku-download "Generated Puzzle")
      :style radio :selected (string= sudoku-download "Generated Puzzle")]
     ["Download"             (setq sudoku-download "Download")
      :style radio :selected (string= sudoku-download "Download")])
    ("Download Method"
     :active sudoku-download
     ["lynx"  (setq sudoku-download-method "lynx") 
      :style radio :selected (string= sudoku-download-method "lynx")]
     ["Native Url Library (cvs only)"  (setq sudoku-download-method "native-url-lib") 
      :style radio :selected (string= sudoku-download-method "native-url-lib")]
     ["wget"  (setq sudoku-download-method "wget") 
      :style radio :selected (string= sudoku-download-method "wget")])
    "---"
    ["Save Options" 
     (mapcar #'(lambda (var) (eval `(customize-save-variable (quote ,var) ,var)))
	     '(sudoku-level
	       sudoku-download
	       sudoku-download-method))]))


(defun sudoku-new ()
  "Sets the \"current-board\" variable, using the
  \"sudoku-create-board\" function, and then runs
  \"sudoku-initialize\", which does the rest."
  (interactive)
  (sudoku-create-new-puzzle)
  (sudoku-initialize))

(defun sudoku-initialize ()
  "Makes the board, based on the \"current board\" variable, and
  sets the buffer for read-only. Used by \"sudoku-new\"."
  (switch-to-buffer (get-buffer-create sudoku-buffer-name))
  (when buffer-read-only
    (setq buffer-read-only nil))
  (erase-buffer)
  (sudoku-print-current-board sudoku-onscreen-instructions)
  (setq cell-point-list (sudoku-get-cell-points))
  (sudoku-goto-cell '(0 0))
  (when (null buffer-read-only)
    (setq buffer-read-only t)))


(defun sudoku-create-board (level download)
  "Checks both the \"sudoku-download\" variable, and the
  \"sudoku-level\" variable. Uses these to either choose a random
  included board (if download is nil) or to download one from
  websudoku.com"
  (cond ((string= download  "Download")
	 (cond ((string= level 'easy)
		(sudoku-download-new-puzzle 1))
	       ((string= level 'medium)
		(sudoku-download-new-puzzle 2))
	       ((string= level 'hard)
		(sudoku-download-new-puzzle 3))
	       ((string= level 'evil)
		(sudoku-download-new-puzzle 4))))
	
	((string= download "Preconfigured Puzzle")
	 (let ((n (mod (random t) 50)))
	   (cond ((string= level 'easy)
		  (nth n easy-puzzles))
		 ((string= level 'medium)
		  (nth n medium-puzzles))
		 ((string= level 'hard)
		  (nth n hard-puzzles))
		 ((string= level 'evil)
		  (nth n evil-puzzles))		 
		 )))
	
	((string= download "Generated Puzzle")	 	   
	 (sudoku-create-new-board level))		 
		 ))

(defun sudoku-quit-immediately ()
  "Quit without a prompt. Designed to be used by other functions."
  (kill-buffer sudoku-buffer-name))

(defun sudoku-quit ()
  "Quit with confirmation."
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit sudoku? ")
    (sudoku-quit-immediately)))

(defun sudoku-restart ()
  "Return the board to its original state."
  (interactive)
  (if (null start-board)
      (message "You have to start before you can restart.")
      (sudoku-reset-board)
      (sudoku-initialize)))

;; Encapsulate the Board
(defun sudoku-get-current-board ()
  current-board)

(defun sudoku-create-new-puzzle ()
  (setq current-board (sudoku-create-board sudoku-level sudoku-download))
  (setq start-board current-board))

(defun sudoku-reset-board ()
  (setq current-board start-board))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic sudoku functions (can also be used for a solver)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-row (board n)
  "Returns the nth row of a board."
  (nth n board))

(defun sudoku-column (board n)
  "Returns the nth column of a board"
  (let ((column nil))
    (reverse (dolist (row board column)
	       (setq column (cons (nth n row) column))))))

(defun sudoku-subsquare (board n)
  "Returns the nth subsquare of a board (as a flat list)"
  (let ((rows (list (sudoku-row board (* 3 (/ n 3)))
		    (sudoku-row board (+ (* 3 (/ n 3)) 1))
		    (sudoku-row board (+ (* 3 (/ n 3)) 2))))
	(col-start-num (* (mod n 3) 3))
	(subsquare nil)
	(subsquare-flat nil))
    (dolist (row rows)
      (setq subsquare 
	    (cons 
	     (butlast (nthcdr col-start-num row) (- 6 col-start-num)) 
	     subsquare)))
    (dolist (row subsquare)
      (dolist (elt (reverse row))
	(setq subsquare-flat (cons elt subsquare-flat))))
    subsquare-flat))

(defun sudoku-cell (board x y)
  "Returns the (x,y) cell of a board"
  (nth x (sudoku-row board y)))

(defun sudoku-cell-elts (board x y)
  "Returns the row, column, and subsquare containing cell (x,y)"
  (let ((subsquare-num 
	 (+ (* (/ y 3) 3) (/ x 3))))
    (list (sudoku-row board y)
	  (sudoku-column board x)
	  (sudoku-subsquare board subsquare-num))))

(defun sudoku-cell-elts-flat (board x y)
  "Returns the row, column and subsquare containing cell (x,y) in
   a flat list"
  (let ((result nil))
    (dolist (elt (sudoku-cell-elts board x y))
      (dolist (atom elt)
	(setq result (cons atom result))))
    result))

(defun sudoku-cell-possibles (board x y)
  "Returns a list with the possible values for a cell (i.e., those not
   already in the row, column, and subsquare containing it. 
   If its already set return the set value."
  (let ((possibilities nil))
    (if (/= (sudoku-cell start-board x y) 0)
	(cons (sudoku-cell board x y) possibilities)
      (progn
	(dotimes (i 9 possibilities)
	  (let ((n (1+ i)))
	    (when (not (member n (remove 0 (sudoku-cell-elts-flat board x y))))
	      (setq possibilities (cons n possibilities)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for displaying the board on-screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sudoku-onscreen-instructions
  "
   Commands:

   Arrow Keys:\t\tMove around the board
   1-9:\t\t\tEnter a numerical value into the current cell
   \[SPC\]:\t\t\tRemove the numerical value from the current cell
   C-c C-i:\t\tInsert single possibilities
   C-c C-c:\t\tShow single possible value per column
   C-c C-r:\t\tShow single possible value per row
   C-c C-s:\t\tShow single possible value per square
   C-c C-n:\t\tStart a new puzzle
   M-<: \t\t\tMove next square
   M->: \t\t\tMove previous square
   M-x: sudoku-solve-it\tTries to solve the puzzle
  ")

(defun sudoku-row-output (row row-num separator)
  "This takes care of most of the outputting work. It makes a
  string of with a separator, and then three numbers, another
  separator, and so on. It also replaces all zeros with the
  `blank-cell' constant. So, if separator and black-cell are
  \"|\" and \"_\", we would get:
  
  (1 2 0 3 4 0 5 6 0) -> | 1 2 _ | 3 4 _ | 5 6 _ |"

  (let ((output-string nil))
    (setq output-string separator)
    (dotimes (i 3)
      (dotimes (j 3)
	(let ((value (nth (+ (* 3 i) j) row)))
	  (cond ((= value 0)
		 ;; If it's equal to 0, we use the blank-cell
		 ;; character.
		 (setq output-string (concat output-string 
					     (format " %s " blank-cell))))
		((/= (sudoku-cell start-board (+ (* 3 i) j) row-num) 0)
		 ;; If it's one of the original numbers, we bold it.
		 (let ((string (propertize (int-to-string value) 'face 'bold)))
		   (setq output-string (concat output-string 
					       (format " %s " string )))))
		(t
		 ;; If it's any other number, we just input.
		 (setq output-string (concat output-string 
					     (format " %s " value )))))))
      (setq output-string (concat output-string separator)))
    output-string))

(defun sudoku-board-output (board)
  "Outputs the visible board. Uses sudoku-row-output to do most
   of the work."
  (let ((corner "+")
	(horiz "---")
	(vert "|")
	(top-piece nil)
	(output-string nil))
    (dotimes (i 3)
      (setq top-piece (concat top-piece corner))
      (dotimes (j 3)
	(setq top-piece (concat top-piece horiz))))
    (setq top-piece (concat top-piece corner))
    (setq output-string (concat top-piece "\n"))
    (dotimes (i 3)
      (dotimes (j 3)
	(let ((row-num (+ (* 3 i) j)))
	  (setq output-string (concat output-string
				      (sudoku-row-output 
				       (sudoku-row board row-num) row-num vert)
				      "\n"))))
      (setq output-string (concat output-string top-piece "\n")))
    output-string))

(defun sudoku-print-current-board (message)
  "Prints the board and the message beneath the board
  together. Usually the message will be the moves. The only other
  message right now is the \"You Win!\" message."
  (save-excursion
    (goto-char (point-min))
    (insert (sudoku-board-output (sudoku-get-current-board)))
    (insert message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions relating to changing cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-change-cell (board x y input)
  "Changes a specific cell"
  (let ((newboard (copy-tree board)))
    ((lambda (lst index val) 
       (setcar (last lst (- (length lst) index)) val))
     (sudoku-row newboard y) x input)
    newboard))

(defun sudoku-test-and-change (x y input)
  "Tests whether a change is valid. If it is, enters the cell and
   redraws the board."
  (cond ((member input (sudoku-cell-possibles (sudoku-get-current-board) x y))	 
	 (setq current-board (sudoku-change-cell (sudoku-get-current-board) x y input))
	 (erase-buffer)
	 (sudoku-print-current-board sudoku-onscreen-instructions))
	(t
	 (if (/= (sudoku-cell start-board x y) 0)
	     (message "Original value. Can't change.")
	   (message "Not a valid move")))))


(defun sudoku-get-cell-points ()
  "This reads a printed board and returns the point of each
   number, counting from 0. So, for a 9x9 board, we should get 81
   pairs. We can then use this to turn each number into a cell
   [e.g. 0 -> (0 0), 1 -> (1 0), 9 -> (0 1)] so we can match up
   each cell with a point on the screen. This function is used once
   at initialization to make the cell-point-list, and then that is
   consulted, so we don't have to keep running this over and over
   again."
  (save-excursion
    (goto-char (point-min))
    (let ((counter 0)
	  (point-list nil))
      (dotimes (i (count-lines (point-min) (point-max)))
	(beginning-of-line)
	(while (not (eolp))
	  (cond ((or (looking-at "[0-9]") (looking-at blank-cell))
		 (setq point-list (cons (list (point) counter) point-list))
		 (setq counter (1+ counter))))
	  (forward-char 1))
	(forward-line 1))
      (reverse point-list))))

(defun sudoku-number-to-cell (num)
  "This takes the numbers from 0 to 80 and turns them into
   coords.\n TODO: Abstract this using (length board) to make this
   not be 9-dependent"
  (let ((x (mod num 9))
	(y (/ num 9)))
    (list x y)))

(defun sudoku-cell-to-number (coords)
  "This turns any cell into a number from 0 to 80."
  (let ((x (car coords))
	(y (car (cdr coords))))
    (+ (* 9 y) x)))

(defun sudoku-get-cell-from-point (num)
  "This uses the \"cell-point-list\" made at initialization to
  return a cell for a point on the screen."
  (let ((result nil))
    (dolist (i cell-point-list)
      (when (= num (car i))
	(setq result (car (cdr i)))))
    (if (null result)
	nil
      (sudoku-number-to-cell result))))

(defun sudoku-get-point-from-cell (coords)
  "Returns a point on the screen for a given cell."
  (let ((result nil))
    (dolist (i cell-point-list)
      (when (= (sudoku-cell-to-number coords) (car (cdr i)))
	(setq result (car i))))
    result))

(defun sudoku-finished? ()
  (= 0 (sudoku-remaining-cells)))

(defun sudoku-not-finished? ()
  (not (sudoku-finished?)))

(defun sudoku-change-point-internal (input)
  (let ((cell (sudoku-get-cell-from-point (point))))
    (save-excursion
      (when buffer-read-only
	(setq buffer-read-only nil))
      (when (not (null cell))
	(let* ((cell (sudoku-get-cell-from-point (point)))
	       (x (car cell))
	       (y (car (cdr cell))))
	  (sudoku-test-and-change  x y input))))
    (sudoku-goto-cell cell))
  (when (null buffer-read-only)
    (setq buffer-read-only t))) 

(defun sudoku-change-point (input)
  "Changes the value at a point, after running tests to ensure
  that the change is a valid one. Checks to see how many cells
  are remaining. If none are, runs the
  sudoku-completion-routine (i.e., \"You Win!\")."
  (sudoku-change-point-internal input)
  (when (sudoku-finished?)
    (sudoku-completion-routine))) 

(defun sudoku-cell-erase ()
  (interactive)
  (let* ((cell (sudoku-get-cell-from-point (point)))
	 (x (car cell))
	 (y (car (cdr cell))))
    (if (= (sudoku-cell start-board x y) 0)
	(setq current-board (sudoku-change-cell current-board x y 0))
      (message "Original value. Can't erase."))
    (setq buffer-read-only nil)
    (erase-buffer)
    (sudoku-print-current-board sudoku-onscreen-instructions)
    (sudoku-goto-cell cell)
    (setq buffer-read-only t)))

(defun sudoku-remaining-cells ()
  "Tests to see how many cells are remaining"
  (let ((remaining 0))
    (dolist (row (sudoku-get-current-board) remaining)
      (setq remaining (+ remaining (cl-count 0 row))))))

(defun sudoku-is-start-value? ()
  (let* ((cell (sudoku-get-cell-from-point (point)))
	 (cell-x (car cell))
	 (cell-y (car (cdr cell))))
    (/= (sudoku-cell start-board cell-x cell-y) 0)))

(defun sudoku-has-value-set? ()
  (let* ((cell (sudoku-get-cell-from-point (point)))
	 (cell-x (car cell))
	 (cell-y (car (cdr cell))))
    (/= (sudoku-cell current-board cell-x cell-y) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here starts the solver section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-completion-routine ()
  "Runs when there are no cells remaining. Gives a message of
   victory, and then asks if you want to play again."
  (let ((victory-message "\n\nYOU WIN!"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (sudoku-print-current-board victory-message)
    (message "You won!")
    (setq buffer-read-only t))
  (if (y-or-n-p "Start another puzzle? ")
      (sudoku-new)
    (sudoku-quit-immediately)))

(defun sudoku-hint ()
  "Shows possible values in the message area"
  (interactive)
  (let* ((cell (sudoku-get-cell-from-point (point)))
	 (cell-x (car cell))
	 (cell-y (car (cdr cell))))
    (cond ((sudoku-is-start-value?) (message "Original value. No other possibilities."))
	  ((sudoku-has-value-set?) (message "Already a value set."))
	  (t
	   (let ((string ""))
	     (dolist (n (cdr (sudoku-cell-possibles (sudoku-get-current-board) cell-x cell-y)))
	       (setq string (concat (int-to-string n) "," string)))
	     (setq string (concat string (int-to-string (car (sudoku-cell-possibles (sudoku-get-current-board) cell-x cell-y)))))
	     (message "Possible values: %s" string))))))

(defun sudoku-insert-single-possibility ()
  "Insert the value if just on possibility remains"
  (interactive)
  (let* ((cell (sudoku-get-cell-from-point (point)))
	 (cell-x (car cell))
	 (cell-y (car (cdr cell)))
	 (possibles (sudoku-cell-possibles (sudoku-get-current-board) cell-x cell-y)))
    (cond ((sudoku-is-start-value?) t)
	  ((sudoku-has-value-set?) t)
	  ((= (length possibles) 1 )
	   (sudoku-change-point-internal (car possibles))))))

(defun sudoku-insert-all-row-possibililities ()
  "Find singular values in the current row and insert it"
  (interactive)
  (sudoku-move-point "upmost")
  (dotimes (i 9)
    (sudoku-insert-row-singularity)
    (sudoku-move-point "down")))

(defun sudoku-insert-all-column-possibililities ()
  "Find singular values in the current column and insert it"
  (interactive)
  (sudoku-move-point "leftmost")
  (dotimes (i 9)
    (sudoku-insert-column-singularity)
    (sudoku-move-point "right")))

(defun sudoku-insert-all-square-possibililities ()
  "Find singular values in the current square and insert it"
  (interactive)
  (sudoku-move-point "upmost")
  (sudoku-move-point "leftmost")
  (dotimes (i 9)
    (sudoku-insert-square-singularity)
    (sudoku-move-point "nextsquare")))

(defun sudoku-insert-all-single-possibility ()
  "This routine goes over the whole board and fill in all single values."
  (interactive)
  (sudoku-move-point "upmost")
  (sudoku-move-point "leftmost")
  (dotimes (i 9)
    (dotimes (j 9)
      (sudoku-insert-single-possibility)
      (sudoku-move-point "right"))
    (sudoku-insert-single-possibility)
    (sudoku-move-point "leftmost")
    (sudoku-move-point "down"))
  (sudoku-move-point "rightmost"))

(defun sudoku-finished-guard-clause (fct-to-call)
  (if (sudoku-not-finished?)
      (funcall fct-to-call)))

(defun sudoku-solve-it ()
  "This routine applies every main strategy on the board, followed 
   by finding all single values on the board"
  (interactive)
  (let ((old-remaining 100))
    (while (and (sudoku-not-finished?)
		(< (sudoku-remaining-cells) old-remaining))
      (setq old-remaining (sudoku-remaining-cells))
      (sudoku-finished-guard-clause 'sudoku-insert-all-single-possibility)
      (sudoku-finished-guard-clause 'sudoku-insert-all-column-possibililities)
      (sudoku-finished-guard-clause 'sudoku-insert-all-row-possibililities)
      (sudoku-finished-guard-clause 'sudoku-insert-all-square-possibililities))
    (if (sudoku-finished?)
	(sudoku-completion-routine)
      (message "Remains:%d" (sudoku-remaining-cells)))))

(defun sudoku-count-occurrences (occ)
  "Count each value in the occurences. 1 at index 1, 2 at index 2, ..."
  (let ((result [0 0 0 0 0 0 0 0 0 0]))
    (fillarray result 0)
    (while occ
      (let ((idx (car occ)))
	(aset result idx (+ 1 (aref result idx))))
      (setq occ (cdr occ)))
    result))

(defun sudoku-get-singularities (number-count-arr)
  "It takes an array with the counted occurences per entity (index zero is not used) 
   and searches numbers with just one occurrence"
  (let ((result)
	(c 0))
    (dotimes (i (length number-count-arr))
      (if (= 1 (aref number-count-arr i)) (setq result (append result (list c))))
      (setq c (1+ c)))
    result))

(defun sudoku-message-singularities (singularities)
  "Message routine for showing the singularities"
  (if (> (length singularities) 0)
      (message "Singularities:%s" (mapconcat 'int-to-string singularities "," ))
    (message "No singularities!")))

(defun sudoku-collect-hints ()
  "This routine checks if on the current point exists possibles. 
   Filters out already set values and given values"
  (let* ((cell (sudoku-get-cell-from-point (point)))
	 (cell-x (car cell))
	 (cell-y (car (cdr cell)))
	 (possibles (sudoku-cell-possibles (sudoku-get-current-board) cell-x cell-y)))
    (cond ((sudoku-is-start-value?) nil)
	  ((sudoku-has-value-set?) nil)
	  (t possibles))))

(defun sudoku-collect-row-singularities  ()
  "Collect singularities in the current row"
  (save-excursion
    (sudoku-move-point "leftmost")
    (let (result)
      (dotimes (i 9)
	(setq result (append result (sudoku-collect-hints)))
	(sudoku-move-point "right"))
      (sudoku-get-singularities (sudoku-count-occurrences result)))))

(defun sudoku-collect-column-singularities ()
  "Shows singularities in the current column"
  (interactive)
  (save-excursion
    (sudoku-move-point "upmost")
    (let (result)
      (dotimes (i 9)
 	(setq result (append result (sudoku-collect-hints)))
 	(sudoku-move-point "down"))
      (sudoku-get-singularities (sudoku-count-occurrences result)))))

(defun sudoku-collect-square-singularities ()
  "Collect singularities in the current square (9 fields)"
  (interactive)
  (save-excursion
    (sudoku-move-point "beginsquare")
    (let (result)
      (dotimes (i 9)
	(setq result (append result (sudoku-collect-hints)))
	(sudoku-move-point "rightsquare"))
      (sudoku-get-singularities (sudoku-count-occurrences result)))))

(defun sudoku-show-row-singularities ()
  (interactive)
  (sudoku-message-singularities (sudoku-collect-row-singularities)))

(defun sudoku-show-column-singularities ()
  (interactive)
  (sudoku-message-singularities (sudoku-collect-column-singularities)))

(defun sudoku-show-square-singularities ()
  (interactive)
  (sudoku-message-singularities (sudoku-collect-square-singularities)))

(defun sudoku-insert-row-singularity ()
  "Retrieves the list with singularities of a row and put it into the board"
  (interactive)
  (progn 
    (sudoku-move-point "leftmost")
    (dotimes (i 9)
      (dolist (s (sudoku-collect-row-singularities))
	(if (member s (sudoku-collect-hints))
	    (sudoku-change-point-internal s)))
      (sudoku-move-point "right"))))

(defun sudoku-insert-column-singularity ()
  "Retrieves the list with singularities of the column and put it into the board"
  (interactive)
  (progn
    (sudoku-move-point "upmost")
    (dotimes (i 9)
      (dolist (s  (sudoku-collect-column-singularities))
	(if (member s (sudoku-collect-hints))
	    (sudoku-change-point-internal s)))
      (sudoku-move-point "down"))))

(defun sudoku-insert-square-singularity ()
  "Retrieves the list with singularities of the square and put it into the board"
  (interactive)
  (progn
    (sudoku-move-point "beginsquare")
    (dotimes (i 9)
      (dolist (s (sudoku-collect-column-singularities))
	(if (member s (sudoku-collect-hints))
	    (sudoku-change-point-internal s)))
      (sudoku-move-point "rightsquare"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Motion functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-goto-cell (coords)
  "Moves to a given pair of coordinates."
  (goto-char (sudoku-get-point-from-cell coords)))

(defun sudoku-move-point (direction)
  "Moves in one of four directions: either left, right, up, or
down. Uses sudoku-goto-cell, but doesn't let you go outside the
bounds of the board."
  (let* ((cell (sudoku-get-cell-from-point (point)))
	 (x (car cell))
	 (y (car (cdr cell)))
	 (nx 0))
    (cond ((string= direction "left")
	   (when (> x 0)
	     (setq x (- x 1))))
	  ((string= direction "leftmost")
	   (setq x 0))
	  ((string= direction "right")
	   (when (< x 8)
	     (setq x (+ x 1))))
	  ((string= direction "rightmost")
	   (setq x 8))
	  ((string= direction "up")
	   (when (> y 0)
	     (setq y (- y 1))))
	  ((string= direction "upmost")
	   (setq y 0))
	  ((string= direction "down")
	   (when (< y 8)
	     (setq y (+ y 1))))
	  ((string= direction "downmost")
	   (setq y 8))
	  ((string= direction "leftsquare")
	   (when (or (/= 0 (% y 3)) (/= 0 (% x 3)))
	     (setq nx (% (+ x 2) 3))
	     (setq x (+ nx (* 3 (/ x 3))))
	     (setq y (- y (/ (+ nx 1) 3)))))
	  ((string= direction "rightsquare")
	   (when (or (/= 2 (% y 3)) (/= 2 (% x 3)))
	     (setq nx (+ 1 (% x 3)))
	     (setq x (+ (% nx 3) (* 3 (/ x 3))))
	     (setq y (+ y (/ nx 3)))))
	  ((string= direction "beginsquare")
	   (setq x (* (/ x 3) 3))
	   (setq y (* (/ y 3) 3)))
	  ((string= direction "endsquare")
	   (setq x (+ 2(* (/ x 3) 3)))
	   (setq y (+ 2(* (/ y 3) 3))))
	  ((string= direction "nextsquare")
	   (when (or (< (+ x 3) 11)  (< (+ y 3) 11))
	     (setq nx (+ 3 x))
	     (setq x (% nx 9 ))
	     (setq y (+ y (/ nx 9))))))
    (sudoku-goto-cell (list x y))))

(defun sudoku-move-point-left ()
  "Moves the point one cell left."
  (interactive)
  (sudoku-move-point "left")
  (sudoku-hint))

(defun sudoku-move-point-leftmost ()
  "Moves the point to the leftmost cell."
  (interactive)
  (sudoku-move-point "leftmost")
  (sudoku-hint))

(defun sudoku-move-point-right ()
  "Moves the point one cell right."
  (interactive)
  (sudoku-move-point "right")
  (sudoku-hint))

(defun sudoku-move-point-rightmost ()
  "Moves the point to the rightmost cell."
  (interactive)
  (sudoku-move-point "rightmost")
  (sudoku-hint))

(defun sudoku-move-point-up ()
  "Moves the point one cell up."
  (interactive)
  (sudoku-move-point "up")
  (sudoku-hint))

(defun sudoku-move-point-upmost ()
  "Moves the point to the upmost cell."
  (interactive)
  (sudoku-move-point "upmost")
  (sudoku-hint))

(defun sudoku-move-point-down ()
  "Moves the point one cell down."
  (interactive)
  (sudoku-move-point "down")
  (sudoku-hint))

(defun sudoku-move-point-downmost ()
  "Moves the point to the downmost cell."
  (interactive)
  (sudoku-move-point "downmost")
  (sudoku-hint))

(defun sudoku-move-point-beginsquare ()
  "Moves the point to the begin of the square"
  (interactive)
  (sudoku-move-point "beginsquare")
  (sudoku-hint))

(defun sudoku-move-point-endsquare ()
  "Moves the point to the end of the square"
  (interactive)
  (sudoku-move-point "endsquare")
  (sudoku-hint))

(defun sudoku-move-point-leftsquare ()
  "Moves the point to the left within the square"
  (interactive)
  (sudoku-move-point "leftsquare")
  (sudoku-hint))

(defun sudoku-move-point-rightsquare ()
  "Moves the point to the right within the square"
  (interactive)
  (sudoku-move-point "rightsquare")
  (sudoku-hint))

(defun sudoku-move-point-nextsquare ()
  "Moves the point to the next square"
  (interactive)
  (sudoku-move-point "nextsquare")
  (sudoku-hint))

(defun sudoku-disabled-key ()
  (interactive)
  (message "Disabled in Sudoku mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;For downloading new puzzles (requires lynx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-download-new-puzzle (level)
  "Uses sudoku-get-board and the parsing functions to return a
  new board from the web. The levels can be either \"easy\",
  \"medium\", \"hard\", or \"evil\"."
  (save-excursion
    (let ((new-board nil)
	  (source (concat "http://play.websudoku.com/?level=" (int-to-string level))))
      (cond ((string= sudoku-download-method "native-url-lib")
	     (get-board-native source))	    
	    ((string= sudoku-download-method "lynx")
	     (get-board-lynx source))
	    ((string= sudoku-download-method "wget")
	     (get-board-wget source))))))

(defun get-board-lynx (source)
  "Downloads a websudoku html file into a temp buffer using lynx
   and turns it into a list. Used by sudoku-download-new-puzzle."
  (with-temp-buffer
    (call-process "lynx" nil t nil "--source" source)
    (sudoku-get-new-puzzle)))

(defun get-board-wget (source)
  "Downloads a websudoku html file into a temp buffer using wget
   and turns it into a list. Used by sudoku-download-new-puzzle."
  (with-temp-buffer
    (call-process "wget" nil t nil "-q" "-O" "-" source)
    (sudoku-get-new-puzzle)))

(defun get-board-native (source)
  "Downloads a websudoku html file into a temp buffer using the
   native emacs url library (emacs >= 22), or downloaded from cvs
   and turns it into a list. Used by sudoku-download-new-puzzle."
  (unless (featurep 'url)
    (require 'url))
  (with-current-buffer
    (url-retrieve-synchronously source)
    (sudoku-get-new-puzzle)))

(defun sudoku-get-new-puzzle ()
  (let ((puzzle (sudoku-clean-up (sudoku-parse-buffer))))
    (if (= 2 (length puzzle))
	(sudoku-make-board-list (sudoku-mask-puzzle (sudoku-string-to-int-list puzzle)))
      (message "Boo! Cannot deal with the page!"))))

(defun sudoku-make-board-list (puzzle-list)
  (let ((res nil))
    (dotimes (i 9 res)
      (setq res (cons (seq-take puzzle-list 9) res))
      (setq puzzle-list (seq-drop puzzle-list 9)))))

(defun sudoku-string-to-int-list (puzzle)
  (let ((mask (nth 0 puzzle))
	(p (nth 1 puzzle))
	(p-res nil)
      	(mask-res nil))
    (dotimes (i 81)
      (setq p-res (cons (string-to-number (substring p i (1+ i))) p-res))
      (setq mask-res (cons (string-to-number (substring mask i (1+ i))) mask-res )))
    (list p-res mask-res)))

(defun sudoku-mask-puzzle (puzzle)
  (let ((p (nth 0 puzzle))
	(mask (nth 1 puzzle))
	(res nil))
    (dotimes (i 81 res)
      (setq res (cons (if (= 0 (pop mask)) (pop p) (progn (pop p) 0))
		      res ))))) 

(defun sudoku-clean-up (line-list)
  (let ((res nil))
    (dolist (l line-list res)
      (when (string-match "[0-9]+" l)
	(let ((match (match-string 0 l)))
	  (if (= 81 (length match))
	      (setq res (cons match res))))))))

(defun sudoku-parse-buffer ()
  "Assumes you are in another buffer, into which the websudoku
   html has been downloaded. Split out because some of the routines
   can use `with-temp-buffer' and others seem to require a
   `set-buffer'. Used by the different get-board-* functions."
  (sudoku-grab-puzzle)
  (goto-char (point-min))
  (let ((res nil))
    (dotimes (i (count-lines (point-min) (point-max)) res)
      (setq res (cons (thing-at-point 'line t) res))
      (beginning-of-line)
      (kill-line 1))
    (list (nth 2 res) (nth 1 res))))

(defun sudoku-grab-puzzle ()
  "Cuts everything out but the value containing the puzzle. Used by
   sudoku-download-new-puzzle."
  (save-excursion
    (goto-char (point-min))
    (delete-region (point-min)
		   (search-forward "<INPUT NAME=cheat ID=\"cheat\" TYPE=hidden VALUE="))
    (delete-region (search-forward "<INPUT NAME=options TYPE=hidden VALUE=") (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to generate own puzzles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst sudoku-symbols '(1 2 3 4 5 6 7 8 9))

(defun sudoku-shuffle (sequence)
  (cl-loop for i from (length sequence) downto 2
        do (cl-rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun sudoku-generate-mask (level)
  (let ((zeros (cond ((string= level 'easy)
		  (- 81 40))
		 ((string= level 'medium)
		  (- 81 32)) 
		 ((string= level 'hard)
		  (- 81 25))
		 ((string= level 'evil)
		  (- 81 20))))
	      (mask nil))
    (dotimes (i zeros)
      (setq mask (cons 0 mask)))
    (message "%d" (- 81 (length mask)))
    (dotimes (i (- 81 (length mask)))
      (setq mask (cons 1 mask)))
    (sudoku-shuffle mask)))

(defun sudoku-create-new-board (difficulty)
  (let ((puzzle (sudoku-generate-board (sudoku-create-empty-board) 0))
	(mask (sudoku-generate-mask difficulty))
	(x 0)
	(y 0))
    (while (< x 9)
      (while (< y 9)
	(unless (/= (nth (+ (* 9 x) y) mask) 0)
	    (setq puzzle (sudoku-change-cell puzzle x y 0)))
	(setq y (1+ y)))
      (setq y 1)
      (setq x (1+ x)))
    puzzle))

(defun sudoku-generate-board (board fnr)
  (let* ((coords (sudoku-number-to-cell fnr))
	(x (car coords))
	(y (car (cdr coords)))
	(hints  (sudoku-shuffle (sudoku-raw-cell-possibles board x y)))
	(valid-board nil))    
    (if (= 72 fnr)
	board
      (progn
	(while (and (< 0 (length hints)) (not valid-board))
	  (setq new-board (sudoku-change-cell board x y (pop hints)))
	  (if (sudoku-board-valid new-board)		
	      (setq valid-board (sudoku-generate-board new-board (1+ fnr)))
	    nil))
	valid-board))))
 
(defun sudoku-create-empty-board ()
  (let ((new-board (list (sudoku-shuffle sudoku-symbols))))
    (while (> 9 (length new-board))
	  (setq new-board (cons '(0 0 0 0 0 0 0 0 0) new-board)))
    new-board))

(defun sudoku-board-valid (board)
  (let ((x 0)
	(y 0)
	(res t))
  (while (and res (< x 9))
    (while (and res (< y 9))      
      (unless (or (/= (sudoku-cell board x y) 0) (sudoku-raw-cell-possibles board x y))
	  (setq res nil))
      (setq y (1+ y)))
    (setq y 0)
    (setq x (1+ x)))
  res))

(defun sudoku-raw-cell-possibles (board x y)
 (let ((possibilities nil))
    (if (/= (sudoku-cell board x y) 0)
	(cons (sudoku-cell board x y) possibilities)
      (progn
	(dotimes (i 9 possibilities)
	  (let ((n (1+ i)))
	    (when (not (member n (remove 0 (sudoku-cell-elts-flat board x y))))
	      (setq possibilities (cons n possibilities)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Puzzles. About 200.
;;50 each of easy, medium, hard, and evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-puzzles '(((0 0 0 0 0 6 0 8 0) (0 1 0 0 2 0 6 0 0) (8 0 0 0 0 1 0 0 2) (3 0 0 0 0 0 0 1 7) (0 4 0 3 0 5 0 2 0) (7 6 0 0 0 0 0 0 8) (4 0 0 7 0 0 0 0 5) (0 0 2 0 4 0 0 6 0) (0 3 0 5 0 0 0 0 0))
		     ((0 1 0 0 4 0 3 0 2) (0 0 9 0 3 0 0 0 0) (7 0 0 0 0 0 0 8 0) (0 0 7 0 2 1 0 0 0) (9 0 0 0 8 0 0 0 6) (0 0 0 3 5 0 1 0 0) (0 5 0 0 0 0 0 0 7) (0 0 0 0 7 0 6 0 0) (8 0 4 0 6 0 0 3 0))
		     ((0 0 0 7 0 0 3 0 0) (0 5 0 0 0 4 2 0 0) (8 0 0 2 0 5 0 0 7) (0 0 4 0 0 0 7 0 8) (3 0 0 0 0 0 0 0 5) (6 0 8 0 0 0 4 0 0) (4 0 0 6 0 7 0 0 1) (0 0 2 8 0 0 0 7 0) (0 0 1 0 0 2 0 0 0))
		     ((0 0 4 0 0 0 0 5 8) (0 0 0 5 0 0 0 0 0) (5 0 0 8 0 3 9 6 0) (0 0 0 0 7 0 0 2 0) (0 8 2 0 0 0 5 1 0) (0 9 0 0 6 0 0 0 0) (0 5 9 7 0 6 0 0 4) (0 0 0 0 0 1 0 0 0) (8 2 0 0 0 0 7 0 0))
		     ((5 0 0 0 0 0 0 0 0) (0 0 0 8 0 0 7 0 5) (9 0 0 1 0 0 3 8 4) (0 0 0 9 1 0 0 7 0) (0 0 3 0 0 0 4 0 0) (0 7 0 0 6 2 0 0 0) (3 5 6 0 0 1 0 0 2) (1 0 2 0 0 6 0 0 0) (0 0 0 0 0 0 0 0 1))
		     ((0 2 3 1 9 0 0 4 0) (0 0 0 4 0 0 0 0 3) (0 0 0 6 0 0 0 1 0) (8 0 0 0 0 0 7 0 0) (0 0 6 0 0 0 1 0 0) (0 0 2 0 0 0 0 0 5) (0 5 0 0 0 8 0 0 0) (6 0 0 0 0 4 0 0 0) (0 7 0 0 3 5 6 9 0))
		     ((9 0 0 0 0 0 0 0 0) (8 0 0 7 0 0 1 5 2) (0 0 0 1 0 0 0 4 9) (0 0 0 8 7 0 4 0 0) (0 5 0 0 0 0 0 2 0) (0 0 4 0 6 3 0 0 0) (7 3 0 0 0 6 0 0 0) (5 6 9 0 0 7 0 0 3) (0 0 0 0 0 0 0 0 7))
		     ((4 0 0 0 0 6 0 9 0) (0 0 0 3 0 0 1 0 0) (1 6 0 0 8 0 2 0 0) (0 0 8 0 0 1 0 0 0) (0 2 0 8 0 5 0 4 0) (0 0 0 4 0 0 8 0 0) (0 0 3 0 4 0 0 2 5) (0 0 7 0 0 9 0 0 0) (0 5 0 6 0 0 0 0 3))
		     ((0 6 0 1 7 0 0 0 0) (0 9 0 0 0 0 7 0 0) (2 0 0 0 6 0 0 0 1) (0 8 0 0 0 0 0 0 0) (7 5 3 0 1 0 6 8 4) (0 0 0 0 0 0 0 9 0) (9 0 0 0 8 0 0 0 3) (0 0 4 0 0 0 0 2 0) (0 0 0 0 9 6 0 4 0))
		     ((0 8 5 0 6 0 0 0 0) (0 6 0 0 9 2 0 0 1) (9 0 0 8 0 0 0 0 0) (3 0 0 0 0 0 9 1 0) (0 0 8 0 0 0 7 0 0) (0 7 6 0 0 0 0 0 3) (0 0 0 0 0 8 0 0 5) (5 0 0 2 3 0 0 6 0) (0 0 0 0 7 0 2 3 0))
		     ((0 0 0 0 7 0 5 0 0) (5 0 0 9 0 0 0 3 0) (0 0 8 0 0 4 0 0 9) (0 1 2 0 0 0 7 0 0) (0 7 0 0 2 0 0 1 0) (0 0 5 0 0 0 4 2 0) (8 0 0 6 0 0 1 0 0) (0 4 0 0 0 2 0 0 3) (0 0 3 0 1 0 0 0 0))
		     ((6 0 0 0 0 0 0 0 0) (0 9 5 8 6 2 0 0 0) (0 3 7 4 0 0 0 0 0) (0 6 0 0 0 0 4 0 3) (0 0 0 7 0 4 0 0 0) (2 0 1 0 0 0 0 8 0) (0 0 0 0 0 7 6 2 0) (0 0 0 2 8 5 9 7 0) (0 0 0 0 0 0 0 0 1))
		     ((0 0 3 0 8 1 0 0 0) (0 5 6 0 0 9 0 0 0) (2 0 0 6 0 0 0 9 0) (7 0 0 0 0 0 0 3 0) (0 6 8 0 0 0 5 1 0) (0 3 0 0 0 0 0 0 7) (0 2 0 0 0 6 0 0 1) (0 0 0 8 0 0 7 5 0) (0 0 0 5 7 0 2 0 0))
		     ((0 0 8 0 0 6 0 0 2) (0 9 0 0 0 4 0 0 0) (0 4 0 0 5 0 0 0 0) (0 0 3 0 0 5 0 2 6) (5 0 0 0 2 0 0 0 3) (7 6 0 1 0 0 4 0 0) (0 0 0 0 3 0 0 8 0) (0 0 0 4 0 0 0 3 0) (8 0 0 5 0 0 6 0 0))
		     ((8 0 0 0 0 0 1 0 0) (9 0 4 0 0 1 0 0 0) (0 6 0 0 0 4 0 0 0) (0 0 0 0 9 5 0 8 0) (6 0 9 0 7 0 2 0 3) (0 2 0 6 4 0 0 0 0) (0 0 0 5 0 0 0 7 0) (0 0 0 8 0 0 3 0 6) (0 0 1 0 0 0 0 0 2))
		     ((9 2 0 0 5 7 0 0 0) (0 0 7 2 0 0 0 0 0) (4 0 3 0 0 0 0 0 0) (2 0 0 0 0 9 0 1 6) (0 0 4 0 0 0 5 0 0) (7 1 0 6 0 0 0 0 2) (0 0 0 0 0 0 3 0 9) (0 0 0 0 0 2 7 0 0) (0 0 0 8 4 0 0 5 1))
		     ((2 0 4 0 5 0 0 7 0) (0 0 0 3 0 0 0 2 0) (1 0 0 0 0 4 9 0 0) (0 5 0 0 0 2 0 0 0) (0 0 7 5 0 8 1 0 0) (0 0 0 1 0 0 0 5 0) (0 0 8 4 0 0 0 0 3) (0 6 0 0 0 9 0 0 0) (0 3 0 0 1 0 7 0 8))
		     ((0 9 0 0 7 0 0 1 0) (0 0 4 2 0 5 0 0 0) (8 0 0 1 0 0 0 0 6) (7 0 0 0 0 4 0 5 0) (0 0 5 0 0 0 4 0 0) (0 4 0 7 0 0 0 0 9) (9 0 0 0 0 1 0 0 7) (0 0 0 5 0 9 3 0 0) (0 1 0 0 2 0 0 9 0))
		     ((0 4 0 0 8 5 0 9 0) (0 0 0 7 0 0 0 0 5) (0 0 0 0 2 0 0 0 8) (6 0 0 0 0 4 3 0 0) (0 5 3 0 0 0 2 7 0) (0 0 8 6 0 0 0 0 1) (7 0 0 0 6 0 0 0 0) (1 0 0 0 0 7 0 0 0) (0 3 0 5 4 0 0 2 0))
		     ((1 0 0 4 0 0 0 0 0) (0 5 0 0 7 0 0 0 9) (0 0 7 2 0 0 4 0 0) (9 0 2 0 0 0 3 0 0) (7 0 0 1 0 4 0 0 5) (0 0 1 0 0 0 2 0 6) (0 0 3 0 0 6 5 0 0) (6 0 0 0 5 0 0 9 0) (0 0 0 0 0 9 0 0 3))
		     ((6 0 0 0 0 4 1 9 0) (0 0 0 0 0 0 0 0 0) (1 4 0 3 8 0 2 0 0) (2 0 0 0 0 0 8 0 0) (9 0 0 4 0 6 0 0 5) (0 0 4 0 0 0 0 0 6) (0 0 1 0 5 8 0 7 2) (0 0 0 0 0 0 0 0 0) (0 7 5 2 0 0 0 0 3))
		     ((0 0 0 7 0 3 9 6 0) (0 2 0 0 0 0 0 0 0) (3 0 0 0 4 5 0 0 7) (6 0 8 0 0 0 0 3 5) (0 0 0 0 0 0 0 0 0) (2 7 0 0 0 0 6 0 8) (8 0 0 3 2 0 0 0 9) (0 0 0 0 0 0 0 8 0) (0 6 4 8 0 7 0 0 0))
		     ((0 0 0 0 0 0 9 6 0) (0 0 0 8 3 0 1 0 4) (0 0 0 0 0 7 0 2 0) (1 0 2 5 0 0 7 0 0) (0 3 0 0 0 0 0 4 0) (0 0 7 0 0 9 5 0 1) (0 2 0 7 0 0 0 0 0) (7 0 9 0 4 2 0 0 0) (0 6 3 0 0 0 0 0 0))
		     ((0 3 0 0 0 5 7 0 0) (8 0 0 3 0 6 0 0 9) (0 0 0 7 0 0 8 0 0) (0 0 9 0 0 0 5 0 6) (4 0 0 0 0 0 0 0 2) (5 0 3 0 0 0 9 0 0) (0 0 2 0 0 3 0 0 0) (3 0 0 4 0 7 0 0 5) (0 0 7 9 0 0 0 4 0))
		     ((0 0 0 0 3 7 4 5 0) (0 6 0 0 0 0 8 0 0) (0 0 1 0 4 0 0 9 0) (3 5 0 0 0 0 0 2 0) (0 0 0 5 0 1 0 0 0) (0 1 0 0 0 0 0 8 4) (0 9 0 0 7 0 6 0 0) (0 0 8 0 0 0 0 4 0) (0 4 7 9 8 0 0 0 0))
		     ((0 0 0 0 0 9 3 0 0) (0 0 1 4 3 0 0 7 0) (0 0 0 0 7 0 0 9 2) (3 1 0 0 0 0 5 0 0) (6 0 0 0 0 0 0 0 9) (0 0 5 0 0 0 0 6 7) (4 5 0 0 6 0 0 0 0) (0 7 0 0 5 4 2 0 0) (0 0 2 9 0 0 0 0 0))
		     ((6 0 3 0 0 0 0 0 0) (0 0 0 0 0 3 4 6 1) (4 0 0 0 0 0 9 0 3) (0 0 8 5 9 0 0 7 0) (0 0 0 0 0 0 0 0 0) (0 4 0 0 2 8 1 0 0) (1 0 7 0 0 0 0 0 5) (2 3 5 9 0 0 0 0 0) (0 0 0 0 0 0 3 0 8))
		     ((0 0 0 0 0 0 7 3 0) (0 2 0 0 8 0 4 0 0) (6 0 5 0 0 1 0 0 0) (0 4 0 3 1 0 0 0 5) (0 0 3 0 0 0 1 0 0) (9 0 0 0 2 7 0 4 0) (0 0 0 1 0 0 8 0 7) (0 0 8 0 3 0 0 9 0) (0 5 2 0 0 0 0 0 0))
		     ((0 4 0 0 8 0 0 0 5) (8 0 0 9 4 0 0 0 0) (9 1 0 6 0 0 0 0 0) (0 0 4 0 0 0 1 0 0) (5 3 0 0 0 0 0 6 7) (0 0 8 0 0 0 3 0 0) (0 0 0 0 0 3 0 7 1) (0 0 0 0 2 8 0 0 9) (6 0 0 0 1 0 0 2 0))
		     ((0 0 0 1 0 0 6 0 0) (0 4 1 0 0 0 0 0 0) (5 7 0 9 0 0 0 3 0) (0 0 0 5 0 6 3 0 9) (9 0 0 0 0 0 0 0 8) (2 0 4 7 0 9 0 0 0) (0 2 0 0 0 1 0 5 6) (0 0 0 0 0 0 2 7 0) (0 0 9 0 0 5 0 0 0))
		     ((0 0 6 4 0 0 0 1 0) (2 0 0 0 3 9 0 0 0) (4 7 0 0 0 1 0 0 0) (0 0 5 0 0 0 0 2 0) (3 4 0 0 0 0 0 9 7) (0 2 0 0 0 0 5 0 0) (0 0 0 3 0 0 0 7 5) (0 0 0 7 5 0 0 0 6) (0 6 0 0 0 4 9 0 0))
		     ((0 0 0 0 0 1 7 3 0) (0 0 0 9 6 4 8 2 0) (0 0 0 0 0 0 0 0 6) (7 1 0 0 0 0 6 0 0) (0 0 0 1 0 3 0 0 0) (0 0 4 0 0 0 0 5 9) (5 0 0 0 0 0 0 0 0) (0 8 3 2 4 9 0 0 0) (0 6 9 3 0 0 0 0 0))
		     ((0 5 0 0 0 1 6 0 0) (0 8 0 0 2 0 0 5 0) (4 0 0 0 0 0 0 0 0) (0 6 1 5 0 0 0 0 9) (0 0 0 1 0 8 0 0 0) (3 0 0 0 0 6 5 7 0) (0 0 0 0 0 0 0 0 7) (0 3 0 0 4 0 0 8 0) (0 0 9 7 0 0 0 2 0))
		     ((4 0 0 0 0 0 0 0 0) (0 0 9 7 0 6 8 0 0) (0 6 0 3 9 0 0 0 4) (0 4 3 0 8 0 0 0 0) (9 0 0 0 0 0 0 0 1) (0 0 0 0 5 0 7 8 0) (3 0 0 0 1 4 0 5 0) (0 0 6 5 0 7 1 0 0) (0 0 0 0 0 0 0 0 9))
		     ((7 0 0 0 3 0 0 0 0) (0 0 0 0 8 5 0 6 2) (0 0 2 0 4 0 1 0 0) (0 0 0 0 0 0 0 9 0) (0 2 5 0 9 0 6 4 0) (0 6 0 0 0 0 0 0 0) (0 0 3 0 7 0 9 0 0) (1 7 0 2 5 0 0 0 0) (0 0 0 0 6 0 0 0 4))
		     ((0 3 0 0 7 1 0 0 0) (8 0 0 0 0 0 0 3 0) (0 0 5 9 0 2 0 0 7) (2 0 0 0 0 0 0 1 0) (0 0 9 0 1 0 7 0 0) (0 8 0 0 0 0 0 0 4) (6 0 0 1 0 5 3 0 0) (0 5 0 0 0 0 0 0 6) (0 0 0 7 9 0 0 2 0))
		     ((5 0 1 0 7 0 0 0 0) (0 4 0 0 0 8 0 0 0) (7 3 0 4 0 1 0 0 0) (0 0 4 0 0 9 0 2 0) (0 6 0 0 0 0 0 7 0) (0 1 0 3 0 0 4 0 0) (0 0 0 9 0 4 0 8 3) (0 0 0 8 0 0 0 6 0) (0 0 0 0 6 0 1 0 5))
		     ((0 0 4 0 0 0 8 0 7) (0 3 0 6 0 0 0 0 0) (1 0 0 2 0 0 0 0 0) (0 7 0 4 0 0 0 0 2) (8 2 0 0 7 0 0 9 6) (6 0 0 0 0 2 0 3 0) (0 0 0 0 0 9 0 0 8) (0 0 0 0 0 5 0 7 0) (5 0 2 0 0 0 1 0 0))
		     ((0 1 0 0 0 6 0 7 0) (0 0 6 0 5 0 1 0 0) (0 0 0 9 0 1 0 0 4) (0 0 2 7 0 0 0 1 0) (9 0 0 0 0 0 0 0 2) (0 7 0 0 0 2 9 0 0) (2 0 0 5 0 9 0 0 0) (0 0 1 0 7 0 6 0 0) (0 3 0 6 0 0 0 8 0))
		     ((0 0 0 0 0 0 8 0 6) (0 0 0 7 0 4 0 0 0) (0 0 0 0 1 0 0 2 7) (3 0 0 0 5 0 2 0 0) (0 4 0 1 3 8 0 7 0) (0 0 9 0 6 0 0 0 5) (6 9 0 0 7 0 0 0 0) (0 0 0 9 0 3 0 0 0) (8 0 2 0 0 0 0 0 0))
		     ((0 0 0 1 0 0 9 8 0) (0 0 0 0 0 6 0 0 5) (1 0 0 5 8 0 0 0 0) (5 6 0 7 0 0 0 0 0) (0 0 7 0 1 0 8 0 0) (0 0 0 0 0 4 0 6 7) (0 0 0 0 7 3 0 0 9) (9 0 0 8 0 0 0 0 0) (0 2 6 0 0 5 0 0 0))
		     ((0 0 0 0 6 1 5 0 0) (5 0 0 0 0 0 7 0 0) (0 6 0 0 9 0 0 4 0) (0 0 0 0 0 0 6 0 0) (4 3 8 0 2 0 9 5 1) (0 0 9 0 0 0 0 0 0) (0 7 0 0 1 0 0 2 0) (0 0 6 0 0 0 0 0 3) (0 0 1 2 3 0 0 0 0))
		     ((8 0 0 0 5 0 0 0 9) (0 0 3 0 6 0 0 0 0) (0 0 0 0 2 1 8 4 0) (0 0 0 0 0 0 0 7 0) (1 8 0 0 7 0 0 5 4) (0 4 0 0 0 0 0 0 0) (0 3 9 8 1 0 0 0 0) (0 0 0 0 4 0 5 0 0) (6 0 0 0 3 0 0 0 7))
		     ((0 1 6 0 0 8 0 0 0) (3 0 0 0 0 6 0 0 0) (0 7 0 0 0 0 8 0 0) (0 0 0 0 1 5 0 0 7) (0 3 1 0 4 0 9 2 0) (9 0 0 3 6 0 0 0 0) (0 0 8 0 0 0 0 9 0) (0 0 0 5 0 0 0 0 4) (0 0 0 7 0 0 2 3 0))
		     ((7 8 2 3 0 0 0 0 0) (3 0 5 0 0 0 0 0 2) (0 0 0 0 0 0 3 0 8) (0 4 0 0 5 6 1 0 0) (0 0 0 0 0 0 0 0 0) (0 0 7 1 9 0 0 2 0) (1 0 3 0 0 0 0 0 0) (6 0 0 0 0 0 4 0 7) (0 0 0 0 0 5 6 3 9))
		     ((1 0 0 0 0 0 0 0 4) (0 0 0 3 0 0 8 0 0) (4 9 0 0 0 8 0 1 5) (0 0 0 7 0 0 0 4 2) (0 0 0 8 0 2 0 0 0) (2 4 0 0 0 3 0 0 0) (5 8 0 2 0 0 0 9 6) (0 0 2 0 0 5 0 0 0) (9 0 0 0 0 0 0 0 1))
		     ((0 0 0 0 0 9 4 1 0) (0 5 0 0 0 0 3 0 0) (0 1 2 8 6 0 0 0 0) (0 9 0 0 8 0 7 0 0) (0 6 0 0 0 0 0 9 0) (0 0 1 0 7 0 0 2 0) (0 0 0 0 3 1 8 4 0) (0 0 5 0 0 0 0 7 0) (0 4 6 7 0 0 0 0 0))
		     ((6 0 4 0 0 0 0 0 0) (5 0 0 3 0 0 0 0 0) (0 3 1 0 2 5 0 0 0) (0 0 3 0 0 1 9 7 0) (4 0 0 0 0 0 0 0 2) (0 7 5 9 0 0 3 0 0) (0 0 0 8 4 0 7 2 0) (0 0 0 0 0 3 0 0 5) (0 0 0 0 0 0 1 0 6))
		     ((0 0 1 0 0 0 3 6 0) (0 0 8 0 7 1 0 0 0) (0 0 0 4 0 0 0 0 7) (5 7 9 0 0 0 0 0 0) (0 0 2 0 0 0 8 0 0) (0 0 0 0 0 0 6 4 5) (8 0 0 0 0 6 0 0 0) (0 0 0 3 9 0 7 0 0) (0 9 5 0 0 0 2 0 0))
		     ((6 5 0 0 4 0 1 0 0) (0 0 0 0 3 0 0 4 0) (0 0 8 0 0 0 0 0 3) (0 0 0 1 8 0 0 7 0) (9 0 0 0 6 0 0 0 4) (0 3 0 0 2 7 0 0 0) (3 0 0 0 0 0 6 0 0) (0 9 0 0 1 0 0 0 0) (0 0 7 0 5 0 0 1 2))
		     ((0 0 0 0 3 0 0 9 4) (0 0 0 0 0 0 8 7 0) (0 0 0 2 0 4 0 0 0) (0 1 0 0 9 0 4 0 0) (3 0 0 7 2 5 0 0 6) (0 0 8 0 1 0 0 2 0) (0 0 0 6 0 3 0 0 0) (0 9 7 0 0 0 0 0 0) (8 3 0 0 5 0 0 0 0))
		     ((0 6 0 0 0 0 9 0 0) (0 0 0 5 2 9 1 0 0) (0 0 0 0 0 4 0 0 3) (0 0 0 7 0 0 8 9 0) (0 8 0 0 0 0 0 5 0) (0 2 1 0 0 6 0 0 0) (3 0 0 9 0 0 0 0 0) (0 0 9 1 4 8 0 0 0) (0 0 7 0 0 0 0 8 0))
		     ((0 0 6 0 7 0 5 0 0) (0 0 3 0 0 1 0 4 0) (1 0 0 0 0 0 0 0 0) (0 2 1 9 0 0 0 0 5) (0 0 0 6 0 8 0 0 0) (4 0 0 0 0 2 9 8 0) (0 0 0 0 0 0 0 0 7) (0 9 0 8 0 0 2 0 0) (0 0 2 0 3 0 6 0 0))
		     ((8 1 0 0 0 0 3 0 0) (0 3 0 0 9 4 0 0 0) (0 0 4 0 7 0 0 0 8) (0 0 0 0 0 5 0 0 6) (0 6 8 0 0 0 7 4 0) (4 0 0 7 0 0 0 0 0) (6 0 0 0 5 0 1 0 0) (0 0 0 6 1 0 0 9 0) (0 0 3 0 0 0 0 5 2))))

(setq hard-puzzles '(((3 0 0 0 9 0 4 0 0) (0 9 2 0 0 3 0 0 1) (0 5 0 8 0 0 0 0 0) (0 0 0 0 8 0 0 3 0) (9 0 4 7 0 1 6 0 8) (0 6 0 0 3 0 0 0 0) (0 0 0 0 0 8 0 6 0) (5 0 0 1 0 0 2 7 0) (0 0 9 0 2 0 0 0 5))
		     ((0 0 0 3 1 6 0 0 8) (9 0 0 2 0 0 0 6 0) (8 0 6 0 0 0 0 3 0) (3 0 0 1 0 0 0 0 0) (6 1 0 0 0 0 0 2 9) (0 0 0 0 0 4 0 0 7) (0 6 0 0 0 0 8 0 4) (0 2 0 0 0 5 0 0 3) (1 0 0 7 8 3 0 0 0))
		     ((0 7 2 8 0 0 0 0 0) (0 0 8 5 0 9 0 6 0) (5 0 0 0 1 0 0 0 3) (0 4 0 6 9 0 0 0 0) (0 6 0 0 0 0 0 4 0) (0 0 0 0 4 7 0 3 0) (6 0 0 0 3 0 0 0 2) (0 3 0 4 0 6 1 0 0) (0 0 0 0 0 8 3 7 0))
		     ((0 0 9 2 0 0 0 0 0) (6 0 0 1 0 0 0 0 2) (0 2 1 0 3 0 0 0 0) (0 3 7 0 0 0 0 1 0) (0 5 0 9 0 3 0 8 0) (0 9 0 0 0 0 4 3 0) (0 0 0 0 2 0 3 5 0) (9 0 0 0 0 7 0 0 1) (0 0 0 0 0 8 9 0 0))
		     ((0 0 5 0 0 4 0 0 3) (0 0 1 9 0 6 0 0 0) (8 4 0 0 0 0 0 0 9) (0 3 0 6 0 2 0 0 0) (0 6 0 7 0 1 0 8 0) (0 0 0 4 0 3 0 7 0) (4 0 0 0 0 0 0 5 1) (0 0 0 5 0 7 2 0 0) (5 0 0 3 0 0 4 0 0))
		     ((0 0 0 0 0 3 0 0 0) (0 0 1 0 7 0 5 8 0) (9 0 0 0 6 0 0 2 0) (5 0 0 0 0 0 4 0 7) (0 2 0 0 0 0 0 5 0) (6 0 8 0 0 0 0 0 9) (0 5 0 0 3 0 0 0 1) (0 7 9 0 4 0 6 0 0) (0 0 0 8 0 0 0 0 0))
		     ((0 0 7 9 0 0 4 6 0) (0 8 0 0 7 0 3 0 0) (9 0 0 2 0 1 0 0 0) (7 0 0 0 0 0 9 1 0) (0 0 0 0 0 0 0 0 0) (0 9 6 0 0 0 0 0 8) (0 0 0 1 0 8 0 0 7) (0 0 8 0 4 0 0 5 0) (0 1 2 0 0 5 6 0 0))
		     ((0 0 1 0 0 9 0 0 0) (0 0 8 0 0 4 0 3 7) (6 0 0 0 7 0 0 8 0) (0 5 0 0 0 3 0 0 2) (8 0 9 0 0 0 3 0 6) (7 0 0 9 0 0 0 1 0) (0 3 0 0 2 0 0 0 8) (5 6 0 4 0 0 7 0 0) (0 0 0 6 0 0 2 0 0))
		     ((7 0 0 0 1 2 8 0 0) (0 0 2 6 5 0 0 9 0) (0 0 0 0 0 9 0 0 1) (0 2 0 3 0 0 0 0 0) (0 6 3 0 0 0 1 7 0) (0 0 0 0 0 1 0 2 0) (4 0 0 5 0 0 0 0 0) (0 9 0 0 2 6 4 0 0) (0 0 8 1 4 0 0 0 9))
		     ((1 0 0 0 0 2 0 9 4) (0 9 0 0 3 1 0 5 0) (0 0 6 0 0 0 1 0 0) (0 0 0 3 9 0 0 0 7) (0 0 0 0 5 0 0 0 0) (7 0 0 0 8 4 0 0 0) (0 0 9 0 0 0 8 0 0) (0 5 0 6 2 0 0 4 0) (6 7 0 9 0 0 0 0 5))
		     ((0 0 2 0 0 0 0 0 1) (0 1 0 0 8 0 0 0 6) (8 0 7 0 0 9 0 0 0) (0 0 0 0 9 0 0 3 4) (0 0 8 4 7 2 6 0 0) (4 5 0 0 3 0 0 0 0) (0 0 0 1 0 0 9 0 5) (5 0 0 0 4 0 0 2 0) (7 0 0 0 0 0 4 0 0))
		     ((9 0 3 0 0 6 0 0 0) (0 2 0 0 4 0 0 1 0) (5 8 0 0 0 0 0 0 0) (0 9 0 7 0 2 1 0 0) (1 0 0 0 0 0 0 0 2) (0 0 7 8 0 5 0 4 0) (0 0 0 0 0 0 0 7 6) (0 6 0 0 7 0 0 8 0) (0 0 0 6 0 0 4 0 5))
		     ((0 0 0 7 0 0 0 5 0) (0 0 0 0 3 0 0 2 8) (0 0 1 0 0 2 9 0 6) (0 1 0 6 0 7 0 0 0) (0 0 7 0 0 0 4 0 0) (0 0 0 8 0 9 0 1 0) (7 0 6 4 0 0 3 0 0) (1 8 0 0 9 0 0 0 0) (0 4 0 0 0 3 0 0 0))
		     ((6 0 0 4 7 2 0 0 0) (9 2 0 0 0 8 0 0 0) (0 1 0 0 0 0 0 0 0) (2 4 0 0 0 0 0 0 6) (0 6 0 0 3 0 0 7 0) (1 0 0 0 0 0 0 5 3) (0 0 0 0 0 0 0 1 0) (0 0 0 2 0 0 0 9 4) (0 0 0 6 8 9 0 0 5))
		     ((3 0 0 5 0 0 8 0 4) (0 2 0 7 0 8 0 0 0) (0 0 5 0 9 0 0 0 7) (0 7 0 0 0 0 6 0 3) (0 0 0 0 0 0 0 0 0) (6 0 8 0 0 0 0 2 0) (1 0 0 0 2 0 7 0 0) (0 0 0 8 0 4 0 6 0) (9 0 3 0 0 6 0 0 2))
		     ((5 0 0 0 0 0 0 1 6) (6 0 0 8 4 0 0 0 0) (0 1 7 0 0 0 0 0 0) (0 0 2 4 3 6 1 8 0) (0 0 0 0 0 0 0 0 0) (0 6 4 9 7 8 3 0 0) (0 0 0 0 0 0 9 6 0) (0 0 0 0 5 3 0 0 1) (2 8 0 0 0 0 0 0 7))
		     ((0 0 7 0 0 0 0 0 3) (0 0 9 0 0 2 0 1 0) (0 0 4 0 1 0 7 0 5) (8 0 2 3 0 0 0 0 0) (0 0 0 2 8 1 0 0 0) (0 0 0 0 0 6 9 0 2) (6 0 5 0 4 0 2 0 0) (0 7 0 9 0 0 3 0 0) (4 0 0 0 0 0 8 0 0))
		     ((0 5 0 0 8 0 0 0 9) (9 0 6 0 0 0 5 0 0) (0 0 8 0 1 0 0 0 0) (0 0 0 5 0 0 0 0 4) (3 9 0 0 6 0 0 7 2) (4 0 0 0 0 2 0 0 0) (0 0 0 0 3 0 4 0 0) (0 0 1 0 0 0 8 0 3) (7 0 0 0 2 0 0 1 0))
		     ((5 9 7 0 0 0 2 0 0) (0 0 6 0 5 2 0 0 0) (0 0 0 4 0 0 8 5 0) (0 0 0 0 3 0 6 0 9) (0 3 0 0 0 0 0 1 0) (4 0 2 0 6 0 0 0 0) (0 6 4 0 0 9 0 0 0) (0 0 0 8 1 0 4 0 0) (0 0 8 0 0 0 1 9 3))
		     ((3 6 0 0 7 0 0 0 0) (2 0 4 6 0 0 8 0 0) (0 9 0 0 0 1 0 0 0) (0 0 0 1 0 2 0 8 0) (0 0 5 0 0 0 1 0 0) (0 8 0 4 0 3 0 0 0) (0 0 0 7 0 0 0 5 0) (0 0 7 0 0 5 2 0 1) (0 0 0 0 4 0 0 3 8))
		     ((0 3 4 0 0 2 0 0 0) (0 0 1 3 7 5 0 0 0) (0 9 0 0 0 0 0 0 0) (0 1 6 0 0 0 9 0 0) (0 8 0 0 6 0 0 5 0) (0 0 5 0 0 0 2 4 0) (0 0 0 0 0 0 0 9 0) (0 0 0 2 8 4 5 0 0) (0 0 0 7 0 0 3 2 0))
		     ((0 0 0 0 4 2 5 0 0) (8 0 0 0 0 6 0 0 3) (4 0 0 8 0 0 0 0 7) (0 0 0 0 0 0 3 0 0) (0 0 9 6 5 4 2 0 0) (0 0 5 0 0 0 0 0 0) (3 0 0 0 0 1 0 0 4) (7 0 0 4 0 0 0 0 2) (0 0 2 7 9 0 0 0 0))
		     ((0 3 0 0 0 0 0 5 0) (1 0 0 2 9 0 0 0 7) (4 0 2 3 0 0 1 0 0) (0 0 4 0 5 7 0 0 0) (0 0 0 0 1 0 0 0 0) (0 0 0 6 3 0 4 0 0) (0 0 8 0 0 9 7 0 3) (3 0 0 0 6 8 0 0 1) (0 2 0 0 0 0 0 8 0))
		     ((0 0 7 0 0 0 0 6 5) (0 6 0 5 4 7 0 0 0) (0 0 5 0 0 3 0 2 0) (0 0 0 0 0 4 0 7 0) (0 2 3 0 0 0 4 5 0) (0 1 0 8 0 0 0 0 0) (0 7 0 9 0 0 3 0 0) (0 0 0 7 6 1 0 4 0) (6 8 0 0 0 0 5 0 0))
		     ((0 0 0 0 0 0 0 0 0) (1 0 0 5 2 0 0 0 0) (0 4 8 0 0 0 9 3 0) (8 0 3 0 6 0 0 9 0) (2 0 0 0 5 0 0 0 3) (0 7 0 0 3 0 6 0 1) (0 3 1 0 0 0 4 8 0) (0 0 0 0 1 4 0 0 5) (0 0 0 0 0 0 0 0 0))
		     ((1 3 0 0 6 0 0 0 0) (0 0 0 0 0 9 0 2 0) (0 0 6 0 4 0 8 1 0) (0 5 2 0 0 4 0 0 7) (0 0 0 0 5 0 0 0 0) (7 0 0 8 0 0 1 4 0) (0 7 5 0 8 0 4 0 0) (0 8 0 9 0 0 0 0 0) (0 0 0 0 2 0 0 5 8))
		     ((7 0 0 0 0 3 0 0 2) (0 0 0 4 0 0 3 0 0) (4 1 0 0 6 9 0 0 0) (0 9 4 0 0 0 0 0 0) (6 0 2 0 7 0 4 0 3) (0 0 0 0 0 0 1 2 0) (0 0 0 7 1 0 0 5 8) (0 0 8 0 0 2 0 0 0) (5 0 0 8 0 0 0 0 4))
		     ((5 0 0 0 7 4 0 2 0) (0 0 0 2 0 0 0 0 5) (0 0 0 0 8 0 4 3 0) (0 4 7 0 2 0 0 9 0) (0 0 0 0 0 0 0 0 0) (0 6 0 0 4 0 2 5 0) (0 8 4 0 6 0 0 0 0) (6 0 0 0 0 3 0 0 0) (0 3 0 4 9 0 0 0 1))
		     ((0 0 0 0 6 0 8 0 7) (8 5 0 0 1 0 0 6 0) (9 0 0 4 0 0 0 0 0) (0 0 2 1 0 0 0 9 3) (0 0 0 0 3 0 0 0 0) (1 8 0 0 0 5 2 0 0) (0 0 0 0 0 4 0 0 5) (0 1 0 0 5 0 0 3 2) (3 0 5 0 9 0 0 0 0))
		     ((0 0 0 0 2 6 0 5 0) (0 0 0 3 0 0 0 4 0) (9 0 0 0 5 0 6 0 0) (6 0 0 0 0 0 4 3 0) (4 0 2 0 3 0 7 0 6) (0 1 3 0 0 0 0 0 2) (0 0 8 0 4 0 0 0 3) (0 6 0 0 0 5 0 0 0) (0 2 0 9 8 0 0 0 0))
		     ((1 0 0 0 0 0 0 0 0) (2 0 5 0 0 9 0 0 0) (0 0 6 4 8 2 0 0 0) (4 0 2 0 0 0 6 0 0) (6 0 0 0 3 0 0 0 8) (0 0 1 0 0 0 3 0 7) (0 0 0 6 9 5 7 0 0) (0 0 0 2 0 0 4 0 5) (0 0 0 0 0 0 0 0 1))
		     ((0 4 0 7 0 0 0 0 0) (6 0 0 5 4 0 0 1 0) (0 0 7 0 9 8 0 0 5) (0 0 0 0 0 2 5 0 0) (4 0 1 0 0 0 8 0 2) (0 0 5 4 0 0 0 0 0) (3 0 0 8 5 0 7 0 0) (0 7 0 0 3 4 0 0 6) (0 0 0 0 0 9 0 3 0))
		     ((4 0 0 0 0 0 0 0 7) (0 7 0 0 0 9 1 2 0) (0 0 1 0 6 7 3 0 0) (0 0 0 6 1 0 0 5 0) (0 0 0 0 3 0 0 0 0) (0 5 0 0 8 2 0 0 0) (0 0 3 4 9 0 2 0 0) (0 4 5 1 0 0 0 3 0) (1 0 0 0 0 0 0 0 8))
		     ((0 3 9 1 6 0 0 0 0) (1 0 4 7 0 5 0 0 0) (0 0 0 0 0 0 0 0 0) (9 0 0 0 0 0 8 0 2) (8 0 0 0 7 0 0 0 6) (5 0 2 0 0 0 0 0 4) (0 0 0 0 0 0 0 0 0) (0 0 0 3 0 2 9 0 5) (0 0 0 0 5 4 2 3 0))
		     ((0 8 0 0 9 0 2 0 0) (0 0 0 8 3 0 0 1 0) (0 0 7 0 0 0 0 0 5) (0 3 0 9 0 0 4 0 6) (0 6 4 0 0 0 5 8 0) (1 0 8 0 0 4 0 3 0) (4 0 0 0 0 0 1 0 0) (0 1 0 0 7 8 0 0 0) (0 0 2 0 4 0 0 5 0))
		     ((0 0 0 9 0 0 2 7 0) (0 3 0 7 2 0 0 0 0) (0 6 0 0 5 0 3 0 0) (3 0 0 0 0 5 0 0 9) (0 2 0 0 3 0 0 8 0) (1 0 0 8 0 0 0 0 3) (0 0 4 0 9 0 0 6 0) (0 0 0 0 1 4 0 5 0) (0 9 1 0 0 8 0 0 0))
		     ((8 7 0 0 0 9 5 1 0) (0 5 3 0 8 0 0 4 0) (0 0 0 5 0 0 0 0 0) (2 0 0 0 0 0 8 0 0) (6 0 0 0 2 0 0 0 1) (0 0 5 0 0 0 0 0 3) (0 0 0 0 0 3 0 0 0) (0 9 0 0 7 0 3 5 0) (0 8 6 9 0 0 0 7 4))
		     ((9 4 5 1 0 0 0 0 3) (0 0 0 9 0 0 0 5 2) (0 0 0 0 0 0 8 0 0) (0 0 0 6 8 0 9 0 0) (7 0 0 0 4 0 0 0 8) (0 0 1 0 9 5 0 0 0) (0 0 3 0 0 0 0 0 0) (6 7 0 0 0 4 0 0 0) (2 0 0 0 0 9 1 3 7))
		     ((0 0 0 3 6 2 5 0 0) (0 0 4 7 0 0 0 0 2) (0 2 5 0 0 0 0 0 3) (0 0 3 6 0 0 0 0 0) (6 0 2 0 0 0 4 0 7) (0 0 0 0 0 9 8 0 0) (2 0 0 0 0 0 9 5 0) (7 0 0 0 0 1 3 0 0) (0 0 6 8 5 3 0 0 0))
		     ((0 0 0 0 2 0 5 0 8) (6 0 0 0 0 1 0 0 2) (0 0 7 4 0 9 0 0 0) (0 0 0 0 0 0 0 3 0) (1 0 3 0 0 0 6 0 9) (0 8 0 0 0 0 0 0 0) (0 0 0 6 0 2 4 0 0) (4 0 0 8 0 0 0 0 7) (9 0 2 0 5 0 0 0 0))
		     ((6 0 3 8 0 0 0 5 0) (9 0 0 0 2 0 7 0 0) (0 0 0 6 0 0 0 2 0) (0 0 5 4 0 0 0 0 1) (0 4 7 0 0 0 6 9 0) (3 0 0 0 0 9 2 0 0) (0 1 0 0 0 4 0 0 0) (0 0 6 0 5 0 0 0 7) (0 7 0 0 0 8 5 0 9))
		     ((0 0 0 0 0 0 0 0 0) (0 1 7 9 0 3 0 0 0) (3 9 0 5 7 0 0 0 0) (0 0 5 0 0 0 7 9 0) (0 0 4 0 2 0 6 0 0) (0 6 9 0 0 0 1 0 0) (0 0 0 0 4 8 0 1 3) (0 0 0 7 0 2 8 5 0) (0 0 0 0 0 0 0 0 0))
		     ((0 0 8 2 0 0 0 0 0) (1 0 2 0 5 0 0 0 3) (0 0 0 6 4 0 0 1 0) (0 0 0 3 0 0 5 0 0) (3 9 0 0 8 0 0 6 4) (0 0 7 0 0 6 0 0 0) (0 1 0 0 7 3 0 0 0) (5 0 0 0 6 0 4 0 8) (0 0 0 0 0 5 9 0 0))
		     ((0 0 7 2 0 0 0 5 0) (0 4 5 1 0 3 0 0 0) (0 0 0 0 9 0 3 0 0) (0 5 0 0 2 0 4 0 0) (0 0 1 0 8 0 6 0 0) (0 0 8 0 6 0 0 9 0) (0 0 6 0 5 0 0 0 0) (0 0 0 4 0 2 5 6 0) (0 3 0 0 0 7 2 0 0))
		     ((0 0 3 0 1 0 0 4 2) (0 0 0 8 3 0 0 0 0) (0 0 0 0 0 0 9 3 1) (3 0 0 0 7 2 0 0 0) (0 7 0 5 0 6 0 8 0) (0 0 0 3 9 0 0 0 5) (7 5 2 0 0 0 0 0 0) (0 0 0 0 2 3 0 0 0) (9 3 0 0 5 0 4 0 0))
		     ((0 0 0 1 3 0 6 0 0) (4 0 6 0 0 7 0 0 0) (0 0 1 0 0 0 3 8 7) (0 6 2 0 4 0 0 0 0) (8 0 0 0 0 0 0 0 3) (0 0 0 0 8 0 4 7 0) (7 9 5 0 0 0 2 0 0) (0 0 0 6 0 0 1 0 9) (0 0 4 0 9 2 0 0 0))
		     ((0 9 7 0 0 6 0 0 0) (0 4 0 0 7 0 0 0 8) (5 0 0 0 1 0 0 2 0) (0 0 1 0 2 0 0 6 0) (4 0 0 6 0 1 0 0 3) (0 2 0 0 9 0 1 0 0) (0 5 0 0 3 0 0 0 6) (3 0 0 0 8 0 0 4 0) (0 0 0 7 0 0 5 3 0))
		     ((0 0 4 0 0 2 7 0 0) (0 9 0 3 7 0 0 0 0) (0 0 1 6 0 0 2 0 0) (0 1 0 0 0 0 0 0 0) (0 3 0 7 9 6 0 8 0) (0 0 0 0 0 0 0 9 0) (0 0 3 0 0 7 4 0 0) (0 0 0 0 8 4 0 3 0) (0 0 7 5 0 0 1 0 0))
		     ((5 0 0 0 0 2 0 0 0) (9 0 0 3 0 0 2 1 0) (0 0 8 6 0 0 3 0 0) (0 0 0 0 0 1 0 4 0) (8 0 7 0 3 0 1 0 6) (0 4 0 7 0 0 0 0 0) (0 0 1 0 0 5 7 0 0) (0 6 5 0 0 7 0 0 3) (0 0 0 2 0 0 0 0 1))
		     ((0 0 9 0 0 7 0 0 0) (3 8 0 0 5 0 0 0 0) (7 0 0 8 2 0 9 0 0) (1 0 0 0 7 0 0 2 8) (0 0 0 0 0 0 0 0 0) (9 7 0 0 8 0 0 0 6) (0 0 4 0 1 8 0 0 3) (0 0 0 0 6 0 0 8 5) (0 0 0 3 0 0 6 0 0))
		     ((1 0 4 0 0 0 5 0 0) (0 2 0 8 1 0 0 0 0) (0 0 0 0 0 5 0 0 1) (9 0 3 2 0 0 8 7 0) (0 0 0 0 0 0 0 0 0) (0 5 7 0 0 8 2 0 4) (3 0 0 9 0 0 0 0 0) (0 0 0 0 6 1 0 8 0) (0 0 8 0 0 0 7 0 6))
		     ((0 0 0 0 3 0 9 0 0) (0 0 1 8 2 0 0 4 0) (0 5 0 0 0 0 0 6 8) (0 0 0 0 0 2 0 0 0) (9 0 6 0 7 0 2 0 4) (0 0 0 5 0 0 0 0 0) (6 7 0 0 0 0 0 9 0) (0 1 0 0 4 8 3 0 0) (0 0 5 0 1 0 0 0 0))
		     ((7 0 4 0 0 8 0 9 0) (8 0 0 0 0 1 0 0 2) (0 0 0 7 0 0 0 5 0) (0 0 3 4 0 0 0 0 0) (4 1 0 0 8 0 0 2 6) (0 0 0 0 0 6 3 0 0) (0 4 0 0 0 7 0 0 0) (6 0 0 5 0 0 0 0 4) (0 8 0 6 0 0 1 0 5))))

(setq medium-puzzles '(((0 0 0 8 7 0 1 0 2) (0 0 1 0 4 0 9 0 6) (0 0 0 0 0 6 0 0 0) (0 0 0 0 8 0 0 5 3) (5 0 6 0 3 0 4 0 9) (8 2 0 0 5 0 0 0 0) (0 0 0 1 0 0 0 0 0) (1 0 5 0 2 0 3 0 0) (7 0 4 0 6 8 0 0 0))
		       ((0 5 0 0 0 8 0 1 0) (0 9 0 0 0 5 0 0 6) (0 0 0 7 4 0 9 0 8) (0 0 0 0 0 0 3 0 5) (0 1 4 0 0 0 8 6 0) (5 0 8 0 0 0 0 0 0) (1 0 9 0 7 6 0 0 0) (7 0 0 2 0 0 0 8 0) (0 2 0 3 0 0 0 9 0))
		       ((0 0 0 0 2 8 0 0 3) (0 0 0 0 0 7 1 0 2) (0 3 2 0 1 0 0 9 0) (1 0 0 0 0 0 7 2 0) (4 0 0 0 0 0 0 0 5) (0 8 6 0 0 0 0 0 4) (0 6 0 0 9 0 3 7 0) (9 0 4 7 0 0 0 0 0) (3 0 0 2 5 0 0 0 0))
		       ((9 8 4 0 0 3 5 0 0) (0 7 0 0 4 0 0 1 0) (0 0 3 0 0 0 0 0 4) (5 0 0 0 9 0 0 7 8) (0 0 0 2 0 8 0 0 0) (3 6 0 0 1 0 0 0 2) (4 0 0 0 0 0 8 0 0) (0 3 0 0 8 0 0 5 0) (0 0 5 4 0 0 7 2 1))
		       ((4 0 0 0 1 0 0 0 2) (0 0 7 2 0 0 4 0 9) (0 0 0 8 0 0 0 6 0) (0 0 0 6 0 8 1 7 0) (0 0 3 0 4 0 2 0 0) (0 6 1 3 0 2 0 0 0) (0 2 0 0 0 9 0 0 0) (7 0 4 0 0 5 6 0 0) (9 0 0 0 8 0 0 0 7))
		       ((0 0 0 3 7 0 0 0 4) (0 7 1 0 0 0 8 0 5) (0 0 3 4 0 0 0 0 7) (0 9 0 8 2 0 0 0 0) (3 0 7 0 0 0 5 0 2) (0 0 0 0 3 7 0 4 0) (7 0 0 0 0 1 2 0 0) (2 0 8 0 0 0 3 6 0) (1 0 0 0 8 3 0 0 0))
		       ((4 0 0 5 6 0 0 0 0) (0 6 0 0 7 8 3 0 0) (0 3 0 1 0 0 0 7 0) (6 0 0 0 0 0 0 0 3) (9 0 1 0 0 0 8 0 4) (2 0 0 0 0 0 0 0 6) (0 2 0 0 0 4 0 6 0) (0 0 9 3 5 0 0 2 0) (0 0 0 0 2 1 0 0 7))
		       ((6 2 1 0 0 9 0 0 4) (8 5 0 0 0 0 0 0 0) (7 0 0 0 0 6 9 1 0) (9 0 0 0 7 0 4 0 2) (0 8 0 0 0 0 0 9 0) (3 0 4 0 1 0 0 0 5) (0 9 2 8 0 0 0 0 3) (0 0 0 0 0 0 0 4 7) (4 0 0 3 0 0 5 2 9))
		       ((0 0 0 8 3 0 0 6 0) (0 5 0 0 9 0 0 3 7) (0 0 0 0 0 0 5 0 0) (2 0 0 0 8 0 0 0 3) (5 3 8 0 1 0 2 4 9) (1 0 0 0 5 0 0 0 6) (0 0 3 0 0 0 0 0 0) (4 2 0 0 6 0 0 1 0) (0 7 0 0 2 1 0 0 0))
		       ((0 0 9 5 8 0 6 0 0) (0 3 0 0 0 9 0 4 0) (7 0 0 0 4 0 5 0 0) (0 0 4 3 0 0 0 6 5) (5 0 0 9 0 8 0 0 1) (2 1 0 0 0 7 3 0 0) (0 0 2 0 3 0 0 0 6) (0 6 0 2 0 0 0 3 0) (0 0 7 0 9 6 1 0 0))
		       ((6 5 0 9 8 0 0 0 2) (0 0 0 0 0 1 9 0 0) (0 0 0 7 6 2 0 0 0) (0 9 7 0 0 0 3 0 0) (8 0 0 0 1 0 0 0 4) (0 0 2 0 0 0 6 5 0) (0 0 0 6 7 8 0 0 0) (0 0 3 1 0 0 0 0 0) (5 0 0 0 9 3 0 2 8))
		       ((0 0 1 0 8 0 2 0 0) (0 0 0 4 0 3 0 0 5) (0 0 2 9 0 0 0 0 0) (0 1 3 0 0 0 0 2 7) (0 6 5 0 7 0 1 4 0) (7 4 0 0 0 0 5 6 0) (0 0 0 0 0 5 9 0 0) (3 0 0 2 0 6 0 0 0) (0 0 9 0 1 0 4 0 0))
		       ((8 0 0 0 0 4 0 0 0) (7 6 0 8 5 0 0 4 0) (4 0 3 0 2 0 0 0 7) (0 1 0 0 9 0 0 5 0) (3 0 0 6 0 2 0 0 1) (0 8 0 0 4 0 0 6 0) (5 0 0 0 3 0 6 0 9) (0 3 0 0 6 5 0 7 8) (0 0 0 7 0 0 0 0 5))
		       ((3 2 0 0 1 0 7 0 0) (1 5 9 6 0 3 0 0 2) (6 0 0 0 0 2 1 0 0) (0 0 0 0 0 0 4 0 1) (0 0 0 7 0 5 0 0 0) (9 0 8 0 0 0 0 0 0) (0 0 3 2 0 0 0 0 7) (2 0 0 9 0 1 5 8 4) (0 0 1 0 8 0 0 6 3))
		       ((0 0 2 0 5 0 9 4 0) (7 3 0 8 0 1 0 0 0) (0 0 5 0 0 0 0 3 0) (8 9 0 0 0 0 1 2 0) (3 0 0 0 0 0 0 0 6) (0 2 7 0 0 0 0 8 3) (0 7 0 0 0 0 6 0 0) (0 0 0 5 0 7 0 1 9) (0 1 3 0 9 0 2 0 0))
		       ((0 0 0 1 6 0 0 2 0) (0 0 7 3 0 0 5 0 0) (2 1 0 0 0 8 0 0 9) (0 0 1 0 9 5 0 0 6) (9 5 0 0 0 0 0 8 4) (7 0 0 2 4 0 9 0 0) (3 0 0 5 0 0 0 6 7) (0 0 5 0 0 7 8 0 0) (0 7 0 0 3 6 0 0 0))
		       ((0 9 8 4 5 0 0 0 2) (0 0 4 8 0 0 6 0 0) (0 0 0 0 0 0 0 8 5) (0 0 5 9 0 0 7 4 0) (1 0 0 0 0 0 0 0 9) (0 4 3 0 0 5 2 0 0) (2 6 0 0 0 0 0 0 0) (0 0 1 0 0 6 9 0 0) (5 0 0 0 3 2 1 6 0))
		       ((9 0 0 0 2 0 0 5 7) (0 0 0 7 9 3 0 0 4) (0 0 0 0 8 0 2 0 0) (0 8 0 0 0 0 0 0 0) (1 2 0 8 3 9 0 7 6) (0 0 0 0 0 0 0 3 0) (0 0 9 0 7 0 0 0 0) (5 0 0 2 1 6 0 0 0) (2 6 0 0 4 0 0 0 3))
		       ((0 0 4 7 0 0 0 0 0) (0 0 0 0 2 0 7 3 0) (9 7 8 5 0 0 1 0 0) (0 0 0 0 6 5 3 1 0) (1 0 0 0 7 0 0 0 6) (0 8 5 3 4 0 0 0 0) (0 0 9 0 0 3 2 7 4) (0 3 2 0 9 0 0 0 0) (0 0 0 0 0 7 6 0 0))
		       ((3 0 0 0 0 0 0 0 6) (0 0 0 4 3 0 1 0 0) (0 0 2 0 0 0 9 0 0) (0 0 9 3 8 0 0 0 5) (6 0 8 7 4 9 3 0 2) (7 0 0 0 5 1 6 0 0) (0 0 4 0 0 0 2 0 0) (0 0 7 0 2 8 0 0 0) (5 0 0 0 0 0 0 0 9))
		       ((7 0 5 6 1 0 0 0 0) (1 0 2 0 4 0 0 0 7) (0 0 0 8 0 0 0 0 0) (0 7 0 0 0 0 6 0 0) (8 0 4 1 9 6 5 0 2) (0 0 6 0 0 0 0 4 0) (0 0 0 0 0 1 0 0 0) (9 0 0 0 2 0 7 0 3) (0 0 0 0 3 9 8 0 4))
		       ((0 0 8 0 0 4 5 0 0) (0 1 0 5 9 0 0 6 0) (5 0 0 0 8 0 0 4 0) (0 8 0 6 0 0 1 0 4) (1 0 0 3 0 9 0 0 2) (2 0 5 0 0 8 0 7 0) (0 2 0 0 7 0 0 0 6) (0 5 0 0 3 2 0 9 0) (0 0 7 9 0 0 8 0 0))
		       ((0 7 1 0 4 0 0 2 0) (0 9 8 2 7 0 0 0 0) (0 0 0 5 0 0 0 0 0) (9 0 0 0 0 0 3 0 0) (0 4 6 3 2 5 9 8 0) (0 0 3 0 0 0 0 0 1) (0 0 0 0 0 8 0 0 0) (0 0 0 0 5 3 6 1 0) (0 1 0 0 9 0 4 5 0))
		       ((5 0 7 0 4 0 0 0 0) (0 0 0 0 0 1 2 0 0) (0 0 4 0 0 5 7 8 1) (9 0 3 5 8 0 0 0 0) (0 6 0 0 1 0 0 2 0) (0 0 0 0 2 3 5 0 6) (1 4 9 3 0 0 6 0 0) (0 0 8 1 0 0 0 0 0) (0 0 0 0 7 0 1 0 5))
		       ((0 0 4 8 0 0 0 0 0) (3 6 0 9 0 1 0 0 4) (1 0 0 3 0 0 2 7 0) (0 0 0 1 0 0 9 2 0) (7 8 0 0 0 0 0 4 3) (0 4 9 0 0 3 0 0 0) (0 1 5 0 0 7 0 0 2) (4 0 0 2 0 8 0 1 6) (0 0 0 0 0 5 7 0 0))
		       ((0 2 5 8 0 0 0 0 0) (7 0 0 0 0 0 8 0 3) (0 0 1 0 7 0 6 0 4) (0 0 0 9 0 3 0 4 0) (0 0 2 0 0 0 5 0 0) (0 7 0 4 0 5 0 0 0) (5 0 4 0 3 0 1 0 0) (2 0 3 0 0 0 0 0 6) (0 0 0 0 0 6 4 3 0))
		       ((3 0 9 0 6 4 0 0 7) (4 0 0 0 0 0 9 3 0) (5 0 0 3 0 0 0 0 0) (0 4 0 0 7 8 0 6 0) (0 0 0 0 0 0 0 0 0) (0 6 0 4 5 0 0 8 0) (0 0 0 0 0 7 0 0 6) (0 5 1 0 0 0 0 0 4) (6 0 0 2 1 0 7 0 5))
		       ((0 0 0 0 0 5 4 0 3) (0 0 6 8 7 0 0 0 0) (0 0 0 0 0 0 1 7 0) (0 0 7 4 8 0 2 3 0) (4 6 0 5 0 2 0 9 8) (0 3 2 0 9 1 6 0 0) (0 2 3 0 0 0 0 0 0) (0 0 0 0 5 7 8 0 0) (5 0 8 6 0 0 0 0 0))
		       ((0 4 0 0 0 0 0 0 7) (3 2 0 0 7 0 0 0 5) (0 0 0 6 0 1 9 4 0) (6 5 0 0 0 0 1 3 0) (0 0 8 0 0 0 4 0 0) (0 1 4 0 0 0 0 5 9) (0 6 3 9 0 7 0 0 0) (5 0 0 0 3 0 0 6 4) (8 0 0 0 0 0 0 9 0))
		       ((0 0 0 0 0 0 0 0 0) (3 5 7 0 0 9 4 0 0) (0 9 0 8 2 0 7 0 0) (5 0 0 0 0 0 1 7 0) (7 3 0 0 8 0 0 2 9) (0 1 8 0 0 0 0 0 5) (0 0 5 0 7 8 0 6 0) (0 0 6 2 0 0 8 5 7) (0 0 0 0 0 0 0 0 0))
		       ((9 0 0 8 0 0 0 0 0) (0 0 0 2 0 1 0 7 0) (3 0 0 0 4 0 0 0 9) (1 0 3 0 0 0 9 5 0) (7 0 6 0 5 0 2 0 3) (0 5 2 0 0 0 6 0 7) (8 0 0 0 3 0 0 0 2) (0 1 0 9 0 6 0 0 0) (0 0 0 0 0 7 0 0 8))
		       ((0 0 2 0 0 5 3 8 0) (0 0 8 1 9 0 0 0 0) (0 6 1 0 7 8 0 0 0) (0 2 0 0 0 0 9 0 0) (0 8 0 0 2 0 0 7 0) (0 0 7 0 0 0 0 2 0) (0 0 0 5 4 0 1 9 0) (0 0 0 0 8 1 4 0 0) (0 1 6 2 0 0 8 0 0))
		       ((4 0 2 9 0 0 0 0 8) (0 0 0 0 5 0 0 0 4) (1 0 0 0 8 3 2 0 0) (2 0 0 0 0 5 0 9 0) (0 0 5 0 0 0 1 0 0) (0 8 0 3 0 0 0 0 5) (0 0 4 2 9 0 0 0 6) (3 0 0 0 7 0 0 0 0) (5 0 0 0 0 1 7 0 2))
		       ((0 0 7 8 0 6 3 0 4) (0 5 0 3 0 0 0 0 1) (6 2 0 0 7 0 0 0 0) (0 0 0 0 0 1 4 0 3) (8 0 0 0 0 0 0 0 6) (2 0 5 6 0 0 0 0 0) (0 0 0 0 1 0 0 6 2) (5 0 0 0 0 3 0 4 0) (7 0 1 2 0 9 5 0 0))
		       ((8 0 0 6 2 0 0 0 1) (0 0 0 0 0 0 0 0 0) (0 1 3 0 5 8 0 2 0) (0 0 0 0 1 3 7 0 0) (0 0 5 7 4 6 2 0 0) (0 0 4 5 9 0 0 0 0) (0 2 0 1 7 0 9 4 0) (0 0 0 0 0 0 0 0 0) (5 0 0 0 8 9 0 0 6))
		       ((0 6 0 0 0 4 1 9 3) (0 0 0 0 0 1 0 8 0) (0 1 7 0 5 0 0 0 0) (0 7 6 4 2 0 0 0 0) (2 0 0 0 1 0 0 0 6) (0 0 0 0 8 7 9 4 0) (0 0 0 0 3 0 7 5 0) (0 2 0 1 0 0 0 0 0) (8 5 1 7 0 0 0 3 0))
		       ((0 0 2 0 0 7 0 8 0) (6 0 9 1 0 5 0 0 2) (0 0 0 0 6 0 1 3 0) (2 0 1 3 0 0 0 0 0) (0 0 4 0 0 0 3 0 0) (0 0 0 0 0 6 7 0 8) (0 1 3 0 9 0 0 0 0) (9 0 0 4 0 3 8 0 7) (0 2 0 7 0 0 6 0 0))
		       ((7 0 0 5 9 0 1 0 0) (2 8 0 0 0 0 9 0 0) (0 9 0 4 2 0 0 0 0) (0 0 0 0 0 0 6 5 7) (0 7 0 0 0 0 0 8 0) (4 5 6 0 0 0 0 0 0) (0 0 0 0 8 7 0 2 0) (0 0 7 0 0 0 0 1 5) (0 0 9 0 5 2 0 0 3))
		       ((0 0 1 3 8 0 0 7 0) (0 0 0 0 0 0 0 0 0) (2 7 6 0 0 1 0 9 0) (6 0 0 0 0 0 7 5 0) (7 0 2 0 3 0 8 0 1) (0 3 5 0 0 0 0 0 6) (0 4 0 8 0 0 6 3 7) (0 0 0 0 0 0 0 0 0) (0 6 0 0 7 3 4 0 0))
		       ((0 0 0 7 0 0 0 0 0) (0 8 5 0 0 0 0 0 0) (7 1 2 9 3 0 0 0 0) (8 2 0 3 0 0 0 7 0) (0 7 1 4 5 9 3 8 0) (0 4 0 0 0 7 0 9 1) (0 0 0 0 4 1 7 3 9) (0 0 0 0 0 0 5 2 0) (0 0 0 0 0 3 0 0 0))
		       ((5 8 0 0 0 9 0 0 7) (0 0 3 8 4 0 0 0 6) (0 0 0 1 0 0 4 0 0) (8 0 0 3 6 0 0 2 0) (0 0 7 0 1 0 9 0 0) (0 1 0 0 7 8 0 0 4) (0 0 9 0 0 1 0 0 0) (2 0 0 0 5 3 6 0 0) (7 0 0 2 0 0 0 4 5))
		       ((0 0 0 2 7 9 0 4 0) (0 0 0 0 1 0 0 0 6) (0 7 0 0 6 0 3 2 0) (0 0 1 0 0 0 0 0 0) (0 8 6 1 9 7 2 5 0) (0 0 0 0 0 0 9 0 0) (0 6 5 0 4 0 0 9 0) (7 0 0 0 2 0 0 0 0) (0 3 0 6 8 5 0 0 0))
		       ((0 0 8 4 7 9 0 0 0) (0 1 9 0 6 0 7 0 0) (6 0 0 0 3 0 0 0 0) (0 0 0 0 0 0 0 3 0) (0 9 5 7 4 3 2 6 0) (0 4 0 0 0 0 0 0 0) (0 0 0 0 9 0 0 0 7) (0 0 4 0 8 0 6 5 0) (0 0 0 5 2 6 1 0 0))
		       ((0 0 2 0 7 5 0 0 1) (0 0 0 0 0 0 0 0 0) (0 0 4 1 0 0 2 9 6) (2 0 3 0 0 0 0 6 0) (7 1 0 0 5 0 0 2 9) (0 6 0 0 0 0 5 0 3) (6 2 5 0 0 7 8 0 0) (0 0 0 0 0 0 0 0 0) (8 0 0 5 2 0 6 0 0))
		       ((0 0 0 2 0 6 0 5 1) (1 0 9 0 5 0 3 0 0) (6 0 0 0 0 0 8 0 0) (3 0 6 0 0 0 0 9 4) (0 9 0 0 0 0 0 8 0) (5 4 0 0 0 0 1 0 3) (0 0 2 0 0 0 0 0 9) (0 0 3 0 2 0 5 0 7) (9 6 0 4 0 1 0 0 0))
		       ((4 0 0 0 0 0 9 6 5) (6 7 0 0 0 0 0 0 0) (0 0 3 8 0 0 0 0 1) (0 4 0 2 0 7 0 0 0) (2 3 0 6 0 4 0 7 8) (0 0 0 5 0 3 0 2 0) (1 0 0 0 0 5 8 0 0) (0 0 0 0 0 0 0 5 2) (7 9 5 0 0 0 0 0 4))
		       ((0 0 0 0 0 0 4 0 0) (0 0 0 7 9 0 0 0 5) (7 0 0 0 6 0 0 1 9) (0 6 0 0 3 0 0 7 0) (1 8 9 0 7 0 2 3 4) (0 4 0 0 2 0 0 9 0) (4 5 0 0 8 0 0 0 3) (6 0 0 0 4 2 0 0 0) (0 0 3 0 0 0 0 0 0))
		       ((2 0 0 5 0 0 0 0 3) (3 0 0 0 8 1 0 6 0) (0 0 4 9 3 0 0 0 0) (0 0 2 0 0 0 3 0 0) (0 7 5 0 0 0 6 9 0) (0 0 1 0 0 0 2 0 0) (0 0 0 0 2 8 5 0 0) (0 1 0 7 4 0 0 0 2) (4 0 0 0 0 9 0 0 1))
		       ((7 0 0 2 0 0 0 0 0) (0 0 0 5 0 8 0 6 0) (4 0 0 0 3 0 0 0 7) (2 0 5 0 0 0 4 9 0) (3 0 4 0 9 0 5 0 2) (0 9 8 0 0 0 3 0 6) (8 0 0 0 1 0 0 0 3) (0 2 0 6 0 4 0 0 0) (0 0 0 0 0 7 0 0 8))
		       ((0 0 0 2 7 0 0 8 4) (2 1 0 3 0 0 0 9 0) (0 0 0 0 5 8 0 2 0) (0 5 0 0 0 0 0 0 9) (7 0 0 0 9 0 0 0 2) (9 0 0 0 0 0 0 7 0) (0 6 0 8 2 0 0 0 0) (0 2 0 0 0 9 0 4 8) (5 8 0 0 6 3 0 0 0))))

(setq easy-puzzles '(((8 0 0 5 0 6 9 4 0) (0 6 0 0 9 0 5 3 0) (0 0 5 7 0 4 0 0 6) (0 0 0 0 2 0 1 0 4) (0 0 0 1 0 8 0 0 0) (6 0 2 0 7 0 0 0 0) (9 0 0 2 0 7 6 0 0) (0 2 6 0 5 0 0 7 0) (0 7 8 6 0 3 0 0 1))
		     ((0 7 0 2 1 8 4 0 6) (0 0 0 5 0 4 0 0 0) (2 0 0 0 0 0 0 9 5) (4 0 8 6 5 0 3 0 7) (0 0 7 0 0 0 6 0 0) (6 0 1 0 8 7 2 0 9) (7 6 0 0 0 0 0 0 4) (0 0 0 4 0 6 0 0 0) (1 0 5 8 2 9 0 6 0))
		     ((0 0 0 0 4 0 0 0 0) (9 0 0 3 0 5 0 0 4) (3 0 7 8 0 2 1 0 6) (0 9 4 5 0 3 0 8 0) (0 0 2 0 8 0 3 0 0) (0 3 0 9 0 4 5 6 0) (7 0 1 2 0 8 4 0 3) (8 0 0 4 0 7 0 0 2) (0 0 0 0 6 0 0 0 0))
		     ((0 0 0 0 1 2 0 8 0) (0 4 0 7 9 0 0 0 6) (2 5 0 0 4 6 0 3 0) (8 0 0 0 0 0 9 0 0) (0 6 5 9 7 8 2 1 0) (0 0 2 0 0 0 0 0 8) (0 8 0 1 2 0 0 6 4) (9 0 0 0 5 3 0 7 0) (0 3 0 4 8 0 0 0 0))
		     ((6 0 0 1 9 3 0 0 8) (1 0 0 0 8 0 6 0 0) (0 0 0 6 0 0 0 4 9) (9 0 0 0 4 6 8 0 0) (2 0 6 0 0 0 5 0 4) (0 0 3 7 5 0 0 0 1) (3 5 0 0 0 2 0 0 0) (0 0 2 0 6 0 0 0 5) (8 0 0 4 3 5 0 0 2))
		     ((0 3 1 0 6 0 0 2 8) (0 0 0 0 3 4 0 0 0) (0 4 0 2 0 0 5 0 0) (0 0 7 0 5 0 0 9 1) (9 1 5 0 7 0 3 4 2) (4 6 0 0 2 0 8 0 0) (0 0 6 0 0 2 0 1 0) (0 0 0 5 4 0 0 0 0) (2 8 0 0 9 0 7 6 0))
		     ((4 0 0 0 0 0 1 0 0) (6 0 7 3 1 0 4 9 8) (0 9 0 8 0 0 0 0 0) (7 0 8 0 0 1 6 5 0) (0 0 3 4 0 7 8 0 0) (0 6 9 5 0 0 2 0 4) (0 0 0 0 0 5 0 2 0) (9 1 4 0 2 3 5 0 6) (0 0 5 0 0 0 0 0 7))
		     ((0 0 0 0 0 1 5 0 0) (0 3 0 0 2 7 0 9 0) (2 1 6 0 0 0 0 8 0) (0 0 7 0 0 4 0 3 8) (6 4 0 7 0 8 0 1 5) (9 5 0 3 0 0 6 0 0) (0 6 0 0 0 0 3 5 1) (0 8 0 1 3 0 0 7 0) (0 0 1 4 0 0 0 0 0))
		     ((0 0 2 9 8 0 0 1 0) (6 0 1 0 0 0 3 0 9) (0 0 9 0 0 6 4 8 0) (0 0 7 0 0 4 0 3 0) (8 3 0 1 0 2 0 9 4) (0 2 0 8 0 0 5 0 0) (0 6 8 5 0 0 9 0 0) (1 0 3 0 0 0 8 0 2) (0 9 0 0 6 8 1 0 0))
		     ((0 2 0 5 0 0 0 1 0) (8 0 4 0 6 1 0 0 7) (0 1 0 0 7 8 5 0 0) (0 0 9 8 0 0 0 0 5) (1 0 2 0 5 0 9 0 8) (5 0 0 0 0 7 4 0 0) (0 0 8 4 2 0 0 5 0) (6 0 0 7 3 0 2 0 4) (0 4 0 0 0 9 0 7 0))
		     ((9 0 0 0 4 1 0 3 0) (0 6 5 0 0 0 7 0 0) (3 2 0 0 5 0 9 0 0) (0 3 8 2 6 0 0 0 0) (6 0 9 0 7 0 3 0 4) (0 0 0 0 1 3 6 7 0) (0 0 6 0 9 0 0 1 3) (0 0 2 0 0 0 4 6 0) (0 4 0 7 8 0 0 0 9))
		     ((0 0 0 0 7 0 8 0 2) (0 0 5 9 0 2 0 3 0) (8 3 0 6 1 0 9 0 0) (5 0 6 0 0 8 0 0 1) (0 0 1 0 5 0 4 0 0) (2 0 0 3 0 0 5 0 9) (0 0 7 0 4 5 0 9 3) (0 5 0 7 0 9 6 0 0) (9 0 8 0 3 0 0 0 0))
		     ((0 3 0 0 0 8 0 0 0) (0 6 2 0 0 3 7 0 1) (0 9 7 0 1 0 0 8 4) (0 0 0 0 4 2 0 6 9) (0 8 0 1 0 5 0 3 0) (4 7 0 8 3 0 0 0 0) (9 4 0 0 2 0 1 5 0) (6 0 8 5 0 0 2 7 0) (0 0 0 3 0 0 0 4 0))
		     ((0 3 7 6 0 0 0 0 4) (0 0 5 7 2 9 6 0 0) (0 0 0 0 3 5 0 7 0) (0 0 0 0 1 6 7 0 9) (3 0 9 0 0 0 1 0 8) (6 0 4 8 9 0 0 0 0) (0 9 0 2 5 0 0 0 0) (0 0 3 1 7 4 9 0 0) (7 0 0 0 0 8 3 1 0))
		     ((0 0 5 8 9 0 0 0 0) (0 0 4 2 7 0 6 0 8) (2 0 0 0 1 3 7 0 0) (0 1 0 0 0 0 0 0 5) (0 8 9 5 3 1 2 6 0) (5 0 0 0 0 0 0 8 0) (0 0 3 4 6 0 0 0 1) (7 0 2 0 8 9 5 0 0) (0 0 0 0 5 7 4 0 0))
		     ((0 0 1 8 0 0 0 6 0) (0 0 2 0 0 3 7 0 4) (9 0 3 7 2 6 8 0 0) (0 0 0 9 4 0 5 8 0) (0 0 9 0 0 0 6 0 0) (0 2 5 0 3 8 0 0 0) (0 0 4 2 5 1 3 0 8) (3 0 7 4 0 0 1 0 0) (0 1 0 0 0 7 9 0 0))
		     ((0 9 2 0 4 0 0 0 5) (0 0 0 0 0 7 3 0 0) (0 4 3 0 0 6 0 8 9) (0 0 0 1 2 0 4 5 0) (0 1 0 4 6 5 0 7 0) (0 5 4 0 7 3 0 0 0) (9 2 0 5 0 0 8 4 0) (0 0 6 7 0 0 0 0 0) (4 0 0 0 8 0 9 1 0))
		     ((0 4 0 3 0 8 1 0 9) (8 0 0 1 0 5 0 7 0) (0 0 1 0 6 0 5 0 8) (0 0 0 0 1 0 0 8 5) (0 0 0 9 0 4 0 0 0) (4 2 0 0 5 0 0 0 0) (6 0 3 0 7 0 8 0 0) (0 8 0 2 0 1 0 0 6) (7 0 2 8 0 6 0 9 0))
		     ((2 0 4 0 0 0 0 0 6) (0 3 0 0 1 8 9 0 0) (0 9 7 0 2 0 0 0 3) (5 0 9 7 4 0 0 0 0) (3 4 0 0 6 0 0 1 9) (0 0 0 0 8 9 6 0 4) (4 0 0 0 3 0 8 9 0) (0 0 1 6 5 0 0 3 0) (7 0 0 0 0 0 4 0 1))
		     ((0 0 7 0 9 8 0 4 0) (8 0 0 7 0 0 9 3 6) (0 9 0 1 0 6 2 0 0) (4 2 0 0 0 0 1 0 0) (0 0 6 0 0 0 7 0 0) (0 0 9 0 0 0 0 8 5) (0 0 8 4 0 1 0 5 0) (1 3 2 0 0 5 0 0 7) (0 5 0 6 7 0 8 0 0))
		     ((0 0 6 5 4 0 0 9 0) (0 4 0 2 0 0 8 0 0) (0 0 5 0 9 0 2 0 4) (1 2 0 6 0 0 0 8 0) (0 0 9 1 0 2 7 0 0) (0 8 0 0 0 5 0 3 2) (4 0 8 0 1 0 9 0 0) (0 0 1 0 0 7 0 5 0) (0 6 0 0 5 9 4 0 0))
		     ((7 6 8 0 0 9 0 0 0) (0 1 4 0 0 5 0 7 0) (5 0 0 6 0 0 0 0 4) (1 0 3 0 2 0 0 0 9) (0 0 5 0 4 0 3 0 0) (2 0 0 0 9 0 1 0 7) (3 0 0 0 0 4 0 0 6) (0 8 0 1 0 0 4 9 0) (0 0 0 3 0 0 7 5 1))
		     ((0 3 0 1 0 2 0 6 0) (0 0 0 8 0 4 7 0 1) (1 5 0 0 0 7 4 0 0) (0 2 0 0 0 8 0 0 5) (7 8 0 0 0 0 0 1 3) (3 0 0 6 0 0 0 8 0) (0 0 6 5 0 0 0 9 2) (5 0 2 4 0 9 0 0 0) (0 7 0 2 0 6 0 5 0))
		     ((0 5 0 0 0 4 8 1 0) (0 0 7 3 8 0 4 0 2) (0 0 0 0 5 6 0 0 3) (0 0 9 4 0 5 2 0 7) (0 0 0 0 2 0 0 0 0) (4 0 6 7 0 3 1 0 0) (5 0 0 8 4 0 0 0 0) (7 0 4 0 6 9 5 0 0) (0 1 8 5 0 0 0 9 0))
		     ((2 0 0 0 0 9 0 1 3) (7 5 3 2 1 8 6 0 0) (0 1 0 0 5 0 0 0 0) (0 0 0 0 9 0 0 0 5) (0 9 7 1 0 3 4 8 0) (3 0 0 0 8 0 0 0 0) (0 0 0 0 4 0 0 6 0) (0 0 8 5 6 1 3 7 4) (6 7 0 9 0 0 0 0 8))
		     ((3 0 8 1 4 0 6 9 0) (0 0 0 0 0 0 0 7 0) (0 0 0 0 8 9 0 0 4) (0 8 3 0 0 5 2 0 0) (6 0 1 2 0 7 9 0 8) (0 0 9 8 0 0 7 4 0) (8 0 0 6 7 0 0 0 0) (0 2 0 0 0 0 0 0 0) (0 1 6 0 2 3 4 0 7))
		     ((6 0 0 4 9 0 2 0 3) (0 9 0 0 0 0 6 0 0) (0 0 2 7 6 0 0 0 0) (5 1 0 8 0 0 7 0 2) (2 3 0 9 0 5 0 6 1) (4 0 8 0 0 7 0 3 9) (0 0 0 0 8 9 4 0 0) (0 0 5 0 0 0 0 2 0) (9 0 6 0 5 2 0 0 7))
		     ((0 0 0 5 0 0 6 0 0) (5 0 7 1 4 6 0 0 0) (0 6 0 0 9 0 2 0 4) (7 0 2 3 6 0 0 0 8) (6 0 4 0 0 0 7 0 3) (9 0 0 0 2 7 4 0 1) (8 0 6 0 1 0 0 4 0) (0 0 0 4 3 8 9 0 6) (0 0 3 0 0 5 0 0 0))
		     ((0 0 0 0 0 3 5 0 7) (4 0 1 0 0 0 9 0 0) (7 2 5 0 9 4 0 0 3) (0 0 0 0 8 9 6 2 0) (8 0 0 3 0 2 0 0 1) (0 9 2 1 6 0 0 0 0) (9 0 0 5 2 0 4 3 6) (0 0 4 0 0 0 1 0 8) (3 0 8 7 0 0 0 0 0))
		     ((0 0 0 0 8 0 0 0 0) (1 0 0 2 0 3 0 0 8) (2 0 5 6 0 4 7 0 9) (0 1 8 3 0 2 0 6 0) (0 0 4 0 6 0 2 0 0) (0 2 0 1 0 8 3 9 0) (5 0 7 4 0 6 8 0 2) (6 0 0 8 0 5 0 0 4) (0 0 0 0 9 0 0 0 0))
		     ((0 5 3 1 8 0 6 0 0) (0 7 0 0 6 3 0 0 5) (0 0 0 0 0 0 0 4 1) (8 0 7 0 0 4 1 5 2) (0 0 0 0 0 0 0 0 0) (2 6 1 8 0 0 4 0 9) (6 2 0 0 0 0 0 0 0) (7 0 0 5 3 0 0 1 0) (0 0 4 0 7 8 5 9 0))
		     ((4 0 8 3 0 0 5 0 0) (0 0 3 1 6 5 4 0 2) (0 5 0 0 0 8 9 0 0) (0 1 6 0 4 2 0 0 0) (0 0 9 0 0 0 7 0 0) (0 0 0 9 3 0 6 2 0) (0 0 5 2 0 0 0 7 0) (9 0 4 8 1 7 2 0 0) (0 0 1 0 0 4 8 0 3))
		     ((3 0 6 0 0 0 7 0 9) (0 0 0 7 0 9 0 0 0) (0 0 8 0 1 2 6 0 0) (2 0 1 9 0 5 3 0 7) (0 4 0 0 3 0 0 2 0) (6 0 3 2 0 7 1 0 5) (0 0 2 5 7 0 8 0 0) (0 0 0 4 0 3 0 0 0) (1 0 7 0 0 0 4 0 2))
		     ((0 0 3 8 9 0 0 4 5) (0 4 0 0 0 0 0 0 0) (0 0 2 3 4 1 0 7 0) (2 0 6 5 0 0 9 0 0) (0 1 9 4 0 8 5 6 0) (0 0 4 0 0 3 7 0 2) (0 9 0 7 5 6 4 0 0) (0 0 0 0 0 0 0 9 0) (4 8 0 0 3 9 1 0 0))
		     ((0 1 0 7 0 9 0 0 8) (6 0 0 3 8 0 0 2 0) (4 8 7 0 0 2 3 0 0) (0 9 0 0 0 0 6 0 1) (0 2 0 0 0 0 0 7 0) (3 0 5 0 0 0 0 8 0) (0 0 2 5 0 0 9 1 4) (0 3 0 0 2 7 0 0 5) (5 0 0 9 0 6 0 3 0))
		     ((0 5 7 0 0 0 1 0 0) (0 3 0 5 1 4 7 0 0) (2 0 6 0 0 3 0 0 4) (0 0 3 0 9 0 0 0 5) (8 9 0 0 0 0 0 4 7) (5 0 0 0 4 0 2 0 0) (6 0 0 4 0 0 5 0 1) (0 0 5 6 8 9 0 2 0) (0 0 4 0 0 0 8 9 0))
		     ((2 0 3 8 5 0 0 0 0) (0 5 0 0 7 0 0 6 0) (7 8 0 0 0 9 3 2 5) (1 0 0 7 6 0 2 0 0) (0 0 2 0 0 0 1 0 0) (0 0 5 0 3 2 0 0 9) (5 3 6 4 0 0 0 8 2) (0 1 0 0 2 0 0 3 0) (0 0 0 0 8 6 5 0 1))
		     ((0 0 8 1 0 0 0 2 7) (9 0 0 5 0 2 0 0 6) (0 2 1 8 0 3 0 0 0) (0 7 0 3 0 0 0 0 5) (2 6 0 0 0 0 0 1 3) (3 0 0 0 0 9 0 6 0) (0 0 0 4 0 8 5 7 0) (7 0 0 9 0 5 0 0 1) (4 5 0 0 0 7 9 0 0))
		     ((0 5 0 0 1 0 0 0 0) (3 1 8 7 5 2 9 0 0) (7 0 0 0 0 4 0 5 8) (0 0 0 0 4 0 0 0 1) (0 4 3 5 0 8 6 2 0) (8 0 0 0 2 0 0 0 0) (9 3 0 4 0 0 0 0 2) (0 0 2 1 9 5 8 3 6) (0 0 0 0 6 0 0 9 0))
		     ((4 0 0 0 1 2 6 0 0) (0 2 1 0 5 9 0 7 0) (9 0 0 8 0 0 0 0 2) (0 0 2 9 0 0 0 4 0) (0 6 8 0 4 0 1 3 0) (0 4 0 0 0 6 8 0 0) (3 0 0 0 0 4 0 0 1) (0 9 0 3 7 0 2 6 0) (0 0 4 6 9 0 0 0 3))
		     ((7 1 8 0 9 6 0 3 0) (3 9 5 0 0 0 6 0 0) (0 0 6 0 0 1 9 0 0) (0 3 0 8 6 0 0 0 0) (0 0 0 7 0 2 0 0 0) (0 0 0 0 4 5 0 6 0) (0 0 7 9 0 0 2 0 0) (0 0 9 0 0 0 7 1 4) (0 4 0 1 7 0 5 9 6))
		     ((2 5 0 1 7 0 0 0 0) (7 0 0 0 0 4 9 0 5) (0 3 4 0 0 9 1 0 7) (0 4 0 0 0 2 0 7 1) (6 0 0 0 0 0 0 0 3) (9 2 0 6 0 0 0 4 0) (4 0 3 9 0 0 8 6 0) (5 0 6 7 0 0 0 0 4) (0 0 0 0 4 6 0 5 9))
		     ((7 0 5 6 0 0 0 4 2) (0 0 0 7 0 0 6 0 0) (0 0 3 0 0 4 0 7 5) (0 3 0 0 0 0 1 5 8) (0 1 6 0 0 0 4 2 0) (4 5 7 0 0 0 0 6 0) (6 2 0 9 0 0 5 0 0) (0 0 9 0 0 8 0 0 0) (5 8 0 0 0 6 7 0 9))
		     ((7 0 0 0 0 6 0 0 0) (0 2 5 0 0 7 6 0 3) (0 6 3 2 0 0 0 0 9) (1 3 4 0 0 0 0 9 0) (2 5 0 0 0 0 0 1 7) (0 7 0 0 0 0 2 3 6) (3 0 0 0 0 8 7 5 0) (6 0 8 7 0 0 3 4 0) (0 0 0 4 0 0 0 0 8))
		     ((0 0 2 0 6 7 0 8 0) (0 3 0 9 1 0 0 2 0) (8 0 0 0 0 0 1 0 6) (0 5 8 0 0 2 0 0 9) (0 4 6 0 9 0 5 3 0) (7 0 0 5 0 0 2 1 0) (5 0 9 0 0 0 0 0 1) (0 8 0 0 2 9 0 5 0) (0 6 0 8 7 0 9 0 0))
		     ((3 0 0 2 5 0 1 0 4) (0 2 0 0 0 9 0 6 0) (0 0 9 4 3 0 0 2 0) (9 0 0 0 0 4 8 0 0) (4 0 8 0 9 0 6 0 2) (0 0 1 3 0 0 0 0 9) (0 9 0 0 6 1 4 0 0) (0 3 0 8 0 0 0 1 0) (1 0 6 0 7 3 0 0 5))
		     ((0 0 0 0 7 0 6 0 0) (6 0 0 8 0 3 2 1 0) (4 0 0 1 0 2 0 0 0) (2 0 5 3 0 0 0 4 0) (0 4 6 0 8 0 5 3 0) (0 8 0 0 0 5 1 0 6) (0 0 0 9 0 8 0 0 1) (0 9 3 4 0 1 0 0 7) (0 0 4 0 5 0 0 0 0))
		     ((6 7 0 2 0 0 8 0 5) (0 0 0 0 7 8 3 2 0) (8 3 0 4 0 0 0 7 0) (0 2 9 8 0 0 7 0 0) (0 8 0 0 0 0 0 6 0) (0 0 7 0 0 9 4 1 0) (0 4 0 0 0 7 0 3 2) (0 9 3 1 4 0 0 0 0) (7 0 6 0 0 2 0 4 1))
		     ((3 7 0 0 9 8 0 0 4) (4 0 9 0 5 7 0 0 1) (0 1 0 3 0 0 7 0 0) (0 0 0 0 2 5 9 8 0) (0 0 0 0 0 0 0 0 0) (0 2 7 9 8 0 0 0 0) (0 0 6 0 0 1 0 4 0) (8 0 0 2 6 0 1 0 7) (7 0 0 8 3 0 0 5 2))
		     ((6 8 0 7 0 0 9 0 5) (0 0 9 4 0 0 0 0 0) (0 1 0 0 5 0 3 0 8) (1 0 5 0 3 2 0 0 0) (4 0 0 1 7 5 0 0 2) (0 0 0 9 4 0 5 0 1) (2 0 8 0 6 0 0 5 0) (0 0 0 0 0 4 7 0 0) (5 0 6 0 0 1 0 8 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'sudoku)

;;; sudoku.el -- Simple sudoku game, can download puzzles from the web.

;; Filename: sudoku.el
;; Copyright (C) 2005 Jesse Rosenthal
;;           (C) 2019 Roman Funk
;; Author: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;;         Roman Funk  <roman.funk66@gmail.com>
;; Maintainer: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;;             Roman Funk  <roman.funk66@gmail.com>
;; Created: 29 Oct 2005
;; Description: Uses either local or downloaded sudoku for a simple puzzle game
;; Version 0.3.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; `sudoku-mode' is a major mode for solving sudoku puzzles. The rules
;; are simple: only one of each number in each of the nine rows,
;; columns, and subsquares. It has a few nifty features, the most
;; notable being that you can download puzzles on the fly from
;; websudoku.com (there are parsing functions to take care of the
;; html). The 200 included puzzles were generated using the
;; python-based generator from Thomas Hinkle's gnome-sudoku
;; (http://gnome-sudoku.sourceforge.net).
;;
;; I've added customization options, You can also now customize via
;; the dropdown menu. If you really want to write in to your .emacs
;; file, though, the three variables are `sudoku-level'
;; ({"easy"|"medium"|"hard"|"evil"}, `sudoku-download' (boolean), and
;; `sudoku-download-method'
;; ({"lynx"|"wget"|"w3"|"native-url-lib"}). But you can do all this
;; interactively. The only thing you need to add is:
;;
;; (require 'sudoku)
;;
;; UPDATE: Downloading files no longer requires lynx! This now offers
;; four options: lynx, wget, the native url library in emacs >=22, and
;; w3 (which seems to be largely obsolete). This is set through the
;; `sudoku-download-method' variable, which is also available through
;; the configuration options. The default is to use "native-url-lib"
;; if gnu emacs is above version 22, and lynx otherwise. If anyone has
;; any suggestions for why another option should be the default,
;; please let me know.
;;
;;
;; The defaults are for `sudoku-level' to be easy and
;; `sudoku-download' to be nil. But there are only about fifty puzzles
;; of each level included, so the chances of you repeating one are
;; pretty good. You're probably better off setting download on, if
;; you're online.

;;; TODO:
;;  - Add more information about current settings onscreen
;;  - Add color?
;;  - Add an sudoku editor to put puzzles from elsewhere into emacs
;;  - Hide the variables current-board und start-board

;;; Code:
