(defun slither()
 "Game definition"
  (format t "Welcome to Slither~&")
  (format t "Objective is to find a single looped path in the given grid with no crosses or branches~&")
  (format t "Tips - 
   1. Game is played on a 5x5 to 25x30 grid with \"+\" at each of the grid 
      corners.
   2. Some of the square contain a number between 0 and 3 inclusively.
   3. Number indicates number of line that will surround the particular square ~&")
   (format t "Game instructions-
   1. There is only one solution.
   2. You must specify each move when prompted in the form of triple. 
       Example shown below.
   	   \"2 1 t\" indicates the box at second row first column starting from 
	   upper leftmost corner without any quotes.
	   \"t-Top, b-Bottom, l-Left, r-Right\"
   3. If you enter a move where there is a line already then the line is removed.") (terpri)

 (loop while (y-or-n-p "Do you wish to play slither?") 
     do (if (y-or-n-p "Load game automatically?(You can choose to load from a filename later)")
            (progn
              (format t "Loading File") (terpri)
              (read-board "testboard.txt") (terpri)
			  (format t "File Loaded") (terpri)
			  (if (y-or-n-p "if you wish to play manually press y, Press n if you wish computer to solve")
			  (progn 
	          (game-in-progress brd))
			  (progn 
			  (solve brd))))
            (progn
              (format t "Please enter a file name") (terpri)
              (format t "(no quotes, e.g. game1.txt not \"game1.txt\" or enter path name like C:/Board1.txt and not \"C:/Board1.txt\"") (terpri)
			  (read-board (filenameloop)) (terpri)
			  (format t "File Loaded")  
			  (if (y-or-n-p "if you wish to play manually press y, Press n if you wish computer to solve")
			  (progn 
	          (game-in-progress brd))
			  (progn 
			  (solve brd)))))))
        

(defun game-in-progress (brd) 
		(terpri) (format t "Starting Game~&")
		(grid-order brd) 
		(setq initialbrd (make-board-list-prepare (+ 1(* 2 grid-row)))) 
		(make-board-list initialbrd brd)
		(setq list-row (* 2 grid-row)) 
		(setq list-col (* 2 grid-col))
		(setq moves-list nil)
		(colprint) (terpri)
		(print-board-list initialbrd)
		;;print board before 
		(let ((move nil))
		(loop for move = (move-from-user)
			do (colprint) (terpri)
			   (print-board-list move)
			   (if (closedlooptest move) 
			   (progn (terpri)(format t "You Win!~&") (return))))) (terpri) (princ "List of moves") (terpri) (princ moves-list))
		

;;Reading file function
(defun filenameloop () (loop for file = (read-line *query-io*) until (probe-file file) do (format t "Incorrect file name") (terpri) finally (return file)))

(defun read-board (pathname) (defparameter *s* (open pathname)) (setq brd (read *s*)))

;;Defining the order of list given
(defun grid-order (lst) (setq grid-row (list-length lst)) (setq grid-col (list-length (car lst))))

;;make-board-list for initial list with no values in it
(defun make-board-list-prepare (y) (cond ((zerop y) NIL) (t (append (list (make-board-set-rows (+ 1(* 2 grid-col)))) (make-board-list-prepare (- y 1))))))

(defun make-board-set-rows (x) (cond ((zerop x) NIL) (t (append (list NIL) (make-board-set-rows (- x 1))))))

;; plus function changed to grid-col to support any grid format
(defun plus-row (row) (loop for x from 0 to (- (list-length row) 1) do (if (evenp x) (setf (elt row x) '+))) row)

;;setting face value in list structure
(defun face-row (row lst) (cond ((null lst))(t (setf (elt row 1) (car lst)) (face-row (cddr row) (cdr lst)))) row)

;;initial board preparation

(defun make-board-list (initialbrd brd) (loop for x from 0 to (- (list-length initialbrd) 1) do (if (evenp x) (setf (elt initialbrd x) (plus-row (elt initialbrd x))) (progn (setq y (/ (- x 1) 2)) (if (< y (list-length brd)) (setf (elt initialbrd x)(face-row (elt initialbrd x) (elt brd y))))))) initialbrd) 

;;Print-board-function
(defun spacing () (princ " ")) 

(defun colprint () (setq z grid-col) (terpri) (spacing) (let ((x 1)) (loop (when (> x z) (return)) (spacing) (princ x) (incf x))))

(defun rowprint () (princ a) (incf a)) 

(defun print-board-list (initialbrd) (setq a 1) (loop for x from 0 to (- (list-length initialbrd) 1) do (if (evenp x) (progn (terpri) (princ " ") (print-board-row (elt initialbrd x))) (progn (terpri) (rowprint) (print-board-row (elt initialbrd x))))))

;;(defun print-board-list (initialbrd) (cond ((null initialbrd) NIL) (t (print-board-row (car initialbrd)) (terpri) (print-board-list (cdr initialbrd)))))

(defun print-board-row (row) (cond ((null row) NIL) (t (if (null (car row)) (princ " ") (princ (car row))) (print-board-row (cdr row)))))

;;Take input from user

(defun move-from-user () (terpri) (setq move1 nil) (format *query-io* "Format of  input should be triple like  \"2 1 t\" (without quotes and space after each letter) indicating the box at second row first column starting from upper leftmost corner") (terpri) (format *query-io* "Please enter move and press enter") (terpri) (push (read *query-io*) move1) (push (read *query-io*) move1)  (push (read *query-io*) move1) (setq move (reverse move1)) (if (isvalidmove move) (progn (princ "X=") (if (find move moves-list :test #'equal) (setf moves-list (delete move moves-list :test #'equal)) (push move moves-list)) (move-update-board initialbrd move)) (princ "Error: Out of index move")) initialbrd)


(defun isvalidmove (move) (let ((x (car move)) (y (cadr move))) (if (and (< x (+ 1 grid-row)) (>= x 0) (< y (+ 1 grid-col)) (>= y 0)) t NIL)))

;;Setting moves from user into list representation
(defun move-update-board (initialbrd move) (let ((x (car move)) (y (cadr move)) (z (caddr move))) (cond ((equal z 't) (setf (elt initialbrd (* 2(- x 1))) (evenrows (elt initialbrd (* 2 (- x 1))) (+ 1 (* 2 (- y 1)))))) ((equal z 'b) (setf (elt initialbrd (+ 2 (* 2(- x 1)))) (evenrows (elt initialbrd (+ 2 (* 2 (- x 1)))) (+ 1 (* 2 (- y 1)))))) ((equal z 'l) (setf (elt initialbrd (+ 1 (* 2 (- x 1)))) (oddrows (elt initialbrd (+ 1 (* 2 (- x 1)))) (* 2 (- y 1))))) ((equal z 'r) (setf (elt initialbrd (+ 1 (* 2 (- x 1)))) (oddrows (elt initialbrd (+ 1 (* 2 (- x 1)))) (+ 2 (* 2 (- y 1)))))) (t (terpri) (princ "Invalid move")))) initialbrd)

(defun evenrows (row ele) (let ((x ele)) (cond ((null(elt row x)) (setf (elt row x)  '-)) (t (setf (elt row x)  NIL)))) row)
(defun evenrows (row ele) (let ((x ele)) (cond ((null(elt row x)) (setf (elt row x)  '-)) (t (setf (elt row x)  NIL)))) row)

(defun oddrows (row ele) (let ((x ele)) (cond ((null(elt row x)) (setf (elt row x)  #\|)) (t (setf (elt row x)  NIL)))) row)

;;remove move from board and representation
(defun move-removefrom-board (initialbrd move) (let ((x (car move)) (y (cadr move)) (z (caddr move))) (cond ((equal z 't) (setf (elt initialbrd (* 2(- x 1))) (evenrows-remove (elt initialbrd (* 2 (- x 1))) (+ 1 (* 2 (- y 1)))))) ((equal z 'b) (setf (elt initialbrd (+ 2 (* 2(- x 1)))) (evenrows-remove (elt initialbrd (+ 2 (* 2 (- x 1)))) (+ 1 (* 2 (- y 1)))))) ((equal z 'l) (setf (elt initialbrd (+ 1 (* 2 (- x 1)))) (oddrows-remove (elt initialbrd (+ 1 (* 2 (- x 1)))) (* 2 (- y 1))))) ((equal z 'r) (setf (elt initialbrd (+ 1 (* 2 (- x 1)))) (oddrows-remove (elt initialbrd (+ 1 (* 2 (- x 1)))) (+ 2 (* 2 (- y 1)))))) (t (terpri) (princ "Invalid move")))) initialbrd)
 
(defun evenrows-remove (row ele) (let ((x ele)) (setf (elt row x)  NIL)) row)

(defun oddrows-remove (row ele) (let ((x ele)) (setf (elt row x)  NIL)) row)

;;row and column index start at 0 so list row and list-col set to these values

;;loop for every move made to check if loop is complete
(defun closedlooptest (initialbrd) (countedgesh initialbrd) (countedgesv initialbrd) (counttotallines initialbrd) (if (and (checkforevenrow row-counter) (checkforevencol row-counter1) (checktotallines row-counter2)) t))

;;checks for no.of horizontal lines in cols
(defun countedgesh (initialbrd) (setq row-counter NIL) (loop for y = 1 then (+ 2 y) do (if (<= y list-col) (countrow y)) until (> y list-col)) row-counter)

(defun countrow (y) (let ((counter 0)) (loop for x = 0 then (+ 2 x) do (if (<= x list-row) (progn (if (equal (sqr x y initialbrd) '-) (incf counter)))) until (> x list-row)) (push counter row-counter)))


;;for checking no. of vertical lines in horizontalrow
(defun countedgesv (initialbrd) (setq row-counter1 NIL) (loop for y = 1 then (+ 2 y) do (if (<= y list-row) (countcol y)) until (> y list-row)) row-counter1)

(defun countcol (y) (let ((counter 0)) (loop for x = 0 then (+ 2 x) do (if (<= x list-col) (progn (if (equal (sqr y x initialbrd) #\|) (incf counter)))) until (> x list-col)) (push counter row-counter1)))


;;counting the matching of edges and number placed in middle
(defun counttotallines (initialbrd) (setq row-counter2 NIL) (loop for y = 1 then (+ 2 y) do (if (<= y list-row) (countlines y)) until (> y list-row)) row-counter2)

(defun countlines (y) (loop for x = 1 then (+ 2 x) do (if (<= x list-col) (progn (if (null (sqr y x initialbrd)) (push 1 row-counter2) (countforsquares (sqr y x initialbrd) y x)))) until (> x list-col)) row-counter2)

(defun countforsquares (val y x) (let ((counter 0)) (if (equal (sqr y (- x 1) initialbrd) #\|) (incf counter)) (if (equal (sqr y (+ x 1) initialbrd) #\|) (incf counter)) (if (equal (sqr (+ y 1) x initialbrd) '-) (incf counter)) (if (equal (sqr (- y 1) x initialbrd) '-) (incf counter)) (if (= counter val) (push 1 row-counter2) (push 0 row-counter2))))

;;results for checking
(defun checkforevenrow (row-counter) (cond ((null row-counter) t) ((evenp (car row-counter)) (checkforevenrow (cdr row-counter))) (t NIL)))

(defun checkforevencol (row-counter1) (cond ((null row-counter1) t) ((evenp (car row-counter1)) (checkforevenrow (cdr row-counter1))) (t NIL)))

(defun checktotallines (row-counter2) (cond ((null row-counter2) t) ((equal (car row-counter2) 1) (checktotallines (cdr row-counter2))) ((equal (car row-counter2) 0) NIL)))

;;Access row and column in list
(defun sqr (row col brd) (nth col (nth row brd)))
;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(defun solve (brd) 

(grid-order brd) 

(setq initialbrd (make-board-list-prepare (+ 1(* 2 grid-row)))) 

(make-board-list initialbrd brd)

(colprint) (terpri)

(print-board-list initialbrd)

(terpri)
(princ "Press any key and then press enter")

(read)

(setq list-row (* 2 grid-row))

(setq list-col (* 2 grid-col))

(check3 initialbrd) 

(if (null sourcecounter) (check2 initialbrd))

(if (null sourcecounter) (check1 initialbrd))

(setq si (-(car sourcecounter) 1)) 

(setq sj (-(cadr sourcecounter) 1))

(setq start (list si sj))

(setq nodepath nil)

(setq edge-list nil)

(setq edge-list2 nil)

(solvetime si sj)

(colprint) (terpri)

(print-board-list initialbrd) (terpri) 

(convertedgelist edge-list)

(terpri)

(setq edge-list2 (reverse edge-list2))

(princ edge-list2))



(defun solvetime (si sj)
(let ((run1 (get-internal-run-time)))
	    
		(processnode si sj NIL)
		 

(let ((run2 (get-internal-run-time)))
	 (format t "  ~f seconds of run time~%"
	 (/ (- run2 run1) internal-time-units-per-second)))))


(defun processnode (i j neighbor-list) 
      (cond ((isloop i j) (cond ((isvalidloop) (return-from processnode 1)) (t (return-from processnode 0))))
	         (t (cond ((inpath1 i j) (terpri) (return-from processnode 0)) (t (placenode i j) 
			                           (push (append (list i) (list (+ j 2))) neighbor-list)
									   (push (append (list i) (list (- j 2))) neighbor-list)
									   (push (append (list (+ i 2)) (list j)) neighbor-list)
									   (push (append (list (- i 2)) (list j)) neighbor-list) 
									  
									   (loop for x from 0 to (- (list-length neighbor-list) 1) 
									        do (terpri) (cond ((isvalidnbr (elt neighbor-list x)) 
																  (cond ((equal start (elt neighbor-list x)) 
																			(cond ((check-edge (elt neighbor-list x) i j) (placeedge start i j)
																			               (cond ((isvalidloop) (return-from processnode 1))
										                                                         (t (remove-edge start i j))))
										                                           (t (cond ((isvalidloop) (return-from processnode 1))
										                                                    (t nil)))))
									                                     (t (cond ((inpath2 (elt neighbor-list x)) nil) 
																	               (t (placeedge (elt neighbor-list x) i j) (setq result (processnode (car (elt neighbor-list x)) (cadr (elt neighbor-list x)) NIL)) 
																			           (cond ((checkresult result) (return-from processnode 1))
													                                         (t (remove-edge (elt neighbor-list x) i j)) ) ) )) ))
														(t nil) )) 
									     (removenode i j) (return-from processnode 0))))))
  
	
	
(defun checkresult (x) (if (= x 1) t NIL)) 
	
(defun isloop (i j) (if (> (list-length nodepath) 0) (progn (if (equal (list i j) start) t NIL)) NIL))

(defun isvalidloop () (if (closedlooptest initialbrd) t NIL))

(defun inpath1 (i j) (if (find (list i j) nodepath :test #'equal) t NIL))

(defun placenode (i j) (push (list i j) nodepath))

(defun removenode (i j) (setf nodepath (delete (list i j) nodepath :test #'equal))) 

(defun callneighbor (i j neighbor-list)            (push (append (list i) (list (+ j 2))) neighbor-list)
												   (push (append (list i) (list (- j 2))) neighbor-list)
												   (push (append (list (+ i 2)) (list j)) neighbor-list)
												   (push (append (list (- i 2)) (list j)) neighbor-list))
												   
												  
												   
(defun isvalidnbr (list) (let ((x (car list)) (y (cadr list))) (if (and (< x (+ 1 list-row)) (>= x 0) (< y (+ 1 list-col)) (>= y 0)) t NIL)))

(defun inpath2 (nbr) (if (find nbr nodepath :test #'equal) t NIL)) 

(defun placeedge (list i j) (let ((x (car list)) (y (cadr list))) (cond ((and (= (- i x) 2) (= (- j y) 0))  (place-odd-edge (- i 1) j)) ((and (= (- i x) -2) (= (- j y) 0)) (place-odd-edge (+ i 1) j)) ((and (= (- j y) 2) (= (- i x) 0)) (place-even-edge i (- j 1))) ((and (= (- j y) -2) (= (- i x) 0)) (place-even-edge i (+ j 1))) (t (princ "node not found")))))

(defun remove-edge (list i j)  (let ((x (car list)) (y (cadr list))) (cond ((and (= (- i x) 2 ) (= (- j y) 0)) (remove-edge-list (- i 1) j)) ((and (= (- i x) -2) (= (- j y) 0)) (remove-edge-list (+ i 1) j)) ((and (= (- j y) 2) (= (- i x) 0)) (remove-edge-list i (- j 1))) ((and (= (- j y) -2) (= (- i x) 0)) (remove-edge-list i (+ j 1))) (t (princ "node not found")))))

(defun check-edge (list i j)  (let ((x (car list)) (y (cadr list))) (cond ((and (= (- i x) 2) (= (- j y) 0)) (check-edge-list (- i 1) j)) ((and (= (- i x) -2) (= (- j y) 0)) (check-edge-list (+ i 1) j)) ((and (= (- j y) 2) (= (- i x) 0)) (check-edge-list i (- j 1))) ((and (= (- j y) -2) (= (- i x) 0)) (check-edge-list i (+ j 1))) (t (princ "node not found")))))  

(defun check-edge-list (x y) (if (equal (elt (elt initialbrd x) y) NIL) t NIL))

(defun remove-edge-list (x  y) (setf (elt (elt initialbrd x) y) NIL) (setf edge-list (delete (list x y) edge-list :test #'equal)))

(defun place-even-edge (x y) (setf (elt (elt initialbrd x) y) '-) (push (list x y) edge-list))

(defun place-odd-edge (x y) (setf (elt (elt initialbrd x) y) #\|) (push (list x y) edge-list))

(defun check3 (initialbrd) (setq sourcecounter nil) (loop for x from 0 to (- (list-length initialbrd) 1) do (if (check31 (elt initialbrd x)) (push x sourcecounter))) sourcecounter)

(defun check31 (list) (loop for x from 0 to (- (list-length list) 1) do (if (equal (elt list x) 3) (progn (push x sourcecounter)(return t)))))

(defun check2 (initialbrd) (setq sourcecounter nil) (loop for x from 0 to (- (list-length initialbrd) 1) do (if (check21 (elt initialbrd x)) (push x sourcecounter)))sourcecounter)

(defun check21 (list) (loop for x from 0 to (- (list-length list) 1) do (if (equal (elt list x) 2) (progn (push x sourcecounter)(return t)))))

(defun check1 (initialbrd) (setq sourcecounter nil) (loop for x from 0 to (- (list-length initialbrd) 1) do (if (check11 (elt initialbrd x)) (push x sourcecounter))) sourcecounter)

(defun check11 (list) (loop for x from 0 to (- (list-length list) 1) do (if (equal (elt list x) 1) (progn (push x sourcecounter)(return t)))))

(defun edgeconvert (list1) (let ((x (car list1)) (y (cadr list1)) (g 0) (h 0)) (cond ((evenp x) (setq g (+ 1 (/ x 2))) (setq h (+ 1 (/ (- y 1) 2))) (checknumbereven g h)) (t (setq g (+ 1 (/ (- x 1) 2))) (setq h (+ 1 (/ y 2))) (checknumberodd g h)))))

(defun convertedgelist (edge-list) (cond ((null edge-list) NIL) (t (edgeconvert (car edge-list)) (convertedgelist (cdr edge-list)))))

(defun checknumbereven (i j) (setq convertlist1 nil)(cond ((and (> i 0 ) (> j 0) (<= i grid-row) (<= j grid-col)) (push 'T convertlist1) (push j convertlist1) (push i convertlist1)) (t (push 'B convertlist1) (push j convertlist1) (push (- i 1) convertlist1))) (push convertlist1 edge-list2))

(defun checknumberodd (i j)(setq convertlist2 nil)(cond ((and (>  i 0) (> j 0) (<= i grid-row) (<= j grid-col)) (push 'L convertlist2) (push j convertlist2) (push i convertlist2)) (t (push 'R convertlist2) (push (- j 1) convertlist2) (push i convertlist2))) (push convertlist2 edge-list2))

