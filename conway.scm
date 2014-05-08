;; 'Scheme'-ish implementation of Conway's Game of Life
;; By Zachary Brown
;; 2014-May

(load "stdlib.scm")

;; Simple board for testing
;; All boards made of lists of zeros and ones
(define simple-board '((0 0 0) 
	               (1 1 1)
		       (0 0 0)))

;; Print the given board
(define (print-board board) 
	(cond ((not (null? board))
	   (map display-ln (map print-row board)))))

;; Print a single row of the board
(define (print-row row)
	(apply string-append 
	  (map (lambda (b)
		 (if (= b 1) "#" "-"))
		row)))

;; returns 0 if outside list bounds, otherwise value at position
;; uses indexing starting at 1
(define (valAt n lst)
  (cond ((> n (length lst)) 0)
	((< n 1) 0)
	((= n 1) (car lst))
	(else (valAt (- n 1) (cdr lst)))))

;; Find the value at position (x,y) in board
;; returns zero for invalid calls
(define (cell x y board)
  (if (list? (valAt y board))
      (valAt x (valAt y board))
      0))

;; Get number of neighbors around a position. 
;; Edges count as 0 neighbors (due to valAt function).
(define (neighbor-sum x y board)
  (+ (cell (- x 1) (- y 1) board)
     (cell (- x 1) y board)
     (cell (- x 1) (+ y 1) board)
     (cell x (- y 1) board)
     (cell x (+ y 1) board)
     (cell (+ x 1) (- y 1) board)
     (cell (+ x 1) y board)
     (cell (+ x 1) (+ y 1) board)))
     
;; Calculate the next state of a cell 
;; (This implements the rules of 'Life' Live on 2,3: Born on 3)
(define (next-cell x y board)
  (define current (cell x y board))
  (define n-sum (neighbor-sum x y board))
  (cond ((and (= current 1) (or (< n-sum 2) (> n-sum 3))) 0)
	((and (= current 0) (= n-sum 3)) 1)
	(else current)))

;; Calculate the next state of row n
;; To calculate a full row, call with output '()
(define (next-row n board output)
  (define width (length (car board)))
  (if (= (length output) width)
      output
      (next-row n board 
	  (cons (next-cell (- width (length output)) n board) output)))) 

(next-row 1 simple-board '())
