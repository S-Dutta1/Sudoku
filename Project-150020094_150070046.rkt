#lang racket/gui
(require 2htdp/image
         2htdp/universe)

(define f (new frame% [label "Sudoku-Solver"] ;;Define a new frame with the title as Sudoku Solver. Dimensions are specified.
                      [width 400]
                      [height 400]
                      [style '(no-resize-border)]))

(define undo-list '())
(define poss (list "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define f2 (new frame% [label "Sudoku-Solver"] ;; A duplicate frame with the title as Sudoku Solver. Same Dimensions.
                       [width 400]
                       [height 400]
                       [style '(no-resize-border)]))

(define panel (new horizontal-panel% [parent f]  ;; Defining panels inside the canvas which will help in hosting buttons upon the frame.
                                     [alignment '(center bottom)]
                                     [vert-margin 10]))

(define panel2 (new horizontal-panel% [parent f2]   ;; A duplicate panel for defining other auxiliary buttons inside the canvas.
                                     [alignment '(center bottom)]
                                     [vert-margin 10]))
(define msg (new message% [parent f]               ;; A window that will keep you updated with the status of the game, starting with wishing you all the best.
                          [label "    Best Of Luck     "]
                          [vert-margin 20]
                          ))

;;;;;;;;;;;;;;;;   GAMES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define board1   
  '((2 7 1 3 0 0 0 5 0)
    (6 0 9 2 5 0 0 1 0)
    (0 0 5 0 1 0 3 2 6)
    (3 9 0 0 4 5 2 0 0)
    (8 0 0 0 9 1 0 0 3)
    (0 0 0 0 3 0 8 0 5)
    (1 6 0 4 0 9 0 0 2)
    (0 2 7 0 0 0 0 0 0)
    (0 4 0 1 0 8 9 0 0)))
(define board2   
 '((0 0 0 0 1 0 0 3 0)
  (0 0 4 0 0 6 0 0 1)
  (8 0 0 4 9 0 0 2 0)
  (0 0 7 0 4 9 0 0 0)
  (0 3 0 0 0 0 0 0 0)
  (0 0 0 3 0 0 6 8 0)
  (0 0 6 9 0 2 0 4 0)
  (0 8 2 0 6 0 7 0 0)
  (0 0 0 0 7 0 0 0 0)))
(define board3   
  '((8 0 0 0 0 0 0 0 0)
    (0 0 3 6 0 0 0 0 0)
    (0 7 0 0 9 0 2 0 0)
    (0 5 0 0 0 7 0 0 0)
    (0 0 0 0 4 5 7 0 0)
    (0 0 0 1 0 0 0 3 0)
    (0 0 1 0 0 0 0 6 8)
    (0 0 8 5 0 0 0 1 0)
    (0 9 0 0 0 0 4 0 0)))
(define board4   
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 0 3)
    (4 0 0 8 0 3 0 0 1)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9)))
(define board5   
  '((0 4 9 0 0 3 0 7 0)
    (0 2 0 5 4 0 3 0 0)
    (0 0 0 8 9 0 0 4 0)
    (9 0 5 0 2 0 0 8 1)
    (0 0 0 0 0 0 0 0 0)
    (4 7 0 0 8 0 9 0 6)
    (0 8 0 0 7 2 0 0 0)
    (0 0 6 0 5 8 0 1 0)
    (0 5 0 1 0 0 8 6 0)))

;;randomly choose a game from the above 5 games
(define board
  (let* [(x (random))](cond ((< x 0.2) board1)
                    ((> x 0.8) board5)
                    ((and (> x 0.2) (< x 0.4)) board2)
                    ((and (> x 0.6) (< x 0.8)) board4)
                    (else board3))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;FIGURE;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fig s) (text/font s 12 "black" #f 'modern 'normal 'normal #f))  ;;specify the specs of the font being used
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   DRAWS SUDOKU   ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define WIDTH 350)
  (define HEIGHT 350)


; Comes up the initial defining of the canvas upon which we are going to draw the board and put up the initial configuration of the game.
(define grid-canvas%
  (class canvas%
    (inherit get-dc get-width get-height)
    (define/override (on-paint)
      (define dc (get-dc))
      (define w (get-width))
      (define h (get-height))
      (send dc set-pen "black" 3 'solid)
      (send dc erase)
      (for ([i (in-range 3)])
        (send dc draw-line 0 (* i (/ h 3)) w (* i (/ h 3)))
        (send dc draw-line (* i (/ w 3)) 0 (* i (/ w 3)) h))
      (send dc set-pen "black" 1 'solid)
      (for ([i (in-range 9)])
        (send dc draw-line 0 (* i (/ h 9)) w (* i (/ h 9))))
      (for ([i (in-range 9)])
        (send dc draw-line (* i (/ w 9)) 0 (* i (/ w 9)) h))
      (for ([i (in-range 9)])
        (for ([j (in-range 9)])
          (cond ((not (= (get board i j) 0))
                 (send dc draw-text (number->string (get board i j))  (+ (* 44 j) 17) (+ (* 39 i) 10))))))
      )
    
    ;;function to create a list of the possible numbres at that position
    
    (define (func i j)
      (define (fh l cnt)
        (if (eq? cnt 10) l
        (if (safe? copy-board i j cnt) (fh (cons (number->string cnt) l) (+ cnt 1)) (fh l (+ cnt 1)))))
      (fh `() 1))  

    ;; create radio box for user to select any of the valid numbers
    (define/override (on-event e)
      (cond [(send e button-down? 'left)
             (define dc (send this get-dc))
             (define-values (_w _h) (send dc get-size))
             (define ex (send e get-x))
             (define ey (send e get-y))
             (define j (quotient ex 44))
             (define i (quotient ey 39))
             (define dbox (new dialog% [parent f]
                          [label "Choose"]
                          [alignment `(center top)]
                          [min-height 300]
                          [min-width 300]
                          ))
             (define chos (func i j))
             
             (define rbox (cond ((not (null? chos))(new radio-box% [label "Select any one"]
                             [choices chos]
                             [parent dbox]
                             [callback (lambda (radio-box event) (send dbox show #f))]
                             ))))
             (cond ((not (null? chos))(send rbox set-selection #f))
                   (else (send msg set-label "Please backtrack")))
             (cond ((and (= (get copy-board i j) 0) (not (null? chos)))
                    (send dbox show #t)
                    (define s (send rbox get-selection))
                    (cond ((not (equal? s #f)) 
                    (begin (set! copy-board (set copy-board i j (string->number (list-ref chos s))))
                    (set! undo-list (reverse (cons (cons i j) (reverse undo-list)))))))
                    ;(set! board (set board i j (string->number (list-ref (func i j) s))))
                    (cond [(not (eq? s #f))
                           (send dc draw-text (number->string (string->number (list-ref chos s))) (+ (* 44 j) 17) (+ (* 39 i) 10))])
                    ;;gets exact position of click
                    (cond ((equal? copy-board final-ans)  (send msg set-label "Congratulations!!")))
                    ))]))
            ;;;;;;;;;;;;;;;;;EDITED;;;;;;;;;;;;;;;;;;;;;;;; 
     (super-new)))


(define the-grid (new grid-canvas% [parent panel] [min-width WIDTH] [min-height HEIGHT]))
;;;;;;;;;;;;;;;;;;;;;;;;; Backtracking Algorithm  ;;;;;;;;;;;;;;;;;;;;

(define UNFILLED 0)
(define N 9)

(define (get l i j)
  (list-ref (list-ref l i) j))

(define (set l i j val) ;;sets a value in a list at (i,j)th position
  (define (getsingle l n)
    (if (= n 0) (car l)
        (getsingle (cdr l) (sub1 n))))
  (define (sub l i j)  
    (if (= i j) '()
        (cons (getsingle l i) (sub l (+ i 1) j))))
  (define (setsing l i val)
    (append (sub l 0 i) (list val) (sub l (+ i 1) (length l))))
  (define (setmul ll i l)
     (append (sub ll 0 i) (list l) (sub ll (+ i 1) (length ll))))
  (let* [(nl (setsing (getsingle l i) j val))
         (fl (setmul l  i nl))]
    fl))

(define (findUnassigned l row col);row=col=0 while calling
  (cond ((and (= row N)(= col 0)) (list #f row col))
        ((= (get l row col) UNFILLED) (list #t row col))
        ((and(not(= (get l row col) UNFILLED))(= col (- N 1))) (findUnassigned l (add1 row) 0))
        ((and(not(= (get l row col) UNFILLED))(not(= col (- N 1))))(findUnassigned l row (add1 col)))))

(define (inRow? l row val col);col=0 while calling
  (cond ((= col N) #f)
        ((= val (get l row col)) #t)
        (else (inRow? l row val (add1 col)))))

(define (inCol? l row val col);row=0 while calling
  (cond ((= row N) #f)
        ((= val (get l row col)) #t)
        (else (inCol? l (add1 row) val col))))

(define (inBox? l strow stcol num row col);call with row=col=0
  (cond ((and (= row 3)(= col 0)) #f)
        ((= (get l (+ row strow) (+ col stcol)) num) #t)
        ((and(not(= (get l (+ row strow) (+ col stcol)) num))(= col 2))(inBox? l strow stcol num (+ row 1) 0))
        ((and(not(= (get l (+ row strow) (+ col stcol)) num))(not(= col 2)))(inBox? l strow stcol num row (+ 1 col)))))

(define (safe? l row col num) ;; check if cell (row,col) is safe for number num to be put in list l
  (let* [(rr (- row (modulo row 3)))
         (cc (- col (modulo col 3)))]
  (and (not(inRow? l row num 0))
       (not(inCol? l 0 num col))
       (not(inBox? l rr cc num 0 0)))))

(define (solve l) ;; fills one valid position in the sudoku
  (let* [(flist (findUnassigned l 0 0))
         (row (list-ref flist 1))
         (col (list-ref flist 2))]
    (if (equal? (car flist) #f) (list #t l)
        (func 1 l row col))))

(define (func num l row col)
  (if (> num N) (list #f l)
   (if (safe? l row col num)
     (let* [(newl (set l row col num))]
        (cond  ((equal? #t (car (solve newl))) (list #t newl))
               (else (func (+ 1 num) l row col)))) ;; backtracking
     (func (+ 1 num) l row col))))

(define (patchup l)  ;;calling this function returns the solved board
  (if (equal? (car(findUnassigned l 0 0)) #f) l
      (patchup (cadr (solve l)))))


(define final-ans (patchup board))
(define copy-board board)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; action on clicking NEW Game button
(new button% [parent f]
             [label "New Game"]
             [min-width 175]
             [min-height 30]
             [callback (lambda (button event)
                         (send f on-exit)
                         (set! board (let* [(x (random))](cond ((< x 0.2) board1)
                                                               ((> x 0.8) board5)
                                                               ((and (> x 0.2) (< x 0.4)) board2)
                                                               ((and (> x 0.6) (< x 0.8)) board4)
                                                               (else board3))))
                         (define dc (send the-grid get-dc))
                         (send dc erase)
                         (set! final-ans (patchup board))
                         (set! copy-board board)
                         (send f show #t))])
                         



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Canvas to store the last frame of last game being played.

(define g-canvas%
  (class canvas%
    (inherit get-dc get-width get-height)
    (define/override (on-paint)
      (define dc (get-dc))
      (define w (get-width))
      (define h (get-height))
      (send dc set-pen "black" 3 'solid)
      (send dc erase)
      (for ([i (in-range 3)])
        (send dc draw-line 0 (* i (/ h 3)) w (* i (/ h 3)))
        (send dc draw-line (* i (/ w 3)) 0 (* i (/ w 3)) h))
      (send dc set-pen "black" 1 'solid)
      (for ([i (in-range 9)])
        (send dc draw-line 0 (* i (/ h 9)) w (* i (/ h 9))))
      (for ([i (in-range 9)])
        (send dc draw-line (* i (/ w 9)) 0 (* i (/ w 9)) h))
      (for ([i (in-range 9)])
        (for ([j (in-range 9)])
          (cond ((not (= (get board i j) 0))
                 (send dc draw-text (number->string (get board i j))   (+ (* 44 j) 17) (+ (* 39 i) 13))))))
      )
    (super-new)))

;; Buttons defined in the Solved screen to start a new game
(new button% [parent f2]
     [label "New Game"]
     [min-width 175]
     [min-height 30]
     [callback (lambda (button event)
                 (send f2 on-exit)
                 (set! board (let* [(x (random))](cond ((< x 0.2) board1)
                                                       ((> x 0.8) board5)
                                                       ((and (> x 0.2) (< x 0.4)) board2)
                                                       ((and (> x 0.6) (< x 0.8)) board4)
                                                       (else board3))))
                 (define dc (send last-grid get-dc))
                 (send dc erase)
                 (set! final-ans (patchup board))
                 (set! copy-board board)
                 (send f show #t)
                 (define dc1 (send last-grid get-dc))
                 (send dc1 erase))])

; The exit button to quit the game

(new button% [parent f2]
     [label "Exit"]
     [min-width 175]
     [min-height 30]
     [callback (lambda (button event)
                 (send f2 on-exit))])

(define last-grid (new g-canvas% [parent panel2] [min-width WIDTH] [min-height HEIGHT]))


; The Solve button is the one, on clicking which, the current frame exits, a new frame gets created which comprises of the solved Board.
; At this point, you have the choice either to quit or to start afresh a new game.
(new button% [parent f]
             [label "Solve"]
             [min-width 175]
             [min-height 30]
             [callback (lambda (button event)
                         (send f on-exit)
                         (set! board final-ans)     
                         (define dc1 (send last-grid get-dc))
                         (send dc1 erase)
                        
                         ;(send msg set-label "Solved")
                         ;;;;;;;;;;;;
                         (send f2 show #t) 
                         )])  ;;this is where the function for solving goes



; The Undo button is used to undo the most recent choice that has been made. The callback function just sets the status label of the window as "Continue" 

(new button% [parent f]
             [label "Undo"]
             [min-width 175]
             [min-height 30]
             [callback (lambda (button event)
                         (when (not (null? undo-list))
                         (set! copy-board (set copy-board (caar (reverse undo-list)) (cdar (reverse undo-list)) 0))
                         (set! undo-list (reverse (cdr (reverse undo-list))))
                         (define dc (send the-grid get-dc))
                         (define h (send the-grid get-height))
                         (define w (send the-grid get-width))
                         (send dc erase)
                         (send dc set-pen "black" 3 'solid)
                         (for ([i (in-range 3)])
                           (send dc draw-line 0 (* i (/ h 3)) w (* i (/ h 3)))
                           (send dc draw-line (* i (/ w 3)) 0 (* i (/ w 3)) h))
                           (send dc set-pen "black" 1 'solid)
                           (for ([i (in-range 9)])
                             (send dc draw-line 0 (* i (/ h 9)) w (* i (/ h 9))))
                           (for ([i (in-range 9)])
                             (send dc draw-line (* i (/ w 9)) 0 (* i (/ w 9)) h))
                           (for ([i (in-range 9)])
                             (for ([j (in-range 9)])
                               (cond ((not (= (get copy-board i j) 0))
                                      (send dc draw-text (number->string (get copy-board i j))
                                            (+ (* 44 j) 17) (+ (* 39 i) 10))))))
      
                           (send msg set-label "Continue")))])



(send f show #t)



;-----------------Citations------------------------------------------

;http://stackoverflow.com/questions/5318884/gui-elements-in-dr-racket
;https://docs.racket-lang.org/draw/pen_.html
;https://docs.racket-lang.org/teachpack/draw.html
;Racket Documentation
;https://en.wikipedia.org/wiki/Sudoku_solving_algorithms