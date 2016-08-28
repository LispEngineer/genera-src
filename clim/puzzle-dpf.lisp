;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-

;;; Adapted from Symbolic's Puzzle-2 CLIM demo.


(define-application-frame fifteen-puzzle-dpf ()
    ((pieces :initform (make-array '(4 4) :initial-contents '((1 2 3 4)
							      (5 6 7 8)
							      (9 10 11 12)
							      (13 14 15 0)))))
  (:panes
    (display :application
	     :text-style '(:fix :bold :very-large)
	     :display-function 'draw-the-display
	     :scroll-bars nil
	     :initial-cursor-visibility nil))
  (:layouts
    (main 
      (vertically () display))))

(define-presentation-type puzzle-piece ())

(defmethod draw-the-display ((application fifteen-puzzle-dpf) stream)
  (with-slots (pieces) application
    (dotimes (y 4)
      (dotimes (x 4)
	(let ((piece (aref pieces y x))
	      (position (+ (* y 4) x)))
	  (write-string " " stream)
	  (with-output-as-presentation (stream position 'puzzle-piece)
	    (if (zerop piece)
		(format stream "  ")
		(format stream "~2D" piece)))))
      (terpri stream))))

;;; if the piece at (xp,yp) can be moved, five values are returned:
;;;  - the coordinates of the space in the puzzle,
;;;  - delta-y and delta-x representing the direction on the puzzle from
;;;     space towards the piece at (xp,yp)
;;;  - and the number of pieces to move
;;; if the piece at (xp,yp) cannot be moved, nil is returned

(defun which-way-to-move (yp xp pieces)
  (macrolet ((is-space (y x) `(zerop (aref pieces ,y ,x))))
    (loop for x from (+ xp 1) to 3 do
      (when (is-space yp x) (return-from which-way-to-move (values yp x 0 -1 (- x xp)))))
    (loop for x from (- xp 1) downto 0 do
      (when (is-space yp x) (return-from which-way-to-move (values yp x 0 1 (- xp x)))))
    (loop for y from (+ yp 1) to 3 do
      (when (is-space y xp) (return-from which-way-to-move (values y xp -1 0 (- y yp)))))
    (loop for y from (- yp 1) downto 0 do
      (when (is-space y xp) (return-from which-way-to-move (values y xp 1 0 (- yp y)))))))

(define-fifteen-puzzle-dpf-command (move) ((yp 'integer) (xp 'integer))
  (with-slots (pieces) *application-frame*
    (multiple-value-bind (start-y start-x dy dx n-moves) (which-way-to-move yp xp pieces)
      (when dx
	(loop repeat n-moves
	      for x1 = start-x then x2
	      for x2 = (+ x1 dx) then (+ x2 dx)
	      for y1 = start-y then y2
	      for y2 = (+ y1 dy) then (+ y2 dy)
	      do (setf (aref pieces y1 x1) (aref pieces y2 x2))
	      finally (setf (aref pieces yp xp) 0))))))

(define-presentation-to-command-translator move-a-piece
    (puzzle-piece move fifteen-puzzle-dpf)
    (object)	  
  (multiple-value-bind (yp xp) (floor object 4)
    `(,yp ,xp)))

(define-fifteen-puzzle-dpf-command (reset :menu t) ()
  (with-slots (pieces) *application-frame*
    (loop for y from 0 to 3 do
      (loop with 4y+1 = (+ (* 4 y) 1)
	    for x from 0 to 3 do
	(setf (aref pieces y x) (mod (+ 4y+1 x) 16))))))

(define-fifteen-puzzle-dpf-command (exit :menu t) ()
  (frame-exit *application-frame*))



; Compile the above with m-X Compile Buffer
; Execute the below by highlighting it in Zmacs and then doing c-sh-E
; If something goes wrong, c-m-ABORT should get you out
#||
()

; The X-windows display to use
(setq xport (clim:find-port :server-path
  '(:clx :host "NEON" :screen nil :display-number 0)))

; Make the application run on X
(setq fpdpf (make-application-frame 'fifteen-puzzle-dpf
	     :left 400 :right 600 :top 150 :bottom 350
             :parent xport))

(run-frame-top-level fpdpf)
||#

