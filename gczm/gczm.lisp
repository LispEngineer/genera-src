;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GCZM; Base: 10; Lowercase: Yes -*-

(defpackage gczm
  (:use clim-lisp clim))

(in-package :gczm)

; Douglas P. Fields, Jr. - https://symbolics.lisp.engineer/
;
; GCZM - (Symbolics) Genera CLIM Z-Machine Interpreter
;
; Intended to play Version 3 Z-machine games.
; Written in Symbolics ANSI-Common-Lisp, which is actually not a fully
; ANSI-compliant Common Lisp implementation.

; Our application looks like the following:
;
; +------------------------------+
; | Menu Menu Menu Menu Menu     |
; +-+----------------------------+
; | | Game text scroll window    |
; | | with scrollbar to the left |
; | | ...                        |
; | | > User Commands echoed     |
; | | ...                        |
; +-+----------------------------+
; | > User commands entered      |
; +------------------------------+
; | Status line                  |
; +------------------------------+
;
; In addition,if possible, we will try to update the Genera
; status line as well. Not sure if that's possible in Genera
; CLIM yet.

; Implementation notes:
; (clim:accept 'string)
; If you want 

; Initial implementation:
; 1. Show some random text
; 2. Accept text
; 3. Put the text in the scrollback
; 4. Go to 1
; 5. Allow user to click "exit" button

(define-application-frame gc-z-machine ()
  ((z-machine :initform nil))

  (:panes
    (display :application
	     :text-style '(:fix :bold :very-large)
	     :display-function 'draw-the-display
	     :scroll-bars t
	     :initial-cursor-visibility nil))
  (:layouts
    (main 
      (vertically () display))))

(defmethod draw-the-display ((application gc-z-machine) stream)
  stream)

(define-gc-z-machine-command (exit :menu t) ()
  (frame-exit *application-frame*))


#||
()
(setq gczm1 (make-application-frame 'gc-z-machine
	    :left 100 :right 600 :top 100 :bottom 500))
(run-frame-top-level gczm1)
||#
