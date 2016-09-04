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
;
; We have our own package, GCZM, to avoid naming conflicts with our (CLIM) commands.

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
; If you want to accept any string.
; See docs on :accept-values pane which might be used with the above?
; To make a Genera activity: clim:define-genera-application
; To control the application frame REPL, try: clim:default-frame-top-level
;   which has command parsers, unparsers, partial parsers, and a prompt

; To have command accelerators work, you need to specialize clim:read-frame-command.
; See below.

; Read documentation "Output Recording in CLIM".

; Initial implementation:
; 1. Show some random text
; 2. Accept text
; 3. Put the text in the scrollback
; 4. Go to 1
; 5. Allow user to click "exit" button

(define-application-frame gc-z-machine ()
  ((z-machine :initform nil))

  (:menu-bar nil) ; disable default menu bar and show it in pane explicitly

  (:panes
    (commands  :command-menu) ; This is supplied automatically unless :menu-bar nil
    (display   :interactor
	       ; :text-style '(:fix :bold :very-large)
	       :display-function 'draw-the-display
               :scroll-bars :vertical
               :initial-cursor-visibility :on)
    (statusbar :application
               :display-function 'draw-the-statusbar
               ; TODO: Set the height to one line of characters - clim:text-size,
	       ;       clim:stream-line-height
	       ; TODO: Set the color to be opposite from main display
               :scroll-bars nil))

  ; Default command table will be named gc-z-machine-command-table
  ; Commands are symbols, conventionally starting with com-
  ; (:command-table (gc-z-machine :inherit-from (clim:accept-values-pane)))

  (:layouts
    (main 
      (vertically () commands display statusbar))))

; Enable Keystroke Accelerators (hotkeys) - per Genera CLIM 2.0 Docs
; clim:read-command-using-keystrokes could have overrides for the
; :command-parser, :command-unparser and :partial-command-parser
(defmethod clim:read-frame-command ((frame gc-z-machine) &key)
  (let ((command-table (clim:find-command-table 'gc-z-machine)))
    (clim:with-command-table-keystrokes (keystrokes command-table)
      (clim:read-command-using-keystrokes command-table keystrokes))))

(defmethod draw-the-display ((application gc-z-machine) stream)
  (fresh-line stream)
  (write-string "Genera CLIM Z-Machine Interpreter v0.01" stream))

(defmethod draw-the-statusbar ((application gc-z-machine) stream)
  (write-string "West of House          Turn 3         Score 73" stream))

(define-gc-z-machine-command (com-exit :menu t       ; Show in menu
                                       :keystroke (:q :meta)
                                       :name "Exit") ; Type "Exit" to quit application
                             ()
  (frame-exit *application-frame*))


#||
() ; Necessary so we can do c-sh-E to execute the below
(run-frame-top-level 
  (setq gczm1 (make-application-frame 'gc-z-machine
               :left 100 :right 600 :top 100 :bottom 500)))
||#
