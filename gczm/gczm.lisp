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
; | Status line                  |
; +------------------------------+
;
; In addition,if possible, we will try to update the Genera
; status line as well. Not sure if that's possible in Genera
; CLIM yet. (Later note: There are CLIM panes that mimic the
; Genera description pane and such that can be used.)

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

; Functions so we can see what's going on afterwards or in the Lisp Listener
(defparameter *log* ())
(setq *log* nil) ; Clear the log if we're recompiling
(defun addlog (message)
  (setf *log* (cons message *log*)))


; Main application frame
; TODO: Figure out how to send initial output to the interactor prior to
; accepting the first command
(define-application-frame gc-z-machine ()
  ((z-machine :initform nil))

  ; Instead of a custom top-level, let's just have a custom read-frame-command
  (:top-level (clim:default-frame-top-level :prompt "> "))
  ;		:command-parser gczm-cl-command-parser))

  (:menu-bar nil) ; disable default menu bar and show it in pane explicitly

  (:panes
    (title     :title)
    #-Genera (pointerd  :pointer-documentation) ; Provided by Genera
    (commands  :command-menu) ; This is supplied automatically unless :menu-bar nil
    (display   :interactor
               :scroll-bars :vertical
               :initial-cursor-visibility :on)
    (statusbar :application
	       :max-height 20 :height 20 ; Calculate this from text size
               :display-function 'draw-the-statusbar
               ; TODO: Set the height to one line of characters - clim:text-size,
	       ;       clim:stream-line-height
	       ; TODO: Set the color to be opposite from main display
               :scroll-bars nil))

  ; Default command table will be named gc-z-machine-command-table
  ; Commands are symbols, conventionally starting with com-
  ; Enabling the accept-values-pane doesn't make "accept" work with the menu
  (:command-table (gc-z-machine :inherit-from (clim:accept-values-pane)))

  (:layouts
    (main 
      (vertically () title commands display statusbar #-Genera pointerd))))

; Enable Keystroke Accelerators (hotkeys) - per Genera CLIM 2.0 Docs
; clim:read-command-using-keystrokes could have overrides for the
; :command-parser, :command-unparser and :partial-command-parser
(defmethod clim:read-frame-command ((frame gc-z-machine) &key)
  (let ((command-table (clim:find-command-table 'gc-z-machine)))
    (clim:with-command-table-keystrokes (keystrokes command-table)
      (clim:read-command-using-keystrokes command-table keystrokes))))

; Custom command reader which accepts any string.
; Note that this seems to prevent clicking on the menu bar and
; command accelerators from working... Although, the accelerators
; are definitely causing the accept to terminate.
(defmethod read-frame-command ((frame gc-z-machine) &key (stream *standard-input*))
  "Specialized for GCZM, just reads a string and returns it with com-input"
  (multiple-value-bind (astring thetype)
      ; Accept returns the object and the type.
      ; It does some parsing on the input, for example, removing surrounding
      ; doublequotes.
      ; TODO: To fix this, we probably need to make our own presentation type
      ; that's exactly a string without any parsing.
      (accept 'string :stream stream :prompt nil
                      :default "" :default-type 'string)
    ; (declare (ignore thetype))
    (addlog (list "read-frame-command" astring thetype))
    ; Now that we have astring & thetype, return our command
    (list 'com-input astring)))   

(defmethod draw-the-statusbar ((application gc-z-machine) stream)
  (write-string "West of House          Turn 3         Score 73" stream))

; Commands ---------------------------------------------------------------

(define-gc-z-machine-command (com-exit :menu t       ; Show in menu
                                       :keystroke (:q :meta)
                                       :name "Exit") ; Type "Exit" to quit application
                             ()
  (addlog (list "Called com-exit"))
  (frame-exit *application-frame*))

; If we get input from the command line processor, this is it...
(define-gc-z-machine-command (com-input) ((astring 'string))
  ; First, write to our interactor
  (fresh-line *standard-input*)
  (write-string "Got: " *standard-input*)
  (write-string astring *standard-input*)
  (fresh-line *standard-input*)
  ; Then store this permanently for debugging
  (addlog (list "Called com-input with" astring))
  astring)

; Testing -------------------------------------------------------------------

#||
() ; Necessary so we can do c-sh-E to execute the below
; c-m-ABORT to get out if it gets stuck (either from the CLIM window or Zmacs)
(run-frame-top-level 
  (setq gczm1 (make-application-frame 'gc-z-machine
               :left 100 :right 600 :top 100 :bottom 500)))
||#
