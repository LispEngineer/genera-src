;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GCZM; Base: 10; Lowercase: Yes -*-

; GCZM - (Symbolics) Genera CLIM Z-Machine Interpreter
; Douglas P. Fields, Jr. - https://symbolics.lisp.engineer/
;
; Intended to play Version 3 Z-machine games.
; Written in Symbolics ANSI-Common-Lisp, which is actually not a fully
; compliant ANSI Common Lisp implementation.

; We have our own package, GCZM, to avoid naming conflicts with our (CLIM) commands.
; Loading this file into the Zwei editor if you don't already have a GCZM package will
; result in package errors that are hard to abort; try Load File first!

(defpackage gczm
  (:use clim-lisp clim))
				    
(in-package :gczm)



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
; CLIM yet. (Later note: There are supposedly CLIM panes that mimic the
; Genera description pane and such that can be used.)

; Implementation notes:
; To make a Genera activity: clim:define-genera-application
; To control the application frame REPL, try: clim:default-frame-top-level
;   which has command parsers, unparsers, partial parsers, and a prompt

; To have command accelerators work, you need to specialize clim:read-frame-command.
; See below.

; Read documentation "Output Recording in CLIM".

; Initial implementation:
; 1. Show some random text
; 2. Accept text
; 3. Repeat the text in the scrollback
; 4. Go to 1
; 5. Allow user to click "exit" menu button (not working)
; 6. Allow user to type m-Q to exit (not working)



; Functions so we can see what's going on afterwards or in the Lisp Listener
(defparameter *log* ())
(setq *log* nil) ; Clear the log if we're recompiling
(defun addlog (message)
  (setf *log* (cons message *log*)))


; Main application frame ---------------------------------------------------------

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


; top level or command reader ------------------------------------------------------

; Enable Keystroke Accelerators (hotkeys) - per Genera CLIM 2.0 Docs
; clim:read-command-using-keystrokes could have overrides for the
; :command-parser, :command-unparser and :partial-command-parser
#| ; Not used right now
(defmethod clim:read-frame-command ((frame gc-z-machine) &key)
  (let ((command-table (clim:find-command-table 'gc-z-machine)))
    (clim:with-command-table-keystrokes (keystrokes command-table)
      (clim:read-command-using-keystrokes command-table keystrokes))))
|#

; Custom command reader which accepts any string.
; Note that this seems to prevent clicking on the menu bar and
; command accelerators from working... Although, the accelerators
; are definitely causing the accept to terminate.
(defmethod read-frame-command ((frame gc-z-machine) &key (stream *standard-input*))
  "Specialized for GCZM, just reads a string and returns it with com-input"
  (multiple-value-bind (retval thetype)
      ; Accept returns the object and the type. The regular string type, however,
      ; does some parsing on the input, for example, removing surrounding
      ; doublequotes. To fix this, we made our own presentation type
      ; that's exactly a string without any parsing, called string-raw.
      (let ((command-table (clim:find-command-table 'gc-z-machine)))
	; This with-command-table-keystrokes addition doesn't seem to do anything useful
	; for us right now.
	(clim:with-command-table-keystrokes (keystrokes command-table)
	  (declare (ignore keystrokes))
	  (with-accept-help ((:subhelp "Enter a game command or click menu bar"))
	    ; The below enables typing strings and clicking on the menu bar,
	    ; and if no commands have :name entries, they won't be invoked by any
	    ; typed input. The order of the ('or ...) is extraordinarily sensitive,
	    ; and you can't do a sub-list (e.g., (command :command-table command-table)).
	    ; Command accelerators (hotkeys/keyboard shortcuts) still don't work.
	    (accept `(or command string-raw)
		    :stream stream :prompt nil
		    :default "" :default-type 'string))))

    (addlog (list "read-frame-command" retval thetype))
    ; Now that we have retval & thetype, return our command
    (cond
      ; If we got a string, return that we got input
      ((eq thetype 'string-raw)   (list 'com-input retval))

      ; If we get a command, return that
      ; FIXME: If the user just enters a blank line, it gets parsed as the NIL
      ; command and that should probably be treated as (com-input "").
      ; If the first keypress is a SPACE, it's also treated as the NIL command.
      ((eq thetype 'clim:command) retval)

      ; Hitting three tab characters causes this to have ("" STRING) ...???
      ; In a Lisp Listener, it turns out (DESCRIBE 'STRING) makes it a symbol in
      ; the lisp package.
      ((eq thetype 'lisp:string)  (list 'com-input retval))

      ; Otherwise, return the command from the command processor with ugliness
      (t (list 'com-input (write-to-string (list "Unknown" retval thetype)))))))

; See ftp://ftp.ai.sri.com/pub/mailing-lists/clim/921231/msg00434.html

; Modified version from CLIM source for read-command-using-keystrokes
; that instead of reading commands, reads strings (accepts strings)
; TODO: CODE ME


; Drawing methods --------------------------------------------------------

(defmethod draw-the-statusbar ((application gc-z-machine) stream)
  (write-string "West of House          Turn 3         Score 73" stream))




; Commands ---------------------------------------------------------------

; Quit the application/game
(define-gc-z-machine-command (com-exit :menu t       ; Show in menu
                                       :keystroke (:q :meta))
                             ()
  (addlog (list "Called com-exit"))
  ; TODO: Add a pop-up confirmation dialog
  (frame-exit *application-frame*))

; Testing menu command
(define-gc-z-machine-command (com-hello :menu t       ; Show in menu
                                        :keystroke (:h :meta))
                             ()
  (addlog (list "Called com-hello"))
  (fresh-line *standard-input*)
  (write-string "And hello to you, too!" *standard-input*)
  (fresh-line *standard-input*))

; If we get input from the command line processor, this is it...
(define-gc-z-machine-command (com-input) ((astring 'string))
  ; First, write to our interactor
  (fresh-line *standard-input*)
  (if (stringp astring)
      (progn
	(write-string "Got: " *standard-input*)
	(write-string astring *standard-input*))
      (progn
	(write-string "Got non-string: " *standard-input*)
	(print-object astring *standard-input*)))
  (fresh-line *standard-input*)
  ; Then store this permanently for debugging
  (addlog (list "Called com-input with" astring))
  ; TODO: If the command is ",exit" then quit
  astring)



; string-raw presentation type ----------------------------------------------

; The string-raw presentation type is exactly like the string presentation
; type except that it is always done "unacceptably" so it should never have
; double-quotes if you click the presentation type (or so the theory goes).
; In other words, the usual "(accept 'string ...)" leaves a record in the
; interactor window that is clickable, but when clicked, enters the string
; into the current input with double quotes. We don't want that.
; Modified from Genera 8.3 CLIM sources.

;; :INHERIT-FROM T is to avoid inheriting from ARRAY, which is not a presentation type
(define-presentation-type string-raw (&optional length)		;max length
  :inherit-from t			;enforce CL definition
  :history t)

(define-presentation-method presentation-type-specifier-p ((type string-raw))
  (or (eq length '*)
      (typep length '(integer 0 *))))

(define-presentation-method presentation-typep (object (type string-raw))
  (or (eq length '*)
      (<= (length object) length)))

(define-presentation-method presentation-subtypep ((subtype string-raw) supertype)
  (let ((length1 (with-presentation-type-parameters (string-raw subtype) length))
	(length2 (with-presentation-type-parameters (string-raw supertype) length)))
    (values (or (eq length2 '*) (eql length1 length2)) t)))

(define-presentation-method describe-presentation-type ((type string-raw) stream plural-count)
  (default-describe-presentation-type "string-raw" stream plural-count)
  (unless (eq length '*)
    (format stream " max length ~D" length)))

(define-presentation-method present (object (type string-raw) stream (view textual-view)
				     &key)
  (if (stringp object)
      (write-string object stream)
      (print-object object stream)))

(define-presentation-method present (object (type string-raw) stream (view textual-dialog-view)
				     &key)
  (if (stringp object)
      (write-string object stream)
      (print-object object stream)))

(define-presentation-method accept ((type string-raw) stream (view textual-view) &key)
  (let ((token (read-token stream)))
    (unless (or (eq length '*) (> (length token) length))
      (input-not-of-required-type token type))
    token))


; Testing -------------------------------------------------------------------

(defparameter *gczm1* nil)
(defun run-new ()
  "Runs a new application frame that is saved in gczm1"
  (run-frame-top-level 
    (setq *gczm1* (make-application-frame 'gc-z-machine
		    :left 100 :right 600 :top 100 :bottom 500)))
  (reverse *log*))

#||
() ; Necessary so we can do c-sh-E to execute the below
; c-m-ABORT to get out if it gets stuck (either from the CLIM window or Zmacs)
(run-frame-top-level 
  (setq gczm1 (make-application-frame 'gc-z-machine
               :left 100 :right 600 :top 100 :bottom 500)))
||#




