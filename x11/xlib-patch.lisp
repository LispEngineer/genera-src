;;; -*- Base: 10; Mode: LISP; Syntax: Common-lisp; Lowercase: T; Patch-File: T -*-
;;;
;;; Douglas Fields
;;; https://symbolics.lisp.engineer/

; Begin Genera Patch Verbiage =====
(sct:files-patched-in-this-patch-file
  "SYS:X11;CLX;DOC.LISP.5003") ; on MacIvory III Genera 8.3

(sct:note-private-patch "XLib set-modifier-mapping noop patch")

(sct:begin-patch-section)
(sct:patch-section-source-file "SYS:X11;CLX;DOC.LISP.5003")
(sct:patch-section-attributes
  "-*- Base: 10; Mode: LISP; Syntax: Common-lisp; Lowercase: T -*-")
; End Genera Patch Verbiage =====

; Patches to XLIB to make it work on a Modern X Server (e.g., Xquartz 2.7.10_beta2).
; After loading these patches, you should probably save your world and restart for
; ease of use.

; On MacIvory III Genera 8.3 I can get this error when using the :xquartz-87-tenkeyless
; keyboard mapping when I do "Start X Screen":
;
; Error: VALUE-ERROR in current request  Code 118.0 [SetModifierMapping] Value 10485766.
;
; XLIB::X-ERROR
;    Arg 0 (CONDITION): XLIB:VALUE-ERROR
;    Rest Arg: (:DISPLAUY #<XLIB:DISPLAY #:0 (The X.Org Foundation R11803000)> :ERROR-KEY XLIB:VALUE-ERROR ...)
;
; One fix was suggested here: http://www.loomcom.com/genera/genera-install.html .
; More detailed analysis is here: http://tech-sketch.jp/2014/12/linux-lispmachine.html .
; This seems to work.

(defun xlib:set-modifier-mapping (display &rest args) (values))

; I haven't seen any more problems, yet, but this page
;   http://tech-sketch.jp/2015/02/linux_lispmachine_2.html
; suggests that there is another problem with xlib:close-display.
; It causes a crash when saving the world from X-Windows.
; If I see this problem, I will try this.

;;; (defun xlib:close-display (display &key abort))

; Of course, the proper fix to these problems is to look into the
; underlying source code and fix it to work with a modern X server.
; For example, the original set-modifier-mapping is at clx>text.lisp.
; There's another version, however, that has almost nothing in it, at
; clx>doc.lisp. Not sure why the code appears twice, but it seems that
; the code in doc.lisp isn't used. This was determined by doing a
; "Edit Definition XLIB:SET-MODIFIER-MAPPING Function" at the Genera
; command prompt...
