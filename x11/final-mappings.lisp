;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: T -*-
;
; Douglas Fields - Copyright 2016 - See LICENSE.txt for Creative Commons License
; https://symbolics.lisp.engineer/

; Final mappings after setting up the :xquartz-87-tenkeyless keyboards in both
; X Mappings and Keyboard Layout. Necessary because for whatever reason, I cannot
; get the X mappings to fully "stick" unless these commands are run.

; How this works:
; (sys:set-keyboard-table-mapping WHAT-TO-SEND KEYBOARD-LAYOUT-NAME KEYCODE-INPUT 
;  :symbol t
;  :shift t
;  :all-shifts t)
;
; KEYCODES:
; 0x3E =  62 = Right Control = 70 in X-Windows Keycodes
; 0x3F =  63 = Right Option = 71 in X-Windows Keycodes
;         51 = delete = 59 in X-Windows Keycodes and called BackSpace
;        117 = delete = 125 in X-Windows called Delete

; Right Option -> Control
(sys:set-keyboard-table-mapping :right-hyper   :xquartz-87-tenkeyless #x3E :all-shifts t)
; Right Control -> Hyper
(sys:set-keyboard-table-mapping :right-control :xquartz-87-tenkeyless #x3F :all-shifts t)
; Right Command -> Meta - already works
; Right Menu -> Symbol - already works

; Swap backspace and rubout (which are delete and delete on Mac)
(sys:set-keyboard-table-mapping #\rubout     :xquartz-87-tenkeyless 51  :all-shifts t)
(sys:set-keyboard-table-mapping #\back-space :xquartz-87-tenkeyless 117 :all-shifts t)

