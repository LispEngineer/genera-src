;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CONDTEST; Base: 10; Lowercase: Yes -*-

;; A comparison of condition handling in modern SBCL 1.3.9 vs
;; Genera 8.3

;; Douglas P. Fields, Jr. - https://symbolics.lisp.engineer/

;; NOTE: On Genera, before loading this file, create the package.
;; Allowing the editor to create the package doesn't make it correctly.
;; At the Genera Lisp Listener:
;;  Set Lisp Context (a lisp syntax name [default Common-Lisp]) ANSI-Common-Lisp
;;  (defpackage condtest (:use cl conditions))


#+SBCL
(defpackage :condtest
  (:use :common-lisp))

#+Genera
(defpackage :condtest
  (:use future-common-lisp conditions))

(in-package :condtest)


;; Do a little test of conditions now, using
;; code originally from http://c2.com/cgi/wiki?CommonLispConditionSystem
;; (but fixed, they had the parentheses wrong)


#+SBCL
(define-condition on-zero-denominator (error)
  ((message :initarg :message :reader message)))

;; Genera requires the :report below. If it's absent, then
;; we get the following error:
;; D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (:FIX :BOLD :NORMAL) "CPTFONTCB")Error: Error in flavor CONDTEST::ON-ZERO-DENOMINATOR
0;;        1Missing method DBG:REPORT (required by CONDITION)
0#+Genera
(define-condition on-zero-denominator (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
	     (format stream "On-zero-denominator: ~A" (message condition)))))

#| 
;; From Genera Document Examiner DEFINE-CONDITION macro docs
;; Note that the original had unbalanced parens; fixed below.
(2 0 (NIL 0) (SAGE:SANS-SERIF-BODY SAGE::TYPEWRITER :NORMAL) "CPTFONT")(DEFINE-CONDITION MACHINE-ERROR (ERROR) 
0  (2(MACHINE-NAME
0    2:INIT-ARG :MACHINE-NAME
0    2:READER   MACHINE-ERROR-MACHINE-NAME))
  (:REPORT (LAMBDA (CONDITION STREAM)
0	     2(FORMAT STREAM "There is a problem with ~A."
0		     2(MACHINE-ERROR-MACHINE-NAME CONDITION)))))
0|#

(defun high-level-code ()
  (handler-bind
      ((on-zero-denominator
        #'(lambda (c)
            (format t "error signaled: ~a~%" (message c))
            (invoke-restart 'return-zero))))
    (determine-infinity))
  
  (handler-bind
      ((on-zero-denominator
        #'(lambda (c)
            (format t "error signaled: ~a~%" (message c))
            (invoke-restart 'return-value 1))))
    (determine-infinity))

  (handler-bind
      ((on-zero-denominator
        #'(lambda (c)
            (format t "error signaled: ~a~%" (message c))
            (invoke-restart 'recalc-using 2))))
    (determine-infinity))

  (handler-bind
      ((on-zero-denominator
        #'(lambda (c)
            (format t "error signaled: ~a~%" (message c))
            (invoke-restart 'just-continue))))
    (determine-infinity))
    
  (format t "Done."))

(defun determine-infinity ()
  (restart-case
      (let ((result 0))
        (setf result (reciprocal-of 0))
        (format t "Value: ~a~%" result))
    (just-continue () nil)))

(defun reciprocal-of (value)
  (restart-case
      (if (/= value 0)
          (/ 1 value)
          (error 'on-zero-denominator :message "cannot use zero"))

    (return-zero () 0)
    (return-value (r) r)
    (recalc-using (v) (reciprocal-of v))))
