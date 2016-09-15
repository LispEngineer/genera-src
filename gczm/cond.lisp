;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CONDTEST; Base: 10; Lowercase: Yes -*-

;; A comparison of condition handling in modern SBCL 1.3.9 vs
;; Genera 8.3+

;; Douglas P. Fields, Jr. - https://symbolics.lisp.engineer/

#+SBCL
(defpackage :condtest
  (:use :common-lisp))

(in-package :condtest)


;; SBCL -------------------------------------------------------------------


;; Code from http://c2.com/cgi/wiki?CommonLispConditionSystem
;; (but fixed, they had the parentheses wrong)
(define-condition on-zero-denominator (error)
  ((message :initarg :message :reader message)))

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
