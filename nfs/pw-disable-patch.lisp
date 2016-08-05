;;; -*- Base: 10; Mode: LISP; Syntax: Common-lisp; Package: RPC; Lowercase: T; Patch-File: T -*-
;;;
;;; Douglas Fields
;;; https://symbolics.lisp.engineer/

; Begin Genera Patch Verbiage =====
(sct:files-patched-in-this-patch-file
  "SYS:NFS;AUTHENTICATION.LISP")

(sct:note-private-patch "NFSv2 password noop patch")

(sct:begin-patch-section)
(sct:patch-section-source-file "SYS:NFS;AUTHENTICATION.LISP")
(sct:patch-section-attributes
  "-*- Base: 10; Mode: LISP; Package: RPC; Syntax: Common-lisp; Lowercase: T -*-")
; End Genera Patch Verbiage =====

; Package RPC

; Modern UNIX servers don't use crypt passwords, so let's just bypass that for now.
;
; username-and-password-valid-p returns two items:
;   username valid?
;   password valid?
; This will modify it to always say the password is valid, for now.

(defun-in-flavor (username-and-password-valid-p unix-authentication-mixin)
		 (username password)
  (declare (values username-valid-p password-valid-p))
  (if (null username)
      (values nil nil)
    (multiple-value-bind (encrypted-password defaulted-p)
	(let ((*unix-authentication-allow-defaulting* t))
	  (username->password (unix-name-lookup-access-path) username))
      (cond ((and defaulted-p
		  (not (string-equal username "anonymous"))
		  (not (string-equal username "lisp-machine"))
		  (not (string-equal username "nobody")))
	     (values nil nil))
	    (t
	     (values t t)))))) ; The password is always correct
