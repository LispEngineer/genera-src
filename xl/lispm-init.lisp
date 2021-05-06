
;; Add Document Examiner to Triangle
(TV:SETUP-SELECT-KEYS '((#\C . "Converse") 
			(#\D . "Document Examiner") 
			(#\E . "Editor")
                        (#\F . "File System Operations") 
			(#\I . "Inspector") 
			(#\L . "Lisp")
                        (#\M . "Zmail") 
			(#\N . "Notifications") 
			(#\P . "Peek")
                        (#\Q . "Frame-Up") 
			(#\T . "Terminal") 
			(#\X . "Flavor Examiner")
                        (#\Triangle . "Namespace Editor") 
			(#\= . "Select Key Selector"))
                      NIL)

;; Put the server name in the bottom WhoLine
(tv:set-screen-options :show-machine-name-in-wholine t
		       :dim-screen-after-n-minutes-idle 3)

;; Set console brightness for PE36-PCONS Serial 10020
;; Then do LOCAL-3 LOCAL-C to set contrast to level 3 (no lisp equivalent!)
;; and set the beep volume
(setf (tv:screen-brightness tv:main-screen) 69/127)
(setf (sys:console-volume) 29/31)

(print "Welcome back, Admiral Fields.")
