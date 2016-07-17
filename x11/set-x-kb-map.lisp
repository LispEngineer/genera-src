;;; -*- Base: 10; Package: X-SCREEN; Mode: LISP; Syntax: Common-lisp; Lowercase: T -*-

; New console command for Genera 8.3:
;   Set X Keyboard Mapping <screen> <mapping>
; Cobbled together from source code found on the Internet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Below from x-console.lisp.~46~

;;New method to set the keyboard layout type after the console has already been 
;;initialized
(defmethod (console-set-keyboard-type x-console) (&optional 
                                                      (layout-type (keyboard-layout-type))
                                                      (offset (second (multiple-value-list (keyboard-layout-type)))))
  (setf (sys:keyboard-layout-type cli::keyboard) layout-type)
  (when offset (setq keycode-offset offset))
  (x-console-update-keyboard-mapping self)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Below from x-program.lisp.~23~

(defun keyboard-signature-name-p (x)
  (cl::member x *keyboard-signatures* :key #'keyboard-signature-name))

(cp:define-command (com-set-x-keyboard-mapping :command-table "User")
    ((screen 'x-screen
             :default (default-x-screen)
             :provide-default (default-x-screen)
             :display-default (default-x-screen)
             :documentation "The X screen to use for the mapping.")
     (keyboard-signature-type `((member ,@(loop for x in *keyboard-signatures*
                                               collect (keyboard-signature-name x)))
                                )
                              :documentation "Keyboard signature type"))
   (console-set-keyboard-type (send screen :console) keyboard-signature-type))

