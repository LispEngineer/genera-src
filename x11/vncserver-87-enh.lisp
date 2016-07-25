;;; -*- Base: 10; Mode: LISP; Syntax: Common-lisp; Lowercase: T -*-
;;;
;;; vnc4server keyboard mapping for 87-key ANSI layout
;;; mapping enhanced to use all keys well
;;; Based upon Xquartz 87 Enhanced

;; Douglas Fields - Copyright 2016 - See LICENSE.txt for CC BY-NC-SA details
;; https://symbolics.lisp.engineer/
;; Mac via VNC to Linux vnc4server, with an 87-Key Tenkeyless keyboard
;; Used under Chicken of the VNC and Real VNC to vnc4server

;; This was cloned from xquartz-87-enh.lisp and modified from there.

;; NOTE: If you load this file and immediately do "Show Keyboard Layout" on this
;; keyboard format, it won't display correctly (for whatever reason). However, if
;; you then activate it with "Set X Keyboard Mapping" it shows correctly.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; $ xmodmap -pk
; There are 2 KeySyms per KeyCode; KeyCodes range from 8 to 255.
;
; (setq dpfd (xlib:open-display "INTERNET|192.168.1.87"))
; type s-A to Use protocol X-WINDOW-SYSTEM on medium TCP.
; #<XLIB:DISPLAY INTERNET|192.168.1.97:0 (The XFree86 Project, Inc R40300000)>
; (xlib:display-vendor dpfd)
; "The XFree86 Project, Inc"
; 40300000

; Keycodes sent by Chicken of the VNC 2.2b2 to vnc4server 4.1.1+xorg4.3.0-37ubuntu5.0.2
; for some of the interesting keys.
; Settings: Local - Remote (in Profile Manager -> Modifiers)
;   Command - Alt (Command)
;   Option  - Meta (Option)
;   Control - Control
;   Shift   - Shift
; Keycodes sent:
;   Left shift    - 50
;   Left control  - 37
;   Left option   - 50 AND 64 (!) - to make it a Meta_L
;   Left command  - 64
;   Right command - 50 AND 64 (!) - again
;   Right option  - 64
;   Menu          -
;   Right control - 37
;   Right shift   - 50
;   F13           - 255
;   F14           - 254 - This will add a mapping to xmodmap -pk
;   F15           - 253 - This will add a mapping to xmodmap -pk
;   Help          - 251
; Chicken can also send special keys with the menu
;   Insert
;   Delete
;   Pause
;   Break
;   Print
;   Execute
; So, Chicken of the VNC is thoroughly badly mapped and nigh unusable.
; Furthermore, sometimes it (or vnc4server) gets its caps lock state messed
; up and becomes permanently caps-locked, at which point you can no longer
; use the vnc4server...

; RealVNC VNC Viewer 5.3.1
; https://www.realvnc.com/docs/mac-keyboard-mapping.html
; VNC parameter mappings: Expert
;   LeftCmdKey  - Alt_L
;   LeftOptKey  - Super_L
;   RightCmdKey - Alt_R
;   RightOptKey - Super_R (But note that server sees Super_L unless you manually do xmodmap)
; Keycodes sent:
;   Left shift    - 50
;   Left control  - 37
;   Left option   - Super-L
;   Left command  - 64 - Alt_L
;   Right command - 113 - Alt_R
;   Right option  - Super_L <--- Ugh
;   Right control - 109
;   Right shift   - 62
; These keys don't have default mappings and need to be set using xmodmap.
;   Menu          - NONE, it doesn't send a keypress
;   F13           - 200
;   F14           - 201
;   F15           - 202
;   Super_L       - 203
;   Super_R       - 204
;   Caps_Lock     - 205
;   Help          - 206 - but VNC Viewer sends it twice!!

(x-screen:define-keyboard-signature :vncserver-87-enhanced
  (:keycode-offset 8
   :vendor-name "The XFree86 Project, Inc"
   :vendor-version 40300000)

  ;8
  (9   :escape                                      )
  (10  :digit-one            :exclamation-point     ) ; Checked
  (11  :digit-two            :commercial-at         ) ; Checked
  (12  :digit-three          :number-sign           ) ; Checked
  (13  :digit-four           :dollar-sign           ) ; Checked
  (14  :digit-five           :percent-sign          ) ; Checked
  (15  :digit-six            :circumflex-accent     ) ; Checked
  (16  :digit-seven          :ampersand             ) ; Checked
  (17  :digit-eight          :asterisk              ) ; Checked
  (18  :digit-nine           :left-parenthesis      ) ; Checked
  (19  :digit-zero           :right-parenthesis     ) ; Checked
  (20  :hyphen               :low-line              ) ; Checked
  (21  :equals-sign          :plus-sign             ) ; Checked
  (22  :backspace                                   ) ; DELETE (i.e., rubout)
  (23  :tab                                         ) ; Checked

  (24  :latin-small-letter-q :latin-capital-letter-q) ; Checked
  (25  :latin-small-letter-w :latin-capital-letter-w) ; Checked
  (26  :latin-small-letter-e :latin-capital-letter-e) ; Checked
  (27  :latin-small-letter-r :latin-capital-letter-r) ; Checked
  (28  :latin-small-letter-t :latin-capital-letter-t) ; Checked
  (29  :latin-small-letter-y :latin-capital-letter-y) ; Checked
  (30  :latin-small-letter-u :latin-capital-letter-u) ; Checked
  (31  :latin-small-letter-i :latin-capital-letter-i) ; Checked
  (32  :latin-small-letter-o :latin-capital-letter-o) ; Checked
  (33  :latin-small-letter-p :latin-capital-letter-p) ; Checked
  (34  :right-square-bracket :right-curly-bracket   ) ; Checked
  (35  :left-square-bracket  :left-curly-bracket    ) ; Checked
  (36  :return                                      ) ; Checked
  (37  :left-control                                ) ; Checked

  (38  :latin-small-letter-a :latin-capital-letter-a) ; Checked
  (39  :latin-small-letter-s :latin-capital-letter-s) ; Checked
  (40  :latin-small-letter-d :latin-capital-letter-d) ; Checked
  (41  :latin-small-letter-f :latin-capital-letter-f) ; Checked
  (42  :latin-small-letter-g :latin-capital-letter-g) ; Checked
  (43  :latin-small-letter-h :latin-capital-letter-h) ; Checked
  (44  :latin-small-letter-j :latin-capital-letter-j) ; Checked
  (45  :latin-small-letter-k :latin-capital-letter-k) ; Checked
  (46  :latin-small-letter-l :latin-capital-letter-l) ; Checked
  (47  :semicolon            :colon                 ) ; Checked
  (48  :apostrophe           :quotation-mark        ) ; Checked
  (49  :grave-accent         :tilde                 ) ; Checked
  (50  :left-shift                                  ) ; Checked
  (51  :reverse-solidus      :vertical-line         ) ; Checked

  (52  :latin-small-letter-z :latin-capital-letter-z) ; Checked
  (53  :latin-small-letter-x :latin-capital-letter-x) ; Checked
  (54  :latin-small-letter-c :latin-capital-letter-c) ; Checked
  (55  :latin-small-letter-v :latin-capital-letter-v) ; Checked
  (56  :latin-small-letter-b :latin-capital-letter-b) ; Checked
  (57  :latin-small-letter-n :latin-capital-letter-n) ; Checked
  (58  :latin-small-letter-m :latin-capital-letter-m) ; Checked
  (59  :comma                :less-than-sign        ) ; Checked
  (60  :full-stop            :greater-than-sign     ) ; Checked
  (61  :solidus              :question-mark         ) ; Checked
  (62  :right-shift                                 )

  ;63  Keypad *
  (64  :left-alt                                    ) ; Command - Alt_L
  (65  :space                                       )
  ;66

  (67  :f1                                          ) ; Checked
  (68  :f2                                          ) ; Checked
  (69  :f3                                          ) ; Checked
  (70  :f4                                          ) ; Checked
  (71  :f5                                          ) ; Checked
  (72  :f6                                          ) ; Checked
  (73  :f7                                          ) ; Checked
  (74  :f8                                          ) ; Checked
  (75  :f9                                          ) ; Checked
  (76  :f10                                         ) ; Checked

  (77 :num-lock                                     ) ; Num_Lock Pointer_EnableKeys
  (78 :scroll-lock                                  ) ; Not on Mac

  ;79 Keypad Home
  ;80 Keypad Up
  ;81 Keypad Prior
  ;82 Keypad Subtract
  ;83 Keypad Left
  ;84 Keypad Begin
  ;85 Keypad Right
  ;86 Keypad Add
  ;87 Keypad End
  ;88 Keypad Down
  ;89 Keypad Insert
  ;90 Keypad Delete
  ;92-94

  (95  :f11                                         ) ; Checked
  (96  :f12                                         ) ; Checked
  (97  :home                                        ) ; Checked
  (98  :up                                          ) ; Checked
  (99  :prior                                       ) ; PAGE UP
  (100 :left                                        ) ; Checked
  ;101

  (102 :right                                       ) ; Checked
  (103 :end                                         ) ; Checked
  (104 :down                                        ) ; Checked
  (105 :next                                        ) ; PAGE DOWN
  (106 :insert                                      ) ; HELP is INSERT on Windows
  (107 :delete                                      ) ; DELETE FORWARD
  ;108 Keypad Enter
  (109 :right-control                               ) ; RIGHT CTRL - ADDED 
  (110 :pause :break                                ) ; F15 is PAUSE on Windows
  (111 :print :execute                              ) ; F13 is PRINT SCREEN on Windows
  ;112 Keypad Divide
  (113 :right-alt                                   ) ; Right Command - Alt_R
  ;114-255

  ; We define our own codes here via xmodmap
  ;   F13           - 200
  ;   F14           - 201
  ;   F15           - 202
  ;   Super_L       - 203
  ;   Super_R       - 204
  ;   Caps_Lock     - 205
  ;   Help          - 206 - but VNC Viewer sends it twice!!
  (200 :f13)
  (201 :f14)
  (202 :f15)
  (203 :left-super)
  (204 :right-super)
  (205 :caps-lock)
  (206 :help)
)

(x-screen:define-keyboard-mapping :vncserver-87-enhanced () 
  ; (:leds ((1 :mode-lock) ; scroll lock?
  ;         (2 :caps-lock)
  ;         (3 :num-lock)))

  (:escape #\Select) ; This doesn't work

  (:f1  #\Function)
  (:f2  #\Network) ; #\Circle)
  (:f3 (:mode-lock :locking t))
  (:f4  #\Escape #\Escape #\ #\)

  (:f5  :left-symbol)
  (:f6  #\Suspend)
  (:f7  #\Resume)
  (:f8  #\Abort)

  (:f9  #\Page)
  (:f10 #\Refresh)
  (:f11 #\Complete)
  (:f12 #\Clear-Input)

  (:f13 #\Square)
  (:f14 #\Circle)
  (:f15 #\Triangle)

  (:home     #\Back-space)
  (:delete   #\Line)
  (:next     #\Scroll)
  (:prior    #\Keyboard:Back-Scroll)

  (:up       #\Keyboard:Up)
  (:left     #\Keyboard:Left)
  (:right    #\Keyboard:Right)
  (:down     #\Keyboard:Down)

  ; Remap the bottom modifier keys (note xmodmap'd keys in 2xx range)
  ; Genera:    Control   Super   Meta      SPACE    Meta    Control Symbol    Hyper
  ; Mac:       Control   Option  Command   SPACE    Command Option  Menu      Control
  ; Keycode:   37        203     64        65       113     204     ---       109
  ; Keysym+    Control_L Super_L Alt_L     space    Alt_R   Super_R NoSymbol  Control_R

)


; Mappings from key codes to actual keys
(defparameter
  *default-vncserver-87-enhanced-keyboard-mapping-table*
  (sys:make-keyboard-table
    '(
      ; Due to the offset of 8, we need to skip the first 8 of these...
      nil ; 8
      #\Select ; esc

      (#\1 #\!)
      (#\2 #\@)
      (#\3 #\#)
      (#\4 #\$)
      (#\5 #\%)
      (#\6 #\^)
      (#\7 #\&)
      (#\8 #\* nil #\)
      (#\9 #\()
      (#\0 #\))
      (#\- #\_ #\)
      (#\= #\+ #\ #\Š)
      #\Rubout ; 59 - Delete backwards
      #\Tab

      (#\q #\Q #\)
      (#\w #\W #\)
      (#\e #\E #\ #\)
      (#\r #\R #\)
      (#\t #\T #\)
      (#\y #\Y #\)
      (#\u #\U #\)
      (#\i #\I #\)
      (#\o #\O #\)
      (#\p #\P #\ #\)
      (#\[ #\{)
      (#\] #\})
      #\Return
      :left-control

      (#\a #\A nil #\)
      (#\s #\S)
      (#\d #\D nil #\)
      (#\f #\F)
      (#\g #\G #\ #\‰)
      (#\h #\H #\)
      (#\j #\J #\)
      (#\k #\K #\)
      (#\l #\L #\ #\ˆ)
      (#\; #\: nil #\Œ)
      (#\' #\" #\ )
      (#\` #\~ #\)
      :left-shift
      (#\\ #\|)

      (#\z #\Z)
      (#\x #\X)
      (#\c #\C)
      (#\v #\V)
      (#\b #\B nil #\)
      (#\n #\N)
      (#\m #\M)
      (#\, #\< #\)
      (#\. #\> #\)
      (#\/ #\? #\‡)
      :right-shift

      nil ; Keypad *
      :left-meta ; Command (was left-super)
      #\Space
      nil ; 66

      #\Function ; F1
      #\Network ; F2
      ((:mode-lock :locking t) (:mode-lock :locking t) (:mode-lock :locking t) (:mode-lock :locking t)) ; F3 - FIXME: Move it to another key, shifted?
      (#\Escape #\Escape #\ #\) ; F4

      :left-symbol
      #\Suspend ; F6
      #\Resume ; F7
      #\Abort ; F8

      #\Page ; F9
      #\Refresh ; F10

      nil ; num lock
      nil ; Could have been #\Circle ; F14 (Scroll Lock on Windows)

      nil ; Keypad Home
      nil ; Keypad Up
      nil ; Keypad Prior
      nil ; Keypad Subtract
      nil ; Keypad Left
      nil ; Keypad Begin
      nil ; Keypad Right
      nil ; Keypad Add
      nil ; Keypad End
      nil ; Keypad Down
      nil ; Keypad Next
      nil ; Keypad Insert
      nil ; Keypad Delete
      nil nil nil ; 92-94

      #\Complete ; F11
      #\Clear-Input ; F12
      #\Back-space ; Home
      #\Keyboard:Up ; Up
      #\Keyboard:Back-Scroll ; Prior (Page Up)
      #\Keyboard:Left ; Left
      nil ; 101

      #\Keyboard:Right ; Right
      #\End ; End
      #\Keyboard:Down ; Down
      #\Scroll ; Next (Page Down)
      nil ; (#\Help #\Help #\Symbol-Help #\Symbol-Help) ; Help (Insert on Windows)
      #\Line ; Delete Forward
      nil ; Keypad Enter

      :right-control
      nil ; #\Triangle ; F15 (Pause on Windows)
      nil ; #\Square ; F13 (Print Screen on Windows)
      nil ; Keypad Divide
      :right-meta ; right alt
     
      nil nil nil nil nil nil nil ; 114-120
      nil nil nil nil nil nil nil nil nil nil ; 121-
      nil nil nil nil nil nil nil nil nil nil ; 131-
      nil nil nil nil nil nil nil nil nil nil ; 141-
      nil nil nil nil nil nil nil nil nil nil ; 151-
      nil nil nil nil nil nil nil nil nil nil ; 161-
      nil nil nil nil nil nil nil nil nil nil ; 171-
      nil nil nil nil nil nil nil nil nil nil ; 181-
      nil nil nil nil nil nil nil nil nil ; 191-199

      ; Our custom xmodmap keys...
      ; keycode 200 = F13
      ; keycode 201 = F14
      ; keycode 202 = F15
      ; keycode 203 = Super_L
      ; keycode 204 = Super_R
      ; keycode 205 = Caps_Lock
      ; keycode 206 = Help
      #\Square
      #\Circle
      #\Triangle
      :left-super
      :right-super
      :caps-lock
      (#\Help #\Help #\Symbol-Help #\Symbol-Help)

      nil nil nil nil ; 207-210
      nil nil nil nil nil nil nil nil nil nil ; 211-
      nil nil nil nil nil nil nil nil nil nil ; 221-
      nil nil nil nil nil nil nil nil nil nil ; 231-
      nil nil nil nil nil nil nil nil nil nil ; 241-
      nil nil nil nil nil ; 251-255
      ; Due to the offset of 8, we need to add 8 more at the end
      nil nil nil nil nil nil nil nil ; 0-7 offset
      )))



; FIXME: Finish the rest of the mapping work below ...
      
; This defines the shape of the keyboard for the
; Show Keyboard Layout command and the Keyboard Control activity.
(sys:define-keyboard-layout :vncserver-87-enhanced
  (:pretty-name "Vncserver 87-key Enhanced")
  (:default-mapping-table *default-vncserver-87-enhanced-keyboard-mapping-table*)
  (:numeric-id #x000500DD)

  ; Actual Mac Key Codes are actually these numbers (see Karabiner's docs for an example).
  ; (And 8 less than Xquartz keycodes.)
  ; Apple's keyboards have all text in lower case except the letter keys
  ; and the F-keys, so I mimic that here.

  (:row :left 0 :top 0
        :keys ((:legend "esc" :code 1 :keysym (#xff #x1b))))
  (:row :left 2 :top 0
        :keys ((:legend "F1" :code 59  :keysym (#xff #xbe))
               (:legend "F2" :code 60  :keysym (#xff #xbf))
               (:legend "F3" :code 61  :keysym (#xff #xc0))
               (:legend "F4" :code 62  :keysym (#xff #xc1))))
  (:row :left 13/2 :top 0
        :keys ((:legend "F5" :code 63  :keysym (#xff #xc2))
               (:legend "F6" :code 64  :keysym (#xff #xc3))
               (:legend "F7" :code 65  :keysym (#xff #xc4))
               (:legend "F8" :code 66  :keysym (#xff #xc5))))
  (:row :left 11 :top 0
        :keys ((:legend "F9"  :code 67 :keysym (#xff #xc6))
               (:legend "F10" :code 68 :keysym (#xff #xc7))
               (:legend "F11" :code 87 :keysym (#xff #xc8))
               (:legend "F12" :code 88 :keysym (#xff #xc9))))
  (:row :left 61/4 :top 0
        :keys ((:legend ("F13" "PrtSc") :code 192 :keysym (#xff #xca))   ; Assigned keycode 200 for F13
               (:legend ("F14" "ScrLk") :code 193 :keysym (#xff #xcb))   ; Assigned keycode 201 for F14
               (:legend ("F15" "Pause") :code 194 :keysym (#xff #xcc)))) ; Assigned keycode 202 for F15

  (:row :left 0 :top 3/2
        :keys ((:legend ("~" "`")      :code 41 :keysym (#x00 #x60))
               (:legend ("!" "1")      :code 2  :keysym (#x00 #x31))
               (:legend ("@" "2")      :code 3  :keysym (#x00 #x32))
               (:legend ("#" "3")      :code 4  :keysym (#x00 #x33))
               (:legend ("$" "4")      :code 5  :keysym (#x00 #x34))
               (:legend ("%" "5")      :code 6  :keysym (#x00 #x35))
               (:legend ("^" "6")      :code 7  :keysym (#x00 #x36))
               (:legend ("&" "7")      :code 8  :keysym (#x00 #x37))
               (:legend ("*" "8")      :code 9  :keysym (#x00 #x38))
               (:legend ("(" "9")      :code 10 :keysym (#x00 #x39))
               (:legend (")" "0")      :code 11 :keysym (#x00 #x30))
               (:legend ("_" "-")      :code 12 :keysym (#x00 #x2d))
               (:legend ("+" "=")      :code 13 :keysym (#x00 #x3d))
               (:legend ("delete")     :code 14 :keysym (#xff #x08) :width 2)))

  (:row :left 0 :top 5/2
        :keys ((:legend "tab"      :code 15 :keysym (#xff #x09) :width 3/2)
               (:legend "Q"        :code 16 :keysym (#x00 #x71))
               (:legend "W"        :code 17 :keysym (#x00 #x77))
               (:legend "E"        :code 18 :keysym (#x00 #x65))
               (:legend "R"        :code 19 :keysym (#x00 #x72))
               (:legend "T"        :code 20 :keysym (#x00 #x74))
               (:legend "Y"        :code 21 :keysym (#x00 #x79))
               (:legend "U"        :code 22 :keysym (#x00 #x75))
               (:legend "I"        :code 23 :keysym (#x00 #x69))
               (:legend "O"        :code 24 :keysym (#x00 #x6f))
               (:legend "P"        :code 25 :keysym (#x00 #x70))
               (:legend ("{" "[")  :code 26 :keysym (#x00 #x5b))
               (:legend ("}" "]")  :code 27 :keysym (#x00 #x5d))
               (:legend ("|" "\\") :code 43 :keysym (#x00 #x5c) :width 3/2)))

  (:row :left 0 :top 7/2
        :keys ((:legend "caps lock" :code 205 :keysym (#xff #xe5) :width 7/4) ; Assigned 205
               (:legend "A"         :code 30 :keysym (#x00 #x41))
               (:legend "S"         :code 31 :keysym (#x00 #x53))
               (:legend "D"         :code 32 :keysym (#x00 #x44))
               (:legend "F"         :code 33 :keysym (#x00 #x46))
               (:legend "G"         :code 34 :keysym (#x00 #x47))
               (:legend "H"         :code 35 :keysym (#x00 #x48))
               (:legend "J"         :code 36 :keysym (#x00 #x4a))
               (:legend "K"         :code 37 :keysym (#x00 #x4b))
               (:legend "L"         :code 38 :keysym (#x00 #x4c))
               (:legend (":" ";")   :code 39 :keysym (#x00 #x3b))
               (:legend ("\"" "'")  :code 40 :keysym (#x00 #x27))
               (:legend "return"    :code 28 :keysym (#xff #x0d) :width 9/4)))

  (:row :left 0 :top 9/2
        :keys ((:name "Left Shift" :legend "shift"  :code 42 :keysym (#xff #xe1) :width 9/4)
               (:legend "Z"                         :code 44 :keysym (#x00 #x7a))
               (:legend "X"                         :code 45 :keysym (#x00 #x78))
               (:legend "C"                         :code 46 :keysym (#x00 #x63))
               (:legend "V"                         :code 47 :keysym (#x00 #x76))
               (:legend "B"                         :code 48 :keysym (#x00 #x62))
               (:legend "N"                         :code 49 :keysym (#x00 #x6e))
               (:legend "M"                         :code 50 :keysym (#x00 #x6d))
               (:legend ("<" ",")                   :code 51 :keysym (#x00 #x2c))
               (:legend (">" ".")                   :code 52 :keysym (#x00 #x2e))
               (:legend ("?" "/")                   :code 53 :keysym (#x00 #x2f))
               (:name "Right Shift" :legend "shift" :code 54 :keysym (#xff #xe2) :width 11/4)))

  (:row :left 0 :top 11/2
        :keys ((:name "Left Control"  :legend "control"    :code 29  :keysym (#xff #xe3) :width 5/4)
               (:name "Left Option"   :legend ("alt" "option")   :code 195    :keysym (#xff #xeb) :width 5/4) ; Assigned 203
               (:name "Left Command"  :legend "command" :code 56  :keysym (#xff #xe9) :width 5/4)
               (:name "Space"         :legend nil       :code 57  :keysym (#x00 #x20) :width 25/4)
               (:name "Right Command" :legend "command" :code 105  :keysym (#xff #xea) :width 5/4)
               (:name "Right Option"  :legend ("alt" "option") :code 196   :keysym (#xff #xeb) :width 5/4) ; Assigned 204
               (:name "Menu"          :legend "menu"  :code 191   :width 5/4) ; No code, no keysym!
               (:name "Right Control" :legend "control"    :code 101  :keysym (#xff #xe4) :width 5/4)))

  (:row :left 61/4 :top 3/2
        :keys ((:legend "help"    :code 198       :keysym (#xff #x6a)) ; Insert on Windows - Assigned code 206
               (:legend "home"          :code 89 :keysym (#xff #x50))
               (:legend ("page" "up")   :code 91 :keysym (#xff #x55))))
  (:row :left 61/4 :top 5/2
        :keys ((:legend "del"         :code 99 :keysym (#xff #xff)) ; Forward delete
               (:legend "end"           :code 95 :keysym (#xff #x57))
               (:legend ("page" "down") :code 97 :keysym (#xff #x56))))

  (:row :left 65/4 :top 9/2
        :keys ((:legend "" :code 90 :keysym (#xff #x52)))) ; Up arrow
  (:row :left 61/4 :top 11/2
        :keys ((:legend "" :code 92 :keysym (#xff #x51))   ; Left arrow
               (:legend "" :code 96 :keysym (#xff #x54))   ; Down arrow
               (:legend "" :code 94 :keysym (#xff #x53)))) ; Right arrow
)



; Final mappings after setting up the :vncserver-87-enhanced keyboards in both
; X Mappings and Keyboard Layout. Necessary because for whatever reason, I cannot
; get the mappings above to fully "stick" unless these commands are run.

; How this works:
; (sys:set-keyboard-table-mapping WHAT-TO-SEND KEYBOARD-LAYOUT-NAME KEYCODE-INPUT 
;  :symbol t
;  :shift t
;  :all-shifts t)
; Note that the KEYCODE-INPUT is the X-Windows keycode MINUS 8!!!

; Fix ESC key
(sys:set-keyboard-table-mapping #\Select :vncserver-87-enhanced 1 :all-shifts  t)
; Fix DELETE key to be Rubout
(sys:set-keyboard-table-mapping #\Rubout :vncserver-87-enhanced 14 :all-shifts t)
; Fix DEL-> key to be Line
(sys:set-keyboard-table-mapping #\Line   :vncserver-87-enhanced 99 :all-shifts t)

; Fix right alt/option to be control
(sys:set-keyboard-table-mapping :right-control :vncserver-87-enhanced 196 :all-shifts t)
; Fix right control to be hyper
(sys:set-keyboard-table-mapping :right-hyper   :vncserver-87-enhanced 101 :all-shifts t)
