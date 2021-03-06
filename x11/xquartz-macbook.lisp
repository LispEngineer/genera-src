;;; -*- Base: 10; Mode: LISP; Syntax: Common-lisp; Lowercase: T -*-
;;;
;;; Xquartz mapping for MacBook Air/Pro keyboard

;; Douglas Fields - Copyright 2016 - See LICENSE.txt for CC BY-NC-SA details
;; https://symbolics.lisp.engineer/
;; Used under Xquartz 2.7.10_beta2

;; This was cloned from xquartz-87-enh.lisp and modified from there.

; Modifiers are different:
;   Genera:    Control   Super  Meta      SPACE    Symbol    Hyper
;   MBA/P:     Control   Option Command   SPACE    Command   Option
;   Keycode:   67        66     63        57       71        69
;   Keysym+    Control-L Alt_L  Meta_L    space    Meta_R    Alt_R


; Key mappings differing from xquartz-87-enh.lisp:
;          Unshifted      Shifted     Notes
; F5       Help
; F6       Suspend        Square
; F7       Resume         Circle
; F8       Abort          Triangle
; enter    Return         Line
; del->    Back-space                 Fn-Delete
; home     <unmapped>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(x-screen:define-keyboard-signature :xquartz-macbook
  (:keycode-offset 8
   :vendor-name "The X.Org Foundation"
   :vendor-version 11803000)
  (8   :latin-small-letter-a :latin-capital-letter-a) ; Checked
  (9   :latin-small-letter-s :latin-capital-letter-s) ; Checked
  (10  :latin-small-letter-d :latin-capital-letter-d) ; Checked
  (11  :latin-small-letter-f :latin-capital-letter-f) ; Checked
  (12  :latin-small-letter-h :latin-capital-letter-h) ; Checked
  (13  :latin-small-letter-g :latin-capital-letter-g) ; Checked
  (14  :latin-small-letter-z :latin-capital-letter-z) ; Checked
  (15  :latin-small-letter-x :latin-capital-letter-x) ; Checked
  (16  :latin-small-letter-c :latin-capital-letter-c) ; Checked
  (17  :latin-small-letter-v :latin-capital-letter-v) ; Checked
  (19  :latin-small-letter-b :latin-capital-letter-b) ; Checked
  (20  :latin-small-letter-q :latin-capital-letter-q) ; Checked
  (21  :latin-small-letter-w :latin-capital-letter-w) ; Checked
  (22  :latin-small-letter-e :latin-capital-letter-e) ; Checked
  (23  :latin-small-letter-r :latin-capital-letter-r) ; Checked
  (24  :latin-small-letter-y :latin-capital-letter-y) ; Checked
  (25  :latin-small-letter-t :latin-capital-letter-t) ; Checked
  (26  :digit-one            :exclamation-point     ) ; Checked
  (27  :digit-two            :commercial-at         ) ; Checked
  (28  :digit-three          :number-sign           ) ; Checked
  (29  :digit-four           :dollar-sign           ) ; Checked
  (30  :digit-six            :circumflex-accent     ) ; Checked
  (31  :digit-five           :percent-sign          ) ; Checked
  (32  :equals-sign          :plus-sign             ) ; Checked
  (33  :digit-nine           :left-parenthesis      ) ; Checked
  (34  :digit-seven          :ampersand             ) ; Checked
  (35  :hyphen               :low-line              ) ; Checked
  (36  :digit-eight          :asterisk              ) ; Checked
  (37  :digit-zero           :right-parenthesis     ) ; Checked
  (38  :right-square-bracket :right-curly-bracket   ) ; Checked
  (39  :latin-small-letter-o :latin-capital-letter-o) ; Checked
  (40  :latin-small-letter-u :latin-capital-letter-u) ; Checked
  (41  :left-square-bracket  :left-curly-bracket    ) ; Checked
  (42  :latin-small-letter-i :latin-capital-letter-i) ; Checked
  (43  :latin-small-letter-p :latin-capital-letter-p) ; Checked
  (44  :return               :return                ) ; Checked
  (45  :latin-small-letter-l :latin-capital-letter-l) ; Checked
  (46  :latin-small-letter-j :latin-capital-letter-j) ; Checked
  (47  :apostrophe           :quotation-mark        ) ; Checked
  (48  :latin-small-letter-k :latin-capital-letter-k) ; Checked
  (49  :semicolon            :colon                 ) ; Checked
  (50  :reverse-solidus      :vertical-line         ) ; Checked
  (51  :comma                :less-than-sign        ) ; Checked
  (52  :solidus              :question-mark         ) ; Checked
  (53  :latin-small-letter-n :latin-capital-letter-n) ; Checked
  (54  :latin-small-letter-m :latin-capital-letter-m) ; Checked
  (55  :full-stop            :greater-than-sign     ) ; Checked
  (56  :tab                  :tab                   ) ; Checked
  (57  :space                :space                 ) ; Checked
  (58  :grave-accent         :tilde                 ) ; Checked
  (59  :backspace            :backspace             ) ; DELETE (i.e., rubout) - the key above the backslash
  (61  :escape               :escape                ) ; Checked

  ; Modifiers ----------------
  ; Xquartz can make alt/option send Alt_L or Mode_switch depending on 
  ; setting of "Option keys send Alt_L and Alt_R"
  (63  :left-meta            :left-meta             ) ; LEFT COMMAND (left ALT on Windows)
  (64  :left-shift           :left-shift            ) ; Checked
  (65  :caps-lock            :caps-lock             ) ; Sends two keycode 65 events in a row
  (66  :left-alt             :left-alt              ) ; This is LEFT alt/option (left WIN on Windows) -
  (67  :left-control         :left-control          ) ; Checked

  (68  :right-shift          :right-shift           )
  (69  :right-alt            :right-alt             ) ; RIGHT alt/option (right WIN on Windows) 
  (70  :right-control        :right-control         ) ; RIGHT CTRL - Not present on this keyboard
  (71  :right-meta           :right-meta            ) ; RIGHT COMMAND (ALT on Windows)

  (104 :f5                  :f5                     ) ; Checked
  (105 :f6                  :f6                     ) ; Checked
  (106 :f7                  :f7                     ) ; Checked
  (107 :f3                  :f3                     ) ; Checked
  (108 :f8                  :f8                     ) ; Checked
  (109 :f9                  :f9                     ) ; Checked
  (111 :f11                 :f11                    ) ; Checked
  (113 :f13                 :print                  ) ; F13 is PRINT SCREEN on Windows
  (115 :f14                 :pause                  ) ; F14 is SCROLL LOCK on Windows
  (117 :f10                 :f10                    ) ; Checked
  ;118 MENU KEY             MENU  ; Right MENU key - Note, this key auto-repeats(!)
  (119 :f12                 :f12                    ) ; Checked
  (121 :f15                 :pause                  ) ; F15 is PAUSE on Windows
  (122 :help                :insert                 ) ; HELP is INSERT on Windows
  (123 :home                :home                   ) ; Checked
  (124 :prior               :prior                  ) ; PAGE UP
  (125 :delete              :delete                 ) ; DELETE FORWARD
  (126 :f4                  :f4                     ) ; Checked
  (127 :end                 :end                    ) ; Checked
  (128 :f2                  :f2                     ) ; Checked
  (129 :next                :next                   ) ; PAGE DOWN
  (130 :f1                  :f1                     ) ; Checked
  (131 :left                :left                   ) ; Checked
  (132 :right               :right                  ) ; Checked
  (133 :down                :down                   ) ; Checked
  (134 :up                  :up                     ) ; Checked

  ; The highest keycode we will have seems to be 134.
  ; However, Xquartz reports that we have keycodes going up to 255.
)

(x-screen:define-keyboard-mapping :xquartz-macbook () 
  ; (:leds ((1 :mode-lock) ; scroll lock?
  ;     (2 :caps-lock)
  ;     (3 :num-lock)))

  (:escape #\Select) ; This doesn't work

  (:f1  #\Function)
  (:f2  #\Network)
  (:f3 (:mode-lock :locking t))
  (:f4  #\Escape #\Escape #\ #\)

  (:f5  #\Help)
  (:f6  #\Suspend      #\Square)
  (:f7  #\Resume       #\Circle)
  (:f8  #\Abort        #\Triangle)

  (:f9  #\Page)
  (:f10 #\Refresh)
  (:f11 #\Complete)
  (:f12 #\Clear-Input)

  (:return #\Return #\Line)

  ; (:home     #\Back-space) ; Home is unmapped
  (:delete   #\Back-space)
  (:next     #\Scroll)
  (:prior    #\Keyboard:Back-Scroll)

  (:up       #\Keyboard:Up)
  (:left     #\Keyboard:Left)
  (:right    #\Keyboard:Right)
  (:down     #\Keyboard:Down)

  ; These three don't work
  (:left-alt :left-super)
  (:right-alt :right-hyper)
  (:right-meta :right-symbol)
)


; Mappings from key codes to actual keys
(defparameter
  *default-xquartz-macbook-keyboard-mapping-table*
  (sys:make-keyboard-table
    '( ; nil nil nil nil nil nil nil nil ; 0-7
      ; Due to the offset of 8, we need to skip the first 8 of these...
      (#\a #\A nil #\) ; 8
      (#\s #\S) ; 9
      (#\d #\D nil #\�) ; 10
      (#\f #\F) ;11
      (#\h #\H #\) ; 12
      (#\g #\G #\ #\�) ; 13
      (#\z #\Z) ; 14
      (#\x #\X) ; 15
      (#\c #\C) ; 16
      (#\v #\V) ; 17
      nil ; 18 (section/plusminus reported by XQuartz)
      (#\b #\B nil #\) ; 19
      (#\q #\Q #\) ; 20
      (#\w #\W #\) ; 21
      (#\e #\E #\ #\) ; 22
      (#\r #\R #\) ; 23
      (#\y #\Y #\) ; 24
      (#\t #\T #\) ; 25
      (#\1 #\!) ; 26
      (#\2 #\@) ; 27
      (#\3 #\#) ; 28
      (#\4 #\$) ; 29
      (#\6 #\^) ; 30
      (#\5 #\%) ; 31
      (#\= #\+ #\ #\�) ; 32
      (#\9 #\() ; 33
      (#\7 #\&) ; 34
      (#\- #\_ #\) ; 35
      (#\8 #\* nil #\) ; 36
      (#\0 #\)) ; 37
      (#\] #\}) ; 38
      (#\o #\O #\) ; 39
      (#\u #\U #\) ; 40
      (#\[ #\{) ; 41
      (#\i #\I #\) ; 42
      (#\p #\P #\ #\) ; 43
      (#\Return #\Line #\Return #\Line) ; 44
      (#\l #\L #\ #\�) ; 45
      (#\j #\J #\) ; 46 
      (#\' #\" #\ ) ; 47
      (#\k #\K #\) ; 48
      (#\; #\: nil #\�) ; 49
      (#\\ #\|) ; 50
      (#\, #\< #\) ; 51
      (#\/ #\? #\�) ; 52
      (#\n #\N) ; 53
      (#\m #\M) ; 54
      (#\. #\> #\) ; 55
      #\Tab ; 56
      (#\Space #\Space #\Space #\Space) ; 57
      (#\` #\~ #\) ; 58
      #\Rubout ; 59 - Delete backwards
      nil ; 60 - ???
      #\Select ; 61 - esc
      nil ; 62
      :left-meta ; 63 - Left Command
      :left-shift ; 64
      :caps-lock ; 65
      :left-super ; 66 - Left Option/Alt
      :left-control ; 67
      :right-shift ; 68
      :right-symbol ; 69 - Right Option/Alt
      :right-control ; 70 - Right Control (not on MacBook keyboards)
      :right-hyper ; 71 - Right Command
      nil ; 72
      ;; Keypad is 73-100
      nil ; 73 - KP .
      nil ; 74
      nil ; 75 - KP *
      nil ; 76
      nil ; 77 - KP +
      nil nil nil nil nil ; 78-82
      nil ; 83 - KP /
      nil ; 84 - KP Enter
      nil ; 85
      nil ; 86 - KP -
      nil nil ; 87-88
      nil ; 89 - KP =
      nil nil nil nil nil nil nil nil ; 90-97 - KP 0 to 7
      nil ; 98
      nil nil ; 99-100 - KP 8-9
      nil nil nil ; 101-103
      (#\Help #\Help #\Symbol-Help #\Symbol-Help) ; 104 - F5
      (#\Suspend #\Square) ; 105 - F6
      (#\Resume #\Circle) ; 106 - F7
      ((:mode-lock :locking t) (:mode-lock :locking t) (:mode-lock :locking t) (:mode-lock :locking t)) ; 107 - F3 ; FIXME: Move it to another key, shifted?
      (#\Abort #\Triangle) ; 108 - F8
      #\Page ; 109 - F9
      nil ; 110
      #\Complete ; 111
      nil ; 112
      #\Square ; 113 - F13 (Print Screen on Windows) - Not on Macbook keyboard
      nil ; 114
      #\Circle ; 115 - F14 (Scroll Lock on Windows) - Not on Macbook keyboard
      nil ; 116
      #\Refresh ; 117 - F10
      :right-symbol ; 118 - Menu / Application / Context - This key auto-repeats!
      #\Clear-Input ; 119 - F12
      nil ; 120
      #\Triangle ; 121 - F15 (Pause on Windows) - Not on Macbook keyboard
      (#\Help #\Help #\Symbol-Help #\Symbol-Help) ; 122 - Help (Insert on Windows) - Not on Macbook keyboard
      nil ; 123 - Home - Leave Unmapped on MacBook Keyboard
      #\Keyboard:Back-Scroll ; 124 - Prior (Page Up)
      #\Back-Space ; 125 - Delete Forward
      (#\Escape #\Escape #\ #\) ; 126 - F4
      #\End ; 127 - End
      #\Network ; 128 - F2
      #\Scroll ; 129 - Next (Page Down)
      #\Function ; 130 - F1
      #\Keyboard:Left ; 131 - Left
      #\Keyboard:Right ; 132 - Right
      #\Keyboard:Down ; 133 - Down
      #\Keyboard:Up ; 134 - Up
      ; Other possibilities: #\Keyboard: Copy, Paste, Cut, Print; Line (#\line)
      ; :local :noop
      ; ((:scroll-lock :locking t) (:scroll-lock :locking t) (:scroll-lock :locking t) (:scroll-lock :locking t))
      ; ((:num-lock :locking t) (:num-lock :locking t) (:num-lock :locking t) (:num-lock :locking t))
      nil nil nil nil nil nil ; 135-140
      nil nil nil nil nil nil nil nil nil nil ; 151-
      nil nil nil nil nil nil nil nil nil nil ; 161-
      nil nil nil nil nil nil nil nil nil nil ; 171-
      nil nil nil nil nil nil nil nil nil nil ; 181-
      nil nil nil nil nil nil nil nil nil nil ; 191-
      nil nil nil nil nil nil nil nil nil nil ; 201-
      nil nil nil nil nil nil nil nil nil nil ; 211-
      nil nil nil nil nil nil nil nil nil nil ; 221-
      nil nil nil nil nil nil nil nil nil nil ; 231-
      nil nil nil nil nil nil nil nil nil nil ; 241-
      nil nil nil nil nil ; 251-255
      ; Due to the offset of 8, we need to add 8 more at the end
      nil nil nil nil nil nil nil nil ; 0-7 offset
      )))

      
; This defines the shape of the keyboard for the
; Show Keyboard Layout command and the Keyboard Control activity.
(sys:define-keyboard-layout :xquartz-macbook
  (:pretty-name "Xquartz MacBook")
  (:default-mapping-table *default-xquartz-macbook-keyboard-mapping-table*)
  (:numeric-id #x000500DC)

  ; Actual Mac Key Codes are actually these numbers (see Karabiner's docs for an example).
  ; (And 8 less than Xquartz keycodes.)
  ; Apple's keyboards have all text in lower case except the letter keys
  ; and the F-keys, so I mimic that here.

  (:row :left 0 :top 0
        :keys ((:legend "esc" :code 53  :keysym (#xff #x1b) :height 1/2 :width 29/28)
               (:legend "F1"  :code 122 :keysym (#xff #xbe) :height 1/2 :width 29/28)
               (:legend "F2"  :code 120 :keysym (#xff #xbf) :height 1/2 :width 29/28)
               (:legend "F3"  :code 99  :keysym (#xff #xc0) :height 1/2 :width 29/28)
               (:legend "F4"  :code 118 :keysym (#xff #xc1) :height 1/2 :width 29/28)
               (:legend "F5"  :code 96  :keysym (#xff #xc2) :height 1/2 :width 29/28)
               (:legend "F6"  :code 97  :keysym (#xff #xc3) :height 1/2 :width 29/28)
               (:legend "F7"  :code 98  :keysym (#xff #xc4) :height 1/2 :width 29/28)
               (:legend "F8"  :code 100 :keysym (#xff #xc5) :height 1/2 :width 29/28)
               (:legend "F9"  :code 101 :keysym (#xff #xc6) :height 1/2 :width 29/28)
               (:legend "F10" :code 109 :keysym (#xff #xc7) :height 1/2 :width 29/28)
               (:legend "F11" :code 103 :keysym (#xff #xc8) :height 1/2 :width 29/28)
               (:legend "F12" :code 111 :keysym (#xff #xc9) :height 1/2 :width 29/28)
               (:legend "pwr" :code 200                     :height 1/2 :width 29/28)))

  (:row :left 0 :top 1/2
        :keys ((:legend ("~" "`")      :code 50 :keysym (#x00 #x60))
               (:legend ("!" "1")      :code 18 :keysym (#x00 #x31))
               (:legend ("@" "2")      :code 19 :keysym (#x00 #x32))
               (:legend ("#" "3")      :code 20 :keysym (#x00 #x33))
               (:legend ("$" "4")      :code 21 :keysym (#x00 #x34))
               (:legend ("%" "5")      :code 23 :keysym (#x00 #x35))
               (:legend ("^" "6")      :code 22 :keysym (#x00 #x36))
               (:legend ("&" "7")      :code 26 :keysym (#x00 #x37))
               (:legend ("*" "8")      :code 28 :keysym (#x00 #x38))
               (:legend ("(" "9")      :code 25 :keysym (#x00 #x39))
               (:legend (")" "0")      :code 29 :keysym (#x00 #x30))
               (:legend ("_" "-")      :code 27 :keysym (#x00 #x2d))
               (:legend ("+" "=")      :code 24 :keysym (#x00 #x3d))
               (:legend ("delete")     :code 51 :keysym (#xff #x08) :width 3/2)))

  (:row :left 0 :top 3/2
        :keys ((:legend "tab"      :code 48 :keysym (#xff #x09) :width 3/2)
               (:legend "Q"        :code 12 :keysym (#x00 #x71))
               (:legend "W"        :code 13 :keysym (#x00 #x77))
               (:legend "E"        :code 14 :keysym (#x00 #x65))
               (:legend "R"        :code 15 :keysym (#x00 #x72))
               (:legend "T"        :code 17 :keysym (#x00 #x74))
               (:legend "Y"        :code 16 :keysym (#x00 #x79))
               (:legend "U"        :code 32 :keysym (#x00 #x75))
               (:legend "I"        :code 34 :keysym (#x00 #x69))
               (:legend "O"        :code 31 :keysym (#x00 #x6f))
               (:legend "P"        :code 35 :keysym (#x00 #x70))
               (:legend ("{" "[")  :code 33 :keysym (#x00 #x5b))
               (:legend ("}" "]")  :code 30 :keysym (#x00 #x5d))
               (:legend ("|" "\\") :code 42 :keysym (#x00 #x5c))))

  (:row :left 0 :top 5/2
        :keys ((:legend "caps lock" :code 57 :keysym (#xff #xe5) :width 7/4)
               (:legend "A"         :code  0 :keysym (#x00 #x41))
               (:legend "S"         :code  1 :keysym (#x00 #x53))
               (:legend "D"         :code  2 :keysym (#x00 #x44))
               (:legend "F"         :code  3 :keysym (#x00 #x46))
               (:legend "G"         :code  5 :keysym (#x00 #x47))
               (:legend "H"         :code  4 :keysym (#x00 #x48))
               (:legend "J"         :code 38 :keysym (#x00 #x4a))
               (:legend "K"         :code 40 :keysym (#x00 #x4b))
               (:legend "L"         :code 37 :keysym (#x00 #x4c))
               (:legend (":" ";")   :code 41 :keysym (#x00 #x3b))
               (:legend ("\"" "'")  :code 39 :keysym (#x00 #x27))
               (:legend ("enter" "return") :code 36 :keysym (#xff #x0d) :width 7/4)))

  (:row :left 0 :top 7/2
        :keys ((:name "Left Shift" :legend "shift"  :code 56 :keysym (#xff #xe1) :width 9/4)
               (:legend "Z"                         :code  6 :keysym (#x00 #x7a))
               (:legend "X"                         :code  7 :keysym (#x00 #x78))
               (:legend "C"                         :code  8 :keysym (#x00 #x63))
               (:legend "V"                         :code  9 :keysym (#x00 #x76))
               (:legend "B"                         :code 11 :keysym (#x00 #x62))
               (:legend "N"                         :code 45 :keysym (#x00 #x6e))
               (:legend "M"                         :code 46 :keysym (#x00 #x6d))
               (:legend ("<" ",")                   :code 43 :keysym (#x00 #x2c))
               (:legend (">" ".")                   :code 47 :keysym (#x00 #x2e))
               (:legend ("?" "/")                   :code 44 :keysym (#x00 #x2f))
               (:name "Right Shift" :legend "shift" :code 60 :keysym (#xff #xe2) :width 9/4)))

  (:row :left 0 :top 9/2
        :keys ((:name "Fn"            :legend "fn" :code 201)
               (:name "Left Control"  :legend "control"    :code 59  :keysym (#xff #xe3))
               (:name "Left Option"   :legend ("alt" "option")   :code 58  :keysym (#xff #xe9))
               (:name "Left Command"  :legend "command" :code 55  :keysym (#xff #xe7) :width 5/4)
               (:name "Space"         :legend nil       :code 49  :keysym (#x00 #x20) :width 5)
               (:name "Right Command" :legend "command" :code 63  :keysym (#xff #xe8) :width 5/4)
               (:name "Right Option"  :legend ("alt" "option")  :code 61  :keysym (#xff #xea))))

  (:row :left 25/2 :top 9/2
        :keys ((:legend "" :code 126 :keysym (#xff #x52) :height 1/2))) ; Up arrow
  (:row :left 23/2 :top 10/2
        :keys ((:legend "" :code 123 :keysym (#xff #x51) :height 1/2)   ; Left arrow
               (:legend "" :code 125 :keysym (#xff #x54) :height 1/2)   ; Down arrow
               (:legend "" :code 124 :keysym (#xff #x53) :height 1/2))) ; Right arrow

  ; Home, End, Page Up and Page Down are fn-arrow keys
  (:row :left 25/2 :top 23/4
        :keys ((:legend "fn " :code 116 :keysym (#xff #x55) :height 1/2))) ; Page Up
  (:row :left 23/2 :top 25/4
        :keys ((:legend "fn " :code 115 :keysym (#xff #x50) :height 1/2)   ; Home
               (:legend "fn " :code 121 :keysym (#xff #x56) :height 1/2)   ; Page Down
               (:legend "fn " :code 119 :keysym (#xff #x57) :height 1/2))) ; End

  ; del is fn-delete
  (:row :left 15/2 :top 23/4
        :keys ((:legend "fn delete" :code 117 :keysym (#xff #xff) :width 3/2)))
)



; Final mappings after setting up the :xquartz-87-enhanced keyboards in both
; X Mappings and Keyboard Layout. Necessary because for whatever reason, I cannot
; get the mappings above to fully "stick" unless these commands are run.

; How this works:
; (sys:set-keyboard-table-mapping WHAT-TO-SEND KEYBOARD-LAYOUT-NAME KEYCODE-INPUT 
;  :symbol t
;  :shift t
;  :all-shifts t)
;
; KEYCODES here are the ones above MINUS 8 (from the X offset)
; 0x3F =  63 = Right Command = 71 in X-Windows Keycodes
;         61 = Right Option  = 69 in X-Windows Keycodes
;         51 = delete        = 59 in X-Windows Keycodes and called BackSpace
;        117 = delete      = 125 in X-Windows called Delete
;         53 = esc
;         96 = F5

; Right Command -> Hyper
(sys:set-keyboard-table-mapping :right-hyper  :xquartz-macbook 63 :all-shifts t)
; Right Option -> Symbol
(sys:set-keyboard-table-mapping :right-symbol :xquartz-macbook 61 :all-shifts t)
; There is no Right Control on this keyboard

; Keyboard  -> Genera
; delete    -> Rubout
; sh-delete -> Backspace 
; Delete->  -> Backspace
(sys:set-keyboard-table-mapping #\rubout     :xquartz-macbook 51               )
(sys:set-keyboard-table-mapping #\back-space :xquartz-macbook 51  :shift t     )
(sys:set-keyboard-table-mapping #\back-space :xquartz-macbook 117 :all-shifts t)

; Make esc key into Select
(sys:set-keyboard-table-mapping #\select :xquartz-macbook 53 :all-shifts t)
