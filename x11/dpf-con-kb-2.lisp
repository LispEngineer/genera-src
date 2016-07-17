;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SYSTEM-APPLICATIONS; Base: 10; Lowercase: T -*-

;;; Douglas Fields
;;; https://symbolics.lisp.engineer/

;;; 87-key Tenkeyless keyboard under Xquartz
;;; See for reference: sys>embedding>keyboards> keyboards.lisp, mac-kcap.lisp, and pc-keyboards.lisp

(defparameter
  *default-xquartz-87-tenkeyless-keyboard-mapping-table*
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
      nil ; 18 (section/plusminus?)
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
      #\Return ; 44
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
      #\Space ; 57
      (#\` #\~ #\) ; 58
      #\Back-Space ; 59 - Delete backwards - FIXME: Map this to Rubout (swap Backspace and Delete)
      nil ; 60 - ???
      (#\Escape #\Escape #\ #\) ; 61 - FIXME: Map this to SELECT
      nil ; 62
      :left-meta ; 63 - Left Command
      :left-shift ; 64
      :caps-lock ; 65
      :left-super ; 66 - Left Option/Alt
      :left-control ; 67
      :right-shift ; 68
      :right-meta ; 69 - Right Command
      :right-hyper ; 70 - Right Control
      :right-control ; 71 - Right Option/Alt
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
      #\Resume ; 104 - F5
      #\Abort ; 105 - F6
      :left-super ; 106 - F7 - FIXME: Map something else
      (#\Function #\Triangle #\Function #\Triangle) ; 107 - F3
      :left-hyper ; 108 - F8 - FIXME: Map something else
      (#\Scroll #\Page #\Scroll #\Page) ; 109 - F9 - FIXME: Map something else than Scroll
      nil ; 110
      (#\Complete #\End #\Complete #\End) ; 111 - F11 - FIXME: Map something else than END
      nil ; 112
      nil ; 113 - F13 (Print Screen on Windows) - FIXME: Map something
      nil ; 114
      nil ; 115 - F14 (Scroll Lock on Windows) - FIXME: Map something
      nil ; 116
      (#\Clear-Input #\Refresh #\Clear-Input #\Refresh) ; 117 - F10
      :right-symbol ; 118 - Menu 
      (#\Help #\Help #\Symbol-Help #\Symbol-Help) ; 119 - F12 - FIXME: Map something else
      nil ; 120
      nil ; 121 - F15 (Pause on Windows) - FIXME: Map something
      (#\Help #\Help #\Symbol-Help #\Symbol-Help) ; 122 - Help (Insert on Windows)
      #\Keyboard:Home ; 123 - Home - FIXME: unnecessary
      #\Keyboard:Back-Scroll ; 124 - Prior (Page Up)
      #\Rubout ; 125 - Delete Forward - FIXME: make backspace
      (#\Suspend (:mode-lock :locking t) #\Suspend (:mode-lock :locking t)) ; 126 - F4
      #\End ; 127 - End
      (#\Network #\Circle #\Network #\Circle) ; 128 - F2
      #\Scroll ; 129 - Next (Page Down)
      (#\Select #\Square #\Select #\Square) ; 130 - F1
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


      

(sys:define-keyboard-layout :xquartz-87-tenkeyless
  (:pretty-name "Xquartz 87-key Tenkeyless")
  (:default-mapping-table *default-xquartz-87-tenkeyless-keyboard-mapping-table*)
  (:numeric-id #x000500DF)

  ; Due to the offset of 8 in dpf-x-kb-2.lisp, we need to reduce all code values by 8

  (:row :left 0 :top 0
        :keys ((:legend "Esc" :code 61 :keysym (#xff #x1b))))
  (:row :left 2 :top 0
        :keys ((:legend "F1" :code 130 :keysym (#xff #xbe))
               (:legend "F2" :code 128 :keysym (#xff #xbf))
               (:legend "F3" :code 107 :keysym (#xff #xc0))
               (:legend "F4" :code 126 :keysym (#xff #xc1))))
  (:row :left 13/2 :top 0
        :keys ((:legend "F5" :code 104 :keysym (#xff #xc2))
               (:legend "F6" :code 105 :keysym (#xff #xc3))
               (:legend "F7" :code 106 :keysym (#xff #xc4))
               (:legend "F8" :code 108 :keysym (#xff #xc5))))
  (:row :left 11 :top 0
        :keys ((:legend "F9"  :code 109 :keysym (#xff #xc6))
               (:legend "F10" :code 117 :keysym (#xff #xc7))
               (:legend "F11" :code 111 :keysym (#xff #xc8))
               (:legend "F12" :code 119 :keysym (#xff #xc9))))
  (:row :left 61/4 :top 0
        :keys ((:legend ("Print" "Screen") :code 113 :keysym (#xff #xca))
               (:legend ("Scroll" "Lock")  :code 115 :keysym (#xff #xcb))
               (:legend ("Pause")          :code 121 :keysym (#xff #xcc))))

  (:row :left 0 :top 3/2
        :keys ((:legend ("~" "`")      :code 58 :keysym (#x00 #x60))
               (:legend ("!" "1")      :code 26 :keysym (#x00 #x31))
               (:legend ("@" "2")      :code 27 :keysym (#x00 #x32))
               (:legend ("#" "3")      :code 28 :keysym (#x00 #x33))
               (:legend ("$" "4")      :code 29 :keysym (#x00 #x34))
               (:legend ("%" "5")      :code 31 :keysym (#x00 #x35))
               (:legend ("^" "6")      :code 30 :keysym (#x00 #x36))
               (:legend ("&" "7")      :code 34 :keysym (#x00 #x37))
               (:legend ("*" "8")      :code 36 :keysym (#x00 #x38))
               (:legend ("(" "9")      :code 33 :keysym (#x00 #x39))
               (:legend (")" "0")      :code 37 :keysym (#x00 #x30))
               (:legend ("_" "-")      :code 35 :keysym (#x00 #x2d))
               (:legend ("+" "=")      :code 32 :keysym (#x00 #x3d))
               (:legend ("Back Space") :code 59 :keysym (#xff #x08) :width 2)))

  (:row :left 0 :top 5/2
        :keys ((:legend "Tab"      :code 56 :keysym (#xff #x09) :width 3/2)
               (:legend "Q"        :code 20 :keysym (#x00 #x71))
               (:legend "W"        :code 21 :keysym (#x00 #x77))
               (:legend "E"        :code 22 :keysym (#x00 #x65))
               (:legend "R"        :code 23 :keysym (#x00 #x72))
               (:legend "T"        :code 25 :keysym (#x00 #x74))
               (:legend "Y"        :code 24 :keysym (#x00 #x79))
               (:legend "U"        :code 40 :keysym (#x00 #x75))
               (:legend "I"        :code 42 :keysym (#x00 #x69))
               (:legend "O"        :code 39 :keysym (#x00 #x6f))
               (:legend "P"        :code 43 :keysym (#x00 #x70))
               (:legend ("{" "[")  :code 41 :keysym (#x00 #x5b))
               (:legend ("}" "]")  :code 38 :keysym (#x00 #x5d))
               (:legend ("|" "\\") :code 50 :keysym (#x00 #x5c) :width 3/2)))

  (:row :left 0 :top 7/2
        :keys ((:legend "Caps Lock" :code 65 :keysym (#xff #xe5) :width 7/4)
               (:legend "A"         :code  8 :keysym (#x00 #x41))
               (:legend "S"         :code  9 :keysym (#x00 #x53))
               (:legend "D"         :code 10 :keysym (#x00 #x44))
               (:legend "F"         :code 11 :keysym (#x00 #x46))
               (:legend "G"         :code 13 :keysym (#x00 #x47))
               (:legend "H"         :code 12 :keysym (#x00 #x48))
               (:legend "J"         :code 46 :keysym (#x00 #x4a))
               (:legend "K"         :code 48 :keysym (#x00 #x4b))
               (:legend "L"         :code 45 :keysym (#x00 #x4c))
               (:legend (":" ";")   :code 49 :keysym (#x00 #x3b))
               (:legend ("\"" "'")  :code 47 :keysym (#x00 #x27))
               (:legend "Enter"     :code 44 :keysym (#xff #x0d) :width 9/4)))

  (:row :left 0 :top 9/2
        :keys ((:name "Left Shift" :legend "Shift"  :code 64 :keysym (#xff #xe1) :width 9/4)
               (:legend "Z"                         :code 14 :keysym (#x00 #x7a))
               (:legend "X"                         :code 15 :keysym (#x00 #x78))
               (:legend "C"                         :code 16 :keysym (#x00 #x63))
               (:legend "V"                         :code 17 :keysym (#x00 #x76))
               (:legend "B"                         :code 19 :keysym (#x00 #x62))
               (:legend "N"                         :code 53 :keysym (#x00 #x6e))
               (:legend "M"                         :code 54 :keysym (#x00 #x6d))
               (:legend ("<" ",")                   :code 51 :keysym (#x00 #x2c))
               (:legend (">" ".")                   :code 55 :keysym (#x00 #x2e))
               (:legend ("?" "/")                   :code 52 :keysym (#x00 #x2f))
               (:name "Right Shift" :legend "Shift" :code 68 :keysym (#xff #xe2) :width 11/4)))

  (:row :left 0 :top 11/2
        :keys ((:name "Left Control"  :legend "Ctrl"   :code 67  :keysym (#xff #xe3) :width 5/4)
               (:name "Left Option"   :legend "Option" :code 66  :keysym (#xff #xe9) :width 5/4)
               (:name "Left Alt"      :legend "Alt"    :code 63  :keysym (#xff #xe7) :width 5/4)
               (:name "Space"         :legend nil      :code 57  :keysym (#x00 #x20) :width 25/4)
               (:name "Right Alt"     :legend "Alt"    :code 69  :keysym (#xff #xea) :width 5/4)
               (:name "Right Option"  :legend "Option" :code 71  :keysym (#xff #xe8) :width 5/4)
               (:name "Menu"          :legend "Menu"   :code 118 :keysym (#x00 #x00) :width 5/4)
               (:name "Right Control" :legend "Ctrl"   :code 70  :keysym (#xff #xe4) :width 5/4)))

  (:row :left 61/4 :top 3/2
        :keys ((:legend "Help"          :code 122 :keysym (#xff #x6a)) ; Insert on Windows
               (:legend "Home"          :code 123 :keysym (#xff #x50))
               (:legend ("Page" "Up")   :code 124 :keysym (#xff #x55))))
  (:row :left 61/4 :top 5/2
        :keys ((:legend ("Del" "Fwd")   :code 125 :keysym (#xff #xff)) ; Forward delete
               (:legend "End"           :code 127 :keysym (#xff #x57))
               (:legend ("Page" "Down") :code 129 :keysym (#xff #x56))))

  (:row :left 65/4 :top 9/2
        :keys ((:legend "" :code 134 :keysym (#xff #x52)))) ; Up arrow
  (:row :left 61/4 :top 11/2
        :keys ((:legend "" :code 131 :keysym (#xff #x51))   ; Left arrow
               (:legend "" :code 133 :keysym (#xff #x54))   ; Down arrow
               (:legend "" :code 132 :keysym (#xff #x53)))) ; Right arrow
)



; Use the IBM style?
;(defparameter *xquartz-87-tenkeyless-layout*
;  '((:wide-key 0    0 1  1)    ; left top width num
;    (:row      2    0 2  5)    ; left top start end
;    (:row      13/2 0 6  9)
;    (:row      11   0 10 13)
;    (:row      61/4 0 14 16))
;)
;
;(define-ibm-keyboard-layout :xquartz-87-tenkeyless
;  :pretty-name "Xquartz 87-key Tenkeyless"
;  :numeric-id #x00030009
;  :key-layout *xquartz-87-tenkeyless-layout*
;  )

