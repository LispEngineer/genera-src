;;; -*- Base: 10; Package: X-SCREEN; Mode: LISP; Syntax: Common-lisp; Lowercase: T -*-
;;;
;;; Douglas Fields's 87-key Tenkeyless modern keyboard

;; Douglas Fields - Copyright 2016 - See LICENSE.txt for CC BY-NC-SA details
;; https://symbolics.lisp.engineer/
;; Mac with an 87-Key Tenkeyless keyboard
;; Used under Xquartz 2.7.10_beta2

; I'm trying to make the ESCAPE key (keycode 61) become the SELECT
; key. However, it won't change in the "mapping" no matter what I try.
; But, if I map it below to F35, and then map F35 to Select, and then
; use xmodmap -e "keycode 61 = f35", it works. Hm.
; Maybe Genera is getting the keysyms from the X SERVER and not from
; the define-keyboard-signature ...!

; These keycodes here were checked with "xev -event keyboard"
; Using a CM Storm Quickfire Rapid - which has no MENU key
; and a Japanese USB keyboard - which has a MENU key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; The purpose of this signature seems to be to show what the X Windows server will
; tell us Genera is conencted to. If it matches, it will apply the
; corresponding keyboard mapping.

(define-keyboard-signature :xquartz-87-tenkeyless
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
  (69  :right-alt            :right-alt             ) ; RIGHT alt/option (right WIN on Windows) - ADDED
  (70  :right-control        :right-control         ) ; RIGHT CTRL - ADDED 
  (71  :right-meta           :right-meta            ) ; RIGHT COMMAND (ALT on Windows) - ADDED

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

(define-keyboard-mapping :xquartz-87-tenkeyless () ; (:leds ((1 :mode-lock) ; scroll lock?
                                               ;     (2 :caps-lock)
                                               ;     (3 :num-lock)))

  ; We can't seem to force any mapping of some keys using this
  ; mechanism, such as :backspace, :escape, :delete, etc.
  ; We can address these issues elsewhere.
  
  (:f1  #\Select #\Square)
  (:f2  #\Network #\Circle)
  (:f3  #\Function #\Triangle)
  (:f4  #\Suspend (:mode-lock :locking t))
  (:f5  #\Resume)
  (:f6  #\Abort)
  (:f7  :left-super)
  (:f8  :left-hyper)
  (:f9  #\Scroll #\Page)
  (:f10 #\Clear-Input #\Refresh)
  (:f11 #\Complete #\End)
  (:f12 #\Help)

  (:home     #\Keyboard:Home)
  (:up       #\Keyboard:Up)
  (:left     #\Keyboard:Left)
  (:right    #\Keyboard:Right)
  (:down     #\Keyboard:Down)
  (:next     #\Scroll)
  (:prior    #\Keyboard:Back-Scroll)

  ; Remap the bottom modifier keys
  ; Genera:    Control   Super  Meta      SPACE    Meta    Control Symbol    Hyper
  ; Mac:       Control   Option Command   SPACE    Command Option  Menu      Control
  ; Keycode:   67        66     63        57       69      71      118       70
  ; Keysym+    Control-L Alt_L  Meta_L    space    Alt_R   Meta_R  NoSymbol  Control_R
  ; BUT IS:                                                Meta              Control
  (:left-alt :left-super)
  (:right-alt :right-meta)
  (110 :right-symbol) ; This has to be 110 = 118 - 8

  ; These two don't work
  (:right-meta :right-control)
  (:right-control :right-hyper)
  ; These two also don't work
  (63 :right-control) ; 63 = 71 - 8
  (62 :right-hyper)   ; 62 = 70 - 8
)
