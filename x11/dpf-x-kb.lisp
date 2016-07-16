;;; -*- Base: 10; Package: X-SCREEN; Mode: LISP; Syntax: Common-lisp; Lowercase: T -*-
;;;
;;; Douglas Fields's 87-key Tenkeyless modern keyboard

;; Douglas Fields
;; Mac with an 87-Key Tenkeyless keyboard
;; Used under Xquartz 2.7.10_beta2

#||
(display-define-keyboard-signature-form :mac-87-tenkeyless '(:keycode-offset 8) "DPF")
||#

; These keycodes here were checked with "xev -event keyboard"
; Using a CM Storm Quickfire Rapid - which has no MENU key

(define-keyboard-signature :mac-87-tenkeyless
                           ;(:keycode-offset 8
                           (:keycode-offset 0
                            :vendor-name "Macintosh Xquartz 87-key Tenkeyless Keyboard")
  (8  :latin-small-letter-a :latin-capital-letter-a) ; Checked
  (9  :latin-small-letter-s :latin-capital-letter-s) ; Checked
  (10 :latin-small-letter-d :latin-capital-letter-d) ; Checked
  (11 :latin-small-letter-f :latin-capital-letter-f) ; Checked
  (12 :latin-small-letter-h :latin-capital-letter-h) ; Checked
  (13 :latin-small-letter-g :latin-capital-letter-g) ; Checked
  (14 :latin-small-letter-z :latin-capital-letter-z) ; Checked
  (15 :latin-small-letter-x :latin-capital-letter-x) ; Checked
  (16 :latin-small-letter-c :latin-capital-letter-c) ; Checked
  (17 :latin-small-letter-v :latin-capital-letter-v) ; Checked
  (19 :latin-small-letter-b :latin-capital-letter-b) ; Checked
  (20 :latin-small-letter-q :latin-capital-letter-q) ; Checked
  (21 :latin-small-letter-w :latin-capital-letter-w) ; Checked
  (22 :latin-small-letter-e :latin-capital-letter-e) ; Checked
  (23 :latin-small-letter-r :latin-capital-letter-r) ; Checked
  (24 :latin-small-letter-y :latin-capital-letter-y) ; Checked
  (25 :latin-small-letter-t :latin-capital-letter-t) ; Checked
  (26 :digit-one            :exclamation-point) ; Checked
  (27 :digit-two            :commercial-at) ; Checked
  (28 :digit-three          :number-sign) ; Checked
  (29 :digit-four           :dollar-sign) ; Checked
  (30 :digit-six            :circumflex-accent) ; Checked
  (31 :digit-five           :percent-sign) ; Checked
  (32 :equals-sign          :plus-sign) ; Checked
  (33 :digit-nine           :left-parenthesis) ; Checked
  (34 :digit-seven          :ampersand) ; Checked
  (35 :hyphen               :low-line) ; Checked
  (36 :digit-eight          :asterisk) ; Checked
  (37 :digit-zero           :right-parenthesis) ; Checked
  (38 :right-square-bracket :right-curly-bracket) ; Checked
  (39 :latin-small-letter-o :latin-capital-letter-o) ; Checked
  (40 :latin-small-letter-u :latin-capital-letter-u) ; Checked
  (41 :left-square-bracket  :left-curly-bracket) ; Checked
  (42 :latin-small-letter-i :latin-capital-letter-i) ; Checked
  (43 :latin-small-letter-p :latin-capital-letter-p) ; Checked
  (44 :return               :return) ; Checked
  (45 :latin-small-letter-l :latin-capital-letter-l) ; Checked
  (46 :latin-small-letter-j :latin-capital-letter-j) ; Checked
  (47 :apostrophe           :quotation-mark) ; Checked
  (48 :latin-small-letter-k :latin-capital-letter-k) ; Checked
  (49 :semicolon            :colon) ; Checked
  (50 :reverse-solidus      :vertical-line) ; Checked
  (51 :comma                :less-than-sign) ; Checked
  (52 :solidus              :question-mark) ; Checked
  (53 :latin-small-letter-n :latin-capital-letter-n) ; Checked
  (54 :latin-small-letter-m :latin-capital-letter-m) ; Checked
  (55 :full-stop            :greater-than-sign) ; Checked
  (56 :tab                  :tab) ; Checked
  (57 :space                :space) ; Checked
  (58 :grave-accent         :tilde) ; Checked
  (59 :backspace            :backspace) ; DELETE (i.e., rubout) - the key above the backslash
  (61 :escape               :escape) ; Checked

  ; Modifiers ----------------
  ; Xquartz can make alt/option send Alt_L or Mode_switch depending on 
  ; setting of "Option keys send Alt_L and Alt_R"
  (63 :left-meta            :left-meta) ; LEFT COMMAND (left ALT on Windows)
  (64 :left-shift           :left-shift) ; Checked
  (65 :caps-lock            :caps-lock) ; Sends two keycode 65 events in a row
  (66 :left-alt             :left-alt) ; This is LEFT alt/option (left WIN on Windows) -
  (67 :left-control         :left-control) ; Checked

  (68 :right-shift          :right-shift)
  (69 :right-alt            :right-alt) ; RIGHT alt/option (right WIN on Windows) - ADDED
  (70 :right-control        :right-control) ; RIGHT CTRL - ADDED 
  (71 :right-meta           :right-meta) ; RIGHT COMMAND (ALT on Windows) - ADDED
  ; FIXME: Add right MENU key - key code 118

  (104 :f5                  :f5) ; Checked
  (105 :f6                  :f6) ; Checked
  (106 :f7                  :f7) ; Checked
  (107 :f3                  :f3) ; Checked
  (108 :f8                  :f8) ; Checked
  (109 :f9                  :f9) ; Checked
  (111 :f11                 :f11) ; Checked
  (113 :f13                 :print) ; F13 is PRINT SCREEN on Windows
  (115 :f14                 :pause) ; F14 is SCROLL LOCK on Windows
  (117 :f10                 :f10) ; Checked
  (119 :f12                 :f12) ; Checked
  (121 :f15                 :pause) ; F15 is PAUSE on Windows
  (122 :help                :insert) ; HELP is INSERT on Windows
  (123 :home                :home) ; Checked
  (124 :prior               :prior) ; PAGE UP
  (125 :delete              :delete) ; DELETE FORWARD
  (126 :f4                  :f4) ; Checked
  (127 :end                 :end) ; Checked
  (128 :f2                  :f2) ; Checked
  (129 :next                :next) ; PAGE DOWN
  (130 :f1                  :f1) ; Checked
  (131 :left                :left) ; Checked
  (132 :right               :right) ; Checked
  (133 :down                :down) ; Checked
  (134 :up                  :up) ; Checked

;  (60 :keypad-enter :keypad-enter) ; NOT PRESENT
;  (73 :keypad-decimal-point :keypad-decimal-point) ; NOT PRESENT
;  (74 :right :asterisk) ; NOT PRESENT
;  (75 :keypad-multiplication-sign :keypad-multiplication-sign) ; NOT PRESENT
;  (77 :keypad-plus-sign :keypad-plus-sign) ; NOT PRESENT
;  (78 :left :plus-sign) ; NOT PRESENT
;  (79 :clear :clear) ; NOT PRESENT
;  (80 :down :equals-sign) ; NOT PRESENT
;  (83 :keypad-division-sign :keypad-division-sign) ; NOT PRESENT
;  (84 :keypad-enter :keypad-enter) ; NOT PRESENT
;  (85 :up :solidus) ; NOT PRESENT
;  (86 :keypad-minus-sign :keypad-minus-sign) ; NOT PRESENT
;  (89 :keypad-equals-sign :keypad-equals-sign) ; NOT PRESENT
;  (90 :keypad-digit-zero :keypad-digit-zero) ; NOT PRESENT
;  (91 :keypad-digit-one :keypad-digit-one) ; NOT PRESENT
;  (92 :keypad-digit-two :keypad-digit-two) ; NOT PRESENT
;  (93 :keypad-digit-three :keypad-digit-three) ; NOT PRESENT
;  (94 :keypad-digit-four :keypad-digit-four) ; NOT PRESENT
;  (95 :keypad-digit-five :keypad-digit-five) ; NOT PRESENT
;  (96 :keypad-digit-six :keypad-digit-six) ; NOT PRESENT
;  (97 :keypad-digit-seven :keypad-digit-seven) ; NOT PRESENT
;  (99 :keypad-digit-eight :keypad-digit-eight) ; NOT PRESENT
;  (100 :keypad-digit-nine :keypad-digit-nine) ; NOT PRESENT


  ; The highest keycode we will natively have seems to be 134.
  ; Keycodes after this seem to be related to OPTION-X and OPTION-SHIFT-X
  ; keys, which I cannot get X to generate. However, definitely see the
  ; apple-extended keyboard for more details.
)

; Intended mapping of bottom row:
; Control Super Meta   SPACE   Meta Control Symbol Hyper

(define-keyboard-mapping :mac-87-tenkeyless (:leds ((1 :mode-lock) ; scroll lock?
                                                    (2 :caps-lock)
                                                    (3 :num-lock)))
  ; #\Select and the others are special Genera-defined character names
  (:f1  #\Escape #\Square)
  (:escape #\Select)
  
  ; Set the "delete" key to actaully delete backwards, i.e., rubout
  (:backspace #\Rubout)
  ; Now the delete key can do something else (but what?)
  (:delete #\backspace)

  (:f2  #\Network #\Circle)
  (:f3  #\Function #\Triangle)
  (:f4  #\Suspend (:mode-lock :locking t))
  (:f5  #\Resume)
  (:f6  #\Abort)
  ; (:f7  :left-super)
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
  (:print    #\Keyboard:Print)
  (:undo     #\Keyboard:Undo)
  (:redo     #\Keyboard:Redo)
  (:find     #\Keyboard:Find)
  (:next     #\Scroll)
  (:prior    #\Keyboard:Back-Scroll)

  (:break    #\Suspend)

  (:num-lock (:num-lock :locking t))

  (:left-alt :left-super)
  (:right-alt :right-symbol)
)

