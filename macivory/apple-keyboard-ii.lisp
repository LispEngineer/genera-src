; Apple Keyboard II Mappings:
; See >sys>embedding>keyboards.lisp for keycodes and ideas

; Keypad mapping                                       Keycode (Hex)
; help      suspend     resume     abort               47 51 4B 43
; complete  clear inp   square     mode lock           59 5B 5C 4E
; function  network     circle     back scroll         56 57 58 45
; refresh   escape      triangle   scroll              53 54 55 4C
; end..............     page       ......              52 52 41 4C
;
; Delete: Rubout, Shift-Delete: Backspace              33
; Return: Return, Shift-Return: Line                   24

; Unmapped Genera/Symbolics keys
; alt (previously F14)

; Shift-arrow keys send movement commands

; TODO:
; 1. Figure out how to remap the arrow keys to be:
;      hyper symbol mouse-m mouse-r       
;      (see notes at end)
; 2. How to tell the system to use the :elmer AKII keyboard, and modify
;    that mapping rather than the AEKII mapping

(sys:set-keyboard-table-mapping #\Help           :elmer #x47)
(sys:set-keyboard-table-mapping #\Help           :elmer #x47 :shift t)
(sys:set-keyboard-table-mapping #\Symbol-Help    :elmer #x47 :symbol t)
(sys:set-keyboard-table-mapping #\Symbol-Help    :elmer #x47 :shift t :symbol t)
(sys:set-keyboard-table-mapping #\Suspend        :elmer #x51 :all-shifts t)
(sys:set-keyboard-table-mapping #\Resume         :elmer #x4B :all-shifts t)
(sys:set-keyboard-table-mapping #\Abort          :elmer #x43 :all-shifts t)

(sys:set-keyboard-table-mapping #\Complete       :elmer #x59 :all-shifts t)
(sys:set-keyboard-table-mapping #\Clear-Input    :elmer #x5B :all-shifts t)
(sys:set-keyboard-table-mapping #\Square         :elmer #x5C :all-shifts t)
; Mode Lock can't be set this way
;(sys:set-keyboard-table-mapping #\Mode-Lock     :elmer #x4E :all-shifts t)
(sys:set-keyboard-table-mapping '(:mode-lock :locking t) :elmer #x4E :all-shifts t)

(sys:set-keyboard-table-mapping #\Function       :elmer #x56 :all-shifts t)
(sys:set-keyboard-table-mapping #\Network        :elmer #x57 :all-shifts t)
(sys:set-keyboard-table-mapping #\Circle         :elmer #x58 :all-shifts t)
; Back-Scroll isn't a character
;(sys:set-keyboard-table-mapping #\Back-Scroll   :elmer #x45 :all-shifts t)
(sys:set-keyboard-table-mapping #\Keyboard:Back-Scroll :elmer #x45 :all-shifts t)

(sys:set-keyboard-table-mapping #\Refresh        :elmer #x53 :all-shifts t)
(sys:set-keyboard-table-mapping #\Escape         :elmer #x54 :all-shifts t)
(sys:set-keyboard-table-mapping #\Triangle       :elmer #x55 :all-shifts t)
(sys:set-keyboard-table-mapping #\Scroll         :elmer #x4C :all-shifts t)

(sys:set-keyboard-table-mapping #\End            :elmer #x52 :all-shifts t)
(sys:set-keyboard-table-mapping #\Page           :elmer #x41 :all-shifts t)

(sys:set-keyboard-table-mapping #\Backspace      :elmer #x33 :shift t)
(sys:set-keyboard-table-mapping #\Line           :elmer #x24 :shift t)

; Remap shift arrow keys
(sys:set-keyboard-table-mapping #\Keyboard:Up    :elmer #x7E :shift t)
(sys:set-keyboard-table-mapping #\Keyboard:Down  :elmer #x7D :shift t)
(sys:set-keyboard-table-mapping #\Keyboard:Left  :elmer #x7B :shift t)
(sys:set-keyboard-table-mapping #\Keyboard:Right :elmer #x7C :shift t)

; Try making a mouse button key on Numpad 0 (52)
; This works and makes the key an instant mouse click.
; How do we make this into a "mouse shift"?
;(sys:set-keyboard-table-mapping :mouse-r :elmer #x52)
; The answer is in the system-applications:keyboard-layout flavor,
; in the default-mouse-button-shift-keys field,
; which for the :apple-extended (and probably also :elmer) keyboard is:
;  SYSTEM-APPLICATIONS:DEFAULT-MOUSE-BUTTON-SHIFT-KEYS:   ((1 125) (2 124))
; All keyboards are kept in the system-applications:*keyboard-layouts* variable.
; View one by clicking on it with SUPER-MOUSE-M (describe).
; Use the Flavor Examiner to look around.
; 
; To accomplish the goal, however, it seems that we need to write software that
; would allow one of the following:
; 1. Changing the Keyboard to :elmer (Apple Keyboard II) and reprogram that from scratch
; 2. Add flavor function to set the keyboard mouse button mapping and use it

