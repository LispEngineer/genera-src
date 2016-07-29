; Apple Keyboard II Mappings:
; See >sys>embedding>keyboards.lisp for keycodes and ideas

; Keypad mapping                                       Keycode (Hex)
; help      suspend     resume     abort               47 51 4B 43
; complete  clear inp   square     mode lock           59 5B 5C 4E
; function  network     circle     back scroll         56 57 58 45
; refresh   escape      triangle   scroll              53 54 55 4C
; (nothing...........)  page       ......              52 52 41 4C
;
; Delete: Rubout, Shift-Delete: Backspace              33
; Return: Return, Shift-Return: Line                   24

; Unmapped Genera/Symbolics keys
; alt (previously F14)

; Unused Mac Keyboard keys
; Keypad 0

; TODO:
; Figure out how to remap the arrow keys to be:
; hyper symbol mouse-m mouse-r       
; Figure out how to remap the SHIFT arrow-keys to be arrow keys

(sys:set-keyboard-table-mapping #\Help        :apple-extended #x47)
(sys:set-keyboard-table-mapping #\Help        :apple-extended #x47 :shift t)
(sys:set-keyboard-table-mapping #\Symbol-Help :apple-extended #x47 :symbol t)
(sys:set-keyboard-table-mapping #\Symbol-Help :apple-extended #x47 :shift t :symbol t)
(sys:set-keyboard-table-mapping #\Suspend     :apple-extended #x51 :all-shifts t)
(sys:set-keyboard-table-mapping #\Resume      :apple-extended #x4B :all-shifts t)
(sys:set-keyboard-table-mapping #\Abort       :apple-extended #x43 :all-shifts t)

(sys:set-keyboard-table-mapping #\Complete    :apple-extended #x59 :all-shifts t)
(sys:set-keyboard-table-mapping #\Clear-Input :apple-extended #x5B :all-shifts t)
(sys:set-keyboard-table-mapping #\Square      :apple-extended #x5C :all-shifts t)
; Mode Lock can't be set this way
;(sys:set-keyboard-table-mapping #\Mode-Lock   :apple-extended #x4E :all-shifts t)
(sys:set-keyboard-table-mapping '(:mode-lock :locking t)   :apple-extended #x4E :all-shifts t)

(sys:set-keyboard-table-mapping #\Function    :apple-extended #x56 :all-shifts t)
(sys:set-keyboard-table-mapping #\Network     :apple-extended #x57 :all-shifts t)
(sys:set-keyboard-table-mapping #\Circle      :apple-extended #x58 :all-shifts t)
; Back-Scroll isn't a character
;(sys:set-keyboard-table-mapping #\Back-Scroll :apple-extended #x45 :all-shifts t)
(sys:set-keyboard-table-mapping #\Keyboard:Back-Scroll :apple-extended #x45 :all-shifts t)

(sys:set-keyboard-table-mapping #\Refresh     :apple-extended #x53 :all-shifts t)
(sys:set-keyboard-table-mapping #\Escape      :apple-extended #x54 :all-shifts t)
(sys:set-keyboard-table-mapping #\Triangle    :apple-extended #x55 :all-shifts t)
(sys:set-keyboard-table-mapping #\Scroll      :apple-extended #x4C :all-shifts t)

;(sys:set-keyboard-table-mapping #\           :apple-extended #x :all-shifts t)
(sys:set-keyboard-table-mapping #\Page        :apple-extended #x41 :all-shifts t)

(sys:set-keyboard-table-mapping #\Backspace   :apple-extended #x33 :shift t)
(sys:set-keyboard-table-mapping #\Line        :apple-extended #x24 :shift t)

; Remap shift arrow keys
(sys:set-keyboard-table-mapping #\Keyboard:Up    :apple-extended #x7E :shift t)
(sys:set-keyboard-table-mapping #\Keyboard:Down  :apple-extended #x7D :shift t)
(sys:set-keyboard-table-mapping #\Keyboard:Left  :apple-extended #x7B :shift t)
(sys:set-keyboard-table-mapping #\Keyboard:Right :apple-extended #x7C :shift t)

; Try making a mouse button key on Numpad 0 (52)
; This works and makes the key an instant mouse click.
; How do we make this into a "mouse shift"?
;(sys:set-keyboard-table-mapping :mouse-r :apple-extended #x52)
; The answer is in the system-applications:keyboard-layout flavor,
; in the default-mouse-button-shift-keys field,
; which for the :apple-extended keyboard is:
;  SYSTEM-APPLICATIONS:DEFAULT-MOUSE-BUTTON-SHIFT-KEYS:   ((1 125) (2 124))
; All keyboards are kept in the system-applications:*keyboard-layouts* variable.
; View one by clicking on it with SUPER-MOUSE-M (describe).
; Use the Flavor Examiner to look around.
; 
; To accomplish the goal, however, it seems that we need to write software that
; would allow one of the following:
; 1. Changing the Keyboard to :elmer (Apple Keyboard II) and reprogram that from scratch
; 2. Add flavor function to set the keyboard mouse button mapping and use it