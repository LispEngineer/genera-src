;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ZMI; Base: 10; Lowercase: Yes -*-

;; Z-Machine Interpreter for v3 only
;; See: http://inform-fiction.org/zmachine/standards/

;; Note: See here for meanings of different comments:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html

#+SBCL
(defpackage :zmi
  (:use :common-lisp))

#+Genera
(defpackage :zmi
  ;; See: ftp://ftp.ai.sri.com/pub/mailing-lists/slug/930331/msg00112.html
  (:use :clim-lisp)) ; Or the base ANSI-Common-Lisp package

(in-package :zmi)


;; Globals -----------------------------------------------------

;; The version of Z-machine code that we support
(defparameter +zm-version+ 3)

;; The default story file we load at Z-Machine startup
;; (remember that Genera doesn't have defconstant)
(defparameter +default-file+ "zork1.z3")

;; Z-Machine Memory
;; This is an unsigned byte 1-dimensional adjustable array (vector).
;; We pre-allocate 128KB and set current length to zero; maximum
;; story file size is 128KB in v3.
(defparameter +z-mem-length+ (* 128 1024))
(defvar *z-mem*
  (make-array +z-mem-length+
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

;; Z-Machine Program Counter
;; (starts at location given by +ml-initial-pc+)
(defvar *z-pc* 0)

;; Z-Machine Stack of 2-byte words (Spec 6.3)
;; TODO: Code me
;; Writing to variable 0x00 pushes a value on the stack
;; Reading from variable 0x00 pops a value from the stack

;; Z-Machine Routine Call State / Stack Frames (Spec 6.1, 6.3.2, 6.4, 6.5)
;; The zmrs structure contains all the local state of a Routine
;; call, including:
;;   Parameters - implemented as an array (of words)
;;   Local variables - implemented as an array (of words)
;;   Stack - implemented as a list (of words)
;;   Return address
;; The call stack is a list of these structures.
;; The starting routine is the lowest state, and has no parameters,
;; and a zero return address.
(defstruct zmrs     ; Z-Machine Routine State
  params            ; 8-size array of words with fill-pointer to actual #
  locals            ; 15-size array of words (set in routine header)
                    ;   with fill-pointer to actual # of locals
  stack             ; list (of words)
  return-address)   ; Address of the next opcode to run when we return
;; Maxmimum number of parameters that a routine can have
(defparameter +zmrs-max-params+ 8)
;; Maximum number of locals a parameter can have
(defparameter +zmrs-max-locals+ 15)
;; Our call stack - pile of ZMRS records as a list
(defvar *call-stack* '())

;; Variable numbers (Spec 4.2.2)
;; 0x00        = top of stack
;; 0x01 - 0x0f = current routine local variables (if exist)
;; 0x10 - 0xff = global variables (see +ml-loc-globals+)

;; Fixed Z-Machine Header Locations (for v3)
;; See Spec 11.1                       ; SIZE in bytes (?)
(defparameter +ml-version+       #x00) ; 1 Z-Machine version
(defparameter +ml-flags-1+       #x01) ; 1 Bit field
(defparameter +ml-high-mem-base+ #x04) ; 2 Base of high memory
(defparameter +ml-initial-pc+    #x06) ; 2 Initial value of program counter
(defparameter +ml-loc-dict+      #x08) ; 2 Location of dictionary
(defparameter +ml-loc-obj+       #x0A) ; 2 Location of object table
(defparameter +ml-loc-globals+   #x0C) ; 2 Location of global variables table
(defparameter +ml-loc-static+    #x0E) ; 2 Base of static memory
(defparameter +ml-flags-2+       #x10) ; 1 Flags 2 (bits 0-1 only in v3)
(defparameter +ml-loc-abbrev+    #x18) ; 2 Location of abbreviations table
(defparameter +ml-file-len+      #x1A) ;   Length of file /2 (Spec 11.1.6)
(defparameter +ml-file-cksum+    #x1C) ;   Checksum of file
(defparameter +ml-std-rev+       #x32) ;   Standard revision number
;; Spec Appendix B
(defparameter +ml-rel-num+       #x02) ; 2 Release number (word)
(defparameter +ml-serial-num+    #x12) ; 6 Serial number (six ASCII chars)

;; What the file length header needs to be multiplied by in order to
;; get the actual file length (Spec 11.1.6).
;; v1-3: 2
;; v4-5: 4
;; v6+: 8
(defparameter +file-length-divider+ 2)

;; Memory header length (Spec 1.1.1.1)
(defparameter +header-length+ #x40)


;; Memory map per the above (Spec 1.1)
;; +----------------+
;; | Dynamic memory | 0x00000 - (- +ml-loc-static+ 1) including header
;; +----------------+
;; | Static memory  | +ml-loc-static+ - lower of last byte of story or 0x0ffff
;; +----------------+
;; | High memory    | +ml-high-mem-base+ - end of story file
;; +----------------+ (may overlap static memory)

;; Global variables are stored starting at +ml-loc-globals+ and consist
;; of 240 2-byte words. (Spec 6.2)

;; Two-byte values are stored MSB first (Spec 2.1)


;; Implementation ---------------------------------------------------------


;; Memory -----------------------------------------------------------------

;; Loads a story file into the memory vector
;; Returns nil on failure
(defun load-file-to-memory (filename)
  (with-open-file (in filename :if-does-not-exist nil
                      :element-type '(unsigned-byte 8))
    (when in
      (setf *z-mem*
            (adjust-array *z-mem* +z-mem-length+
                          :fill-pointer (file-length in)))
      (read-sequence *z-mem* in)
      (close in))
    (not (not in)))) ; Convert result to t or nil
      
;; Load a byte from memory
;; TODO: Make this safe for the actual memory size
(defun mem-byte (loc)
  (aref *z-mem* loc))

;; Load a word from memory - MSB first
;; TODO: Make this safe for the actual memory size
(defun mem-word (loc)
  (+ (ash (aref *z-mem* loc) 8) ;; Positive ASH amounts are to the left
     (aref *z-mem* (1+ loc))))

;; Return a subset of memory as a vector starting at specified
;; location and including N bytes. This shares space with the
;; main z-machine memory so BE CAREFUL!!!
(defun mem-slice (loc slen)
  ;; TODO: Check limits of loc and slen
  (make-array slen
              :element-type '(unsigned-byte 8)
              :Displaced-to *z-mem*
              :displaced-index-offset loc))

;; Loads an ASCII string from memory
(defun mem-ascii (loc slen)
  ;; Get a sub-vector of memory starting at loc and
  ;; continuing for slen
  (map 'string #'code-char (mem-slice loc slen)))

;; Gets the serial number from the header as a string
;; mh = memory header
(defun mh-serial-num ()
  (mem-ascii +ml-serial-num+ 6))

;; Gets the file length from the header and adjusts
;; it to be a number in bytes.
;; TODO: Make this version sensitive
(defun mh-file-len ()
  (* +file-length-divider+ (mem-word +ml-file-len+)))

;; Calculates the checksum of the file per the
;; verify opcode/instruction (Spec page 103):
;; Sum all bytes unsigned from the header (0x40) onwards and take
;; the result modulo 0x10000 (i.e., lower 16 bits).
;; We need to stop at the header-specified file length.
;; TODO: Make this safe for memory size
(defun mem-calc-checksum ()
  (let* ((summed-area (mem-slice +header-length+ (- (mh-file-len) +header-length+)))
         (sum (reduce #'+ summed-area)))
    (mod sum #x10000)))
        
  
;; Routine Frames ---------------------------------------------------------

;; Creates a new zmrs structure and sets up appropriate empty values of all
;; the fields of the structure
(defun new-zmrs ()
  (let ((retval (make-zmrs)))
    (setf (zmrs-params retval)
          (make-array +zmrs-max-params+
                      :element-type '(unsigned-byte 16)
                      :adjustable t
                      :fill-pointer 0))
    (setf (zmrs-locals retval)
          (make-array +zmrs-max-locals+
                      :element-type '(unsigned-byte 16)
                      :adjustable t
                      :fill-pointer 0))
    (setf (zmrs-stack retval) '())
    (setf (zmrs-return-address retval) 0)
    retval))

;; Initialize the call stack for an entirely new game
(defun initialize-call-stack ()
  (setf *call-stack*
        (list (new-zmrs))))

;; Creates an initialized  ZMRS for calling a routine
;; at the specified address with the specified parameters,
;; and the specified return address.
;; This handles setting the locals up from the routine
;; header and also returns the first instruction address.
;; TODO: CODE ME

;; Story File Load/Initialization -----------------------------------------

;; Loads a story file and resets all state of the Z-M to be able to
;; immediately start executing the story. Returns t on success
;; and has a second return with an error message.
(defun load-story-file (filename)
  ;; Load the file
  (unless (load-file-to-memory filename)
    (return-from load-story-file
      (values nil (format nil "Could not load file: ~A" filename))))
  (let ((ver     (mem-byte +ml-version+))
        (hcksum  (mem-word +ml-file-cksum+))  ; Checksum from header
        (acksum  (mem-calc-checksum))         ; Actual checksum
        (init-pc (mem-word +ml-initial-pc+))
        (rel     (mem-word +ml-rel-num+))
        (serial  (mh-serial-num)))
    ;; Check the version
    (unless (equalp ver +zm-version+)
      (return-from load-story-file
        (values nil (format nil "Wrong version ~D in file: ~A" ver filename))))
    ;; Check the checksum (the game will do it itself, though)
    (unless (equalp hcksum acksum)
      (return-from load-story-file
        (values nil (format nil "Wrong checksum ~x (desired ~x) in file: ~A"
                            acksum hcksum filename))))
    ;; Create an empty call stack
    (initialize-call-stack)
    ;; Set the initial Program Counter (Spec 5.5)
    (setf *z-pc* init-pc)
    ;; Success
    (values t (format nil "Loaded ~A release ~D serial ~A" filename rel serial))))


;; Instruction Decoder --------------------------------------------------------

;; Per Spec 4.1, instructions are coded as follows:
;;
;; *Opcode            1 or 2 bytes
;;  Operand types     1 or 2 bytes, 4 or 8 2-bit fields
;; *Operands          0-16 bytes, 0-8 of these at 1 or 2 bytes each
;;  Store variable    1 byte
;;  Branch offset     1 or 2 bytes
;;  Text              Encoded string (unlimited length)
;;
;; Components with * exist in all instructions.

;; Operand types (Spec 4.2)
(defparameter +op-type-const-large+ #b00) ; 2 bytes, MSB first (Spec 4.2.1)
(defparameter +op-type-const-small+ #b01)
(defparameter +op-type-variable+    #b10) ; Variable by value (Spec 4.2.3)
(defparameter +op-type-omitted+     #b11)
;; Give names to all the above possibilities as an indexed array
(defparameter +op-type-list+
  #(const-large const-small variable omitted))

;; Instruction opcode forms (Spec 4.3)
;; These are "top two" bits of the opcode,
;; (except v5+ opcode 0xBE is always "extended" form - ignore for v3)
(defparameter +op-form-variable+    #b11)
(defparameter +op-form-short+       #b10)
(defparameter +op-form-long+        #b00) ; LSB in here is an operand type
(defparameter +op-form-long-2+      #b01)
;; Which opcode bit decides if we're not a long bit?
(defparameter +op-form-bit-not-long+ 7)
;; Which opcode bit decides if we're variable (and not short)?
(defparameter +op-form-bit-variable-not-short+ 6)

;; --- Operand Counts (0OP, 1OP, 2OP, VAR) ---
;; - Short form operand type (Spec 4.3.1)
;; Taken from bits 4 & 5
;; Opcode NUMBER is in bits 0-3 (bottom 4 bits)
(defparameter +op-short-count-0+    #b11) ; 0 operands, otherwise 1
;; - Long form operand type (Spec 4.3.2)
;; Operand count is always 2
;; Opcode NUMBER is in bottom 5 bits
;; - Variable form operand type (Spec 4.3.3)
;; Bit 5 == 0: 2OP otherwise VAR
;; Opcode NUMBER is in bottom 5 bits
(defparameter +op-var-count-2+      #b0) ; 2 operands if bit 5 is 0

;; --- Operand Types --- (Spec 4.4)
;; - Short form: Opcode Bits 4-5 give operand type (Spec 4.4.1)
;; per the +op-type-XXX+ choices above
;; - Long form (Spec 4.4.2)
;; Bits 6 and 5 give the types of the first and second operands respectively
;; The only choices small constant (if 0) or variable (if 1)
(defparameter +op-long-type-const-small+ #b0)
(defparameter +op-long-type-variable+    #b1)
;; - Variable form (Spec 4.4.3)
;; A byte of 4 operand types follows opcode per +op-type-XXX+ above.
;; Once one type is omitted, all remaining types are (must be) omitted.
;; Two opcodes are "double variable" with two bytes of opcode types
;; (Spec 4.4.3.1): opcode numbers 12, 26: call_vs2, call_vn2
;; but those are not supported in v3


;; NOTE: The initial version of this full Z-Machine will not be optimized for
;; performance in any way, shape, or form, but instead for correctness and
;; ease of reading/understanding the code.


;; Instruction decoding algorithm:
;; First byte:
;;   Bit 7 = 0 -> Long Form Decoder
;;   Bit 7 = 1
;;     Bit 6 = 0 -> Short Form Decoder
;;     Bit 6 = 1 -> Variable Form Decoder

;; Example:
;; E0 = 1110_0000 = call
;;      11        = Variable instruction form decoder (4.3)
;;        1       = VAR number of arguments (4.3.3)
;;         0_0000 = Opcode ("call") (14 page 73)
;; BB = 1011_1011 = new_line (0OP:187)
;;      10        = Short instruction form decoder
;;        11      = Operand type - omitted -> 0OP
;;           1011 = Opcode ("new_line") 


(defstruct decoded-instruction
  memory-location   ; Where this instruction starts
  first-byte        ; What the first byte of the instruction is
  form              ; 'long, 'short, 'variable (Spec 4.3)
  operand-count     ; '0OP, 1OP, 2OP, VAR (Spec 4.3)
  operand-types     ; list of 'const-large, 'const-small, 'variable
  opcode            ; The actual opcode (within the operand-count)
  opcode-name       ; The name of the opcode (for human consumption)
  operands          ; list of bytes or words
  ;; TODO: CODE ME
  )

;; Gets the specified bit of the specified byte
;; (Bit 0 = LSB, bit 7 = MSB)
(defun get-bit (byte bitnum)
  ;; TODO: Protect against bad inputs?
  ;; Negative ASH is to the right
  (boole boole-and (ash byte (- bitnum)) 1))

;; Gets the specified number of bits (MSB first) of the specified byte
(defun get-bits (byte bitnum numbits)
  ;; TODO: Protect against bad inputs?
  (let ((shiftamt  (- bitnum (1- numbits))) ; We shift off unneeded bits
        (andtarget (1- (ash 1 numbits))))
    (boole boole-and (ash byte (- shiftamt)) andtarget)))

(defun nonzerop (num)
  (not (zerop num)))

;; Top level instruction decoder: returns decoded-instruction
;; Determines if it's a long, short or variable form instruction
(defun decode-instruction (mloc)
  (let ((retval (make-decoded-instruction))
        (first  (mem-byte mloc)))
    (setf (decoded-instruction-memory-location retval) mloc)
    (setf (decoded-instruction-first-byte      retval) first)
    ;; First determine instruction form, and then handle that form
    (cond
      ((zerop (get-bit first +op-form-bit-not-long+)) ; Not long bit is clear?
       (setf (decoded-instruction-form retval) 'long)
       (di-decode-long retval))
      ((zerop (get-bit first +op-form-bit-variable-not-short+))
       (setf (decoded-instruction-form retval) 'short)
       (di-decode-short retval))
      (t ; For V3, if it's not long and not short, it's variable
       (setf (decoded-instruction-form retval) 'variable)
       (di-decode-variable retval)))
    ;; Decode the opcode number to a name for ease of debugging
    (setf (decoded-instruction-opcode-name retval)
          (aref (cdr (assoc (decoded-instruction-operand-count retval) +opcode-names+))
                (decoded-instruction-opcode retval)))
    retval))
           

;; Decode a long form instruction (Spec 4.3.2, 4.4.2)
(defun di-decode-long (retval)
  (let ()
    ;; Long form is always 2OP
    (setf (decoded-instruction-operand-count retval)
          '2OP)
    ;; Bits 6 & 5 give the operand types (Spec 4.4.2)
    ;; 0 = small constant, 1 = variable
    (setf (decoded-instruction-operand-types retval)
          (list
           (aref #(const-small variable)
                 (get-bit (decoded-instruction-first-byte retval) 6))
           (aref #(const-small variable)
                 (get-bit (decoded-instruction-first-byte retval) 5))))
    ;; Opcode is given in bottom 5 bits
    (setf (decoded-instruction-opcode retval)
          (get-bits (decoded-instruction-first-byte retval) 4 5)) ;; FIXME: Magic numbers
    retval))

;; Decode a short form instruction (Spec 4.3.1, 4.4.1)
(defun di-decode-short (retval)
  ;; Bits 4 and 5 give an operand type
  ;; FIXME: Magic numbers
  (let ((op-type (aref +op-type-list+
                       (get-bits (decoded-instruction-first-byte retval) 5 2))))
    ;; If the operand type is omitted then this is a 0OP, otherwise it's a 1OP
    (cond
      ((eq op-type 'omitted)
       (setf (decoded-instruction-operand-count retval) '0OP) ; Section 4.3.1
       (setf (decoded-instruction-operand-types retval) '())) ; Section 4.4.1
      (t
       (setf (decoded-instruction-operand-count retval) '1OP) ; Section 4.3.1
       (setf (decoded-instruction-operand-types retval) (list op-type)))) ; Section 4.4.1
    ;; Opcode is given in bottom 4 bits
    (setf (decoded-instruction-opcode retval)
          (get-bits (decoded-instruction-first-byte retval) 3 4)) ;; FIXME: Magic numbers
    retval))

;; Decode a variable form instruction (Spec 4.3.3, 4.4.3)
(defun di-decode-variable (retval)
  (let ()
    ;; If bit 5 is 0, then 2OP, otherwise VAR (Spec 4.3.3)
    (setf (decoded-instruction-operand-count retval)
          (if (zerop (get-bit (decoded-instruction-first-byte retval) 5))
              '2OP
              'VAR))
    ;; Get the operand types
    ;; Does this differ based upon 'VAR or '2OP?
    ;; XXX: CODE ME
    ;; Opcode number in bottom 5 bits
    (setf (decoded-instruction-opcode retval)
          (get-bits (decoded-instruction-first-byte retval) 4 5)) ;; FIXME: Magic numbers
    retval))

;; Opcode to name mappings - Spec 14
;; These are for human readability
;; anything that is nil is an invalid opcode in v3

(defparameter +2op-opcode-names+
  #(nil         ; 0
    je          ; 1
    jl          ; 2
    jg          ; 3
    dec_chk     ; 4
    inc_chk     ; 5
    jin         ; 6
    test        ; 7
    or          ; 8
    and         ; 9
    test_attr   ; A
    set_attr    ; B
    clear_attr  ; C
    store       ; D
    insert_obj  ; E
    loadw       ; F
    loadb       ; 10
    get_prop    ; 11
    get_prop_addr ; 12
    get_next_prop ; 13
    add           ; 14
    sub           ; 15
    mul           ; 16
    div           ; 17
    mod           ; 18
    nil nil nil nil nil nil nil))

(defparameter +1op-opcode-names+
  #(jz             ; 0
    get_sibling    ; 1
    get_child      ; 2
    get_parent     ; 3
    get_prop_len   ; 4
    inc            ; 5
    dec            ; 6
    print_addr     ; 7
    nil            ; 8
    remove_obj     ; 9
    print_obj      ; A
    ret            ; B
    jump           ; C
    print_paddr    ; D
    load           ; E
    not))          ; F

(defparameter +0op-opcode-names+
  #(rtrue        ; 0
    rfalse       ; 1
    print        ; 2
    print_ret    ; 3
    nop          ; 4
    save         ; 5
    restore      ; 6
    restart      ; 7
    ret_popped   ; 8
    pop          ; 9
    quit         ; A 
    new_line     ; B
    show_status  ; C
    verify       ; D
    nil          ; E
    nil))        ; F

(defparameter +var-opcode-names+
  #(call          ; 0
    storew        ; 1
    storeb        ; 2
    put_prop      ; 3
    sread         ; 4
    print_char    ; 5
    print_num     ; 6
    random        ; 7
    push          ; 8
    pull          ; 9
    split_window  ; A
    set_window    ; B
    nil           ; C
    nil nil nil   ; D-F
    nil nil nil   ; 10-12
    output_stream ; 13
    input_stream  ; 14
    sound_effect  ; 15 (only used in one v3 game)
    nil nil nil   ; 16-18
    nil nil nil   ; 19-1B
    nil nil nil   ; 1C-1E
    nil))         ; 1F

;; Association list of ____ to names for the opcode number
(defparameter +opcode-names+
  (list
   (cons '2OP +2op-opcode-names+)
   (cons '1OP +1op-opcode-names+)
   (cons '0OP +0op-opcode-names+)
   (cons 'VAR +var-opcode-names+)))
