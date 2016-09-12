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
      
;; Load an (unsigned) byte from memory
;; TODO: Make this safe for the actual memory size
(defun mem-byte (loc)
  (aref *z-mem* loc))

;; Load an (unsigned) word from memory - MSB first
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
        
;; This gets a word at the specified location (MSB first),
;; strips off the top two bits, then turns it into a signed
;; number (from its 14-bit twos compliment representation).
;; Used for branches. (Spec 4.7)
(defun mem-signed-14-bit (loc)
  (us-to-s (mem-word loc) 14))

;; Convert the unsigned (positive) "bits"-bit integer
;; provided to a signed "bits"-bit integer. First we
;; make sure it's only got those lower "bits" bits.
(defun us-to-s (anum bits)
  (let* ((mask (1- (ash 1 bits))) ; An all 1's bit mask of bits bits
         (num (boole boole-and anum mask))) ; Our masked number (just in case)
    ;; Top bit is the sign bit
    (if (zerop (get-bit num (1- bits)))
        ;; We're not negative, so no changes needed.
        num
        ;; Sign bit says we're negative, so two's compliment
        ;; ourselves to positive and arithmetically make us negative.
        ;; Reminder: XOR anything with 1 will invert it.
        (- (1+ (boole boole-xor num mask))))))

  
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
;; Give operand byte lengths to all the above possibilities (Spec 4.2)
(defparameter +op-type-len+
  '((const-large . 2)
    (const-small . 1)
    (variable    . 1)
    (omitted     . 0)))

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
;; 54 = 0101_0100 = add
;;      0         = Long form decoder (4.3)
;;       1        = First operand is a variable (4.4.2)
;;        0       = Second operand is a small constant (4.4.2)
;;         1_0100 = Opcode ("add") (4.3.2)


(defstruct decoded-instruction
  memory-location   ; Where this instruction starts
  first-byte        ; What the first byte of the instruction is
  form              ; 'long, 'short, 'variable (Spec 4.3)
  operand-count     ; '0OP, 1OP, 2OP, VAR (Spec 4.3)
  operand-types     ; list of 'const-large, 'const-small, 'variable
  opcode            ; The actual opcode (within the operand-count)
  opcode-info       ; The oci of the opcode or nil if invalid
  operands          ; list of bytes or words
  store             ; Variable ID to store into, or nil (Spec 4.6)
  branch-if         ; t or nil: we branch when the condition is true or false
  branch-offset     ; Where we branch to, a signed number (Spec 4.7)
  length            ; How long the instruction is in bytes
  text-loc          ; If has string: start memory location (Spec 4.8)
  text-data         ; If has string: raw string binary as vector
  text-ascii        ; If has string: this is the local computer text (ASCII?) form
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
    ;; We start with length 1 byte, and add more as necessary
    (setf (decoded-instruction-length          retval) 1)
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
    ;; Decode the opcode number into opcode information
    (setf (decoded-instruction-opcode-info retval)
           (aref (cdr (assoc (decoded-instruction-operand-count retval) +opcode-names+))
                 (decoded-instruction-opcode retval)))
    ;; Assert: Now we know these things:
    ;; 1. Opcode info
    ;; 2. Operand types (if any) as a list
    ;; 3. Instruction length so far (1 or 2 bytes)
    ;;
    ;; Next, let's load the Operands (Spec 4.1) from the operand types
    (decode-operands retval)
    ;; We have to decode the store, if necessary for this opcode
    (when (oi-store (decoded-instruction-opcode-info retval))
      (decode-store-variable retval))
    ;; We also have to decode the branch offset, if necessary for this opcode
    (when (oi-branch (decoded-instruction-opcode-info retval))
      (decode-branch-offset retval))
    ;; XXX: CODE ME: Finally, we have to get our text to print, if necessary
    (when (oi-text (decoded-instruction-opcode-info retval))
      (decode-instruction-text retval))
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
    ;; OPEN QUESTION: Does this differ based upon 'VAR or '2OP?
    (di-decode-op-type-byte retval)
    ;; Opcode number in bottom 5 bits
    (setf (decoded-instruction-opcode retval)
          (get-bits (decoded-instruction-first-byte retval) 4 5)) ;; FIXME: Magic numbers
    retval))

;; Decodes an operator type byte (Spec 4.4.3).
;; This consists of four two-bit fields.
;; If it's a '2OP we always include the first two (even if they
;; are omitted), but for a VAR we cut the operand list at the first
;; omitted one.
;; The instruction length is incremented since we decoded another byte.
(defun di-decode-op-type-byte (retval)
  (let* ((op-types (mem-byte (1+ (decoded-instruction-memory-location retval))))
         ;; Make our full list
         (otlist (list
                  (aref +op-type-list+
                        (get-bits op-types 7 2)) ; 7-6 first per Spec 4.4.3
                  (aref +op-type-list+
                        (get-bits op-types 5 2))
                  (aref +op-type-list+
                        (get-bits op-types 3 2))
                  (aref +op-type-list+
                        (get-bits op-types 1 2)))))
    ;; If we have a type byte, our instruction is one longer
    (incf (decoded-instruction-length retval) 1)
    ;; Only VARIABLE forms will have a types byte. If it's a 2OP version, then
    ;; we should cut the list its two items.
    (setf (decoded-instruction-operand-types retval)
          (if (eq (decoded-instruction-operand-count retval) '2OP)
              (subseq otlist 0 2) ; First two operands
              (subseq otlist 0 (position 'omitted otlist)))) ; Operands before 1st omitted
    retval))

;; Loads the operands of the instruction per the
;; (previously decoded) operand type list. (Spec 4.1, 4.2)
;; Increments the instruction length for each operand loaded.
(defun decode-operands (retval)
  ;; Store our current memory location from which we'll read the
  ;; next operand value and the total length of operands read so far.
  (let ((curmemloc (+ (decoded-instruction-memory-location retval)
                      (decoded-instruction-length retval)))
        (oplen 0))
    ;; Now get our operands one at a time
    (setf (decoded-instruction-operands retval)
          ;; By mapping a count & retrieve function to all operand types
          (map 'list
               (lambda (ot) ; operand type
                 (let ((origmemloc curmemloc))
                   (cond
                     ((eq ot 'const-large)
                      (incf curmemloc 2)
                      (incf oplen 2)
                      (mem-word origmemloc))
                     ;; Should never get 'omitted so the only remaining
                     ;; ones are one byte each, just differ in interpretation
                     (t
                      (incf curmemloc)
                      (incf oplen)
                      (mem-byte origmemloc)))))
               (decoded-instruction-operand-types retval)))
    ;; Update our instruction length
    (incf (decoded-instruction-length retval) oplen)))

;; We load the store variable into the instruction and
;; increment our instruction length. (Spec 4.6)
;; Assert: This is only called for instructions that we know
;; use a store variable.
(defun decode-store-variable (retval)
  (let ((curmemloc (+ (decoded-instruction-memory-location retval)
                      (decoded-instruction-length retval))))
    ;; Our instruction is one byte longer
    (incf (decoded-instruction-length retval))
    ;; And stores into this (stack, local, global)
    (setf (decoded-instruction-store retval)
          (mem-byte curmemloc))
    retval))

;; We load the branch information: (Spec 4.7)
;; 1. Branch if true or false (1 bit)
;; 2. Branch offset type (1 bit)
;;    a. 6-bit positive number
;;    b. 14-bit signed number
;; Assert: This is only called for instructions that we know
;; use a branch offset.
(defun decode-branch-offset (retval)
  (let* ((curmemloc (+ (decoded-instruction-memory-location retval)
                       (decoded-instruction-length retval)))
         (b1 (mem-byte curmemloc))
         (twobytes (zerop (get-bit b1 6))))
    ;; Do we branch if the result is true or false?
    (setf (decoded-instruction-branch-if retval)
          (not (zerop (get-bit b1 7))))
    ;; What is our offset?
    (setf (decoded-instruction-branch-offset retval)
          (if (not twobytes)
              ;; A 1-byte offset uses the bottom 6 bits as a positive only offset
              (get-bits b1 5 6)
              ;; A 2-byte offset uses the bottom 6 bits plus the next byte as a
              ;; signed 14-bit offset
              (mem-signed-14-bit curmemloc)))
    ;; And increment our instruction length
    (incf (decoded-instruction-length retval)
          (if twobytes 2 1))
    retval))

;; Precondition: This instruction has following text string.
;; 1. Pulls the encoded text string out
;; 2. TODO: Decodes the string into local character set (ASCII for now)
;; 3. Updates the length of the instruction with the length of the string
(defun decode-instruction-text (retval)
  ;; First get our text memory location
  (setf (decoded-instruction-text-loc retval)
        (+ (decoded-instruction-memory-location retval)
           (decoded-instruction-length retval)))
  ;; Now load our encoded text
  (setf (decoded-instruction-text-data retval)
        (mem-string (decoded-instruction-text-loc retval)))
  ;; Now update our length
  (incf (decoded-instruction-length retval)
        (length (decoded-instruction-text-data retval)))
  ;; XXX: Decode the text to local character set (ASCII?)
  retval)


;; String handling ---------------------------------------------------

;; See: Spec 3 "How text and characters are encoded"
;; Given a start memory location, returns a vector (that is actually
;; displaced into main memory - so be careful!) that contains an entire
;; Z-character string as encoded per Spec 3.2.
(defun mem-string (loc)
  (let ((lastloc (mem-string-find-end loc)))
    (mem-slice loc (- lastloc loc -2))))

;; Given the starting word location of a string, gives the word
;; location of the final word of the string (which could be the
;; same as the first word).
(defun mem-string-find-end (start)
  ;; Step through memory one word at a time (2 bytes)
  ;; and find our last word of text memory location.
  ;; TODO: Protect from memory overflow
  (do* ((lastword start               (+ lastword 2))
        (textword (mem-word lastword) (mem-word lastword)))
       ;; The top bit is set only in the last word of a string (Spec 3.2)
       ((nonzerop (get-bit textword 15)) lastword)))

;; Z-characters (Spec 3.2 - 3.5.end)
;; Note that these are ZSCII, but ZSCII for standard printable
;; ASCII character codes are the same, and I'm assuming a CL
;; being used runs on ASCII...
(defparameter +zchars-a0+
  #(#\Space     ; Character  0 is always a space
    nil nil nil ; Characters 1-3 are from the abbreviation table
    nil nil     ; Characters 4-5 are shifts to A1 and A2
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(defparameter +zchars-a1+
  #(#\Space     ; Character  0 is always a space
    nil nil nil ; Characters 1-3 are from the abbreviation table
    nil nil     ; Characters 4-5 are shifts to A1 and A2
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(defparameter +zchars-a2+
  #(#\Space     ; Character  0 is always a space
    nil nil nil ; Characters 1-3 are from the abbreviation table
    nil nil     ; Characters 4-5 are shifts to A1 and A2
    nil         ; ZSCII specifier (Spec 3.5.3 and 3.4)
    #\Newline
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\. #\, #\! #\? #\_ #\# #\' #\" #\/ #\\ #\- #\: #\( #\)))

;; Convert an encoded text Z-character string to a vector of
;; ZSCII codes. No decoding is done; we just break the string
;; as follows:
;; 1. Every two bytes is turned into a word
;; 2. Every word is turned into three 5-bit fields omitting the
;;    first bit (Spec 3.2)
;; 3. Those are added to the return vector
;; We return a vector of these 5-bit values.
(defun break-zchar-string (ztext)
  ;; Preallocate our return value array since it will have
  ;; a known size (3 entries per every two bytes in ztext)
  (let ((retval (make-array (* 3 (floor (length ztext) 2))
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    ;; For every two bytes, turn into three Z-characters
    (loop for (b1 b2) on (coerce ztext 'list) by #'cddr do
         ; (format t "~A ~A~%" b1 b2)
         (let ((combined (+ (* 256 b1) b2)))
           ;; We use vector-push-extend even though
           ;; vector-push should be fine, just in case...
           (vector-push-extend (get-bits combined 14 5) retval)
           (vector-push-extend (get-bits combined  9 5) retval)
           (vector-push-extend (get-bits combined  4 5) retval)))
    retval))

;; Convert a z-character string into a ZSCII string.
;; (This first version will be very inefficient.)
;; We'll build up a list of characters and strings in reverse
;; order, one item at a time,
;; then reverse it and concatenate it all into one string.
;; There are several special z-character sequences to take care of:
;;    0     = always a space (Spec 3.5.1)
;;    z x   = (z=1/2/3) Abbreviation 32 * (z - 1) + x (Spec 3.3)
;;    4 a   = shift to set a1 for a (can overlap, e.g., 5 5 a) (Spec 3.2.3)
;;    5 b   = shift to set a2 for b
;; a2:6 m n = ZSCII code m * 32 + n (a2 only) (Spec 3.4)
;;
;; NOTE: Abbreviation decoding is slightly modified, see Spec 3.3.1,
;; and not currently handled here.
;;
;; We'll step through each z-character in a modal fashion:
;; a0 = starting mode
;; a1 = mode after previous z-character was a 4
;; a2 = mode after previous z-character was a 5
;; abv = mode after previous z-character was a 1/2/3 (abv variable set to that)
;; zsc1 = mode after reading an a2:6 to read first zscii char (zscii-high set)
;; zsc2 = mode after reading an a2:6 and zscii-high
(defun z-characters-to-string (zchars)
  (let ((raccum nil)      ; List of accumulated components of output in reverse order
        (mode 'a0)        ; Decoding mode
        (abv nil)         ; (1, 2 or 3) which abbreviation initial number
        (zscii-high nil)) ; High caracter of a2:6 ZSCII decoder found
    (loop for zc across zchars do
         ;(format t "zc: ~A, start mode: ~A, " zc mode)
         (cond
           ;; Handle second byte of our abbreviation:
           ;; 1. Insert abbreviation
           ;; 2. Return to mode a0
           ((eq mode 'abv)
            (push (get-abbrev abv zc) raccum)
            (setf mode 'a0))
           ;; Handle first byte of zscii
           ;; 1. Save first byte
           ;; 2. Set mode to zsc2
           ((eq mode 'zsc1)
            (setf mode 'zsc2 zscii-high zc))
           ;; Handle second byte of zscii
           ;; 1. Add character to our accumulator
           ;; 2. Set mode to a0
           ((eq mode 'zsc2)
            (push (zscii-from-zchars zscii-high zc) raccum)
            (setf mode 'a0))
           ;; Handle beginning of zscii from a2 character 6
           ((and (eq mode 'a2) (= zc 6))
            (setf mode 'zsc1))
           ;; ASSERT: Mode = a0, a1, a2
           ;; Z-character 0 is always a space
           ((= zc 0)
            (push #\Space raccum)
            (setf mode 'a0))
           ;; Z-characters 1-3 are always abbreviations
           ((or (= zc 1) (= zc 2) (= zc 3))
            (setf mode 'abv abv zc))
           ;; Z-characters 4 and 5 are always shifts
           ((= zc 4)
            (setf mode 'a1))
           ((= zc 5)
            (setf mode 'a2))
           ;; All other characters can be looked up from one of the
           ;; three +zchars-a#+ lookup tables
           (t
            (push (aref (cdr (assoc mode +mode-to-chars+)) zc) raccum)
            (setf mode 'a0)))
         ;(format t "end mode: ~A~%" mode)
         )

    ;; Reverse our accumulator
    (setf raccum (nreverse raccum))
    ;; Concatenate our accumulator's strings and characters into
    ;; one giant string
    ;; XXX: CODE ME
    raccum))

;; Assoc list of modes to Z-character sets used in
;; z-characters-to-string above
(defparameter +mode-to-chars+
  (list (cons 'a0 +zchars-a0+)
        (cons 'a1 +zchars-a1+)
        (cons 'a2 +zchars-a2+)))

;; Make a zscii character from two 5-bit z-characters (Spec 3.8 and 3.4)
(defun zscii-from-zchars (a b)
  ;; Just assume this is ASCII for now <sigh>
  (code-char (+ (* a 32) b)))

;; Get a fully decoded abbreviation string ready for printing
;; (Spec 3.3, 3.3.1, and ???)
(defun get-abbrev (x y)
  ;; XXX: CODE ME
  (format nil "Abv:~A" (+ (* 32 (1- x)) y)))


         
            
            

  
           
;; Opcode Information ------------------------------------------------
                  
;; Opcode information (name, branch, store) - Spec 14
;; Names are for human readability
;; anything that is nil is an invalid opcode in v3
;; See "Reading the opcode tables" after Spec 14.2.4
(defstruct oci ; OpCode Information
  name         ; Symbol: Disassembly name of the opcode (Spec 14)
  store        ; Boolean: Store a result in a variable?
  branch       ; Boolean: Provides a label to jump to
  text)         ; Boolean: Does this opcode include a following text string
;; TODO: Add these?
;;  number     ; Integer: The opcode number (calculated)
;;  count      ; Symbol: '0OP, '1OP, '2OP, 'VAR
  
;; Easily construct an opcode information struct
(defmacro oci-> (name store branch &optional text)
  `(make-oci
    :name   (quote ,name)
    :store  ,store
    :branch ,branch
    :text   ,text))

;; nil-safe oci accessors
(defun oi-name   (o) (if (not o) nil (oci-name   o)))
(defun oi-store  (o) (if (not o) nil (oci-store  o)))
(defun oi-branch (o) (if (not o) nil (oci-branch o)))
(defun oi-text   (o) (if (not o) nil (oci-text   o)))



(defparameter +2op-opcode-names+
  (vector
    nil                         ; 0
    (oci-> je         nil t  )  ; 1
    (oci-> jl         nil t  )  ; 2
    (oci-> jg         nil t  )  ; 3
    (oci-> dec_chk    nil t  )  ; 4
    (oci-> inc_chk    nil t  )  ; 5
    (oci-> jin        nil t  )  ; 6
    (oci-> test       nil t  )  ; 7
    (oci-> or         t   nil)  ; 8
    (oci-> and        t   nil)  ; 9
    (oci-> test_attr  nil t  )  ; A
    (oci-> set_attr   nil nil)  ; B
    (oci-> clear_attr nil nil)  ; C
    (oci-> store      nil nil)  ; D
    (oci-> insert_obj nil nil)  ; E
    (oci-> loadw      t   nil)  ; F
    (oci-> loadb      t   nil)  ; 10
    (oci-> get_prop   t   nil)  ; 11
    (oci-> get_prop_addr t nil) ; 12
    (oci-> get_next_prop t nil) ; 13
    (oci-> add        t   nil)  ; 14
    (oci-> sub        t   nil)  ; 15
    (oci-> mul        t   nil)  ; 16
    (oci-> div        t   nil)  ; 17
    (oci-> mod        t   nil)  ; 18
    nil nil nil nil nil nil nil)) ; 19-1F

(defparameter +1op-opcode-names+
  (vector
    (oci-> jz           nil t  )  ; 0
    (oci-> get_sibling  t   t  )  ; 1
    (oci-> get_child    t   t  )  ; 2
    (oci-> get_parent   t   nil)  ; 3
    (oci-> get_prop_len t   nil)  ; 4
    (oci-> inc          nil nil)  ; 5
    (oci-> dec          nil nil)  ; 6
    (oci-> print_addr   nil nil)  ; 7
    nil                           ; 8
    (oci-> remove_obj   nil nil)  ; 9
    (oci-> print_obj    nil nil)  ; A
    (oci-> ret          nil nil)  ; B
    (oci-> jump         nil nil)  ; C
    (oci-> print_paddr  nil nil)  ; D
    (oci-> load         t   nil)  ; E
    (oci-> not          t   nil))) ; F

(defparameter +0op-opcode-names+
  (vector
    (oci-> rtrue       nil nil) ; 0
    (oci-> rfalse      nil nil) ; 1
    (oci-> print       nil nil t) ; 2 - includes a string
    (oci-> print_ret   nil nil t) ; 3 - includes a string
    (oci-> nop         nil nil) ; 4
    (oci-> save        nil t  ) ; 5
    (oci-> restore     nil t  ) ; 6
    (oci-> restart     nil nil) ; 7
    (oci-> ret_popped  nil nil) ; 8
    (oci-> pop         nil nil) ; 9
    (oci-> quit        nil nil) ; A 
    (oci-> new_line    nil nil) ; B
    (oci-> show_status nil nil) ; C
    (oci-> verify      nil t  ) ; D
    nil                         ; E
    nil))                       ; F

(defparameter +var-opcode-names+
  (vector
    (oci-> call          t   nil) ; 0
    (oci-> storew        nil nil) ; 1
    (oci-> storeb        nil nil) ; 2
    (oci-> put_prop      nil nil) ; 3
    (oci-> sread         nil nil) ; 4
    (oci-> print_char    nil nil) ; 5
    (oci-> print_num     nil nil) ; 6
    (oci-> random        t   nil) ; 7
    (oci-> push          nil nil) ; 8
    (oci-> pull          nil nil) ; 9
    (oci-> split_window  nil nil) ; A
    (oci-> set_window    nil nil) ; B
    nil                           ; C
    nil nil nil                   ; D-F
    nil nil nil                   ; 10-12
    (oci-> output_stream nil nil) ; 13
    (oci-> input_stream  nil nil) ; 14
    (oci-> sound_effect  nil nil) ; 15 (only used in one v3 game)
    nil nil nil                   ; 16-18
    nil nil nil                   ; 19-1B
    nil nil nil                   ; 1C-1E
    nil))                         ; 1F

;; Association list of ____ to names for the opcode number
(defparameter +opcode-names+
  (list
   (cons '2OP +2op-opcode-names+)
   (cons '1OP +1op-opcode-names+)
   (cons '0OP +0op-opcode-names+)
   (cons 'VAR +var-opcode-names+)))



;; Testing -------------------------------------------------------

;; Some good zork1.z3 opcode decoding memory locations
;; Disassembly generated by "txd -d -n"
(defparameter ++test-opcode-decode-locs++
  '(#x4f05   ; e0 03 2a 39 80 10 ff ff 00  CALL            5472 (#8010,#ffff) -> -(SP)
    #x4f0e   ; e1 97 00 00 01              STOREW          (SP)+,#00,#01
    #x4f3a   ; e3 57 9c 06 04              PUT_PROP        "magic boat",#06,#04
    #x4f51   ; 54 1e 02 00                 ADD             G0e,#02 -> -(SP)
    #x4f7e   ; 4a 10 03 c8                 TEST_ATTR       G00,#03 [TRUE] 4f88
    #x4f87   ; bb                          NEW_LINE
    #x4f8e   ; 2d 90 7f                    STORE           G80,G6f
    #x4f91   ; 6e 7f 10                    INSERT_OBJ      G6f,G00
    #x4f9e   ; 8c ff 66                    JUMP            4f05
    #x503c   ; 21 01 03 4d                 JE              #01,L02 [FALSE] 504b
    #x5040   ; a0 02 c6                    JZ              L01 [TRUE] 5047
    #x504e   ; b2 ...                      PRINT           "You wouldn't find any"
    #x4e3b   ; b2 ...                      PRINT           "a " ; shortest string!
    #x5010   ; b3 ...                      PRINT_RET       " here!""
    ))
(defparameter ++decoded-opcodes++
  (map 'list #'decode-instruction ++test-opcode-decode-locs++))

#|
;; Some tests of breaking strings
(break-zchar-string (decoded-instruction-text-data (car (last ++decoded-opcodes++))))
(break-zchar-string (decoded-instruction-text-data (car (last ++decoded-opcodes++ 2))))
|#
