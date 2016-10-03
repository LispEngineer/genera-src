;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ZMI; Base: 10; Lowercase: Yes -*-

;; ZMI - Common Lisp Z-Machine Interpreter (hopefully reasonably portable)
;; Douglas P. Fields, Jr. - https://symbolics.lisp.engineer/

;; NOTE: On Genera, before loading this file, create the package.
;; Allowing the editor to create the package doesn't make it correctly.
;; At the Genera Lisp Listener:
;;  Set Lisp Context (a lisp syntax name [default Common-Lisp]) ANSI-Common-Lisp
;;  (defpackage ...using the command from below...)


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
  (:use future-common-lisp conditions))

(in-package :zmi)


;; Debugging ---------------------------------------------------

;; Is debugging output on?
(defparameter +debug+ t)

;; Which instructions are we debugging
(defparameter +debug-instrs+ nil)
#|
  '(insert_obj remove_obj print_obj
    test_attr set_attr clear_attr
    get_prop get_prop_addr get_next_prop get_prop_len
    get_sibling get_child get_parent))
|#

;; Writes to the output if debugging is on.
;; The string and subsequent arguments are exactly the same as format.
;; If obj is provided, it only prints debugging if that type
;; of object is currently being debugged
(defun dbg (stream obj string &rest values)
  ;; Don't debug when debugging is off
  (when (not +debug+)
    (return-from dbg))
  ;; Don't debug if our instruction isn't in the instruction list
  (when (and obj +debug-instrs+
             (eql 'decoded-instruction (type-of obj))
             (not (null (decoded-instruction-opcode-info obj)))
             (null (find (oci-name (decoded-instruction-opcode-info obj)) +debug-instrs+)))
    (return-from dbg))
  (apply #'format stream string values))


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

;; A log of all the instructions we run, in reverse order
(defvar *pc-log* nil)

(defun save-pc-log (fn)
  (with-open-file (s fn :direction :output :if-exists :supersede)
    (let ((*print-base* 16))
      (write (reverse *pc-log*) :stream s)
      t)))

;; Z-Machine Stack of 2-byte words (Spec 6.3)
;; TODO: Code me
;; Writing to variable 0x00 pushes a value on the stack
;; Reading from variable 0x00 pops a value from the stack

;; Z-Machine Routine Stack frames (Spec 6.1, 6.3.2, 6.4, 6.5)
;; The zmrs structure contains all the local state of a Routine
;; call, including:
;;   Parameters - implemented as an array (of words)
;;   Local variables - implemented as an array (of words)
;;   Stack - implemented as a list (of words)
;;   Calling instruction - for handling the return value storage
;;                         and calculating the return address
;;                         (which is that instruction's location + length)
;;                         THE MAIN ROUTINE WILL BE NIL
;; The call stack is a list of these structures.
;; The starting routine is the lowest state, and has no parameters,
;; and a zero return address.
(defstruct zmrs     ; Z-Machine Routine State
  locals            ; 15-size array of words (set in routine header)
                    ;   with fill-pointer to actual # of locals
  stack             ; list (of words)
  instr)            ; Calling instruction (including store and return PC)
;; Maxmimum number of parameters that a routine can have, although in
;; v3 we can really only have up to 3 - this is a limitation of the
;; number of arguments to a CALL instruction/opcode. Parameters are
;; copied over the locals (if enough locals are present).
(defparameter +zmrs-max-params+ 8)
;; Maximum number of locals a routine (stack frame) can have
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
;; NOTE: Flags 2 is word (2-byte) sized and only bits 1 and 2 have
;; any meaning in V3:
;; 0: transcript (store transcript when on)
;; 1: "Game sets to force printing in fixed-width font." 
(defparameter +ml-flags-2+       #x10) ; 2 Flags 2 (bits 0-1 only in v3)
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


;; FIXME: XXX: Anywhere we write a value, make sure it's an 8- or 16-bit
;; unsigned value (e.g., pushing data onto a stack)


;; Implementation ---------------------------------------------------------


;; Conditions -------------------------------------------------------------
;; Spec, Appendix A ("Error Messages")

;; TODO: Add a Z-machine Call Frame Stack Backtrace with these
;; (per Spec Appendix A page 111 point 7), which would be very
;; interesting to see.


;; Read a new value from the user when doing a restart
(defun restart-read-new-value ()
  (format t "Enter a new value: ")
  (multiple-value-list (eval (read))))

;; There's an error with an instruction (e.g., no opcode,
;; not yet implemented)
(define-condition instr-error (error)
  ((message :initarg :message :reader message))
  ;; Below required by Genera
  (:report (lambda (condition stream)
	     (format stream "Instruction error: ~A" (message condition)))))

;; TODO: Add conditions for:
;; * Memory read error
;; * Memory write error (e.g., writing to read-only sections of header)
;; * Stack pop error (stack underflow)
;; * Local variable read/write errors (accessing undefined local)

;; The value stack (one for each call frame) has underflowed.
(define-condition stack-underflow (error)
  ((message :initarg :message :reader message))
  ;; Below required by Genera
  (:report (lambda (condition stream)
	     (format stream "Value stack underflow: ~A" (message condition)))))

;; We requested to access a missing local variable
(define-condition missing-local-variable (error)
  ((message :initarg :message :reader message))
  ;; Below required by Genera
  (:report (lambda (condition stream)
	     (format stream "Missing local variable: ~A" (message condition)))))

;; We requested to access a non-existent global variable
(define-condition missing-global-variable (error)
  ((message :initarg :message :reader message))
  ;; Below required by Genera
  (:report (lambda (condition stream)
	     (format stream "Missing global variable: ~A" (message condition)))))

;; Attempt to use an omitted operand (Spec 4.2)
(define-condition use-omitted-operand (error)
  ((message :initarg :message :reader message))
  ;; Below required by Genera
  (:report (lambda (condition stream)
	     (format stream "Attempt to use omitted operand: ~A" (message condition)))))

;; Incorrect number of operands
(define-condition invalid-operand-count (error)
  ((message :initarg :message :reader message))
  ;; Below required by Genera
  (:report (lambda (condition stream)
	     (format stream "Wrong number of operands: ~A" (message condition)))))

;; Request for an invalid (missing, out of range) propery
(define-condition invalid-property (error)
  ((message :initarg :message :reader message))
  ;; Below required by Genera
  (:report (lambda (condition stream)
	     (format stream "Invalid property: ~A" (message condition)))))

;; Request for an invalid (out of range, missing) object
(define-condition invalid-object (error)
  ((message :initarg :message :reader message))
  ;; Below required by Genera
  (:report (lambda (condition stream)
	     (format stream "Invalid object: ~A" (message condition)))))

;; Division by zero. Let's have a different condition so we can
;; differentiate the z-machine code /0 by a real Lisp /0
(define-condition z-division-by-zero (error)
  ((message :initarg :message :reader message))
  ;; Below required by Genera
  (:report (lambda (condition stream)
	     (format stream "Division by zero: ~A" (message condition)))))

;; Program Counter --------------------------------------------------------

(defun set-pc (newpc)
  (setf *z-pc* newpc))
(defun get-pc ()
  *z-pc*)


;; Output streams ---------------------------------------------------------

;; The default output for text printing
(defvar *z-output* *standard-output*)


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

;; Write an (unsigned, 8-bit) byte to memory
;; XXX: Make this safe for the actual memory size
;; XXX: Check if the location is allowed to be written (Spec 1.1)
(defun mem-byte-write (loc val)
  (setf (aref *z-mem* loc) val))

;; Write an (unsigned, 16-bit max) word to memory - MSB first
;; XXX: Check the value fits into 16-bit unsigned size
(defun mem-word-write (loc val)
  ;; XXX: Assert val is 0-65535
  (mem-byte-write loc (ash (logand val #xff00) -8)) ;; Negative is to the right
  (mem-byte-write (1+ loc) (logand val #xff)))

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

;; Writes an ASCII string to memory
;; with the specified maximum length
;; and terminated with a 0 byte.
;; Spec page 95
;; Note, it's not clear if maxlen includes the 0 byte or not,
;; so I'm going to be safe and say it doesn't.
;; FIXME: We may have an off-by-one error here on maxlen meaning
;; a. maximum number of bytes written to loc total
;; b. maximum number of text bytes written to loc (then plus 1 more for the 0 at the end)
(defun mem-ascii-write (loc text maxlen)
  (let* ((text-len   (length text))
         (text-trunc (subseq text 0 (min (1- maxlen) text-len)))
         (tt-len     (length text-trunc)))
    (dbg t nil "maxlen: ~A, text-len: ~A, text-trunc: '~A', tt-len: ~A~%"
         maxlen text-len text-trunc tt-len)
    ;; Copy the ZSCII characters
    (loop
       for i from 0 below tt-len
       for l = loc then (1+ l)
       do
         (dbg t nil "i: ~A, l: ~A~%" i l)
         (setf (aref *z-mem* l) (char-code (aref text-trunc i))))
    ;; Append the zero byte
    (setf (aref *z-mem* (+ loc tt-len)) 0)))

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

;; Convert the signed "bits"-bit integer to an unsigned
;; "bits"-bit two's compliment version. It omits any bits
;; beyond the necessary ones.
;; Some examples with 16-bits:
;; -1     -> #xffff
;; -32768 -> #x8000
;; 32767  -> #x7fff
;; 32768  -> #x8000
;; http://clhs.lisp.se/Body/f_logand.htm
(defun s-to-us (anum bits)
  (let* ((mask (1- (ash 1 bits)))) ; An all 1's bit mask of bits bits
    (logand anum mask)))

;; Fixed 16-bit version of the above
(defun s-to-us-16 (anum)
  (logand anum #xffff))


;; Memory: Object Handing Routines ----------------------------------------

;; See: zmach06e.pdf Chapter 3.4
;; See: Spec Chapter 12

;; Per zmach06e.pdf Chapter 3.5:
;; 'object number 0' is often used to mean 'no such object exists.'
;; this 'object' is unlike all others: it has no name, no attributes,
;; and no properties.

;; NOTE CONVENTION:
;;    obj- takes an object ID
;; object- takes an object ID
;;   zobj- takes a zobj

;; A structure which holds information about a z-object's properties
;; Spec 12.4.1
(defstruct zprop
  ;; TODO: Add an object ID?
  id              ; Which property number this is
  mem-loc         ; Memory location of this property (at size byte)
  data-size       ; How many (actual data bytes)
  data-val        ; The byte or word size data
  data-vec        ; The data as a vector of bytes
  ; data-loc      ; Memory location of this property's data
  )

;; A structure which holds information about an object
;; Spec 12.3.1, 12.4, 12.4.1
(defstruct zobj
  id              ; Which numer this object is
  tree-loc        ; Where the object's location starts
  attributes      ; The attributes as a 32-bit unsigned value
  parent          ; The parent ID
  sibling         ; The sibling ID
  child           ; The child ID
  props-loc       ; Where the object's properties start in memory
  ;; Properties -------------------------------------------------------
  text-length     ; The length of the short name text in (2 byte) words
  text-zchars     ; The raw Z-characters of the text of that object (byte vector)
  short-name      ; The ASCII decoded short-name of the object
  first-prop-loc  ; Where the first actual property is in memory
  properties      ; An list of properties (zprops)
  )

;; Offsets from the start of an object's location in the object tree
;; for the various pieces (Spec 12.3.1)
(defparameter +zobj-parent-ml-offset+  4)
(defparameter +zobj-sibling-ml-offset+ 5)
(defparameter +zobj-child-ml-offset+   6)

;; Loads a property from the specified location,
;; or returns nil if the size byte at the location is 0.
;; Spec 12.4.1
(defun load-property (loc)
  (let ((size (mem-byte loc)))
    (when (= size 0)
      (return-from load-property nil))
    ;; Top 3 bits of size contains length of data MINUS ONE
    ;; Bottom 5 bits of size is the property number
    (let* ((d-size (1+ (ash size -5))) ; Right-shift 5 bits
           (r (make-zprop :mem-loc loc
                          :id (logand size #x1F)
                          :data-size d-size
                          :data-vec (mem-slice (1+ loc) d-size)
                          :data-val (case d-size
                                      (0 nil)
                                      (1 (mem-byte (1+ loc)))
                                      (otherwise (mem-word (1+ loc)))))))
      ; (dbg t nil "Read property: ~A~%" r)
      r)))

;; Loads an object from memory
(defun load-object (which)
  (let* ((tloc (object-loc which))
         (r (make-zobj :id       which
                       :tree-loc tloc))
         (ploc nil)  ; Property location we're going to read
         (prop nil)) ; Property we just read
    ;; Populate information from object tree entry (Spec 12.3.1)
    (setf (zobj-attributes r)
          (+ (ash (mem-word tloc) 16)
             (mem-word (+ 2 tloc))))
    (setf (zobj-parent    r) (mem-byte (+ +zobj-parent-ml-offset+  tloc)))
    (setf (zobj-sibling   r) (mem-byte (+ +zobj-sibling-ml-offset+ tloc)))
    (setf (zobj-child     r) (mem-byte (+ +zobj-child-ml-offset+   tloc)))
    (setf (zobj-props-loc r) (mem-word (+ 7 tloc)))
    ;; Get the name of the object (Spec 12.4)
    (setf (zobj-text-length r) (mem-byte (zobj-props-loc r)))
    ;; Note: We don't use mem-string since we have an explicit length
    (setf (zobj-text-zchars r) (mem-slice (1+ (zobj-props-loc r))
                                          (* 2 (zobj-text-length r))))
    (setf (zobj-short-name r) (z-characters-to-string
                               (break-zchar-string
                                (zobj-text-zchars r))))
    ;; Properties
    ;; Get address of first property
    (setf (zobj-first-prop-loc r)
          (+ (zobj-props-loc r)
             1 ; For the size of the short name
             (* 2 (zobj-text-length r))))
    ;; Loop through all properties until we get a nil
    (setf (zobj-properties r) nil)
    (setf ploc (zobj-first-prop-loc r))
    (loop
       (setf prop (load-property ploc))
       (when (null prop) (return))
       (push prop (zobj-properties r))
       (setf ploc (+ ploc 1 (zprop-data-size prop))))
    ;; Reverse our properties list to reflect in-memory (descending ID) order
    (setf (zobj-properties r) (nreverse (zobj-properties r)))
    r))

;; Gets the specified property from a zobj, or nil if no property
;; with that number (so use the default). Returns a zprop object.
(defun zobj-prop (zo propnum)
  (flet ((isprop (prop)
           (= (zprop-id prop) propnum)))
    (let ((props (zobj-properties zo)))
      (find-if #'isprop props))))

;; Returns all attributes as a list of those which are set
(defun zobj-attribs (zo)
  (let ((attrnum 31)
        (attrs (zobj-attributes zo))
        (retval nil))
    (loop
       (when (nonzerop (logand #x00000001 attrs))
         (push attrnum retval))
       (decf attrnum)
       (setf attrs (ash attrs -1))
       (when (< attrnum 0) (return retval)))))

;; Gets the object table start location
;; Spec 12.1
;; See: +ml-loc-obj+
(defun object-table-loc ()
  (mem-word +ml-loc-obj+))

;; Gets a default property value (word) (Spec 12.2)
;; Property defaults table starts at the object table location.
;; Properties are numbered 1-31
(defun get-property-default (prop)
  (when (or (< prop 1) (> prop 31))
    ;; XXX: Add restarts (return 0, return value)
    (error 'invalid-property
           :message (format nil "Cannot get default for property ~A" prop)))
  ;; XXX: FIXME: Spec is not clear if the property defaults are indexed
  ;; by the property number or one less (since properties start at 1).
  ;; It says the defaults table contains 31 words, which would imply that
  ;; property 31 would not have a default...
  (let* ((prop-loc (+ (object-table-loc)
                      ;; (* 2 prop)))
                      (* 2 (1- prop))))
         (def-val (mem-word prop-loc)))
    (dbg t nil "Default property ~A: 0x~x~%" prop def-val)
    def-val))

;; Gets the base of the object tree. Spec 12.3.
;; This is right after the object table's 31 default properties.
(defun object-tree-loc ()
  (+ (object-table-loc) (* 31 2)))

;; How large each entry is in the object tree in bytes (Spec 12.3.1)
(defparameter +object-tree-entry-size+ 9)

;; Gets the location of a specified object. (Spec 12.3, 12.3.1)
;; This is located after the default objects. The object table size
(defun object-loc (which)
  (when (or (< which 1) (> which 255))
    (error 'invalid-object :message
           (format nil "No such object number: ~A" which)))
  (+ (object-tree-loc) (* +object-tree-entry-size+ (1- which))))


;; Sets the child (parent, sibling) of the zobj to child-id
;; Spec 12.3.1
(defun zobj-set-child (zo id)
  ;; XXX: Validate id is in allowable range
  ;; Update the zobj (just in case)
  (setf (zobj-child zo) id)
  ;; Update memory
  (mem-byte-write (+ (zobj-tree-loc zo) +zobj-child-ml-offset+) id))
(defun zobj-set-parent (zo id)
  ;; XXX: Validate id is in allowable range
  (setf (zobj-parent zo) id)
  (mem-byte-write (+ (zobj-tree-loc zo) +zobj-parent-ml-offset+) id))
(defun zobj-set-sibling (zo id)
  ;; XXX: Validate id is in allowable range
  (setf (zobj-sibling zo) id)
  (mem-byte-write (+ (zobj-tree-loc zo) +zobj-sibling-ml-offset+) id))

;; Removes the specified object from its parent (or parent's sibling chain).
;; Optionally (but by default) sets the object's parent and siblings to none.
;; This is not remotely efficient.
;; Returns nothing.
;; TESTS:
;;   1. Object is child of parent
;;      a. With/without siblings
;;   2. Object is in sibling-chain of parent
;;   3. Object is at the end of sibling-chain of parent
(defun object-remove (object-id &optional (and-clear t))
  (when (= object-id 0)
    ;; XXX: Give an alert?
    (return-from object-remove))
  (let* ((object (load-object object-id))
         (parent-id (zobj-parent object))
         (i nil)) ; An object we use to iterate
    (when (= 0 parent-id)
      ;; Object already has no parent, so nothing to do
      (return-from object-remove))
    ;; Load the parent
    (setf i (load-object parent-id))
    ;; If the parent's child is us, it's easy
    ;; Otherwise it's not as easy
    (if (= object-id (zobj-child i))
        ;; Set the parent's child to the object's sibling
        (zobj-set-child i (zobj-sibling object))
        ;; Otherwise, we need to walk the sibling chain <sigh>
        ;; and find the sibling who's sibling is object-id
        (progn
          ;; Load the first sibling in the chain (the parent's child)
          ;; XXX: Check if the parent's child is 0!
          (setf i (load-object (zobj-child i)))
          (loop
             (when (= (zobj-sibling i) object-id)
               ;; We found the sibling containing object, bypass object
               (zobj-set-sibling i (zobj-sibling object))
               (return))
             (when (= (zobj-sibling i) 0)
               ;; ERROR: We're not in our parent's sibling chain!
               ;; XXX: Raise a condition, but ignore
               (return))
             ;; Try the next sibling
             (setf i (load-object (zobj-sibling i))))))
    (when and-clear
      ;; Clear the object's parent and sibling to be safe
      (zobj-set-parent  object 0)
      (zobj-set-sibling object 0))))

;; Sets the specified attribute (bit) on the specified object.
;; Spec 12.3.1
(defun object-set-attribute (object-id attribute)
  (let* ((abit  (- 31 attribute))
         (abyte (floor attribute 8)) ; which byte the attribute is in
         (tloc  (+ (object-tree-loc) (* (1- object-id) +object-tree-entry-size+)))
         (bloc  (+ tloc abyte))
         (bbit  (mod abit 8))) ; which bit in the byte
    (dbg t nil "Attribute ~d is bit ~d of byte ~d~%" attribute bbit abyte)
    (mem-byte-write bloc
     (logior (mem-byte bloc) ; "inclusive" or
             (ash 1 bbit)))))

;; Clears the specified attribute (bit) on the specified object.
;; Spec 12.3.1
;; TODO: Integrate object-set/clear-attribute
(defun object-clear-attribute (object-id attribute)
  (let* ((abit  (- 31 attribute))
         (abyte (floor attribute 8)) ; which byte the attribute is in
         (tloc  (+ (object-tree-loc) (* (1- object-id) +object-tree-entry-size+)))
         (bloc  (+ tloc abyte))
         (bbit  (mod abit 8))) ; which bit in the byte
    (dbg t nil "Attribute ~d is bit ~d of byte ~d~%" attribute bbit abyte)
    (mem-byte-write bloc
                    (logand (lognot (ash 1 bbit))
                            (mem-byte bloc)))))

;; Dictionary -------------------------------------------------------------

;; The ASCII-length of entries in the dictionary (v3)
(defparameter +dict-str-len+ 6)

;; Dictionary header plus entries (Spec 13.2)
(defstruct zdict
  word-seps    ; String of all the keyboard input codes usually ".,\""
  wss          ; word-seps with space added
  entry-len    ; Byte length of each entry (4+ bytes in v3)
  num-entries  ; Number of entries in the dictionary
  entries      ; Array of zdentries
  )

(defstruct zdentry
  zchars       ; The z-characters of the entry, padded with 5's (byte vector)
  text         ; The ASCII decoded text of the entry (up to 6 characters)
  data         ; The data (byte vector)
  addr         ; The (byte) address of this entry
  )

;; The dictionary, fully parsed out of memory
(defvar *zdict* nil)

;; Parses out the dictionary from memory and into the zdict structure
;; for ease of later use, and stores it in *zdict*.
(defun parse-zdict ()
  (let* ((dict-loc (mem-word +ml-loc-dict+))
         ;; First, parse the header
         (ws-len (mem-byte dict-loc))
         (num-entries (mem-word (+ dict-loc 1 ws-len 1)))
         ;; (FIXME: Should I make an initial entry? We're going to set each one...)
         (entries (make-array num-entries :element-type 'zdentry
                              :initial-element (make-zdentry)))
         (entry-base (+ dict-loc 1 ws-len 1 2)) ; Where entries start
         (entry-len (mem-byte (+ dict-loc 1 ws-len)))
         (word-seps (mem-ascii (1+ dict-loc) ws-len))
         (zd (make-zdict
              :word-seps   word-seps
              :wss         (concatenate 'string " " word-seps)
              :entry-len   entry-len
              :num-entries num-entries
              :entries     entries)))
    (dbg t nil "Dictionary: Word seps: ~A, entries: ~A, entry-len: ~A~%"
         ws-len num-entries entry-len)
    ;; Now, create the zdentries
    (loop
       for i from 0 below num-entries do
         (let* ((el (+ entry-base (* entry-len i))) ; entry-loc
                (zchars (mem-slice el 4))
                (data (mem-slice (+ el 4) (- entry-len 4)))
                (ascii (z-characters-to-string (break-zchar-string zchars)))
                (zde (make-zdentry :addr   el
                                   :zchars zchars
                                   :text   ascii
                                   :data   data)))
           (setf (aref entries i) zde)))
    ;; And store it all in our global
    (setf *zdict* zd)))

;; Searches the dictionary for the (ASCII) string.
;; Returns the byte address of the dictionary entry if found.
;; FIXME: Modify this to a binary search from the current
;; super inefficient but easy to code linear search.
(defun zdict-find (str)
  (let* ((str (subseq str 0 (min +dict-str-len+ (length str))))
         (zdes (zdict-entries *zdict*))
         (found (find str zdes :test #'equal :key #'zdentry-text)))
    (if found
        (zdentry-addr found)
        0)))

;; Tokenizes the specified string by turning it into a list of
;; lists of the following pieces of data:
;; (dict-entry-loc token-len token-str-pos)
(defun z-tokenize (str)
  (let ((split (z-split (zdict-wss *zdict*) str)))
    (flet ((find-token (split-piece)
             (let ((token-str-pos (car split-piece))
                   (token (cdr split-piece)))
               (list (zdict-find token)
                     (length token)
                     ;; SUPER IMPORTANT NOTE:
                     ;; The z-machine seems to think the first byte in buffer is 1, not 0!!
                     ;; If we count from zero, it shows the wrong bytes.
                     (1+ token-str-pos)))))
      (map 'list #'find-token split))))
                                 


;; Routine Frames ---------------------------------------------------------

;; Creates a new zmrs structure and sets up appropriate empty values of all
;; the fields of the structure
(defun new-zmrs ()
  (let ((retval (make-zmrs)))
    (setf (zmrs-locals retval)
          (make-array +zmrs-max-locals+
                      :element-type '(unsigned-byte 16)
                      :adjustable t
                      :fill-pointer 0))
    (setf (zmrs-stack retval) '())
    (setf (zmrs-instr retval) nil)
    retval))

;; Initialize the call stack for an entirely new game
(defun initialize-call-stack ()
  (setf *call-stack*
        (list (new-zmrs))))

;; Add a new stack frame to the call stack
(defun push-call-stack (a-zmrs)
  (push a-zmrs *call-stack*))

;; Remove the current stack frame from the call stack and return it
(defun pop-call-stack ()
  (pop *call-stack*))

;; Gets the current stack frame
(defun cur-zmrs ()
  (car *call-stack*))

;; Pops a value onto the current frame's stack
;; Raises a condition if the stack is empty
(defun pop-stack ()
  (if (zmrs-stack (cur-zmrs))
      ;; We have a value to pop
      (pop (zmrs-stack (cur-zmrs)))
      (restart-case
          (progn
            (dbg t nil "Warning: Stack underflow~%")
            (error 'stack-underflow :message "Stack empty"))
        (return-zero ()
          :report "Ignore, returning 0"
          0)
        (use-value (value)
          :report "Ignore, returning specified value"
          :interactive restart-read-new-value
          value))))

;; Pushes a value onto the current frame's stack
;; XXX: Verify that the value fits in a 16-bit unsigned spot
;; and raise a condition if not
(defun push-stack (value)
  (push value (zmrs-stack (cur-zmrs))))

;; Story File Load/Initialization -----------------------------------------

;; The last loaded story file, so we can reload it at a restart.
(defvar *last-story-file* nil)

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
    (set-pc init-pc)
    (setf *pc-log* nil) ; And clear our log of PCs
    ;; Set the initial flags (Spec 11.1) (p52 of zmach06.pdf)
    ;; Status line NOT available: header byte 1 bit 4            0
    ;; Screen-splitting available: header byte 1 bit 5           0
    ;; Variable pitch font not default: header byte 1 bit 6      0
    ;; So we need to clear bits 4-5-6
    (mem-byte-write +ml-flags-1+
     (logand (mem-byte +ml-flags-1+) #x1F))
    ;; XXX: Set standard revision number, header byte 32
    ;;
    ;; Parse the dictionary into *zdict* for later use
    (parse-zdict)
    ;; Success
    (setf *last-story-file* filename)
    (values t (format nil "Loaded ~A release ~D serial ~A" filename rel serial))))



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

(defparameter +2op-opcodes+
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

(defparameter +1op-opcodes+
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

(defparameter +0op-opcodes+
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

(defparameter +var-opcodes+
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
(defparameter +opcodes+
  (list
   (cons '2OP +2op-opcodes+)
   (cons '1OP +1op-opcodes+)
   (cons '0OP +0op-opcodes+)
   (cons 'VAR +var-opcodes+)))


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
;; per the +op-type-X+ choices above
;; - Long form (Spec 4.4.2)
;; Bits 6 and 5 give the types of the first and second operands respectively
;; The only choices small constant (if 0) or variable (if 1)
(defparameter +op-long-type-const-small+ #b0)
(defparameter +op-long-type-variable+    #b1)
;; - Variable form (Spec 4.4.3)
;; A byte of 4 operand types follows opcode per +op-type-X+ above.
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
;;        1       = VAR opcode table ... VAR number of arguments (4.3.3)
;;         0_0000 = Opcode ("call") (14 page 73)
;; BB = 1011_1011 = new_line (0OP:187)
;;      10        = Short instruction form decoder
;;        11      = Operand type - omitted -> 0OP (opcode-table?)
;;           1011 = Opcode ("new_line")
;; 54 = 0101_0100 = add
;;      0         = Long form decoder (4.3) (2OP opcode-table?)
;;       1        = First operand is a variable (4.4.2)
;;        0       = Second operand is a small constant (4.4.2)
;;         1_0100 = Opcode ("add") (4.3.2)
;; C1 = 1100_0001 = JE
;;      11        = Variable instruction form decoder
;;        0       = opcode table is 2OP (still uses variable operand byte)
;;         0_0001 = Opcode (JE)


(defstruct decoded-instruction
  memory-location   ; Where this instruction starts
  first-byte        ; What the first byte of the instruction is
  form              ; 'long, 'short, 'variable (Spec 4.3)
  ;; XXX: operand-count should really be called opcode-table, as this
  ;; sometimes has nothing to do with the number of operands
  ;; FIXME: Rename operand-count to opcode-table?
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
  ;; ------------------------------------------------------------------------
  ;; Everything below here is context dependent and used by the z-machine
  ;; interpreter. Populating these values CAN HAVE SIDE EFFECTS on the
  ;; global state of the z-machine, most specifically, due to popping
  ;; values off the local call frame stack.
  ;; See: retrieve-operands function
  rop-cache%        ; Retrieved operand cache
  rop-is-cached%    ; Have we retrieved our operands? nil if not, t if so
  )


;; Formats an operand for disassembly printing
;; If is-read, then we're popping the stack, otherwise
;; we're pushing it
(defun disassemble-operand (operand-type operand &optional (is-read t))
  (cond
    ((eql operand-type 'const-large)
     (format nil "#~4,'0X" operand))
    ((eql operand-type 'const-small)
     (format nil "#~2,'0X" operand))
    ;; Otherwise, variables
    ((= operand 0)
     (if is-read "(SP)+" "-(SP)"))
    ((>= operand #x10)
     (format nil "G~2,'0X" (- operand #x10)))
    (t
     (format nil "L~2,'0X" (1- operand)))))

;; Outputs the branching destination as a string
;; See also sinstruction-jx
(defun disassemble-branch-dest (instr)
  (let ((offset (decoded-instruction-branch-offset instr))
        (i-pc  (decoded-instruction-memory-location instr))
        (i-len (decoded-instruction-length instr)))
    (cond
      ((= offset 0) "RET(FALSE)")
      ((= offset 1) "RET(TRUE)")
      (t (format nil "~4,'0X" (+ i-pc i-len offset -2))))))

;; Formats our decoded instruction in a format somewhat akin to TXD.
;; Returns a string.
(defun disassemble-instr (instr)
  ;; XXX: This works on SBCL, needs to be tested on Genera
  (with-slots (memory-location
               operand-count
               operand-types
               opcode
               opcode-info
               operands
               (store-loc store)
               branch-if
               branch-offset
               text-ascii) instr
    (with-slots ((instr-name  name)
                 (store-flag  store)
                 (branch-flag branch)
                 (text-flag   text)) opcode-info
      (let ((str (make-string-output-stream))
            (ops operands) ; We may modify these locally
            (op-types operand-types))
        (format str "~4,'0X: ~15A " memory-location (string-upcase instr-name))
        ;; Special instructions -------------
        (cond
          ;; First operand in CALL is the destination as a word-address
          ((eql instr-name 'call)
           (format str "~4,'0X " (* 2 (car ops)))
           (setf ops (cdr ops))
           (setf op-types (cdr op-types)))
          ;; Jump destination is relative to instruction
          ((eql instr-name 'jump)
           (format str "~4,'0X " (jump-destination instr))
           (setf ops (cdr ops))
           (setf op-types (cdr op-types)))
          ;; First operand in STORE, INC_CHK is a variable location
          ((member instr-name '(store inc_chk))
           (format str "~A," (disassemble-operand 'variable (car ops)))
           (setf ops (cdr ops))
           (setf op-types (cdr op-types)))
          ) ; cond
        ;; End special instructions ---------
        (format str "~{~A~^,~} "
                (map 'list #'disassemble-operand op-types ops))
        (when store-flag
          (format str "-> ~A " (disassemble-operand 'variable store-loc nil)))
        (when branch-flag
          (format str "[~A] ~A " (if branch-if "TRUE" "FALSE")
                  (disassemble-branch-dest instr)))
        (when text-flag
          (format str "\"~A\"" text-ascii))
        (get-output-stream-string str)))))


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
           (aref (cdr (assoc (decoded-instruction-operand-count retval) +opcodes+))
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
    ;; Finally, we have to get our text to print, if necessary
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
    ;; If bit 5 is 0, then use the 2OP instruction table, otherwise VAR (Spec 4.3.3)
    ;; NOTE: This doesn't actually tell us the number of operands; that still
    ;; seems to come from the opcode-type byte!!!
    ;; Example from txd and zork1.z3:
    ;;  8edc:  c1 ab 7f 01 00 48       JE              G6f,L00,(SP)+ [FALSE] 8ee8
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
    ;; Only VARIABLE forms will have a types byte.
    ;; NOTE: If it's a 2OP opcode, then we should NOT cut the list its two items.
    ;; See decode for example of this instruction from tzd and zork1.z3
    ;;  8edc:  c1 ab 7f 01 00 48       JE              G6f,L00,(SP)+ [FALSE] 8ee8
    (setf (decoded-instruction-operand-types retval)
          (subseq otlist 0 (position 'omitted otlist))) ; Operands before 1st omitted
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
  ;; Decode the text to local character set (ASCII?)
  (setf (decoded-instruction-text-ascii retval)
        (z-characters-to-string
         (break-zchar-string
          (decoded-instruction-text-data retval))))
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

;; Assoc list of modes (used below in z-characters-to-string)
;; to Z-character sets
(defparameter +mode-to-chars+
  (list (cons 'a0 +zchars-a0+)
        (cons 'a1 +zchars-a1+)
        (cons 'a2 +zchars-a2+)))

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
;;
;; Parameter no-abbrevs is set if we want to disable abbreviations recursively
;; per Spec 3.3.1. (In theory, we should also make sure that we have no incomplete
;; multi-Z-character constructs, but they are harmless so ignore that. I imagine
;; that was because the old Z-machines didn't handle recursion and that the
;; abbreviation decoding modified the original string decoder's state.)
;; This is obviously only set when called from get-abbrev.
(defun z-characters-to-string (zchars &optional no-abbrevs)
  ; (format nil "z-chars: ~A, no-abv: ~A~%" zchars no-abbrevs)
  (let ((raccum nil)      ; List of accumulated components of output in reverse order
        (mode 'a0)        ; Decoding mode
        (abv nil)         ; (1, 2 or 3) which abbreviation initial number
        (zscii-high nil)) ; High caracter of a2:6 ZSCII decoder found
    (loop for zc across zchars do
         ;(format t "zc: ~A, start mode: ~A, " zc mode)
         (cond
           ;; Handle second byte of our abbreviation:
           ;; 1. Insert abbreviation or a placeholder if no abbreviations allowed
           ;; 2. Return to mode a0
           ((eq mode 'abv)
            (if no-abbrevs
                (push (format nil "[ABV:~A,~A]" abv zc) raccum)
                (push (get-abbrev abv zc) raccum))
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
    ;; one giant string and return it. We'll use the format mini-language
    ;; for that. (See: http://cl-cookbook.sourceforge.net/strings.html)
    (format nil "~{~A~}" raccum)))

;; Make a zscii character from two 5-bit z-characters (Spec 3.8 and 3.4)
(defun zscii-from-zchars (a b)
  ;; Just assume this is ASCII for now <sigh>
  (code-char (+ (* a 32) b)))

;; Get a fully decoded abbreviation string ready for printing
;; (Spec 3.3, 3.3.1, and ???)
;; Note that the Spec is really bad about this. I used the older 06e Spec
;; to figure this out.
;; 1. Get the "byte address" word in header at location +ml-loc-abbrev+
;; 2. This memory location contains a table of 96 word-size "word addresses",
;;    for each abbreviation (whose actual "byte address" is double)
;; 3. Look up entry 32 * (x - 1) + y in this table as a string
;; 4. Decode and return that string as a CL string
;; TODO: Detect and prevent recursion (why? Do we care?)
(defun get-abbrev (x y)
  (let* ((loc-abv-table  (mem-word +ml-loc-abbrev+))
         ;; Which entry in the word-size table above?
         (abv-entry      (+ (* 32 (1- x)) y))
         ;; Calc that entry - *2 because each entry is 2 bytes
         (loc-abv-entry  (+ loc-abv-table (* 2 abv-entry)))
         ;; Now, load that entry for the location of the abbreviation string
         (loc-abv        (mem-word loc-abv-entry))
         ;; Finally, load that location as a string
         ;; REMEMBER that this is a "word address" and hence must be doubled
         (z-char-str     (mem-string (* 2 loc-abv))))
    #| ; Debugging
    (dbg t nil "Called (get-abbrev ~A ~A)~%" x y)
    (dbg t nil "Abbrev table: ~x, entry num: ~A, abv entry loc: ~x~%"
            loc-abv-table abv-entry loc-abv-entry)
    (dbg t nil "Abv loc: ~x~%" loc-abv)
    (dbg t nil "Z-characters: ~A~%" z-char-str)
    |#
    (z-characters-to-string
     (break-zchar-string z-char-str) t))) ; = no abbreviations

;; We split the string at any of these d- characters,
;; and return a list of the entire string, split up at
;; any of these characters.
;; With thanks to split-sequence at:
;; https://github.com/Publitechs/cl-utilities/blob/master/split-sequence.lisp
(defun split-string (d- str)
  "Split string 'str' at any of the characters in delimiter string 'd-', and return the entire list of all characters in 'str' in multiple strings"
   (let* ((len (length str))
          (d (map 'list #'identity d-)))
    (loop
       for left = 0 then (+ right 1)
       for right = (min (or (position-if (lambda (c) (member c d)) str :start left)
                            len)
                        len)
       unless (= left right)
       collect (subseq str left right) into subseqs
       unless (= right len)
       collect (subseq str right (if (< right len) (1+ right) right)) into subseqs
       until (>= right len)
       finally (return subseqs))))

;; We take a list of strings (that represents a single string that was split up)
;; and turn them into an association list with the car being the starting position
;; in the whole string and the cdr being that part of the string.
(defun add-string-start-positions (sl) ; string-list
  (let ((l 0)) ; Our length so far
    (flet ((position-adder (s)
             (let ((retval (cons l s)))
               (incf l (length s))
               retval)))
      (map 'list #'position-adder sl))))

;; This removes spaces from lists of the form add-string-start-positions makes.
(defun remove-parsed-spaces (sl) ; string-list
  (remove-if
   (lambda (as) (equal (cdr as) " "))
   sl))

;; This parses a string into an association list of components and their
;; positions in the start of the original string.
;; Delims is a string of all should include a #\space as well as all the
;; z-machine word separators.
;; The string is downcased before splitting.
(defun z-split (delims str)
  (remove-parsed-spaces
   (add-string-start-positions
    (split-string delims (string-downcase str)))))

;; Variable Handling --------------------------------------------

;; Reads a local variable from the current stack frame.
;; which must be between 0 and 14.
;; Signals a condition if there is no such local variable.
(defun var-local-read (which)
  (let* ((frame (cur-zmrs))
         (locals (zmrs-locals frame)))
    (if (and (< which (length locals)) (>= which 0))
        (aref locals which)
        (restart-case
            (progn
              (dbg t nil "Error: Local ~A not available in ~A~%" which locals)
              (error 'missing-local-variable
                     :message (format nil "No such local variable: ~A" which)))
          (return-zero ()
            :report "Ignore, returning 0"
            0)
          (use-value (value)
            :report "Ignore, returning specified value"
            :interactive restart-read-new-value
            value)))))

;; Writes a local variable from the current stack frame.
;; which must be between 0 and 14.
;; Signals a condition if there is no such local variable.
;; XXX: Validate variable within 16-bit range
(defun var-local-write (which value)
  (let* ((frame (cur-zmrs))
         (locals (zmrs-locals frame)))
    (if (and (< which (length locals)) (>= which 0))
        (setf (aref locals which) value)
        (restart-case
            (progn
              (dbg t nil "Error: Cannot write ~A to unavailable local ~A in ~A~%"
                   value which locals)
              (error 'missing-local-variable
                     :message
                     (format nil "Cannot write ~A to non-exitent local ~A" value which)))
          (ignore ()
            :report "Ignore"
            nil)
          ;; TODO: Add more restarts? Pick different local to write to?
          ))))

;; Reads a global variable (Spec 6.2)
;; which must be between 0 and 239 (0xEF)
(defun var-global-read (which)
  ;; TODO: Precalculate and cache the base
  (let* ((global-base (mem-word +ml-loc-globals+)))
    (if (and (>= which 0) (<= which #xEF))
        (mem-word (+ global-base (* 2 which)))
        (restart-case
            (progn
              (dbg t nil "Error: No such global variable 0x~x~%" which)
              (error 'missing-global-variable
                     :message (format nil "No such global variable: 0x~x" which)))
          (return-zero ()
            :report "Ignore, returning 0"
            0)
          (use-value (value)
            :report "Ignore, returning specified value"
            :interactive restart-read-new-value
            value)))))

;; Writes a global variable (Spec 6.2) MSB first
;; Variable number must be between 0 and 239 (0xEF)
;; Value must be 0-65535
;; Conditions could be raised...
(defun var-global-write (which value)
  ;; TODO: Precalculate and cache the base
  (let* ((global-base (mem-word +ml-loc-globals+)))
    (if (and (>= which 0) (<= which #xEF))
        (mem-word-write (+ global-base (* 2 which)) value)
        (restart-case
            (progn
              (dbg t nil "Error: No such global variable 0x~x to write 0x~x~%" which value)
              (error 'missing-global-variable
                     :message (format nil "No such global variable: 0x~x to write 0x~x"
                                      which value)))
          (ignore ()
            :report "Ignore"
            0)
          ;; TODO: Other possible restarts
          ))))

;; Reads the specified variable (single byte) and returns it.
;; Spec 4.2.2
;; This can raise several conditions:
;;   Stack underflow if reading variable 0x00
;;   Local variable not present for 0x01 thru 0x0f
;;   (Memory errors indirectly)
;; This routine may modify the current frame's stack or locals.
(defun var-read (which)
  (cond
    ;; Pop the stack and return that
    ((= which #x00)
     (pop-stack))
    ;; Read a local variable from the current frame
    ((and (>= which #x01) (<= which #x0f))
     (var-local-read (1- which)))
    ;; Read a global variable
    (t
     (var-global-read (- which #x10)))))

;; Writes to a variable location (Spec 4.6)
;; 00      - pushes onto the stack
;; 01 - 0f - local variables
;; 10 - ff - global variables
;; This can signal conditions.
(defun var-write (which value)
  (cond
    ;; Push the stack
    ((= which 0)
     (push-stack value))
    ;; Write a local variable from the current frame
    ((and (>= which #x01) (<= which #x0f))
     (var-local-write (1- which) value))
    ;; Write a global variable
    (t
     (var-global-write (- which #x10) value))))

;; Instruction Implementations ----------------------------------

;; Each instruction is named after the mnemonic in the instruction
;; tables. It's called instruction-X where X is the name (such as
;; instruction-call). Since the PC, Memory and Stack Frames are
;; all globals, the only parameter needed is the particular
;; instruction to be executed. Special instruction functions are
;; named sinstruction-X.
;;
;; Returned are two values, as per run-next-instruction.

;; Moves to the next instruction assuming no branches, etc.
(defun advance-pc (instr)
  (set-pc (+ (decoded-instruction-memory-location instr)
             (decoded-instruction-length instr))))

;; Returns a list of all the operands by value, i.e.,
;; dereference any variable operands. (Spec 4.2.X)
;; For const-large and const-small, this means just return their
;; value verbatim. For variables, this means to give:
;; 00    - top of stack (of current frame)
;; 01-0F - local variable index 0-E (of current frame) (local #1 to #15)
;; 10-FF - global variables
;; Reading from the stack pops the value off (Spec 6.3)
;; Possible errors:
;; 1. Reading from an empty stack
;; 2. Accessing non-existent local variable
;; Note clarification on Spec p161:
;; Operands are always evaluated from first to last, so if the stack
;; is read twice, it's popped twice IN ORDER, e.g.,
;; SUB SP SP means subtract 2nd top stack item from top stack item
;;
;; If we have already retrieved the operands for this instruction
;; before, we return the cached ones. Otherwise, we retrieve them,
;; cache them, and return them.
(defun retrieve-operands (instr)
  ;; Check cache first
  (when (decoded-instruction-rop-is-cached% instr)
    (let ((retval (decoded-instruction-rop-cache% instr)))
      (dbg t instr "Retrieved (cached) operands ~A ~A: ~A~%"
           (decoded-instruction-operand-types instr)
           (decoded-instruction-operands      instr)
           retval)
      (return-from retrieve-operands retval)))

  ;; Load operands and cache them
  (dbg t instr "Retrieving operands for ~A: ~A~%"
       (decoded-instruction-operand-types instr)
       (decoded-instruction-operands      instr))
  ;; Loop through all operands and turn them into values
  (let ((retval
         (loop for type  in (decoded-instruction-operand-types instr)
               for opval in (decoded-instruction-operands      instr)
            collect
              (cond
                ;; Handle each operand type
                ((eql type 'const-small) opval)
                ((eql type 'const-large) opval)
                ((eql type 'variable) (var-read opval)) ;; Can raise a condition
                (t (raise-omitted-operand instr)))))) ;; Should never happen (!)
    (dbg t instr "Retrieved and cached operands: ~A~%" retval)
    (setf (decoded-instruction-rop-cache% instr) retval)
    (setf (decoded-instruction-rop-is-cached% instr) t)
    retval))

;; We attempted to use an omitted operand
(defun raise-omitted-operand (instr)
  (restart-case
      (error 'use-omitted-operand
             :message
             (format nil "Instruction at 0x~x"
                     (decoded-instruction-memory-location instr)))
    (return-zero ()
      :report "Ignore error, use 0 for operand"
      0)
    (use-value (value)
      :report "Ignore error, use specified value for operand"
      :interactive restart-read-new-value
      value)))


;; Helper: Checks that we have the right number of operands.
;; Returns the decoded operands.
(defun retrieve-check-operands (instr numops)
  (let ((operands (retrieve-operands instr)))
    (when (not (= numops (length operands)))
      ;; Raise condition for invalid number of args.
      ;; This should really just crash the whole program.
      (error 'invalid-operand-count :message
             (format nil "~A: Got ~A operands, expected ~A"
                     (string-upcase (oci-name (decoded-instruction-opcode-info instr)))
                     (length operands) numops)))
    operands))



;; CALL FRAME INSTRUCTIONS ---------------------------------------------------

;; The first instruction in Zork 1: CALL
;; If the routine is 0, we do nothing and "return"/store false/zero.
;; The routine address is a "packed address" which is located at
;; double it's number in v3, call it raddr. (Spec 1.2.3)
;; At raddr is a byte that says how many word local variables this
;; routine has, which immediately follow.
;; The first zero-to-three local variables are replaced by the
;; caller's arguments. If there are more arguments than variables,
;; then those arguments are ignored.
;; We set the local variable
;; Operands:
;; 0 - routine
;; 1-3 - arguments (optional, 0, 1, 2 or 3)
;; Store: handled by the return (!!!)
(defun instruction-call (instr)
  ;; If we have no operands, we have an error
  (when (zerop (length (decoded-instruction-operands instr)))
    (return-from instruction-call
     (values nil (format nil "CALL: No routine argument. Loc: 0x~x"
                         (decoded-instruction-memory-location instr)))))
  ;; Get our routine address, new routine stack frame, etc.
  (let* (
         ;; Get the operands
         (operands (retrieve-operands instr))
         ;; Get the packed routine address
         (praddr (first operands))
         ;; Unpack the routine address (v3)
         (raddr (* 2 praddr))
         ;; Get the (up-to-three) arguments
         (arguments (cdr operands))
         (result-var (decoded-instruction-store instr))
         ;; TODO: Check number of arguments is 0-3
         ;; TODO: Check that the memory address is fine
         ;; Get the number of locals for the routine
         (numlocals (mem-byte raddr))
         ;; TODO: Check that the number of locals is within bounds
         ;; Get the routine start instruction address
         (r-pc (+ raddr 1 (* 2 numlocals)))
         ;; Get the local values
         (localdefaults (loop for l below numlocals
                           collect (mem-word (+ raddr 1 (* 2 l)))))
         ;; Now calculate the starting locals: use arguments for the first
         ;; ones then the default locals for the rest
         (locals
          (loop
             for l in localdefaults
             for a = arguments then (cdr a)
             collect (if a (car a) l)))
         ;; Create our next stack frame
         (newframe (new-zmrs)))

    ;; Call to packed address 0 should do nothing and just
    ;; return false (0) into the result. Spec 6.4.3.
    ;; This means we're not RETURNING from the current stack frame,
    ;; merely putting a 0 into the store variable for this CALL instruction.
    (when (= praddr 0)
      (dbg t instr "CALL: Called zero, storing 0 into VAR 0x~x~%" result-var)
      (var-write result-var 0)
      (advance-pc instr)
      (return-from instruction-call (values t "CALL-0")))

    (dbg t instr
         "CALL: Routine address: 0x~x, # args: ~A, # locals ~A, routine start pc: 0x~x~%"
         raddr (length arguments) numlocals r-pc)
    (dbg t instr "CALL: Default locals: ~A~%" localdefaults)
    (dbg t instr "CALL: Starting locals: ~A~%" locals)
    ;; Set up our new stack frame
    (setf (zmrs-instr newframe) instr)
    (loop for l in locals do (vector-push-extend l (zmrs-locals newframe)))
    ;(dbg t instr "CALL: New stack frame: ~A~%" newframe)
    ;; Push our stack frame onto the call stack
    (push-call-stack newframe)
    ;; Update our PC to be at the new location
    (set-pc r-pc)
    ;; And we're done
    (values t "Called")))


;; Handles returning the specified value, storing it in the
;; location in the stack frame's caller's instruction,
;; unwinding the stack frame and setting the PC to the
;; proper location (from the caller's instruction).
(defun sinstruction-ret (instr value)
  (let* ((frame (pop-call-stack)) ; Our removed frame
         (finst (zmrs-instr frame)) ; Our calling instruction
         (retdest (decoded-instruction-store finst))) ; Where to store return value
    ;; XXX: Returning from the last frame is an error
    (dbg t instr "~A: Returning 0x~x into variable 0x~x (caller address 0x~x)~%"
         (string-upcase (oci-name (decoded-instruction-opcode-info instr)))
         value retdest (decoded-instruction-memory-location finst))
    (var-write retdest value)
    (set-pc (+ (decoded-instruction-memory-location finst)
               (decoded-instruction-length finst)))
    (values t "Returned")))

;; RET: Returns specified value from routine (Spec page 97)
(defun instruction-ret (instr)
  (let* ((operands (retrieve-operands instr))
         (retval
          (if (= 1 (length operands))
              ;; Correct number of operands
              (car operands)
              (restart-case
                  (error 'invalid-operand-count :message
                         (format nil "Exactly one argument required, got: ~A"
                                 (length operands)))
                (return-zero ()
                  :report "Ignore, return 0"
                  0)
                (use-first ()
                  ;; FIXME: Only display this if there are >0 operands
                  :report (lambda (stream)
                            (format stream "Ignore, using first operand: ~A" (car operands)))
                  (car operands))
                (use-value (value)
                  :report "Ignore, returning specified value"
                  :interactive restart-read-new-value
                  value)))))
    ;; And do our actual return
    (sinstruction-ret instr retval)))

;; RET_POPPED (Spec page 98)
;; Pops top of stack and returns that.
;; (This is equivalent to ret sp, but is one byte cheaper.)
(defun instruction-ret_popped (instr)
  ;; Reading variable 0 will pop the stack, so just do that.
  (sinstruction-ret instr (var-read #x00)))

;; RTRUE: Returns true (1) from current routine (Spec page 98)
(defun instruction-rtrue (instr)
  (sinstruction-ret instr 1))

;; RFALSE: Returns false (0) from current routine (Spec page 98)
(defun instruction-rfalse (instr)
  (sinstruction-ret instr 0))

;; QUIT: (Spec page 94)
;; Exit the game immediately. (Any "Are you sure?" question must be
;; asked by the game, not the interpreter.) It is not legal to return
;; from the main routine (that is, from where execution first begins)
;; and this must be used instead.
(defun instruction-quit (instr)
  (advance-pc instr)
  (dbg t instr "QUIT~%")
  (values nil "QUIT"))


;; MATH INSTRUCTIONS ------------------------------------------------------------


;; Prints a warning if the number is outside the
;; range of a normal 16-bit number
(defun warn-16-bit-size (num)
  (when (or (< num -32768) (> num 32767))
    (dbg t nil "Warning: Value outside of signed 16-bit range: ~A~%" num)))

;; ADD: Signed 16-bit addition (Spec p79, 15.3)
;; Signed info: Spec 2.2-2.3
(defun instruction-add (instr)
  (sinstruction-math instr #'+))

;; ADD: Signed 16-bit addition (Spec p102, 15.3)
;; Signed info: Spec 2.2-2.3
(defun instruction-sub (instr)
  (sinstruction-math instr #'-))

;; MUL: Signed 16-bit multiplication (Spec p89, 15.3)
;; Signed info: Spec 2.2-2.3
(defun instruction-mul (instr)
  (sinstruction-math instr #'*))

;; DIV a b -> (result) - Spec p83, 2.2.1, 2.3.1, p16 remarks
;; Signed 16-bit division. Division by zero should halt the interpreter
;; with a suitable error message.
;; Examples:
;; -11 /  2 -> -5
;; -11 / -2 ->  5
;;  11 / -2 -> -5
;; This corresponds to the Common Lisp TRUNCATE instruction
;; Ref: http://www.lispworks.com/documentation/HyperSpec/Body/f_floorc.htm
(defun instruction-div (instr)
  (let* ((operands (retrieve-check-operands instr 2))
         (divisor (second operands)))
    ;; Check for division by zero before it happens
    (when (= 0 divisor)
      ;; XXX: Add some restarts
      (error 'z-division-by-zero :message
             (format nil "0x~4,0X: DIV ~d ~d"
                     (decoded-instruction-memory-location instr)
                     (first operands) divisor)))
    (sinstruction-math instr #'truncate)))

;; MOD a b -> (result) - Spec p88, p16
;; Examples:
;; -13 %  5 = -3
;;  13 % -5 =  3
;; -13 % -5 = -3
;; These examples are accurately handled by the second return
;; value of the Common Lisp TRUNCATE function.
;; FIXME: Combine with instruction-div for code reuse.
;; NOTE: This appears exactly twice in one routine in Zork 1.
(defun instruction-mod (instr)
  (let* ((operands (retrieve-check-operands instr 2))
         (divisor (second operands)))
    ;; Check for division by zero before it happens
    (when (= 0 divisor)
      ;; XXX: Add some restarts
      (error 'z-division-by-zero :message
             (format nil "0x~4,0X: MOD ~d ~d"
                     (decoded-instruction-memory-location instr)
                     (first operands) divisor)))
    (sinstruction-math instr
                       (lambda (x y)
                         (nth-value 1 (truncate x y))))))


;; AND: Bitwise AND. (Spec p79)
(defun instruction-and (instr)
  (sinstruction-math-unsigned instr #'logand))

;; OR: Bitwise OR. (Spec p90)
;; In Zork 1, this is used in exactly one place,
;; in the "script" game command for starting/ending
;; a transcript. In that case, it sets
;; bit 1 of Flags 2 using this (and "unscript" uses the AND
;; above to clear it).
(defun instruction-or (instr)
  (sinstruction-math-unsigned instr #'logior))


;; NOT value -> (result): Bitwise NOT. (Spec p89) - v1-4 only
;; NOTE: This is never used in Zork 1.
;; This must flip all the 16 bits in the value.


;; INC (variable) value - Spec page 86
;; Increment variable by 1. (This is signed, so -1 increments to 0.)
;; See page 77 for notes on the variable (which is encoded as a small constant).
;; Page 161: In the seven opcodes that take indirect variable references
;; (inc, dec, inc_chk, dec_chk, load, store, pull), an indirect reference
;; to the stack pointer does not push or pull the top item of the stack -
;; it is read or written in place.
(defun instruction-inc (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         ;; FIXME: These local variable names suck
         (var (first operands))
         (varval (var-read var))
         (incvarval (logand #xFFFF (1+ varval)))) ; SIGNED
    ;; Do the increment
    ;; Note: If we pop the stack with the read, we immediately re-push it
    ;; with the write, so it's all good.
    (var-write var incvarval)
    (dbg t instr "INC: Incremented ~A (newval: ~A)~%"
         (disassemble-operand 'variable var) incvarval)
    (advance-pc instr)
    (values t "INC")))

;; DEC (variable) value - Spec page 83
;; Decrement variable by 1. This is signed, so 0 decrements to -1.
;; See page 77 for notes on the variable (which is encoded as a small constant).
;; Page 161: In the seven opcodes that take indirect variable references
;; (inc, dec, inc_chk, dec_chk, load, store, pull), an indirect reference
;; to the stack pointer does not push or pull the top item of the stack -
;; it is read or written in place.
(defun instruction-dec (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         ;; FIXME: These local variable names suck
         (var (first operands))
         (varval (var-read var))
         (decvarval (logand #xFFFF (1- varval)))) ; SIGNED
    ;; Do the increment
    ;; Note: If we pop the stack with the read, we immediately re-push it
    ;; with the write, so it's all good.
    (var-write var decvarval)
    (dbg t instr "DEC: Decremented ~A (newval: ~A)~%"
         (disassemble-operand 'variable var) decvarval)
    (advance-pc instr)
    (values t "DEC")))

;; Generalize math instruction. Takes a function and implements
;; the rest of the instruction.
(defun sinstruction-math (instr func)
  (let* ((operands (retrieve-operands instr))
         (a (us-to-s (first operands) 16))
         (b (us-to-s (second operands) 16))
         (result (funcall func a b))
         (unsigned-result (s-to-us-16 result))
         (inst-name (string-upcase (oci-name (decoded-instruction-opcode-info instr))))
         (dest (decoded-instruction-store instr)))
    (warn-16-bit-size result)
    (var-write dest unsigned-result)
    (dbg t instr "~A: ~A op ~A = ~A (0x~x -> var 0x~x)~%"
         inst-name a b result unsigned-result dest)
    (advance-pc instr)
    (values t inst-name)))

;; Generalize math instruction. Takes a function and implements
;; the rest of the instruction, without doing sign conversion.
(defun sinstruction-math-unsigned (instr func)
  (let* ((operands (retrieve-operands instr))
         (a (first operands))
         (b (second operands))
         (result (funcall func a b))
         (inst-name (string-upcase (oci-name (decoded-instruction-opcode-info instr))))
         (dest (decoded-instruction-store instr)))
    (warn-16-bit-size result)
    (var-write dest result)
    (dbg t instr "~A: ~A op ~A = ~A (-> var 0x~x)~%"
         inst-name a b result dest)
    (advance-pc instr)
    (values t inst-name)))

;; RANDOM range -> (result)
;; Spec 2.4, remarks thereafter, page 94
;; If range is positive, returns a uniformly random number between 1 and range.
;; If range is negative, the random number generator is seeded to that value and
;; the return value is 0. Most interpreters consider giving 0 as range illegal
;; (because they attempt a division with remainder by the range), but correct
;; behaviour is to reseed the generator in as random a way as the interpreter
;; can (e.g. by using the time in milliseconds).
;; (Some version 3 games, such as 'Enchanter' release 29, had a debugging verb
;; #random such that typing, say, #random 14 caused a call of random with -14.)
(defun instruction-random (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         (dest (decoded-instruction-store instr))
         (range (first operands))
         (mode 'random) ; random or predictable - future use
         (result 0))
    (cond
      ((> range 0)
       ;; XXX: CODE ME: Use a random or predictable setup
       (setf result (1+ (random range))))
      ((= range 0)
       ;; XXX: CODE ME: Reset random state to randomized value
       )
      (t ; < range 0
       ;; XXX: CODE ME: Set random state seed to known value and switch to
       ;; predictable mode
       ))
    (dbg t instr "RANDOM ~d -> 0x~x, result: ~d, mode: ~A~%" range dest result mode)
    (var-write dest result)
    (advance-pc instr)
    (values t (format nil "RANDOM ~a" mode))))

;; OUTPUT INSTRUCTIONS -----------------------------------------------------

;; PRINT (Spec page 91, Chapter 7)
;; XXX: This initial implementation only outputs to *z-output*
;; XXX: Do buffering
(defun instruction-print (instr)
  (write-string (decoded-instruction-text-ascii instr) *z-output*)
  (dbg t instr "PRINT: \"~A\"~%" (decoded-instruction-text-ascii instr))
  (advance-pc instr)
  (values t "PRINT"))

;; NEWLINE (Spec page 89)
;; XXX: This initial implementation only outputs to *z-output*
;; XXX: Do buffering (reset the buffering count)
(defun instruction-new_line (instr)
  (write-char #\Newline *z-output*)
  (dbg t instr "NEW_LINE~%")
  (advance-pc instr)
  (values t "NEW_LINE"))

;; PRINT_RET (Spec page 92)
;; Print the quoted (literal) Z-encoded string, then print a new-line and
;; then return true (i.e., 1).
;; XXX: This initial implementation only outputs to *z-output*
;; XXX: Do buffering
(defun instruction-print_ret (instr)
  (dbg t instr "PRINT_RET: \"~A\"~%" (decoded-instruction-text-ascii instr))
  (write-string (decoded-instruction-text-ascii instr) *z-output*)
  (write-char #\Newline *z-output*)
  (sinstruction-ret instr 1))

;; PRINT_NUM (Spec page 92)
;; XXX: This initial implementation only outputs to *z-output*
;; XXX: Do buffering (reset the buffering count)
(defun instruction-print_num (instr)
  (let ((operands (retrieve-operands instr)))
    (when (not (= 1 (length operands)))
      ;; Raise condition for invalid number of args.
      ;; This should really just crash the whole program
      (error 'invalid-operand-count :message
             (format nil "PRINT_NUM: Got ~A operands, expected 1" (length operands))))
    (format *z-output* "~d" (us-to-s (first operands) 16))
    (dbg t instr "PRINT_NUM: 0x~X as ~d~%" (first operands) (us-to-s (first operands) 16))
    (advance-pc instr)
    (values t "PRINT_NUM")))

;; PRINT_CHAR char-code (Spec page 92, 7.1.2.2.1)
;; char-code is a ZSCII character that must be in the range 0-1023.
;; We're going to assume ZSCII is just ASCII for the time being.
(defun instruction-print_char (instr)
  (let ((operands (retrieve-operands instr)))
    (when (not (= 1 (length operands)))
      ;; Raise condition for invalid number of args.
      ;; This should really just crash the whole program
      (error 'invalid-operand-count :message
             (format nil "PRINT_CHAR: Got ~A operands, expected 1" (length operands))))
    (write-char (code-char (first operands)) *z-output*)
    (dbg t instr "PRINT_CHAR: 0x~X as ~A~%" (first operands) (code-char (first operands)))
    (advance-pc instr)
    (values t "PRINT_NUM")))

;; PRINT_OBJ (Spec page 92)
;; Prints the short-name of the specified object
;; XXX: This initial implementation only outputs to *z-output*
;; XXX: Do buffering (reset the buffering count)
(defun instruction-print_obj (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         (object-id (first operands))
         (object (load-object object-id)))
    (write-string (zobj-short-name object) *z-output*)
    (dbg t instr "PRINT_OBJ: Object ~d as \"~A\"~%" object-id (zobj-short-name object))
    (advance-pc instr)
    (values t "PRINT_OBJ")))

;; PRINT_PADDR paddr (Spec page 92)
;; Print the (Z-encoded) string at the given packed address in high memory.
;; Packed addresses are 2* the given number. (Spec 1.2.3)
;; XXX: This initial implementation only outputs to *z-output*
;; XXX: Do buffering (reset the buffering count)
(defun instruction-print_paddr (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         (paddr  (first operands))
         (addr   (* 2 paddr))
         (zchars (mem-string addr))
         (str    (z-characters-to-string (break-zchar-string zchars))))
    (write-string str *z-output*)
    (dbg t instr "PRINT_PADDR: Packed address 0x~x as \"~A\"~%" paddr str)
    (advance-pc instr)
    (values t "PRINT_PADDR")))

;; PRINT_ADDR paddr (Spec page 91)
;; Print the (Z-encoded) string at the given byte address.
;; XXX: This initial implementation only outputs to *z-output*
;; XXX: Do buffering (reset the buffering count)
(defun instruction-print_addr (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         (addr   (first operands))
         (zchars (mem-string addr))
         (str    (z-characters-to-string (break-zchar-string zchars))))
    (write-string str *z-output*)
    (dbg t instr "PRINT_ADDR: Address 0x~x as \"~A\"~%" addr str)
    (advance-pc instr)
    (values t "PRINT_ADDR")))


;; INPUT INSTRUCTIONS ------------------------------------------------------

;; Updates the status bar
;; XXX: CODE ME
(defun update-status-bar ()
  )

;; SREAD: baddr1 baddr2 (zmach06e.pdf 8.9, Spec p94-96)
;; SREAD: text parse
;; 1. Show/refresh/paint the status bar
;; 2. Read a command, echoing it to the screen as if it were print_char'd
;;    (but sending nothing to any output streams), until newline (13)
;;    which is also echoed
;; 3. Store downcased input characters at bytes starting at (baddr1 + 1)
;;    and ending with a 0 byte, with at most (N - 1) bytes (where N = the byte
;;    at baddr1, which is the buffer size). No terminating character (like
;;    newline) should be put into the buffer.
;; 4. Lexical analysis is performed.
;;    a. byte 0 of parse contains max # of textual words which can be parsed
;;    b. the buffer must be at least 2 + 4*n bytes long
;;    c. interpreter divides text into words and looks them up in dictionary
;;    d. number of words written in byte 1
;;    e. one 4-byte block is written per word
;;    f. Each block consists of:
;;       i. the byte address of the word in the dictionary, if it is in the
;;          dictionary, or 0 if it isn't;
;;       ii. followed by a byte giving the number of letters in the word;
;;       iii. and finally a byte giving the position in the text-buffer of
;;            the first letter of the word. IMPORTANT NOTE: This seems to
;;            count from 1, though (so weird)
;;
;; NOTE: This initial version will just read a line of input from the
;; standard input. A future version will return a special code that
;; tells the interpreter's executor that an SREAD instruction needs
;; input, and that the continuation function should be called after
;; that input has been obtained (with the input). This will allow
;; the interpreter to return control to CLIM for input, and then CLIM
;; to return control to the interpreter once the input is obtained.
;;
(defun instruction-sread (instr)
  (let* ((operands (retrieve-check-operands instr 2))
         (text-len-loc (first operands))     ; Location of max # of input chars byte
         (text-body-loc (1+ text-len-loc))   ; Where to write the input text (ZSCII) nul-term
         (parse-max-loc (second operands))   ; Number of parsed tokens allowed (4 bytes each)
         (parse-len-loc (1+ parse-max-loc))   ; Where we write the # of parsed tokens
         (parse-base-loc (1+ parse-len-loc))  ; First parsed token loc
         (text-max (mem-byte (first operands))) ; Maximum text length to store
         (parse-max (mem-byte parse-max-loc))   ; Maximum parsed entries to store
         (text)
         (text-trunc)
         (tokenized))
    ;; Update our status bar
    (update-status-bar)
    ;; Do our input
    (setf text (read-line))
    ;; Truncate our input for tokenization
    ;; FIXME: This may have an off by one or two error on the text-max use
    (setf text-trunc (subseq text 0 (min (length text) (1- text-max))))
    ;; Store our downcased input at the location, 0-character terminated
    (mem-ascii-write text-body-loc (string-downcase text-trunc) text-max)
    ;; Parse and tokenize our string
    (setf tokenized (z-tokenize text-trunc))
    ;; And write the tokens to memory in the parse buffer
    (mem-byte-write parse-len-loc (min parse-max (length tokenized)))
    (loop
       for pn from 0 below parse-max
       for token in tokenized
       do
       ;; Write a token
         (mem-word-write (+ parse-base-loc (* 4 pn))   (first token))
         (mem-byte-write (+ parse-base-loc (* 4 pn) 2) (second token))
         (mem-byte-write (+ parse-base-loc (* 4 pn) 3) (third token)))
    ;; And that's all...?
    (dbg t instr "SREAD: text-max: ~d, parse-max ~d~%" text-max parse-max)
    (dbg t instr "SREAD: Read trunc '~A'~%" text-trunc)
    (dbg t instr "SREAD: Tokenization: ~A~%" tokenized)
    (advance-pc instr)
    (values t "SREAD")))
    
    
    



;; BRANCH INSTRUCTIONS -----------------------------------------------------

;; Tells us if we should take the branch
;; given the result of the comparison and the "branch-if" bit.
;; Returns true if both are true or both are false
(defun do-branch? (result branch-if)
  (or (and result branch-if)
      (and (not result) (not branch-if))))

;; JE a b [c] [d]: (Spec page 86)
;; Jump if a is equal to any of the subsequent operands.
;; (Thus @je a never jumps and @je a b jumps if a = b.)
;; Errors if there is exactly one arg
;;
;; Per page 161:
;; je can take between 2 and 4 operands. je with just 1 operand is not permitted.
;; Per page 86:
;; Thus @je a never jumps.
;; SO... WHAT TO DO? Error or silently ignore (never jump) with 1 arg?
(defun instruction-je (instr)
  (flet ((test-je (operands)
          ;; Implement the JE test here
          (cond
            ((null operands)) ; No args are always equal to each other
            ((= (length operands) 1)
             ;; raise condition for invalid # of args
             (restart-case
                 (error 'invalid-operand-count :message
                        "JE provided exactly one arg (needs 0 or 2+)")
               (return-true  () :report "Consider it equal" t)
               (return-false () :report "Consider it unequal" nil)))
            (t
             (some (lambda (b) (= (car operands) b)) (cdr operands))))))
    (sinstruction-jx instr #'test-je)))

;; TEST bitmap flags: Spec page 102
;; Jump if all of the flags in bitmap are set (i.e. if bitmap & flags == flags
(defun instruction-test (instr)
  (flet ((test-test (operands)
           (let ((bitmap (first operands))
                 (flags (second operands)))
             (= (logand bitmap flags) flags))))
    (let ((operands (retrieve-check-operands instr 2)))
      (declare (ignore operands)) ; We just check the #
      (sinstruction-jx instr #'test-test))))

;; JZ: Jump if the argument is zero. (Spec page 87)
;; SEE sinstruction-jz for more details.
(defun instruction-jz (instr)
  (flet ((test-jz (operands)
           (cond
             ((not (= (length operands) 1))
              ;; raise condition for invalid # of args
              (restart-case
                  (error 'invalid-operand-count :message
                         "JZ not provided exactly one arg")
                (return-true  () :report "Consider it zero" t)
                (return-false () :report "Consider it nonzero" nil)))
             (t
              (= (car operands) 0)))))
    (sinstruction-jx instr #'test-jz)))

;; TEST_ATTR: Branch if object has attribute. (Spec page 103)
;; Operands: object attribute
;; NOTE: There's either a bug in my interpreter, or ZORK1 is trying
;; to do a TEST_ATTR #7 on Object ID 0. Let's have a special check
;; for Object ID 0 (no attributes), but also log a message when that
;; happens for now while we decide FIXME: if this should stand.
(defun instruction-test_attr (instr)
  ;; XXX: Check # of operands for exactly 2
  (flet ((test-test_attr (operands)
           (let* ((obj-id (first operands))
                  (attrib (second operands))
                  (o-attr ; The attributes of the specified object
                   (if (= 0 obj-id)
                       (progn
                         (format t "[Warning: TEST_ATTR on Object 0 at 0x~4,'0x]~%"
                                 (decoded-instruction-memory-location instr))
                         0)
                       (zobj-attributes (load-object obj-id))))
                  (shift-amt (- 31 attrib)))

             ;; Attribute 0 is the top bit but 31 is the bottom bit.
             (nonzerop
              (logand o-attr
                      (ash 1 shift-amt))))))
    (sinstruction-jx instr #'test-test_attr)))

;; JIN obj1 obj2 (Spec page 87)
;; Jump if obj1 is a direct child of obj2
;; (i.e., obj1.parent == obj2)
(defun instruction-jin (instr)
  (flet ((test-jin (operands)
           (let* ((obj1    (load-object (first operands)))
                  (obj2-id (second operands)))
             (= obj2-id (zobj-parent obj1)))))
    (sinstruction-jx instr #'test-jin)))

;; INC_CHK (variable) value ?(label) - Spec page 86
;; Increment a variable and branch if it exceeds the
;; specified value.
;; Page 161: In the seven opcodes that take indirect variable references
;; (inc, dec, inc_chk, dec_chk, load, store, pull), an indirect reference
;; to the stack pointer does not push or pull the top item of the stack -
;; it is read or written in place.
(defun instruction-inc_chk (instr)
  ;; XXX: Check # of operands
  (let* ((operands (retrieve-operands instr))
         ;; FIXME: These local variable names suck
         (var (first operands))
         (value (second operands))
         (varval (var-read var))
         (incvarval (logand #xFFFF (1+ varval))))
    ;; Do the increment
    ;; Note: If we pop the stack with the read, we immediately re-push it
    ;; with the write, so it's all good.
    (var-write var incvarval)
    (dbg t instr "INC_CHK: Incremented ~A (newval: ~A) and tested against ~A~%"
         (disassemble-operand 'variable var) incvarval value)
    ;; Now run the test
    (flet ((test-inc_chk (operands)
             (declare (ignore operands))
             ;; We ignore the operands and just return the test
             ;; of our captured variables.
             (> incvarval value)))
      (sinstruction-jx instr #'test-inc_chk))))

;; DEC_CHK (variable) value ?(label) - Spec p 83
;; Increment a variable and branch if now is less than the given value
;; Page 161: In the seven opcodes that take indirect variable references
;; (inc, dec, inc_chk, dec_chk, load, store, pull), an indirect reference
;; to the stack pointer does not push or pull the top item of the stack -
;; it is read or written in place.
(defun instruction-dec_chk (instr)
  ;; XXX: Check # of operands
  (let* ((operands (retrieve-operands instr))
         ;; FIXME: These local variable names suck
         (var (first operands))
         (value (us-to-s (second operands) 16))
         (varval (var-read var))
         (svarval (us-to-s varval 16))
         (decvarval (1- svarval)))
    ;; Do the decrement
    ;; Note: If we pop the stack with the read, we immediately re-push it
    ;; with the write, so it's all good.
    (var-write var (s-to-us-16 decvarval))
    (dbg t instr "DEC_CHK: Decremented ~A (old: ~A, new: ~A) and tested against ~A~%"
         (disassemble-operand 'variable var) svarval decvarval value)
    ;; Now run the test
    (flet ((test-dec_chk (operands)
             (declare (ignore operands))
             ;; We ignore the operands and just return the test
             ;; of our captured variables.
             (< decvarval value)))
      (sinstruction-jx instr #'test-dec_chk))))


;; Jl a b ?(label) - Spec page 87
;; Jump if a < b (using a signed 16-bit comparison).
(defun instruction-jl (instr)
  ;; FIXME: Ugly - incorporate the op # check into sinstruction-jx?
  (retrieve-check-operands instr 2) ; Discard result, just check for 2 operands
  (flet ((test-jl (operands)
           (let* ((a (us-to-s (first  operands) 16))
                  (b (us-to-s (second operands) 16)))
             (< a b))))
    (sinstruction-jx instr #'test-jl)))

;; jg a b ?(label) - Spec page 87
;; Jump if a > b (using a signed 16-bit comparison).
(defun instruction-jg (instr)
  ;; FIXME: Ugly - incorporate the op # check into sinstruction-jx?
  (retrieve-check-operands instr 2) ; Discard result, just check for 2 operands
  (flet ((test-jg (operands)
           (let* ((a (us-to-s (first  operands) 16))
                  (b (us-to-s (second operands) 16)))
             (> a b))))
    (sinstruction-jx instr #'test-jg)))

;; Implements all jump-if instructions by taking a test
;; function. The text function takes one argument, which is
;; the list of operands, and returns true if the test passes,
;; false otherwise. The test function is allowed to (and is
;; encouraged to) raise conditions.
(defun sinstruction-jx (instr testfunc)
  (let* ((operands (retrieve-operands instr))
         (i-pc  (decoded-instruction-memory-location instr))
         (i-len (decoded-instruction-length instr))
         (br-if (decoded-instruction-branch-if instr))
         (offset (decoded-instruction-branch-offset instr)) ; Signed (!!!)
         (inst-name (oci-name (decoded-instruction-opcode-info instr)))
         (result (funcall testfunc operands)))
    (dbg t instr "~A: ~A if ~A to ~A: Result ~A~%" (string-upcase inst-name) operands
         (if br-if "true" "false") offset (if result "true" "false"))
    (cond
      ;; Don't branch
      ((not (do-branch? result br-if))
       (advance-pc instr)
       (values t "Branch not taken"))
      ;; Return from current routine with 0/false or 1/true
      ((or (= offset 0) (= offset 1))
       (sinstruction-ret instr offset))
      ;; Take branch - calculate new location
      (t
       (set-pc (+ i-pc i-len offset -2))
       (values t "Branched")))))


;; JUMP INSTRUCTION -----------------------------------------------------

;; Calculates the jump destination for this instruction
;; ASSERT: It's a JUMP
;; XXX: What if there are a wrong number of operands?
;; XXX: What if the operand isn't a 'const-large?
(defun jump-destination (instr)
  (let* ((i-pc  (decoded-instruction-memory-location instr))
         (i-len (decoded-instruction-length instr))
         (operands (retrieve-operands instr))
         (unsigned-offset (car operands))
         (signed-offset (us-to-s unsigned-offset 16)))
    (+ i-pc i-len signed-offset -2)))


;; JUMP: Jump unconditionally to signed offset from current PC
;; (Spec, page 87, 161)
;; Destination: address after instruction + offset - 2
;; Legal to jump between routines (!) without changing call frame
;; XXX: Check if we're jumping somewhere within memory
(defun instruction-jump (instr)
  (let* ((op-types (decoded-instruction-operand-types instr)))
    (cond
      ((not (= 1 (length op-types)))
       ;; No restarts!
       (error 'invalid-operand-count :message
              (format nil "Exactly one argument required, got: ~A"
                      (length op-types))))
      ((not (eql 'const-large (car op-types)))
       (error 'instr-error :message
              (format nil "JUMP: Operand type not implemented: ~A"
                      (car op-types))))))
  (let* ((ml-dest (jump-destination instr)))
    ;; XXX: Check destination is within memory
    (dbg t instr "JUMP: Jumping to 0x~4,'0x~%" ml-dest)
    (set-pc ml-dest)))


;; MEMORY INSTRUCTIONS --------------------------------------------------


;; STOREW (Spec page 102)
;; Operands: array word-index value
;; Stores "value" at memory location (array + 2 * word-index)
;; Memory location must be in dynamic memory (0x0000 to +ml-loc-static+).
;; XXX: Enforce memory restriction
(defun instruction-storew (instr)
  (let ((operands (retrieve-operands instr)))
    (when (not (= 3 (length operands)))
      ;; Raise condition for invalid number of args.
      ;; This should really just crash the whole program
      (restart-case
          (error 'invalid-operand-count :message
                 (format nil "STOREW: Got ~A operands, expected 3" (length operands)))
        (ignore () :report "Ignore; store nothing"))
      ;; XXX: This doesn't seem like it will work, it doesn't advance PC
      (return-from instruction-storew (values nil "Invalid # of args")))
    (let* ((array-base (first operands))
           (array-index (second operands))
           (value (third operands))
           (ml-dest (+ array-base (* 2 array-index))))
      (dbg t instr "STOREW: Writing 0x~x to 0x~x (as ~x[~x])~%"
           value ml-dest array-base array-index)
      (mem-word-write ml-dest value)
      (advance-pc instr)
      (values t "Stored word"))))

;; STOREB (Spec page 102)
;; Operands: array byte-index value
;; array->byte-index = value, i.e. stores the given value in the byte
;;; at address array+byte-index (which must lie in dynamic memory).
;; XXX: Enforce memory restriction
(defun instruction-storeb (instr)
  (let* ((operands (retrieve-check-operands instr 3))
         (array-base (first operands))
         (array-index (second operands))
         (value (third operands))
         (ml-dest (+ array-base array-index)))
    (dbg t instr "STOREB: Writing 0x~x to 0x~x (as ~x[~x])~%"
         value ml-dest array-base array-index)
    (mem-byte-write ml-dest value)
    (advance-pc instr)
    (values t "Stored byte")))

;; STORE (variable) value (Spec page 102)
;; Set the variable referenced by the first operand to the specified value
(defun instruction-store (instr)
  (let ((operands (retrieve-operands instr)))
    (when (not (= 2 (length operands)))
      ;; Raise condition for invalid number of args.
      (error 'invalid-operand-count :message
             (format nil "STOREW: Got ~A operands, expected 2" (length operands))))
    (var-write (first operands) (second operands))
    (dbg t instr "STORE: Wrote 0x~4,'0x to variable ~X~%" (second operands) (first operands))
    (advance-pc instr)
    (values t "Stored value in variable")))

;; Loadw (Spec page 88)
;; Operands: array word-index
;; Loads word value from memory at (array + 2 * word-index)
;; and stores into specified variable
(defun instruction-loadw (instr)
  (let ((operands (retrieve-operands instr)))
    (when (not (= 2 (length operands)))
      ;; Raise condition for invalid number of args.
      ;; This should really just crash the whole program
      (restart-case
          (error 'invalid-operand-count :message
                 (format nil "LOADW: Got ~A operands, expected 2" (length operands)))
        (ignore () :report "Ignore; load nothing"))
      ;; XXX: This doesn't seem like it will work, it doesn't advance PC
      (return-from instruction-loadw (values nil "Invalid # of operands")))
    (let* ((array-base (first operands))
           (array-index (second operands))
           (var-dest (decoded-instruction-store instr))
           (ml-source (+ array-base (* 2 array-index)))
           (value (mem-word ml-source)))
      (dbg t instr "LOADW: Loaded 0x~x from 0x~x (as ~x[~x]) into VAR 0x~x~%"
           value ml-source array-base array-index var-dest)
      (var-write var-dest value)
      (advance-pc instr)
      (values t "Loaded word"))))

;; LOADB array byte (Spec page 88)
;; Loads byte value into memory from (array + byte) to the result variable
(defun instruction-loadb (instr)
  (let ((operands (retrieve-operands instr)))
    (when (not (= 2 (length operands)))
      ;; Raise condition for invalid number of args.
      ;; This should really just crash the whole program
      (error 'invalid-operand-count :message
             (format nil "LOADB: Got ~A operands, expected 2" (length operands))))
    (let* ((array-base (first operands))
           (array-index (second operands))
           (var-dest (decoded-instruction-store instr))
           (ml-source (+ array-base (* array-index)))
           (value (mem-byte ml-source)))
      (dbg t instr "LOADB: Loaded 0x~x from 0x~x (as ~x[~x]) into VAR 0x~x~%"
           value ml-source array-base array-index var-dest)
      (var-write var-dest value)
      (advance-pc instr)
      (values t "Loaded byte"))))

;; LOAD (variable) -> (result) - Spec page 87
;; The value of the variable referred to by the operand is stored in the result.
(defun instruction-load (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         (var-src  (first operands))
         (var-dest (decoded-instruction-store instr)))
    (var-write var-dest (var-read var-src))
    (dbg t instr "LOAD: Copied variable 0x~x to variable 0x~x~%" var-src var-dest)
    (advance-pc instr)
    (values t "LOAD")))
                    


;; PUSH value (Spec page 93)
;; "Pushes a value onto the game stack."
;; Presumably it means the current call frame's stack
(defun instruction-push (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         (value (car operands)))
    (push-stack value)
    (advance-pc instr)
    (dbg t instr "PUSH: ~A~%" value)
    (values t "Pushed")))

;; PULL value (Spec page 77, 93, 161)
;; The operand is a variable ID to write the pull'd value from
(defun instruction-pull (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         (variable (car operands))
         (value    (pop-stack)))
    (var-write variable value)
    (advance-pc instr)
    (dbg t instr "PULL: ~A -> VAR 0x~x~%" value variable)
    (values t "Pulled")))





;; OBJECT INSTRUCTIONS --------------------------------------------------


;; PUT_PROP object property value
;; (Spec page 94, Chapter 12)
;; (zmach06e.pdf Chapter 3.4)
;;
;; From spec: Writes the given value to the given property of the given object.
;; If the property does not exist for that object, the interpreter should halt
;; with a suitable error message. If the property length is 1, then the interpreter
;; should store only the least significant byte of the value. (For instance,
;; storing -1 into a 1-byte property results in the property value 255.) As with
;; get_prop the property length must not be more than 2: if it is, the behaviour
;; of the opcode is undefined.
;;
;; Notes:
;; Objects are numbered from 1; there is no object 0 (no name, attribs, props).
;; There are at most 255 objects. It's an error to reference a non-existent object.
;; Properties are numbered 1-31.
;; Each property present has 1-8 bytes of data.
;; THIS INSTRUCTION only deals with properties of 1-2 bytes of data.
(defun instruction-put_prop (instr)
  (let ((operands (retrieve-operands instr)))
    (when (not (= 3 (length operands)))
      ;; Raise condition for invalid number of args.
      ;; This should really just crash the whole program.
      (error 'invalid-operand-count :message
             (format nil "PUT_PROP: Got ~A operands, expected 3" (length operands))))
    (let* ((obj-id  (first operands))
           (prop-id (second operands))
           (value   (third operands))
           (object (load-object obj-id))
           (prop   (zobj-prop object prop-id)))
      ;; If we didn't find the property, it's an error
      (when (null prop)
        (error 'invalid-property :message
               (format nil "No property ~A on object ~A to set to ~A"
                       prop-id obj-id value)))
      (if (= 1 (zprop-data-size prop))
          (mem-byte-write (1+ (zprop-mem-loc prop)) (logand value #xFF))
          (mem-word-write (1+ (zprop-mem-loc prop)) value))
      (dbg t instr "PUT_PROP: Put ~4,'0X to object ~A property ~A~%"
           value obj-id prop-id)
      (advance-pc instr)
      (values t "PUT_PROP"))))


;; INSERT_OBJ object destination (Spec page 86)
;;
;; From Spec:
;; Moves object O to become the first child of the destination object D.
;; (Thus, after the operation the child of D is O, and the sibling of O
;; is whatever was previously the child of D.) All children of O move
;; with it. (Initially O can be at any point in the object tree; it may
;; legally have parent zero.)
;;
;; From zmach06e.pdf page 41:
;; remove_obj obj — 1OP:$9
;; Remove obj from its current location in the object tree; all its children move with it.
;;   The given object is removed from between its siblings (closing the sibling chain
;; again), and is changed to have no parent and no siblings. If obj has no parent –
;; and therefore no siblings – nothing happens.
;;
;; insert_obj obj1 obj2 — 2OP:$E
;; Remove obj1 from its current location in the object tree, and insert it
;; as the first child of obj 2, before all other children. All obj1’s children move with it.
;;  Object obj1 is first removed from its current location, as with remove_obj obj1.
;; It is then made the (first) child of obj2, with the formerly first child as its
;; (next) sibling.
;;   Note that a consistent non-recursive object tree is made recursive by this
;; instruction if and only if obj 1 occurs in the (finite) parent chain that begins
;; with obj 2. In this case the emulator might print a warning message, since this
;; is probably a bug.
;;
;; Algorithm: (note that each of these steps needs to be broken down further)
;; 0.
;; 1. Delink "object" from its parent
;;    a. If "object"'s parent's child is "object", then just set "object"'s
;;       parent's child to "object"'s sibling (i.e., it's the parent's first child)
;;       IF object.parent.child == object THEN object.parent.child = object.sibling
;;    b. Otherwise, walk the sibling-chain of the parent to find which object has
;;       "object" as a sibling, and set that object's sibling to "object"'s sibling
;;       (hard to put in a simple x.y.z = a.b.c)
;; 2. Change the parent of "object" to "destination"
;;    object.parent = destination
;; 3. Make "object" the first child of "destination"
;;    a. Set the sibling of "object" to the (first) child of "destination"
;;       object.sibling = destination.child
;;    b. Set the child of the "destination" to "object"
;;       destination.child = object
(defun instruction-insert_obj (instr)
  (let* ((operands (retrieve-check-operands instr 2))
         (object-id (first operands))
         (dest-id   (second operands))
         (object    nil)  ; Will load later
         (dest      nil)) ; Will load later
    ;; XXX: Validate that the objects are within range 0-255
    ;; Handle the 0 destination specially... Maybe an error?
    ;; Otherwise it's just like remove_obj I think.
    (if (= 0 dest-id)
        (object-remove object-id)
        ;; Otherwise, do the full algo above
        (progn
          ;; 1. First remove "object-id" from it's parent
          (object-remove object-id nil) ; We'll set the parent and siblings next
          (setf object (load-object object-id))
          (setf dest   (load-object dest-id))
          ;; 2. Change the object's parent to the destination
          (zobj-set-parent object dest-id)
          ;; 3. Change the object's parent to have the object
          (zobj-set-sibling object (zobj-child dest))
          (zobj-set-child   dest   object-id)))
    (dbg t instr "INSERT_OBJ: Moved object ~A to parent ~A~%" object-id dest-id)
    (advance-pc instr)
    (values t "INSERT_OBJ")))

;; REMOVE_OBJ object-id (Spec page 96)
;; Detach the object from its parent, so that it no longer has any
;; parent. (Its children remain in its possession.)
;; NOTE: This is essentially just the first step of INSERT_OBJ.
;; NOTE: In Zork 1, this instruction appears exactly once.
(defun instruction-remove_obj (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         (object-id (first operands)))
    ;; XXX: Validate that the objects are within range 0-255
    (object-remove object-id)
    (dbg t instr "REMOVE_OBJ: Moved object ~A to top level~%" object-id)
    (advance-pc instr)
    (values t "REMOVE_OBJ")))


;; SET_ATTR object-id attribute (Spec page 99)
;; XXX: Check attribute number
(defun instruction-set_attr (instr)
  (let* ((operands (retrieve-check-operands instr 2))
         (object-id (first operands))
         (attribute (second operands)))
    (object-set-attribute object-id attribute)
    (advance-pc instr)
    (dbg t instr "SET_ATTR: Set attribute ~d on object ~d~%" attribute object-id)
    (values t "SET_ATTR")))

;; CLEAR_ATTR object-id attribute (Spec page 82)
;; Make object not have the attribute numbered attribute
;; XXX: Check attribute number
(defun instruction-clear_attr (instr)
  (let* ((operands (retrieve-check-operands instr 2))
         (object-id (first operands))
         (attribute (second operands)))
    (object-clear-attribute object-id attribute)
    (advance-pc instr)
    (dbg t instr "CLEAR_ATTR: Clear attribute ~d on object ~d~%" attribute object-id)
    (values t "CLEAR_ATTR")))


;; GET_PARENT object -> (result) (Spec page 85)
(defun instruction-get_parent (instr)
  (let* ((operands  (retrieve-check-operands instr 1))
         (object-id (first operands))
         (object    (load-object object-id))
         (parent-id (zobj-parent object)))
    (var-write (decoded-instruction-store instr) parent-id)
    (advance-pc instr)
    (dbg t instr "GET_PARENT: Got parent ~d of object ~d stored into VAR 0x~x~%"
         parent-id object-id (decoded-instruction-store instr))
    (values t "GET_PARENT")))

;; GET_CHILD object -> (result) ?(label) (Spec page 84)
;; Get first object contained in given object, branching if this exists,
;; i.e. is not nothing (i.e., is not 0).
(defun instruction-get_child (instr)
  (let* ((operands  (retrieve-check-operands instr 1))
         (object-id (first operands))
         (object    (load-object object-id))
         (child-id  (zobj-child object))
         (result-v  (decoded-instruction-store instr)))
    (dbg t instr "GET_CHILD: Got child ~d of object ~d stored into VAR 0x~x~%"
         child-id object-id result-v)
    (var-write result-v child-id)
    ;; This is a branch too, so let's do the branch test
    (flet ((test-get_child (operands)
             (declare (ignore operands))
             (nonzerop child-id)))
      (sinstruction-jx instr #'test-get_child))))

;; GET_SIBLING object -> (result) ?(label) (Spec page 85)
;; Get next object in tree, branching if this exists, i.e. is not 0.
;; (Note, nigh identical to get_child.)
(defun instruction-get_sibling (instr)
  (let* ((operands  (retrieve-check-operands instr 1))
         (object-id (first operands))
         (object    (load-object object-id))
         (sib-id    (zobj-sibling object))
         (result-v  (decoded-instruction-store instr)))
    (dbg t instr "GET_SIBLING: Got sibling ~d of object ~d stored into VAR 0x~x~%"
         sib-id object-id result-v)
    (var-write result-v sib-id)
    ;; This is a branch too, so let's do the branch test
    (flet ((test-get_sibling (operands)
             (declare (ignore operands))
             (nonzerop sib-id)))
      (sinstruction-jx instr #'test-get_sibling))))

;; GET_PROP object-id property -> (result) (Spec page 85)
;; Read property from object (resulting in the default value if it had
;; no such declared property). If the property has length 1, the value
;; is only that byte. If it has length 2, the first two bytes of the
;; property are taken as a word value. It is illegal for the opcode to
;; be used if the property has length greater than 2, and the result is
;; unspecified.
;;
;; NOTE: This is not very efficient.
(defun instruction-get_prop (instr)
  (let* ((operands    (retrieve-check-operands instr 2))
         (object-id   (first operands))
         (property-id (second operands))
         (object      (load-object object-id))
         (property    (zobj-prop object property-id))
         ;; Wish CL was lazy like Haskell sometimes
         (property-def (get-property-default property-id))
         (result-var   (decoded-instruction-store instr))
         (prop-val
          (cond
            ((null property) property-def)
            ((> (zprop-data-size property) 2)
             ;; XXX: Add a restart-case that allows use of property-def
             (error 'invalid-property :message
                    (format nil "Cannot get property ~d of size ~d from obj ~d~%"
                            property-id object-id (zprop-data-size property))))
            ((zprop-data-val property)))))
    (var-write result-var prop-val)
    (advance-pc instr)
    (dbg t instr "GET_PROP: Prop ~d of object ~d is 0x~x into VAR 0x~x~%"
         property-id object-id prop-val result-var)
    (values t "GET_PROP")))

;; GET_NEXT_PROP object property -> (result) - Spec page 85
;; Gives the number of the next property provided by the quoted object. This
;; may be zero, indicating the end of the property list; if called with zero,
;; it gives the first property number present. It is illegal to try to find the
;; next property of a property which does not exist, and an interpreter should
;; halt with an error message (if it can efficiently check this condition).
;;
;; NOTE: Not efficient.
(defun instruction-get_next_prop (instr)
  (let* ((operands    (retrieve-check-operands instr 2))
         (object-id   (first operands))
         (property-id (second operands))
         (object      (load-object object-id))
         ;; Find our property in the list of properties, as the
         ;; car of a list of properties
         (property    (member-if (lambda (x) (= property-id (zprop-id x)))
                                 (zobj-properties object)))
         ;; Wish CL was lazy like Haskell sometimes
         (result-var  (decoded-instruction-store instr))
         (next-id     0))
    (when (and (null property) (nonzerop property-id))
      ;; We are being asked for a non-existent property
      ;; XXX: Add a restart-case that allows use of property-def
      (error 'invalid-property :message
             (format nil "Cannot get non-existent property ~d from obj ~d~%"
                     property-id object-id)))
    (setf next-id
          (cond
            ((and (zerop property-id) (zobj-properties object))
             ;; Get first property ID, cause we have a property
             (zprop-id (car (zobj-properties object))))
            ((zerop property-id)
             ;; No properties, so zero meaning no next property too
             0)
            ((null (cdr property))
             ;; No next property, so zero
             0)
            (t
             ;; Get property ID of next property
             (zprop-id (cadr property))))) ; (car (cdr property))
    (var-write result-var next-id)
    (advance-pc instr)
    (dbg t instr "GET_NEXT_PROP: Next prop ~d of object ~d is 0x~x into VAR 0x~x~%"
         property-id object-id next-id result-var)
    (values t "GET_NEXT_PROP")))

;; GET_PROP_ADDR object property -> (result) - Spec page 85
;; Get the byte address (in dynamic memory) of the property data for the
;; given object's property. This returns 0 if the object doesn't have
;; the property.
;;
;; NOTE: There's either a bug in my interpreter, or ZORK1 is trying
;; to do a GET_PROP_ADDR on Object ID 0 for property 0x12. Let's have a special check
;; for Object ID 0 (no attributes), but also log a message when that
;; happens for now while we decide FIXME: if this should stand.
;;
;; NOTE: This is not very efficient.
(defun instruction-get_prop_addr (instr)
  (let* ((operands    (retrieve-check-operands instr 2))
         (object-id   (first operands))
         (property-id (second operands))
         (object      (if (zerop object-id)
                          (progn
                            (format t "[Warning: GET_PROP_ADDR on Object 0 at 0x~4,'0x]~%"
                                    (decoded-instruction-memory-location instr))
                            nil)
                          (load-object object-id)))
         (property    (if (null object) nil
                          (zobj-prop object property-id)))
         ;; Wish CL was lazy like Haskell sometimes
         (result-var   (decoded-instruction-store instr))
         (retval
          (if property
              ;; The memory location is of the size/ID byte, so the actual
              ;; data is the next byte.
              (1+ (zprop-mem-loc property))
              0)))
    (var-write result-var retval)
    (advance-pc instr)
    (dbg t instr "GET_PROP_ADDR: Prop ~d of object ~d is at 0x~4,'0x into VAR 0x~x~%"
         property-id object-id retval result-var)
    (values t "GET_PROP_ADDR")))

;; GET_PROP_LEN prop-addr -> (result) - Spec page 85
;; Get length of property data (in bytes) for the given object's property.
;; It is illegal to try to find the property length of a property which does
;; not exist for the given object, and an interpreter should halt with an error
;; message (if it can efficiently check this condition).
;;
;; FIXME: We can definitely prepare a list of all known object property addresses,
;; so we could potentially check this, since the object table doesn't seem to be able
;; to have its number of objects or number of properties per object be changed.
;; But, for now, we're definitely not checking anything about addresses and are
;; simply returning the byte at the specified location.
;;
;; NOTE: The address provided to this instruction is the address of the first byte
;; of the property's DATA. The size of the property, therefore, is the byte previous.
;; Sigh. Why is this stuff not documented anywhere?
(defun instruction-get_prop_len (instr)
  (let* ((operands (retrieve-check-operands instr 1))
         (prop-addr (first operands))
         (result-var (decoded-instruction-store instr))
         (prop-size (mem-byte (1- prop-addr))) ; Property size byte
         ;; See load-property
         ;; Size of the property data is in the top 3 bits, PLUS ONE
         (retval (1+ (ash prop-size -5))))
    (var-write result-var retval)
    (advance-pc instr)
    (dbg t instr "GET_PROP_LEN: Len of prop at loc 0x~4,'0x is ~d -> 0x~2,'0x~%"
         prop-addr retval result-var)
    (values t "GET_PROP_LEN")))



;; SAVE/RESTORE INSTRUCTIONS --------------------------------------------

;; SAVE ?(label) - Spec page 98
;; On Versions 3 and 4, attempts to save the game (all questions about
;; filenames are asked by in- terpreters) and branches if successful.
;;
;; NOTE: This is unimplemented and just steps to the next instruction.
;; FIXME: IMPLEMENT ME
(defun instruction-save (instr)
  (advance-pc instr)
  (dbg t instr "SAVE: Ignored, no branch")
  (values t "SAVE (ignored)"))


;; RESTORE ?(label) - Spec page 97
;; See save. In Version 3, the branch is never actually made, since either
;; the game has successfully picked up again from where it was saved, or
;; it failed to load the save game file.
;;
;; NOTE: This is unimplemented and just steps to the next instruction.
;; FIXME: IMPLEMENT ME
(defun instruction-restore (instr)
  (advance-pc instr)
  (dbg t instr "RESTORE: Ignored, no branch")
  (values t "RESTORE (ignored)"))


;; RESTART - Spec page 97
;; Restart the game. (Any "Are you sure?" question must be asked by the game,
;; not the interpreter.) The only pieces of information surviving from the
;; previous state are the "transcribing to printer" bit (bit 0 of 'Flags 2'
;; in the header, at address $10) and the "use fixed pitch font" bit (bit 1
;; of 'Flags 2').
;;
;; In particular, changing the program start address before a restart will
;; not have the effect of restarting from this new address.
;;
;; FIXME: FULLY IMPLEMENT the saving of the two above flags
(defun instruction-restart (instr)
  (multiple-value-bind (success msg)
      (load-story-file *last-story-file*)
    (unless success
      (advance-pc instr))
    (values t msg)))

;; META-INSTRUCTIONS ----------------------------------------------------

;; No instruction provided (should never happen)
(defun sinstruction-nil (instr)
  (declare (ignore instr))
  (values nil "No instruction provided"))

;; Decoded opcode not found
(defun sinstruction-invalid (instr)
  ;; ASSERT: instr is not nil
  (values nil (format nil "Invalid instruction: Loc: 0x~x, first byte: 0x~x"
                      (decoded-instruction-memory-location instr)
                      (decoded-instruction-first-byte      instr))))

;; Decoded opcode not yet implemented
(defun sinstruction-nyi (instr)
  ;; ASSERT: instr is not nil AND instr.opcode-info is not nil
  (let* ((oi (decoded-instruction-opcode-info instr))
         (errmsg (format nil "Unimplemented: Loc 0x~x, first byte: 0x~x, opcode: ~A"
                         (decoded-instruction-memory-location instr)
                         (decoded-instruction-first-byte      instr)
                         (oci-name oi))))
    (restart-case
        ;; Always throw an error
        (error 'instr-error :message errmsg)
      (do-nothing () (values nil errmsg))
      (skip-instruction ()
        (set-pc (+ (decoded-instruction-memory-location instr)
                   (decoded-instruction-length instr)))
        (values t "Instruction skipped")))))



;; Instruction Main Routine -------------------------------------

;; This decodes and executes a single instruction. This will
;; definitely change the Program Counter. We'll see about the
;; rest of it...
;; Return values:
;; t   _      - everything was good
;; nil reason - error, with human-readable message in reason
(defun run-next-instruction ()
  (let* ((start-pc   (get-pc))
         (instr      (decode-instruction start-pc))
         (instr-func (find-instruction-function instr)))
    #+(or)
    (dbg t instr "Executing instruction: 0x~x: ~A~%" start-pc instr-func)
    (push (list start-pc (retrieve-operands instr)) *pc-log*)
    (funcall instr-func instr)))

;; This locates an instruction execution function for the
;; decoded instruction. If the instruction is not implemented,
;; it returns a placeholder, and if the instruction is
;; invalid, it returns a special function about that.
;; Note that it would probably just be better to store the
;; functions in the oci structure. :) But, this way is more fun.
(defun find-instruction-function (instr)
  (cond
    ((not instr)
     ;; No instruction provided - severe error
     #'sinstruction-nil)
    ((not (decoded-instruction-opcode-info instr))
     ;; No such opcode
     #'sinstruction-invalid)
    (t
     ;; Create our instruction name as a string, all caps (per internal CL storage)
     ;; Make a symbol out of it
     ;; And see if we have a function with that name
     ;; if = Instruction Function
     (let* ((if-name (nstring-upcase
                      (format nil "instruction-~A"
                              (oci-name (decoded-instruction-opcode-info instr)))))
            (if-symbol (find-symbol if-name))
            ;; This returns nil on nil input (since fboundp on nil is nil)
            (if-func (symbol-function-or-nil if-symbol)))
       (if if-func
           ;; Call our function, if we found one
           if-func
           ;; Not yet implemented
           #'sinstruction-nyi)))))

;; Gets the function of a symbol or nil if none.
;; From Common Lisp Hyperspec... http://clhs.lisp.se/Body/f_symb_1.htm
(defun symbol-function-or-nil (symbol)
  (if (and (fboundp symbol)
           (not (macro-function symbol))
           (not (special-operator-p symbol)))
      (symbol-function symbol)
      nil))

;; Testing -------------------------------------------------------

;; Some good zork1.z3 opcode decoding memory locations
;; Disassembly generated by "txd -d -n"
;(load-story-file "zork1.z3")

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

;; Step through the program
(defun debug-run ()
  (format t "~A~%" (disassemble-instr (decode-instruction *z-pc*)))
  (format t "[N]ext, [Q]uit: ")
  (let ((cmd (read-line)))
    (when (string-equal cmd "q")
      (return-from debug-run nil))
    (handler-case
        (run-next-instruction)
      (error (e)
        (progn
          (format t "~%Ending due to condition: ~A~%" e)
          (return-from debug-run nil))))
    (format t "[N]ext, [Q]uit: ")
    (setf cmd (read-line))
    (when (string-equal cmd "q")
      (return-from debug-run nil))
    (debug-run))) ; tail recursion

;; Run the program with tracing but no debugging
(defun trace-run ()
  (trace-run-until 'no-such-instruction-name-will-be-found))

;; Runs the program with tracing until we get to a specified instruction
(defun trace-run-until (inst-name &optional (show-disassembly t) (show-debug nil))
  (let ((+debug+ show-debug)
        (retval nil)
        (msg nil))
    (loop
       (handler-case
           (let* ((next-instr (decode-instruction *z-pc*))
                  (disass-instr (disassemble-instr next-instr))
                  (name-instr (oci-name (decoded-instruction-opcode-info next-instr))))
             (when show-disassembly
               (format t "~A~%" disass-instr))
             (when (eql inst-name name-instr)
               (format t "~%Ending due to instruction found: ~A~%" name-instr)
               (return))
             (multiple-value-setq (retval msg) (run-next-instruction))
             (when (null retval)
               (format t "~%Ending due to failed instruction: ~A~%" msg)
               (return)))
         (error (e)
           (progn
             (format t "~%Ending due to condition: ~A~%" e)
             (return)))))))

#|
;; Some tests of breaking strings
(break-zchar-string (decoded-instruction-text-data (car (last ++decoded-opcodes++))))
(break-zchar-string (decoded-instruction-text-data (car (last ++decoded-opcodes++ 2))))
|#
