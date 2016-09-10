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
  (+ (ash (aref *z-mem* loc) 8)
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

;; Program Counter --------------------------------------------------------


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
