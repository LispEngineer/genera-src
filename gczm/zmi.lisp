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

;; Z-Machine Routine Call State / Stack Frames (Spec 6.4, 6.5)
;; TODO: Code me (Spec 6.1, 6.3.2)
;; (we need to remember how to pop all of a routine's stack pushes after a return)

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
;; See Spec Appendix B
(defparameter +ml-rel-num+       #x02) ; 2 Release number (word)
(defparameter +ml-serial-num+    #x12) ; 6 Serial number (six ASCII chars)


;; Memory map per the above (Spec 1.1)
;; +----------------+
;; | Dynamic memory | 0x00000 - (- +ml-loc-static+ 1)
;; +----------------+
;; | Static memory  | +ml-loc-static+ - lower of last byte of story or 0x0ffff
;; +----------------+
;; | High memory    | +ml-high-mem-base+ - end of story file
;; +----------------+ (may overlap static memory)

;; Global variables are stored starting at +ml-loc-globals+ and consist
;; of 240 2-byte words. (Spec 6.2)

;; Two-byte values are stored MSB first (Spec 2.1)




;; Implementation ---------------------------------------------------------

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
      
                  
