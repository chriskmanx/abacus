;;;; abavus.lisp
;;;; Copyright (c) 2014 Christoph Kohlhepp 

(defpackage :abacus
  ; import namespaces from the following packages
  (:use :common-lisp :optima :let-over-lambda)
 
  ; abacus package exported symbols
  (:export #:amatch 
           #:algebraic-match-with
           #:algebraic-guard
           #:use-extended-match-syntax
           #:disable-extended-match-syntax
           :left-bracket
           :right-bracket
           :*readtables-stack*))

;; Export needed functionality from let-over-lambda
(in-package #:let-over-lambda)
(export 'defmacro!)

;;; Define package Abacus
(in-package :abacus)

;;http://dorophone.blogspot.com.au/2008/03/common-lisp-reader-macros-simple.html
;;https://gist.github.com/chaitanyagupta/9324402

;; Stack of Lisp compiler syntax readtables 
(defvar *readtables-stack* nil)

;; Our pairwise delimiters
(defconstant left-bracket #\[)
(defconstant right-bracket #\])

(defmacro use-extended-match-syntax ()
  "A macro to enable the extended match syntax;
   eval-when controls when this is executed"
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *readtables-stack*)
    (setq *readtable* (copy-readtable))
    (set-macro-character right-bracket 'read-delimiter)
    (set-macro-character left-bracket 'read-left-bracket )))

(defmacro disable-extended-match-syntax ()
  "A macro to disable the extended match syntax;
   eval-when controls when this is executed"
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *readtables-stack*))))


(defun tokenequal (x y)
  "A function which compares tokens based on aesthetic rendering equivalence,
   deliberately ignoring which package a symbol is interned in; only found to behave
   differently from equalp predicate in the context of reader macros"
  (let ((xstring (format nil "~A" x))
        (ystring (format nil "~A" y)))
    (equal xstring ystring)))


(defun parse-match-forms (forms)
  "A macro using defun syntax as we don't care about delaying evaluation of arguments here in
   that we  will be called from the compiler via (read-left-bracket) dispatching through the 
   *readtable*. This parses an expression of the form [ token ... token => token ... token ]
   and returns an s-expression of the form ((token token) (token token)) to be consumed  
   by algebraic-match-with macro. Malformed syntax raises appropriate compiler errors."
  
  (if (not forms)
    ; case [] empty expression
    (error "ABACUS: Empty match [] operation")    
    (progn
  
;;      (format t "~%; compiling ABACUS: parsing forms  ~S" forms)
      (if (not (eq 1 (count '=> forms :test #'tokenequal)))
          ; case [token...token] but no =>
          (error "ABACUS: Synax error. Match clause must contain exaxtly 1 => symbol")

          (let* ((arrow-position (position '=> forms :test #'tokenequal))
                 (pattern-specifier  (subseq forms  0 arrow-position))) 
                       
            ;; Don't make copy of match expression; we retain all formatting
            ;; We copy the pattern-specifier via subeq though
            (loop for x from 0 to arrow-position do (setf forms (cdr forms)))        
            (let ((match-expression forms))
      
              (if (not pattern-specifier)
                  ; case [=> token ... token]
                  (error "ABACUS: No pattern specifier given to match []")
                  (if (not match-expression)
                      ; case [token...token =>] 
                      (error "ABACUS: No match expression given to match [~S]" pattern-specifier)
                      (progn
                        (format t "~%; compiling ABACUS: generating ~S" 
                                 `(,@pattern-specifier ,@match-expression)) 
                        `(,@pattern-specifier ,@match-expression))))))))))


(defun read-left-bracket (stream char)
  (declare (ignore char))
  (let* ((match-list (read-delimited-list right-bracket stream t)))
      (parse-match-forms match-list)))

;; We need this as otherwise the simple expression '[x => x]
;; would fail to parse since x] would be read as an atom resulting in END-OF-FILE
(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))


(defmacro acase (&body clauses)
  (parse-match-forms clauses))


(defvar abacus-typespec nil) ; only used at compile time
                             ; silence compiler about undefined variable
                             ; at runtime

;; This macro needs no once-only as long as each input
;; is expanded but once. This is presently the case.
;; Adjust if necessary in the future.

(defmacro amatch (arg &body clauses)
"[Macro] amatch
 amatch arg &body clauses
 Same as MATCH, except that handling of algebraic types is enabled"
  (if (not (boundp 'abacus-typespec))
      (defvar abacus-typespec nil))
  `(let  ((abacus-it nil))
     (match ,arg ,@clauses)))


(defmacro algebraic-match-with  (&body clauses)
" Macro wrapper around cl-algebraic:match
  abacus-typespec is generated at compile time by algebraic-guard
  and initially defvar'ed by amatch. 
  abacus-it is also set  by code generated by algebraic-guard
  but at runtime."
  (if (or (not (boundp 'abacus-typespec)) (not abacus-typespec))
      (progn
        (error "~%ALGEBRAIC-MATCH-WITH no type specification! Did you use algebraic-guard?")
        (setf abacus-typespec nil))
      (format t "~%; compiling (ALGEBRAIC-MATCH-WITH over type ~A...)" abacus-typespec))
  `(progn
     ;;(if (not (boundp 'abacus-it ))
     ;;   (warn "~%ALGEBRAIC-MATCH-WITH no match argument! Did you use algebraic-guard?")
     (adt:match ,abacus-typespec abacus-it ,@clauses)))

;; Note use of o! and g! prefixes.
;; O-Bang provides automatic once-only bindings to gensyms
;; G-Bang dereferences through these gensyms inside the macro
;; These simple prefixes will should make our macro hygenic.
;; We guard the argument in this way, but not the type
;; Dereferencing a type ought to be side-effect free

(defmacro! algebraic-guard  (o!arg argtype)
  "Same as typep, except that it checks for algebraic type also
   and sets the abacus-match local variables abacus-it and abacus-typespec
   to reflect the last guarded instance and type.
   Expects type argument quoted like typep"

   (setf abacus-typespec argtype)
   (format t "~%; compiling (ALGEBRAIC-GUARD over type ~A...)"  abacus-typespec)
   
  `(progn
    (setf abacus-it ,g!arg)
    (and (typep ,g!arg ',argtype )
        (adt:algebraic-data-type-p ',argtype))
  )
)
