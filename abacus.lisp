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

;;;===============================
;;; Extended Pattern Match Syntax
;;;===============================

;; Stack of Lisp syntax tables
;; We use this to modify and restore current syntax tables 
(defvar *readtables-stack* nil)

;; Our pairwise delimiters - retain idiomatically Lisp outlook on scope.
;; What to use here is purely a judgement call...
;; [] are a possibility - yet Clojure uses this for argument sequences.
;; We deem readable pattern matching more paramount to the language.
;; So presently we use the OR symbol to start and ] to delimit. 
(defconstant left-bracket #\|)
(defconstant right-bracket #\])

(defmacro use-extended-match-syntax ()
  "A macro to enable the extended match syntax;
   eval-when controls when this is executed"
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *readtables-stack*)
    (setq *readtable* (copy-readtable))
    (set-macro-character right-bracket 'read-delimiter)
    (set-macro-character left-bracket 'read-expression )))

(defmacro disable-extended-match-syntax ()
  "A macro to disable the extended match syntax;
   eval-when controls when this is executed"
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *readtables-stack*))))


(defun tokenequal (x y)
  "A function which compares tokens based on aesthetic rendering equivalence,
   deliberately ignoring which package a symbol is interned in; only found to behave
   differently from equalp predicate in the context of reader macros. 
   Compiler macros seem unaffected."
  (let ((xstring (format nil "~A" x))
        (ystring (format nil "~A" y)))
    (equal xstring ystring)))


(defun parse-match-forms (forms)
  "A macro using defun syntax as we don't care about delaying evaluation of arguments here as
   we  will be called by way of the Lisp reader itself via (read-expression) dispatching 
   through the *readtable*. We essentially don't have to worry about macro hygine here. 
   This parses an expression of the form | token ... token => token ... token ]
   and returns an s-expression of the form ((token token) (token token)) to be consumed  
   by algebraic-match-with macro. Malformed syntax raises appropriate compiler errors."
  
  (if (not forms)
    ;; case |] empty expression - raise compiler error
    (error "ABACUS: Empty match |] operation")    
    (progn
      ;; Debug statement; uncommment as necessary
      ;; (format t "~%; compiling ABACUS: parsing forms  ~S" forms)
      (if (not (eq 1 (count '=> forms :test #'tokenequal)))
          ;; case [token...token] but no =>
          (error "ABACUS: Synax error. Match clause must contain exaxtly 1 => symbol")

          ;; let* is analog to OCaml let...in construct
          (let* ((arrow-position (position '=> forms :test #'tokenequal))
                 (pattern-specifier  (subseq forms  0 arrow-position))) 
                       
            ;; Don't make copy of match expression; we retain all formatting
            ;; inclusive of line feeds --- useful when examining debug statements
            ;; We do, however, copy the pattern-specifier via subeq
            (loop for x from 0 to arrow-position do (setf forms (cdr forms)))        
            (let ((match-expression forms))
      
              (if (not pattern-specifier)
                  ;; case |=> token ... token]
                  (error "ABACUS: No pattern specifier given to match |]")
                  (if (not match-expression)
                      ;; case | token...token =>] 
                      (error "ABACUS: No match expression given to match [~S]" pattern-specifier)
                      (progn
                        ;; All good, generate code
                        
                        ;; Debug statement; uncommment as necessary
                        ;;(format t "~%; compiling ABACUS: generating ~S" 
                        ;;   `(,@pattern-specifier ,@match-expression)) 
                        `(,@pattern-specifier ,@match-expression))))))))))


(defun read-expression (stream char)
  "A function to be associated, via the Lisp syntax read table,
   with the reading of pattern matching expressions"
  (declare (ignore char))
  (let* ((match-list (read-delimited-list right-bracket stream t)))
      (parse-match-forms match-list)))

(defun read-delimiter (stream char)
  "A function to be associated, via the Lisp syntax read table,
   with the delimiter of pattern matching expressions.
   We need this as otherwise the simple expression '|x => x]
   would fail to parse since x] would be read as an atom resulting in END-OF-FILE."
  (declare (ignore stream))
  (error "Delimiter of pattern matching expressions ~S found without preceeding pattern match" char))

;;;================================================================
;;; Unified handling of Optima Expressions and Algebraic Data Types
;;;================================================================

(defvar abacus-typespec nil) ;; This variable is used only in the compilation process,
                             ;; but having this declaration here silences the  compiler
                             ;; warning about an othwerwise undefined variable  at runtime


(defmacro amatch (arg &body clauses)
"[Macro] amatch
 amatch arg &body clauses
 Same as Optima MATCH, except that handling of algebraic types is enabled"

;; This macro needs no once-only to ensure hygene as long as each input
;; is expanded but once. This is presently the case. Adjust if necessary in the future.

  (if (not (boundp 'abacus-typespec)) ;; Forward declaration of algebraic type specification for
      (defvar abacus-typespec nil))   ;;   use by algebraic-guard in compile time expansion 
  `(let  ((abacus-it nil))            ;; Pitch forward match term  for use by algebraic-match-with 
     (match ,arg ,@clauses)))         ;;   in expansion for run-time use by atd:match macro
                                      
 

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
     ;; Uncomment to generate compiler warning
     ;;(if (not (boundp 'abacus-it ))
     ;;   (warn "~%ALGEBRAIC-MATCH-WITH no match argument! Did you use algebraic-guard?")
     (adt:match ,abacus-typespec abacus-it ,@clauses)))

;; Note use of o! and g! prefixes and defmacro! macro from Let-Over-Lambda.
;; O-Bang provides automatic once-only bindings to gensyms
;; G-Bang dereferences through these gensyms inside the macro.
;; We guard the argument for macro hygene in this way, but not the type.
;; Dereferencing a type ought to be side-effect free

(defmacro! algebraic-guard  (o!arg argtype)
  "Macro type guard -  same as typep, except that it checks for algebraic type also
   and sets the abacus-match local variables abacus-it and abacus-typespec to 
   reflect the last guarded instance and type; expects type argument un-quoted unlike typep"

  (setf abacus-typespec argtype)

  ;; Uncomment to obtain compiler notes
  (format t "~%; compiling (ALGEBRAIC-GUARD over type ~A...)"  abacus-typespec)
   
  `(progn
    (setf abacus-it ,g!arg)
    (and (typep ,g!arg ',argtype )
        (adt:algebraic-data-type-p ',argtype))
  )
)
