# Abacus 


## Overview

Abacus is a set of macros that unify Common Lisp syntax for deep destructuring pattern matching from Optima and CL-ALGEBRAIC-DATA-TYPES that is modelled on universal quantification. Abacus also provides an extended syntax for pattern matching of both Optima patterns and CL-ALGEBRAIC-DATA-TYPES using a syntax that leans on Haskell and OCaml. This syntax uses the OR symbol | to introduce match expressions and the arrow symbol => to introduce match expressions.  

A detailed elaboration may be found at <word-press-site>

## Example Use of S-Expression Based Syntax:

Somewhere defining an algebraic data type...

(adt:defdata (batter-ingredient)
  (milk float)
  (flour float)
  (eggs integer))

Then somewhere else we dispatch on a variable called message.
The naming message is arbitrary and could be any variable name.

  (amatch  message

    ...
  
    (all when (algebraic-guard all batter-ingredient)
      (algebraic-match-with  
       ((milk ml) ; constructor pattern
        (format t "~%Adding ~a milliliters of milk" ml))
       ((flour gr) ; constructor pattern
        (format t "~%Adding ~a grams of flour" gr))
       ((eggs numeggs) ; constructor pattern
        (format t "~%Adding ~a egg(s)" numeggs))))     

    ... )
        
## Example Use of Extended Syntax:

Somewhere defining an algebraic data type...

(adt:defdata (batter-ingredient)
  (milk float)
  (flour float)
  (eggs integer))

Enable extended match syntax programmatically

(use-extended-match-syntax)
  
Then somewhere else we dispatch on a variable called message.
The naming message is arbitrary and could be any variable name.


  (amatch  message


    ; Algebraic Type
    (all when (algebraic-guard all batter-ingredient) 
        (algebraic-match-with
         | (milk ml)  =>
           (format t "~%Adding ~a milliliters of milk" ml)]
         | (flour gr)  =>
           (format t "~%Adding ~a grams of flour" gr)]
         | (eggs numeggs) => 
           (format t "~%Adding ~a egg(s)" numeggs) ] ) )

    ; Deep destructuring match on a conventional data structure
    ; ...  here a list based pair of symbols, one constant, one variable
    | (list :add myvariable)  =>

      ... do something with "myvariable"
      
      ]    
           
    ... further match clauses

    )

    
Optionally revert to conventional syntax

(disable-extended-match-syntax)
    

