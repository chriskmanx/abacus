

;build & import via (asdf:load-system :bakery)
(defpackage :bakery
  ; import namespaces from the following packages
  (:use :common-lisp :cl-actors :optima :bordeaux-threads :cells :abacus)
 
  ; bakery package exported symbols
  (:export #:baker 
           :cake
           :batter-ingredient
           :milk
           :flour
           :eggs))


(in-package :bakery)


; (adt:algebraic-type-p 'batter-ingredient)
                                                                                
                                        
;(defun get-constructor-symbols (adt)
;  (mapcar #'car (adt:get-constructors adt)))

; (eq 'milk  (first (get-constructor-symbols 'batter-ingredient)))

; (get-constructor-symbols 'batter-ingredient)

(adt:defdata (batter-ingredient)
  (milk float)
  (flour float)
  (eggs integer))

(adt:defdata (icing-ingredient)
  (sugar keyword))

(adt:defdata (decoration-ingredient)
  (candles keyword))


(defun construct-accumulator () 
  "A closure constructor, lambda-over-let-over-lambda pattern"
  (let ((elements  (list)))
    (lambda (element)
      (setf elements (remove nil (adjoin element elements)))
      elements)))

(defvar *all-ingredients-fu* nil)
(defvar *all-dones-fu* nil)


(declaim (sb-ext:muffle-conditions style-warning))
(declaim (sb-ext:muffle-conditions sb-kernel:redefinition-with-defmethod))



;; The model defines nodes and how edges connect them to build up a computation graph.
;; Incidentally the model derives from the Common Lisp Object System (CLOS)
;; Hence Common Lisp object oriented semntics and syntax hold.

(defmodel cake ()
  (

    ; Closures
    ;---------
   (allingredientsfu :cell nil :accessor allingredientsfu :initform *all-ingredients-fu*)
   (alldonesfu :cell nil :accessor alldonesfu :initform *all-dones-fu*) 
   
    ; Constraints
    ;------------ 

    ; To have batter we need milk, eggs & flour
    (batter :reader batter :initform '(:milk :eggs :flour))

    ; To have an iced cake we must have sugar topping
    (icing :reader icing :initform '(:sugar))

    ; To have a birthday cake we must have candles
    (decoration :reader decoration :initform '(:candles))

    ; Basic actions that need to be performed
    (todos :reader todos :initform '(:knead :bake :decorate))
    
    ; Events Nodes
    ;-------------

    ; A new ingredient is mixed in
    (mixin  :initform (c-in nil))

    ; A new action is performed
    (action :accessor action :initform (c-in nil))

    ; Dependent Nodes & inir forms represent Edges
    ;---------------------------------------------

    ; At any time the total set of ingredients
    ; is the set union of the last mixin and all previous ingredients
    (ingredients     
     :accessor ingredients
     :initform (c? (funcall (allingredientsfu self)  (mixin self))))

    
    ; At any time the set of done actions or "dones""
    ; is the set union of the last action and all previus actions
    (dones
     :accessor dones
     :initform (c? (funcall (alldonesfu self)  (action self))))
    
    ; Batter predicate "batter-p": At any time batterp is satisfied
    ; if the batter constraint set is a subset of the ingredients.
    ; This model permits adding other ingredients, such as spices
    ; so the subset relationship is a good fit here.
    (batter-p
     :accessor batter-p
     :initform (c? (subsetp (batter self) (ingredients self))))
    
    ; All done predicate "alldone-p": At any time we are "all done"
    ; if the set difference of todos and the set of dones is an emty set
    ; This essentially says, follow the recipe. If you perform other tasks
    ; we don't warrant the outcome. The cake may be destroyed.
    (alldone-p
     :accessor alldone-p
     :initform (c? (not (set-difference (todos self) (dones self)))))
   
  )
)


;; Of course there is room for things to go wrong here
;; but this is the realistic model of a cake. It's passive.
;; Stick  on candles before baking, and you get a molten mess of wax.
;; However tempting it may be to model desired invariants at this
;; level, we should not. We will, however encode predicates as
;; dependent nodes. 


(declaim (sb-ext:unmuffle-conditions style-warning))
(declaim (sb-ext:unmuffle-conditions sb-kernel:redefinition-with-defmethod))


;; Observers are triggered when there is a change in an oberved cell
;; 
;; self has ref to model instance
;; old value is in old-value
;; new value is in new-value
;;
;; Note in particular that we don't have to track before and after states.
;; These are provided by the FRP framework each time we are triggered,
;; and represent the delta of change. This is particularly useful
;; in modelling rate of change problems. Note also how terse the model is.
;; If you ever had the displeasure of modelling the observer pattern in
;; Java or C++, you will appreciate this.

(defobserver batter-p ((self cake))
  "An observer on cell batter-p on instances of cake models"
  (if new-value ; new
        (format t "~%CAKE OBSERVER: Batter now complete" )))

(defobserver  alldone-p  ((self cake))
  "An observer on cell alldone-p  on instances of cake models"
  (if new-value ; new
        (format t "~%CAKE OBSERVER: Cake is all done" )))

(defobserver ingredients ((self cake))
  "An observer on cell ingredients on instances of cake models"
  (format t "~%CAKE OBSERVER: Ingredients now ~A" new-value ))

(defobserver dones  ((self cake))
  "An observer on cell dones on instances of cake models"
  (format t "~%CAKE OBSERVER: Completed tasks now ~A" new-value ))



(defvar *auxprint* nil)

;(trace "BAKERY")
;(untrace "BAKERY"))
;(use-package :bakery)
;(trace traci:traci-connect :break t)
; (trace "BAKERY" :break t)

(defun auxprint-on ()
  (setf *auxprint* t))

(defun auxprint-off ()
  (setf *auxprint* nil))

(defun auxprint (x)
  (if *auxprint* 
      (print x *standard-output*)           
      (format t "~%")))                      


;; An abstraction of RULES/INVARIANTS
;; Only state is cake itself

(use-extended-match-syntax)

(defactor baker 

  ; State Form - let bindings for actor local state
  ;------------------------------------------------ 
  ((*all-ingredients-fu* (construct-accumulator))
   (*all-dones-fu* (construct-accumulator))
   (mycake (make-instance 'cake)))

  ; Message Form - We match on a single argument
  ;---------------------------------------------
  (message)                          

  ; Behavior Form
  ;-------------
  (amatch  message
 
    (it when (algebraic-guard it batter-ingredient)
      (algebraic-match-with  
       ((milk ml) ; constructor pattern
        (format t "~%Adding ~a milliliters of milk" ml))
       ((flour gr) ; constructor pattern
        (format t "~%Adding ~a grams of flour" gr))
       ((eggs numeggs) ; constructor pattern
        (format t "~%Adding ~a egg(s)" numeggs))))     

    [ it when (algebraic-guard it batter-ingredient) =>
        (algebraic-match-with
         [ (milk ml) =>  
           (format t "~%Adding ~a milliliters of milk" ml) ]
         [ (flour gr) => 
           (format t "~%Adding ~a grams of flour" gr) ]
         [ (eggs numeggs) =>
           (format t "~%Adding ~a egg(s)" numeggs) ] ) ]
    
    ; match adding batter ingredients only
    [(list :add ingredient)  when 
     (member ingredient (batter mycake)) =>
     
     (if (batter-p mycake) ; batter already done ?  
        (format t "~%ERROR: Batter complete. Don't need ~A" ingredient )
        (if (member ingredient (ingredients mycake)) 
           (format t "~%Error: Already have ~A in batter" ingredient )
           (setf (mixin mycake) ingredient)))] ; update cake here

    ; match adding icing ingredients but only after baking
    [(list :add ingredient)  when
     (and (member ingredient (icing mycake))
          (member :bake (dones mycake))) =>
     
     (if (member ingredient (ingredients mycake))
        (format t "~%ERROR: Already have ~A on cake" ingredient) 
        (setf (mixin mycake) ingredient))] ; update cake here

    ; match adding decoration ingredients but only after baking
    ((list :add ingredient)  when
     (and (member ingredient (decoration mycake))
          (member :bake (dones mycake)))
     
     (if (member ingredient (ingredients mycake))
        (format t "~%ERROR: Already have ~A on cake" ingredient ) 
        (progn 
           (setf (mixin mycake) ingredient) ; update cake here
           (setf (action mycake) :decorate)))) ; update cake here

    ; match actions
    ((list :act todo)  when
     (member todo (todos mycake))
     
     (if (alldone-p  mycake) ; cake already finished?  
        (format t "~%ERROR: Cake is finished. Decline to do ~A" todo )
        (if (member todo (dones  mycake)) ; todo already done? 
           (format nil "~%ERROR: Already did  ~A" todo  )
            
           (cond ((equal todo :bake) ; bake only after kneading dough
                  (if (not (member :knead (dones mycake)))
                     (format t "~%ERROS: Knead batter first. Can't do ~A" todo  )
                     (setf (action mycake) todo) ; update cake here
                  )                     
                 )
                 ((equal todo :knead) ; knead dough only after batter complete
                  (if (not (batter-p mycake))
                      (format t "~%ERROR: Batter not ready. Can't knead dough" )
                      (setf (action mycake) todo) ; update cake here
                  )
                 )
                 (t (format t "~%ERROR: Don't know  ~A" todo ))))))
    
    ; fall-through             
    (_
     (format t  "~%ERROR: recipe error"))
    
  )

  ; Match next message
  ;-------------------
  next)

                                        
                                        ; Construct like so
                                        ; (defvar mrbean (baker)
                                        ; (send mrbean :connect)
