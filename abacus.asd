(defsystem "abacus"
   :description "Coherent syntax to merge OPTIMA and CL-ALGEABRAIC-DATA-TYPES"
   :version "0.1"
   :author "Chris Kohlhepp"
   :licence "All rights reserved"
   :depends-on ("optima" "cl-algebraic-data-type" "let-over-lambda") 
   :components ((:file "abacus"))
)
