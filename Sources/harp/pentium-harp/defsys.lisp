(in-package user)


(defsystem dylan-pentium-harp (:default-type :long-dylan-file)
  :members
  (
   "pentium-module"
   "pentium-macros"
   "back-end"
   "registers"
   "instructions"
   "codefrag"
   "general"
   "grp-1"
   "moves"
   "shifts"
   "pushes"
   "allocate"
   ;;  "bignums" ;; ???
   "bits"
   "branches"
   "copyword"
   "jumps"
   "mul-div"
   "save-res"
   "overflow"
   "three-2"
   "pcspecif"
   "flt-general"
   "flt-ariths"
   "flt-branches"
   "flt-moves"
   "flt-transcendentals"
   "asm-assemble"
   )
  :rules
  ((:in-order-to :load :all (:requires (:load :serial)))
   (:in-order-to :compile :all (:requires (:load :serial)))))



(defsystem dylan-test-pentium-harp (:default-type :system)
  :members (
            dylan-pentium-harp
            ("dummy-test" :type :long-dylan-file)
            )
  :rules
  ((:in-order-to :load :all (:requires (:load :serial)))
   (:in-order-to :compile :all (:requires (:load :serial)))))


;; eof
