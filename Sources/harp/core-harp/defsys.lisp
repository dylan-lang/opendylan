(in-package user)


(defsystem dylan-harp-module (:default-type :long-dylan-file)
  :members ("harp-library"
	    "harp-internal-module"
            "harp-module"
            )
  :rules
  ((:in-order-to :compile :all (:requires (:load :serial)))))

(defsystem dylan-harp-utils (:default-type :long-dylan-file)
  :members ("logcount"
            "bitset"
            )
  :rules
  ((:in-order-to :compile :all (:requires (:load :serial)))))

(defsystem dylan-harp-macros (:default-type :long-dylan-file)
  :members ("macro-support"
            "template-macros"
            "harp-macros"
            "harp-definitions"
            "instruction-macros"
            "bb-macros"
            "bitset-macros"
            )
  :rules
  ((:in-order-to :compile :all (:requires (:load :serial)))))

(defsystem dylan-base-harp (:default-type :long-dylan-file)
  :members ("harp-support"
            "harp-constants"
            "vreg-state"
            "harp-vars"
            "virtual-register"
            "register-model"
            "instruction-support"
            "harp-back-end"
            "harp-invoke"
            "register-support"
            "tag"
            "op"
            "basic-block"
            "sdi"
            "constant-ref"
            "harp-predicates"
            "harp-spread"
            "harp-outputters"
            "core-instructions"
            "instruction-set"
            "instruction-specials"
            "bb" 
            "spill"
            "real-register"
            "indep-utils"
	    "consistency"
            "function-offsets"

            )
  :rules
  ((:in-order-to :compile :all (:requires (:load :serial)))))



(defsystem dylan-harp-harp (:default-type :long-dylan-file)
  :members
  (
   "span-dependent"
   "asm-allocate"
   "asm-colour-graph" 
   "asm-code-select"
   "asm-linearise"
   "post-cg-lambda"
   "asm-top-level"
   "leaf-case"
   )

  :rules
  ((:in-order-to :load :all (:requires (:load :serial)))
   (:in-order-to :compile :all (:requires (:load :serial)))))



(defsystem dylan-complex-harp (:default-type :long-dylan-file)
  :members
  (
   "complex-instruction-set"
   )
  :rules
  ((:in-order-to :load :all (:requires (:load :serial)))
   (:in-order-to :compile :all (:requires (:load :serial)))))

(defsystem dylan-harp-print (:default-type :long-dylan-file)
  :members
  (
   "harp-print"
   )

  :rules
  ((:in-order-to :load :all (:requires (:load :serial)))
   (:in-order-to :compile :all (:requires (:load :serial)))))



(defsystem dylan-harp (:default-type :system)
  :members (
            "dylan-harp-module"
            "dylan-harp-macros"
            "dylan-harp-utils"
            "dylan-base-harp"
            "dylan-harp-harp"
            "dylan-harp-print"
            )
  :rules
  ((:in-order-to :load :all (:requires (:load :serial)))
   (:in-order-to :compile :all (:requires (:load :serial)))))

(defsystem dylan-harp-all (:default-type :system)
  :members (
            "dylan-harp"
            "dylan-complex-harp"
            )
  :rules
  ((:in-order-to :load :all (:requires (:load :serial)))
   (:in-order-to :compile :all (:requires (:load :serial)))))



;; eof
