(in-package user)

(defsystem dylan-infix-macros 

  (:default-type     :dylan-file)

  :members (

            "fragment-macros" 
            "pattern-macros"

            "fragments"
            "patterns"

            "constraint-parsing"
            "constraints"

            "match"
            "templates"
            "macros"

            "rules"
            "statement-rules"
            "define-rules"
            "function-rules"
            "local-declaration-rules"

            "emulator-hacks"
            "load-forms"
            "fragment-printers"
            "tracing"

            "prefix-macros"
            "procedural-templates"
            "procedural-patterns"

            )

  :rules ((:in-order-to :compile :all
                        (:requires (:load :previous))))

)

;; eof

