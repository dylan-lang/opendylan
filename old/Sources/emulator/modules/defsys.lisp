(in-package user)

;; Modules for use by the translator.

(defsystem dylan-modules (:default-type :dylan-file)
  :members ("../share/modules"

            "~dylan/translator/lib/app/extended-dylan/union-collection"

            "filter"
            "filtered-ekc"

            "namespace"
            "translator-module"

            "define-module"))

;; eof
