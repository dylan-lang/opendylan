(in-package "SYSTEM")

(defsystem source-change-tool ()
  :members (
            ("pkg" :source-only t)
            "source-change-recording"
            "hope"
            "editor"
            )
  :rules ((:in-order-to :compile :all (:requires (:load :previous)))))
