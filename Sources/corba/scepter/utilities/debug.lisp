(in-package "CL-USER")

(setq lw::*reuse-inspectors* t)

(defun graph-ast ()
  (let* ((tp (make-instance 'tk:top-level :width 500 :height 500 :title "IDL AST Grapher"))
         (sp (make-instance 'tk:scroll-pane :parent tp))
         (sf (make-instance 'tk:scroll-frame :parent sp))
         (gp (make-instance 'tk:graph-pane
                            :parent sf
                            :graph (list DYLAN+DYLAN/SCEPTER::*ROOT*)
                            :children-function 'DYLAN+DYLAN/SCEPTER::SCOPE-DECLARATORS-AS-LIST)))
    (clue:add-callback gp :select 'tools::w-dylan-inspect)
    gp))

