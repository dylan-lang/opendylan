;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/pkg.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "SYS")


(defpackage "PARSERGEN"
  (:nicknames)
  (:size 294)
  (:external-size 19)
  (:use "COMMON-LISP" "LISPWORKS")
  (:export "*INDENT*" "*LEXER*" "*SYMBOL-TO-STRING*"
           "*TOKEN-POS-FUNCTION*" "CURRENT-SYMBOL"
           "CURRENT-SYMBOL-VALUE" "DEFPARSER" "DISCARD-INPUT"
           "INDENT-IN" "INDENT-OUT" "INIT-INDENT" "OUTPUT-ERROR"
           "OUTPUT-MESSAGE" "OUTPUT-WARNING" "PEEK-NEXT-LEXEME"
           "PUSH-AND-RESTART" "PUSH-TOKEN" "READ-NEXT-LEXEME"
           "RUN-PARSER"))
