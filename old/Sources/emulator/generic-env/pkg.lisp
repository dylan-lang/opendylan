;; Generic env package...

(defpackage "GENERIC-ENV"

  (:use "COMMON-LISP")

  (:export 

    "GENERIC-LOAD"
    "GENERIC-COMPILE"
    "GENERIC-COMPILE-IF-NEEDED"
    "REGISTER-FILE-TYPE"

    "LANGUAGE-LISTENER"
    "LANGUAGE-LISTEN"

  )

)

;; eof
