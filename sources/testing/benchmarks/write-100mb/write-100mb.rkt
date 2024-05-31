#lang racket/base

(require racket/port)

(define (write-10000mb)
  ;; Use #"xxx" to write a byte string instead. MUCH faster.
  (let ((string "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
    (let outer ((o 100))
      (when (> o 0)
        (call-with-output-file
          #:exists 'truncate
          "/tmp/100mb.racket.txt"
          (lambda (port)
            (let loop ((n (* 1024 1024)))
              (when (> n 0)
                (display string port)
                (loop (- n 1))))))
        (outer (- o 1))))))

(write-10000mb)
