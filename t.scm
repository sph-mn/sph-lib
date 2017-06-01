(import (sph) (sph process) (sph read-write)
  ;(sph process create)
  )

(process-eval (q (display "hello")) (l (result)
    (display (port->string result))
    ))

#;(process-create "/usr/bin/echo" (list (string-append "/tmp/" (number->string (current-time) 32)))
  #f (current-output-port) (current-error-port))
