(library (sph json)
  (export
    scm->json
    scm->json-string)
  (import
    (guile)
    (rnrs base)
    (rnrs hashtables)
    (sph)
    (sph vector)
    (only (sph hashtable) hashtable-each))

  (define (list->json arg port) (display "[" port)
    (if (not (null? arg))
      (let (rest (tail arg)) (scm->json (first arg) port)
        (if (not (null? rest)) (each (l (ele) (display "," port) (scm->json ele port)) rest))))
    (display "]" port))

  (define (vector->json arg port) (display "[" port)
    (let (arg-length (vector-length arg))
      (if (> arg-length 0)
        (begin (scm->json (vector-ref arg 0) port)
          (let loop ((index 1))
            (if (< index arg-length)
              (begin (display "," port) (scm->json (vector-ref arg index) port) (loop (+ 1 index))))))))
    (display "]" port))

  (define (number->json arg port)
    (display
      (number->string (if (and (rational? arg) (not (integer? arg))) (exact->inexact arg) arg)) port))

  (define (pair->json arg port) (display "[" port)
    (scm->json (first arg) port) (display "," port) (scm->json (tail arg) port) (display "]" port))

  (define-syntax-rule (string->json arg port) (write arg port))

  (define (hashtable->json arg port) (display "{" port)
    (hashtable-each
      (l (key value) (object-key->json key port)
        (display ":" port) (scm->json value port) (display "," port))
      arg)
    (if (> (hashtable-size arg) 1) (unread-char port)) (display "}" port))

  (define-syntax-rule (object-key->json arg port)
    (if (string? arg) (string->json arg port) (string->json (symbol->string arg) port)))

  (define-syntax-rule (boolean->json arg port) (display (if arg "true" "false") port))

  (define* (scm->json arg #:optional (port (current-output-port)))
    (cond ((list? arg) (list->json arg port)) ((vector? arg) (vector->json arg port))
      ((string? arg) (string->json arg port))
      ((symbol? arg) (string->json (symbol->string arg) port))
      ((number? arg) (number->json arg port)) ((hashtable? arg) (hashtable->json arg port))
      ((pair? arg) (pair->json arg port)) ((boolean? arg) (boolean->json arg port))
      (else (throw (q json-invalid)))))

  (define (scm->json-string arg) (call-with-output-string (l (port) (scm->json arg port)))))