;a json writer. converts scheme-data to json. particularly fast

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

  (define (list->json a port) "list port ->"
    (display "[" port)
    (if (not (null? a))
      (let (rest (tail a)) (scm->json (first a) port)
        (if (not (null? rest)) (each (l (e) (display "," port) (scm->json e port)) rest))))
    (display "]" port))

  (define (vector->json a port) "vector port ->"
    (display "[" port)
    (let (a-length (vector-length a))
      (if (> a-length 0)
        (begin (scm->json (vector-ref a 0) port)
          (let loop ((index 1))
            (if (< index a-length)
              (begin (display "," port) (scm->json (vector-ref a index) port) (loop (+ 1 index))))))))
    (display "]" port))

  (define (number->json a port) "number port ->"
    (display (number->string (if (and (rational? a) (not (integer? a))) (exact->inexact a) a)) port))

  (define (pair->json a port) (display "[" port)
    (scm->json (first a) port) (display "," port) (scm->json (tail a) port) (display "]" port))

  (define-syntax-rule (string->json a port) (write a port))

  (define (hashtable->json a port) "rnrs-hashtable port ->"
    (display "{" port)
    (hashtable-each
      (l (key value) (object-key->json key port)
        (display ":" port) (scm->json value port) (display "," port))
      a)
    (if (> (hashtable-size a) 1) (unread-char port)) (display "}" port))

  (define-syntax-rule (object-key->json a port)
    ;"string/symbol port ->"
    (if (string? a) (string->json a port) (string->json (symbol->string a) port)))

  (define-syntax-rule (boolean->json a port)
    ;"boolean port ->"
    (display (if a "true" "false") port))

  (define* (scm->json a #:optional (port (current-output-port))) "any ->"
    (cond ((list? a) (list->json a port)) ((vector? a) (vector->json a port))
      ((string? a) (string->json a port)) ((symbol? a) (string->json (symbol->string a) port))
      ((number? a) (number->json a port)) ((hashtable? a) (hashtable->json a port))
      ((pair? a) (pair->json a port)) ((boolean? a) (boolean->json a port))
      (else (throw (q json-invalid)))))

  (define (scm->json-string a) "any -> string"
    (call-with-output-string (l (port) (scm->json a port)))))
