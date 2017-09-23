(library (sph lang config)
  (export
    config-write
    config-read
    config-read-file
    config-read-string)
  (import
    (rnrs eval)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph io)
    (only (guile) call-with-input-string call-with-input-file)
    (only (sph tree) tree-map-lists-self))

  (define sph-lang-config-description
    "a scheme syntax configuration file format for associative data structures.
     * all elements are scheme expressions
     * all elements are read as being quasiquoted
     * unquote can be used to create dynamically calculated results
     * hashtables (eventually nested) are the parsing result
     the file content or string is parsed with scheme read elements of a quasiquoted list.
     in that list and sub-lists, expressions stand for key and value alternatingly.
     lists and sub-lists become hashtables, with the following exception:
     sub-lists that begin with a double period are returned as lists with the double period removed
     # example file content
       default-type \"text\"
       mode development
       start-id #xa
       preview-image-size (unquote (+ 255 1))
       browse
       ( page-size 75
         include-types (.. \"itml\" \"plaintext\"))
       other #(1 2 4)")

  (define config-env (environment (q (sph))))

  (define* (config-read port #:optional (eval-environment config-env))
    (tree-map-lists-self
      (l (a)
        (if (null? a) (ht-make-eq 0)
          (if (eq? (q ..) (first a)) (tail a) (ht-from-list a eq? ht-hash-symbol))))
      (eval (list (q quasiquote) (port->datums port)) eval-environment)))

  (define (config-write a port) "experimental" (each (l (a) (write a port) (newline)) (ht-alist a (inf))))
  (define (config-read-string a . b) (call-with-input-string a (l (a) (apply config-read a b))))
  (define (config-read-file a . b) (call-with-input-file a (l (a) (apply config-read a b)))))
