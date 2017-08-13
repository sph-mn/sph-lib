(library (sph lang config)
  (export
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
    (only (sph tree) tree-map-lists-and-self))

  (define sph-lang-config-description
    "a scheme syntax configuration file format for associative data structures.
     * all elements are scheme expressions
     * all elements are read as being quasiquoted
     * unquote can be used to create dynamically calculated results
     * hashtables (eventually nested) are the parsing result
     the file content or string is parsed with scheme read as a quasiquoted list.
     in that list and sub-lists, expressions stand for key and value alternatingly.
     lists and sub-lists become hashtables, with the following exception:
     sub-lists that begin with a period are returned as lists with the period removed
     # example file content
       default-type \"text\"
       mode development
       start-id #xa
       preview-image-size (unquote (+ 255 1))
       browse
       ( page-size 75
         include-types (. \"itml\" \"plaintext\"))
       other #(1 2 4)")

  (define config-env (environment (q (sph))))

  (define (config-read port)
    (tree-map-lists-and-self
      (l (a)
        (if (null? a) (ht-make-eq 0)
          (if (eq? (q ..) (first a)) (tail a) (ht-from-list a eq? ht-hash-symbol))))
      (eval (list (q quasiquote) (port->datums port)) config-env)))

  (define (config-write port) "")
  (define (config-read-string a) (call-with-input-string a config-read))
  (define (config-read-file a) (call-with-input-file a config-read)))
