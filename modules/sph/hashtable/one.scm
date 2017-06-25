(library (sph hashtable one)
  (export
    ht-indent-tree-string
    ht-invert
    ht-key
    ht-key-proc
    ht-make-immutable
    ht-map
    ht-merge
    ht-set-multiple
    ht-set-multiple-q
    ht-tree-from-list
    ht-tree-merge
    sph-hashtable-one-description)
  (import
    (sph)
    (sph alist)
    (sph hashtable)
    (sph list)
    (sph string)
    (sph tree)
    (only (guile) compose string-join))

  (define sph-hashtable-one-description "more hashtable procedures")

  (define (ht-key-proc a)
    "hashtable -> procedure:{value -> key}
     returns a procedure for getting keys by value from hashtable"
    (let-values (((keys values) (ht-entries a)))
      (let (values-list (vector->list values))
        (l (a)
          "any:value -> any:key
          retrieves the key for a value in hashtable"
          (let (index (list-index-value values-list a)) (and index (vector-ref keys index)))))))

  (define (ht-key ht value)
    "any:value -> any:key
     get a key by value.
     this is a relatively costly operation. for repeated calls use ht-key-proc"
    ((ht-key-proc ht) value))

  (define* (ht-indent-tree-string ht #:optional (indent 0) (indent-string "  "))
    "hashtable [integer string] -> string
     format key and values of hashtable and nested hashtables as strings using write
     with indent corresponding to nesting
     example:
       key
         key value
       key value"
    (string-join
      (ht-fold
        (l (key value r)
          (pair
            (string-append (string-multiply indent-string indent) (any->string-write key)
              " "
              (if (ht? value)
                (let (r (ht-indent-tree-string value (+ indent 1)))
                  (if (> indent 0) (string-append r "\n") (string-append "\n" r)))
                (any->string-write value)))
            r))
        (list) ht)
      "\n"))

  (define (ht-make-immutable a)
    "hashtable -> hashtable
     results in a new hashtable that is an immutable version of the given one.
     this does not affect nested hashtables"
    (ht-copy a #f))

  (define (ht-tree-from-list a)
    "list -> hashtable
     converts a list and all sub-lists to individual hashtables.
     example:
     (key value key (key value key value)) -> hashtable"
    (tree-map-lists-and-self (compose ht-from-alist list->alist) a))

  ;-- non-mutating variants

  (define (ht-map proc a)
    "procedure:{key value -> value} hashtable -> hashtable
     copy hashtable shallowly with a mapped value for every key value association"
    (ht-copy* a (l (a) (ht-map! proc a))))

  (define-syntax-rule (ht-set-multiple-q a key/value ...)
    ; hashtable [any:unquoted-key any:value] ...
    (apply ht-set-multiple a (quote-odd key/value ...)))

  (define (ht-invert a)
    "hashtable -> hashtable
     use values as keys and keys as values, like a reverse dictionary"
    (let (r (ht-copy-empty a)) (ht-each (l (k v) (ht-set! r v k)) a) r))

  (define (ht-set-multiple a . assoc)
    "hashtable key/value ...
     return a new hashtable with multiple values updated values"
    (ht-copy* a (l (a) (apply ht-set-multiple! a assoc))))

  (define (ht-merge a b)
    "hashtable hashtable -> hashtable
     return a new hashtable that contains the keys and values of all hashtables,
     overwriting the keys from the hashtables specified on the left with the ones further right"
    (ht-copy* a (l (a) (ht-merge! a b))))

  (define (ht-tree-merge a b)
    "hashtable hashtable -> hashtable
     like ht-tree-merge! but none of the given hashtables is modified"
    (ht-copy* a (l (a) (ht-tree-merge! a b)))))
