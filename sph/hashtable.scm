#;((sph hashtable) - rnrs-hashtable processing
  written for the guile scheme interpreter
  Copyright (C) 2010-2014 sph <tantalum@online.de> (current maintainer)

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, see <http://www.gnu.org/licenses/>.
  -----------------------)

(library (sph hashtable)
  (export
    alist->hashtable
    eq-hashtable
    eqv-hashtable
    hashtable
    hashtable->alist
    hashtable->indent-tree-string
    symbol-hashtable-bind
    hashtable-copy-empty
    hashtable-each
    hashtable-each-key
    hashtable-fold
    hashtable-invert
    hashtable-invert!
    hashtable-key
    hashtable-key-proc
    hashtable-make-immutable
    hashtable-merge
    hashtable-merge!
    hashtable-ref
    hashtable-set-multiple
    hashtable-tree-merge
    hashtable-tree-merge!
    hashtable-update-multiple!
    hashtable-values
    hashtables-ref
    hashtables-set!
    list->hashtable
    rnrs-hashtable-ref
    string-hashtable
    symbol-hashtable)
  (import
    (rnrs base)
    (sph)
    (only (guile) string-join)
    (only (rnrs hashtables) make-hashtable)
    (only (sph list) map-slice list-index-value)
    (only (sph one) quote-odd)
    (only (sph string)
      any->string-write
      string-multiply
      string-equal?)
    (only (sph vector) vector-each vector-each-with-index)
    (rename (rnrs hashtables) (hashtable-ref rnrs-hashtable-ref)))

  (define* (alist->hashtable lis #:optional (equal-proc equal?) (hash-proc equal-hash))
    "convert assoc-list\\alist 'lis to an r6rs standard library hashtable"
    (let ((ht (make-hashtable hash-proc equal-proc)))
      (each (l (alist-part) (hashtable-set! ht (first alist-part) (tail alist-part))) lis) ht))

  (define (hashtable . associations)
    "{key value} ... -> hashtable
    creates a hashtable.
    example (hashtable 'a 1 'b 2 'c 3)"
    (list->hashtable associations))

  (define-syntax-rules symbol-hashtable (() (make-hashtable symbol-hash eqv?))
    ((associations ...) (list->hashtable (quote-odd associations ...) eqv? symbol-hash)))

  (define-syntax-rules string-hashtable (() (make-hashtable string-hash string-equal?))
    ((associations ...) (list->hashtable (quote-odd associations ...) string-equal? string-hash)))

  (define-syntax-rules eq-hashtable (() (make-eq-hashtable))
    ((associations ...) (list->hashtable (quote-odd associations ...) eq? hashq)))

  (define-syntax-rules eqv-hashtable (() (make-eqv-hashtable))
    ((associations ...) (list->hashtable (quote-odd associations ...) eqv? hashv)))

  (define-syntax-rule (symbol-hashtable-bind ht (key ...) body ...)
    ;selectively bind keys of alist to variables
    ((lambda (key ...) body ...) (hashtable-ref ht (quote key)) ...))

  (define (hashtable-copy-empty arg)
    "hashtable -> hashtable
    creates a new empty hashtable with the same equivalence and hash function as the input hashtable."
    (make-hashtable (hashtable-equivalence-function arg) (hashtable-hash-function arg)))

  (define (hashtable-each proc ht)
    "procedure hashtable ->
    {key value ->}
    call proc for each key and value association in hashtable"
    (let-values (((keys values) (hashtable-entries ht)))
      (vector-each-with-index (l (ele index) (proc ele (vector-ref values index))) keys)))

  (define (hashtable-values arg) "hashtable -> vector"
    (call-with-values (l () (hashtable-entries arg)) (l (keys values) values)))

  (define-syntax-rule (hashtable-each-key proc ht) (vector-each proc (hashtable-keys ht)))

  (define (hashtable-fold proc init arg)
    "procedure:{key value state -> state} any hashtable -> list"
    (let-values (((keys values) (hashtable-entries arg)))
      (fold proc init (vector->list keys) (vector->list values))))

  (define (hashtable-make-immutable a)
    "hashtable -> hashtable
    results in a new hashtable that is an immutable version of the given one.
    this does not affect nested hashtables"
    (hashtable-copy a #f))

  (define (hashtable-invert arg)
    "hashtable -> hashtable
    use values as keys and keys as values"
    (let ((res (hashtable-copy-empty arg))) (hashtable-each (l (k v) (hashtable-set! res v k)) arg)))

  (define (hashtable-invert! arg)
    "hashtable -> hashtable
    use values as keys and keys as values"
    (hashtable-each (l (k v) (hashtable-set! arg v k)) arg))

  (define (hashtable-key-proc ht)
    "hashtable -> procedure:{value -> key}
    results in a procedure that caches the preparations for creating its result
    so it is much more efficient for repeated calls."
    (call-with-values (l () (hashtable-entries ht))
      (l (keys values)
        (let (values-list (vector->list values))
          (l (arg)
            "any:value -> any:key
            retrieves the key of a value in hashtable"
            (let (index (list-index-value values-list arg))
              (if index (vector-ref keys index) index)))))))

  (define (hashtable-key ht value) "any:value -> any:key" ((hashtable-key-proc ht) value))

  (define (hashtable-merge! a b)
    "hashtable hashtable -> unspecified
    copy the values of hash b to hash a. key values are overwritten."
    (call-with-values (l () (hashtable-entries b))
      (l (keys values)
        (vector-each-with-index (l (key index) (hashtable-set! a key (vector-ref values index)))
          keys))))

  (define (hashtable-merge a b)
    "hashtable hashtable -> hashtable
    like hashtable-merge! but not side-effecting. it instead works on a copy of hash a and results in it"
    (let (res (hashtable-copy a #t)) (hashtable-merge! res b) res))

  (define (hashtable-tree-merge! a b)
    "hashtable hashtable -> unspecified
    like hashtable-merge!, but for keys that occur in both hashtables and which have hashtables as value
    hashtable-tree-merge! is called to merge the hashtables instead of just overwriting the value in a."
    (call-with-values (l () (hashtable-entries b))
      (l (keys values)
        (vector-each-with-index
          (l (key index)
            (let ((a-value (hashtable-ref a key)) (b-value (vector-ref values index)))
              (hashtable-set! a key
                (if (and (hashtable? a-value) (hashtable? b-value))
                  (begin (hashtable-tree-merge! a-value b-value) a-value) b-value))))
          keys))))

  (define (hashtable-tree-merge a b)
    "hashtable hashtable -> hashtable
    like hashtable-tree-merge! but not side-effecting. it instead works on a copy of hashtable and results in it"
    (let (res (hashtable-copy a #t)) (hashtable-tree-merge! res b) res))

  (define (hashtable-set-multiple ht . assoc)
    (let (res (hashtable-copy ht #t))
      (map-slice 2 (l (key value) (hashtable-set! res key value)) assoc) res))

  (define (hashtable-update-multiple! ht keys proc)
    "hashtable list procedure:{any:values ... -> (any:new-values ...)} -> hashtable
    set values for \"keys\" in hashtable to new values by mapping using \"proc\""
    (each (l (k v) (hashtable-set! ht k v)) keys
      (apply proc (map (l (e) (hashtable-ref ht e)) keys))))

  (define-syntax-rules hashtable-ref
    ;h:hashtable k:key d:default-if-not-found
    ((h k d) (rnrs-hashtable-ref h k d)) ((h k) (rnrs-hashtable-ref h k #f)))

  (define-syntax-rules hashtables-ref ((h k) (hashtable-ref h k #f))
    ((h k ... k-last) (hashtable-ref (hashtable-ref h k ...) k-last #f)))

  (define-syntax-rules hashtables-set! ((h k v) (hashtable-set! h k v))
    ((h k ... k-last v) (hashtable-set! (hashtable-ref h k ...) k-last v)))

  (define* (hashtable->alist ht #:optional (depth 0))
    "rnrs-hashtable [integer] -> alist
    converts a hashtable to an alist. if depth is greater than 0 any other
    hashtables being values up to this nesting depth will be converted too."
    (hashtable-fold
      (l (key value prev)
        (cons
          (cons key
            (if (and (> depth 0) (hashtable? value)) (hashtable->alist value (- depth 1)) value))
          prev))
      (list) ht))

  (define* (hashtable->indent-tree-string ht #:optional (indent 0) (indent-string "  "))
    (string-join
      (hashtable-fold
        (l (key value prev)
          (cons
            (string-append (string-multiply indent-string indent) (any->string-write key)
              " "
              (if (hashtable? value)
                ( (l (res) (if (> indent 0) (string-append res "\n") (string-append "\n" res)))
                  (hashtable->indent-tree-string value (+ indent 1)))
                (any->string-write value)))
            prev))
        (list) ht)
      "\n"))

  (define* (list->hashtable lis #:optional (equal-proc equal?) (hash-proc equal-hash))
    "convert a list to an r6rs standard library hashtable. nested lists are not converted to a hash.
    example
    (hashtable-ref (list->hashtable (list 'a 1 'b 2)) 'b #f)
    -> 2"
    (let ((ht (make-hashtable hash-proc equal-proc)))
      (fold (l (ele prev) (if prev (begin (hashtable-set! ht prev ele) #f) ele)) #f lis) ht)))