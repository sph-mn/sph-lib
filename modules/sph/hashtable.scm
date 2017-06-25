; written for the guile scheme interpreter
; Copyright (C) 2010-2017 sph <sph@posteo.eu>
; This program is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sph hashtable)
  (export
    ht-alist
    ht-bind
    ht-clear!
    ht-contains?
    ht-copy
    ht-copy*
    ht-copy-deep
    ht-copy-deep*
    ht-copy-empty
    ht-create
    ht-create-eq
    ht-create-eqv
    ht-create-string
    ht-create-symbol
    ht-delete!
    ht-each
    ht-each-key
    ht-entries
    ht-equivalence-function
    ht-fold
    ht-from-alist
    ht-from-list
    ht-hash-equal
    ht-hash-function
    ht-hash-string
    ht-hash-symbol
    ht-invert!
    ht-keys
    ht-make
    ht-make-eq
    ht-make-eqv
    ht-map!
    ht-merge!
    ht-ref
    ht-ref-q
    ht-select
    ht-set!
    ht-set-multiple!
    ht-set-multiple-q!
    ht-size
    ht-tree-merge!
    ht-tree-ref
    ht-tree-ref-q
    ht-tree-set!
    ht-update-multiple!
    ht-values
    ht?
    sph-hashtable-description)
  (import
    (sph)
    (sph string)
    (only (sph list) map-slice)
    (only (sph vector) vector-each vector-each-with-index)
    (rename (rnrs hashtables)
      (hashtable-ref rnrs-ht-ref)
      (hashtable-set! ht-set!)
      (hashtable-equivalence-function ht-equivalence-function)
      (hashtable-hash-function ht-hash-function)
      (hashtable-size ht-size)
      (hashtable-entries ht-entries)
      (hashtable-keys ht-keys)
      (hashtable-delete! ht-delete!)
      (hashtable-clear! ht-clear!)
      (string-hash ht-hash-string)
      (symbol-hash ht-hash-symbol)
      (equal-hash ht-hash-equal)
      (hashtable-contains? ht-contains?)
      (hashtable? ht?)
      (hashtable-copy ht-copy)
      (make-hashtable ht-make)
      (make-eq-hashtable ht-make-eq)
      (make-eqv-hashtable ht-make-eqv)))

  (define sph-hashtable-description "rnrs-hashtable processing")

  (define-syntax-rules ht-ref
    ; ht-ref with an optional default argument
    ; h:hashtable k:key d:default-if-not-found
    ((h k d) (rnrs-ht-ref h k d)) ((h k) (rnrs-ht-ref h k #f)))

  (define-syntax-rules ht-ref-q ((h k d) (ht-ref h (quote k) d)) ((h k) (ht-ref h (quote k) #f)))

  (define-syntax-rules ht-tree-ref ((h k) (ht-ref h k #f))
    ((h k ... k-last) (ht-ref (ht-ref h k ...) k-last #f)))

  (define-syntax-rule (ht-tree-ref-q a key ...) (ht-tree-ref a (quote key) ...))

  (define-syntax-rules ht-tree-set! ((h k v) (ht-set! h k v))
    ((h k ... k-last v) (ht-set! (ht-ref h k ...) k-last v)))

  (define-syntax-rules ht-create-symbol
    ;like hashtable, but create a hashtable optimised for symbol keys
    (() (ht-make ht-hash-symbol eqv?))
    ((associations ...) (ht-from-list (quote-odd associations ...) eqv? ht-hash-symbol)))

  (define-syntax-rules ht-create-string
    ;like hashtable, but create a hashtable designated and optimised for string keys
    (() (ht-make ht-hash-string string-equal?))
    ((associations ...) (ht-from-list (quote-odd associations ...) string-equal? ht-hash-string)))

  (define-syntax-rules ht-create-eq
    ; like hashtable, but create a hashtable that uses eq? as a comparison function
    (() (ht-make-eq))
    ((associations ...) (ht-from-list (quote-odd associations ...) eq? ht-hash-symbol)))

  (define-syntax-rules ht-create-eqv (() (ht-make-eqv))
    ;not the best hash function
    ((associations ...) (ht-from-list (quote-odd associations ...) eqv? ht-hash-equal)))

  (define-syntax-rule (ht-bind ht (key ...) body ...)
    ;selectively bind keys of hashtable to variables
    ((lambda (key ...) body ...) (ht-ref ht (quote key)) ...))

  (define-syntax-rule (ht-each-key proc ht) (vector-each proc (ht-keys ht)))

  (define-syntax-rule (ht-set-multiple-q! a key/value ...)
    ;hashtable [any:unquoted-key any:value] ...
    (apply ht-set-multiple! a (quote-odd key/value ...)))

  (define* (ht-from-alist a #:optional (equal-proc equal?) (hash-proc ht-hash-equal))
    "convert assoc-list/alist \"a\" to an r6rs hashtable"
    (let ((ht (ht-make hash-proc equal-proc)))
      (each (l (alist-part) (ht-set! ht (first alist-part) (tail alist-part))) a) ht))

  (define (ht-create . associations)
    "{key value} ... -> hashtable
     creates a hashtable.
     example: (hashtable 'a 1 'b 2 'c 3)"
    (ht-from-list associations))

  (define (ht-each proc ht)
    "procedure:{key value ->} hashtable ->
     call proc for each key and value association in hashtable"
    (let-values (((keys values) (ht-entries ht)))
      (vector-each-with-index (l (e index) (proc e (vector-ref values index))) keys)))

  (define (ht-values a) "hashtable -> vector"
    (call-with-values (nullary (ht-entries a)) (l (keys values) values)))

  (define (ht-fold proc init a) "procedure:{key value state -> state} any hashtable -> list"
    (let-values (((keys values) (ht-entries a)))
      (fold proc init (vector->list keys) (vector->list values))))

  (define (ht-map! proc a) (ht-each (l (k v) (ht-set! a k (proc k v))) a))

  (define (ht-invert! a)
    "hashtable -> hashtable
     use values as keys and keys as values"
    (ht-each (l (k v) (ht-set! a v k)) a))

  (define (ht-merge! a b)
    "hashtable hashtable -> unspecified
     copy the values of hash b to hash a. existing key values are overwritten"
    (let-values (((keys values) (ht-entries b)))
      (vector-each-with-index (l (key index) (ht-set! a key (vector-ref values index))) keys)))

  (define (ht-tree-merge! a b)
    "hashtable hashtable -> unspecified
     like ht-merge!, but for keys that occur in both hashtables and which have hashtables as value
     ht-tree-merge! is called to merge the hashtables instead of just overwriting the value in a."
    (call-with-values (nullary (ht-entries b))
      (l (keys values)
        (vector-each-with-index
          (l (key index)
            (let ((a-value (ht-ref a key)) (b-value (vector-ref values index)))
              (ht-set! a key
                (if (and (ht? a-value) (ht? b-value))
                  (begin (ht-tree-merge! a-value b-value) a-value) b-value))))
          keys))))

  (define (ht-set-multiple! ht . assoc)
    "hashtable key/value ...
     return a new hashtable with multiple values having been updated"
    (map-slice 2 (l (key value) (ht-set! ht key value)) assoc))

  (define (ht-update-multiple! ht keys proc)
    "hashtable list procedure:{any:values ... -> (any:new-values ...)} -> hashtable
     set values for \"keys\" in hashtable to new values by mapping using \"proc\""
    (each (l (k v) (ht-set! ht k v)) keys (apply proc (map (l (a) (ht-ref ht a)) keys))))

  (define* (ht-alist ht #:optional (depth 0))
    "rnrs-hashtable [integer] -> list
     converts a hashtable to an alist. if depth is greater than 0 any other
     hashtables being values up to this nesting depth will be converted too"
    (ht-fold
      (l (key value r)
        (pair (pair key (if (and (> depth 0) (ht? value)) (ht-alist value (- depth 1)) value)) r))
      (list) ht))

  (define* (ht-from-list a #:optional (equal-proc equal?) (hash-proc ht-hash-equal))
    "convert a list to an r6rs standard library hashtable. nested lists are not converted to a hash.
     example
     (ht-ref (ht-from-list (list 'a 1 'b 2)) 'b #f)
     -> 2"
    (let ((ht (ht-make hash-proc equal-proc)))
      (fold (l (e r) (if r (begin (ht-set! ht r e) #f) e)) #f a) ht))

  (define (ht-select a . keys) (map (l (b) (ht-ref a b)) keys))

  (define (ht-copy-empty a)
    "hashtable -> hashtable
     creates a new empty hashtable with the same equivalence and hash function as the input hashtable."
    (ht-make (ht-hash-function a) (ht-equivalence-function a)))

  (define (ht-copy* a proc) "call proc with a copy of hashtable and return it"
    (let (r (ht-copy a #t)) (proc r) r))

  (define (ht-copy-deep a)
    (ht-fold (l (k v r) (ht-set! r k (if (ht? v) (ht-copy-deep v) v)) r) (ht-copy-empty a) a))

  (define (ht-copy-deep* a proc) (let (r (ht-copy-deep a)) (proc r) r)))
