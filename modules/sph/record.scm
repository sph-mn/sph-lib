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

(library (sph record)
  (export
    alist->record
    define-record
    define-record-accessors
    define-record-setters
    make-record
    make-record-layout
    record
    record->vector
    record-accessor
    record-accessors
    record-append
    record-field-names
    record-field-names-unordered
    record-layout->predicate
    record-layout-extend!
    record-layout-length
    record-layout-merge
    record-layout-merge!
    record-layout?
    record-length
    record-list-filter-value
    record-ref
    record-set
    record-setter
    record-setters
    record-take
    record-update
    record-update-p
    record?
    vector->record)
  (import
    (sph)
    (sph vector)
    (except (guile)
      record?
      record-accessor)
    (sph hashtable)
    (sph list)
    )

  (define sph-record-description
    "vectors as records
     the main goal this library tries to archieve is to offer a dictionary data-structure where field values can be accessed by field name, but where the access happens indexed as for vectors and not using a hash function for example.
     records use less memory and less access time.
     this library is supposed to be simpler in definition and usage than existing record libraries (rnrs, srfi) and more flexible by being based on the less restricted interobability with vectors (for records) and hashtables (for layouts).
     any vector can be accessed as a record and records can be accessed like vectors.
     if type information is desired then it has to be added manually by storing a type name in the first record field for example.
     usage:
     (define-record my-record a b c)
     (define-record my-other-record (a my-a-accessor-name my-a-setter-name) b (c my-c-accessor-n))
     (define x (record my-record 1 2))
     (my-record-a x) -> 1
     (my-record-c x) -> #f
     (my-record-c-set! x 3)")

  (define (any->symbol a)
    "any -> symbol/false
     converts strings, numbers and symbols to symbol, or false for everything else"
    (cond ((string? a) (string->symbol a)) ((number? a) (string->symbol (number->string a)))
      ((symbol? a) a) (else #f)))

  (define (record-update-p record-layout a . field-name/value)
    (let (a (vector-copy a))
      (let loop ((b field-name/value))
        (if (null? b) a
          (let (b-tail (tail b))
            (vector-set! a (ht-ref record-layout (first b) #f) (first b-tail))
            (loop (tail b-tail)))))))

  (define-syntax-rule (record-update record-layout a field-name/value ...)
    (apply record-update-p record-layout a (quote-odd field-name/value ...)))

  (define* (alist->record a record-layout)
    "alist record-layout -> record
     extract record data from alist using record-layout and result in one record.
     currently, string keys are also recognized"
    (vector-map (l (name) (or (assoc-ref a name) (assoc-ref a (symbol->string name))))
      (record-field-names record-layout)))

  (define-syntax-rule (define-record-accessors record-layout (identifier field-name) ...)
    (begin (define identifier (record-accessor record-layout field-name)) ...))

  (define-syntax-rule (define-record-setters record-layout (identifier field-name) ...)
    (begin (define identifier (record-setter record-layout field-name)) ...))

  (define (make-record record-layout) "record-layout -> record"
    ;if calling ht-size is too slow, it could be cached in the layout under a non-symbol key.
    ;except changing the layout data-type this seems to be the best possible without variable definitions
    ;for record metadata
    (make-vector (ht-size record-layout)))

  (define (make-record-layout field-spec)
    "(symbol ...) -> record-layout
     results in a new record-layout with the given field names"
    (let (r (ht-make ht-hash-symbol eqv? (length field-spec)))
      (fold (l (e index) (ht-set! r e index) (+ index 1)) 0 field-spec) r))

  (define (record record-layout . values)
    "record-layout (any ...) -> record
     create a new record by specifying the layout and the values in
     the same order as they are specified in layout field-spec. not all values have to be given, unspecified fields are set to <unspecified>"
    (apply vector
      (let loop ((count (ht-size record-layout)) (v values))
        (if (> count 0)
          (if (null? v) (pair #f (loop (- count 1) v)) (pair (first v) (loop (- count 1) (tail v))))
          (list)))))

  (define (record-accessor record-layout field-name)
    "record-layout symbol -> procedure {record -> field-value}
     returns an accessor procedure for the given record-layout and field-name."
    (let (index (ht-ref record-layout field-name #f))
      (if (integer? index) (l (record) (vector-ref record index)) (raise (q no-such-field)))))

  (define* (record-accessors record-layout)
    "hashtable:record-layout -> (proc ...)
     returns all accessors for the given record-layout in a list"
    (map-integers (ht-size record-layout) (l (index) (l (record) (vector-ref record index)))))

  (define record-append vector-append)

  (define (record-field-names record-layout)
    "hashtable:record-layout -> vector:#(symbol ...)
     result in the field-names of record in the same order as they were specified."
    (call-with-values (nullary (ht-entries record-layout))
      (l (keys values)
        (let ((r (make-vector (vector-length keys))))
          (vector-each-with-index (l (e index) (vector-set! r (vector-ref values index) e)) keys) r))))

  (define record-field-names-unordered ht-keys)

  (define (record-layout-extend! layout-1 layout-2)
    (let (layout-1-size (ht-size layout-1))
      (ht-each
        (l (key value)
          (if (ht-ref layout-1 key #f)
            (raise (q fail-record-layout-field-not-existant))
            (ht-set! layout-1 key (+ value layout-1-size))))
        layout-2)))

  (define record-layout-length ht-size)
  (define record-layout-merge! ht-merge!)
  (define record-layout? ht?)
  (define record-length vector-length)

  (define* (record-layout->predicate a #:optional type-prefix)
    "record-layout [symbol:type-name] -> procedure:{vector -> boolean}
     if type-prefix is given, the first field of the record is required to contain the type-prefix"
    (let (record-length (record-layout-length a))
      (if type-prefix
        (l (a)
          (and (vector? a) (= record-length (vector-length a)) (eqv? type-prefix (vector-first a))))
        (l (a) (and (vector? a) (= record-length (vector-length a)))))))

  (define (record-ref record record-layout field-name)
    "record record-layout symbol -> any
     get the value for field-name of the given record.
     record-ref is considerably slower than using an accessor procedure"
    (vector-ref record (ht-ref record-layout field-name #f)))

  (define (record-set! record record-layout field-name value)
    "record record-layout symbol any -> unspecified
     record-set! is considerably slower than using a setter procedure"
    (vector-set! record (ht-ref record-layout field-name #f) value))

  (define (record-setter record-layout field-name)
    "record-layout symbol -> procedure {record value -> unspecified}
     returns a setter procedure for the given layout and field-name"
    ( (l (index) (l (record value) (vector-set! record index value)))
      (ht-ref record-layout field-name #f)))

  (define (record-setters record-layout)
    "record-layout (symbol ...) -> proc ...
     returns all setters for the given layout in a list"
    (map-integers (ht-size record-layout)
      (l (index) (l (record value) (vector-set! record index value)))))

  (define record? vector?)

  (define (vector->record record-layout a)
    "this adjusts the length of the given vector to match the length of the layout.
     extra fields in are left out if the layout is smaller"
    (let ((record-layout-size (ht-size record-layout)) (vec-size (vector-length a)))
      (if (eqv? record-layout-size vec-size) a
        (if (< vec-size record-layout-size) (vector-extend a (- record-layout-size vec-size))
          (vector-shrink a (- vec-size record-layout-size))))))

  (define record-take vector->record)

  (define (define-record-prepare-field-spec record-name a)
    "(symbol/(symbol [symbol symbol]) ...) -> ((symbol symbol symbol) ...)"
    (map
      (l (e)
        (if (symbol? e)
          (list e (symbol-append record-name (q -) e) (symbol-append record-name (q -) e (q -set!)))
          (or
            (and (list? e)
              (case (length e)
                ( (1)
                  (let (e (first e))
                    (list e (symbol-append record-name (q -) e)
                      (symbol-append record-name (q -) e (q -set!)))))
                ( (2 3)
                  (apply
                    (l (name name-accessor . name-setter)
                      (list name name-accessor
                        (if (null? name-setter) (symbol-append record-name (q -) name (q -set!))
                          (first name-setter))))
                    e))
                (else #f)))
            (raise (q define-record-syntax-error)))))
      a))

  (define-syntax-case (define-record name field-name/get/set ...) s
    (let
      ( (field-spec
          (define-record-prepare-field-spec (syntax->datum (syntax name))
            (syntax->datum (syntax (field-name/get/set ...)))))
        (name (syntax->datum (syntax name))))
      (datum->syntax s
        (pairs (q begin)
          (list (q define) name
            (list (q make-record-layout) (list (q quote) (map first field-spec))))
          (fold
            (l (e r)
              (apply
                (l (field-name name-accessor name-setter)
                  (pairs
                    (list (q define) name-accessor
                      (list (q record-accessor) name (list (q quote) field-name)))
                    (list (q define) name-setter
                      (list (q record-setter) name (list (q quote) field-name)))
                    r))
                e))
            (list) field-spec)))))

  (define (record-list-filter-value record-list value match-accessor retrieve-accessor)
    "list procedure:accessor procedure:accessor -> false/(any ...)
     filter record list entries by values retrieved by match-accessor that match the given value,
     and return a list of values retrieved by retrieve-accessor"
    (filter-map (l (a) (and (equal? value (match-accessor a)) (retrieve-accessor a))) record-list)))
