(sc-comment
  "a macro that defines set data types and functions for arbitrary value types.
   * compared to hashtable.c, this uses less than half the space and operations are faster (about 20% in first tests)
   * linear probing for collision resolve
   * sph-set-declare-type allows the null value (used for unset elements) to be part of the set
     * except the null value, values are in .values starting from index 1
     * notnull is used at index 0 to check if the empty-value is included
   * sph-set-declare-type-nonull does not allow the null value to be part of the set and should be a bit faster
     * values are in .values starting from index 0
   * null and notnull arguments are user provided so that they have same data type as other set elements
   * primes from https://planetmath.org/goodhashtableprimes
   * automatic resizing is not implemented")

(pre-include "stdlib.h" "inttypes.h")

(declare sph-set-primes
  (array uint32-t ()
    53 97 193
    389 769 1543
    3079 6151 12289
    24593 49157 98317
    196613 393241 786433
    1572869 3145739 6291469
    12582917 25165843 50331653 100663319 201326611 402653189 805306457 1610612741))

(define sph-set-primes-end uint32-t* (+ sph-set-primes 25))

(pre-define
  (sph-set-hash-integer value hashtable-size) (modulo value hashtable-size)
  (sph-set-equal-integer value-a value-b) (= value-a value-b)
  (sph-set-declare-type-shared-1 name value-type set-hash set-equal null size-factor)
  (begin
    (declare (pre-concat name _t) (type (struct (size size-t) (values value-type*))))
    (define ((pre-concat name _calculate-size) min-size) (size-t size-t)
      (set min-size (* size-factor min-size))
      (declare primes uint32-t*)
      (for ((set primes sph-set-primes) (<= primes sph-set-primes-end) (set+ primes 1))
        (if (<= min-size *primes) (return *primes)))
      (sc-comment "if no prime has been found, make size at least an odd number")
      (return (bit-or 1 min-size)))
    (define ((pre-concat name _clear) a) (void (pre-concat name _t))
      (declare i size-t)
      (for ((set i 0) (< i a.size) (set i (+ 1 i))) (set (array-get a.values i) null)))
    (define ((pre-concat name _free) a) (void (pre-concat name _t)) (begin (free a.values))))
  (sph-set-declare-type-shared-2 name value-type set-hash set-equal null size-factor)
  (begin
    (define ((pre-concat name _remove) a value) (uint8-t (pre-concat name _t) value-type)
      "returns 0 if the element was removed, 1 if it was not found"
      (define v value-type* ((pre-concat name _get) a value))
      (if v (begin (set *v null) (return 0)) (return 1))))
  (sph-set-declare-type-with-null name value-type set-hash set-equal null notnull size-factor)
  (begin
    (define ((pre-concat name _new) min-size result) (uint8-t size-t (pre-concat name _t*))
      "returns 0 on success or 1 if the memory allocation failed"
      (declare temp (pre-concat name _t))
      (set
        temp.size (+ 1 ((pre-concat name _calculate-size) min-size))
        temp.values (calloc temp.size (sizeof value-type)))
      (if (not temp.values) (return 1))
      ((pre-concat name _clear) temp)
      (set *result temp)
      (return 0))
    (define ((pre-concat name _get) a value) (value-type* (pre-concat name _t) value-type)
      "returns the address of the value or 0 if it was not found.
       if value is the null value and exists, then address points to the notnull value"
      (declare i size-t hash-i size-t)
      (if (set-equal null value) (return (if* (set-equal notnull *a.values) a.values 0)))
      (set hash-i (+ 1 (set-hash value (- a.size 1))))
      (set i hash-i)
      (while (< i a.size)
        (if (set-equal null (array-get a.values i)) (return 0)
          (if (set-equal value (array-get a.values i)) (return (+ i a.values))))
        (set+ i 1))
      (sc-comment "wraps over")
      (set i 1)
      (while (< i hash-i)
        (if (set-equal null (array-get a.values i)) (return 0)
          (if (set-equal value (array-get a.values i)) (return (+ i a.values))))
        (set+ i 1))
      (return 0))
    (define ((pre-concat name _add) a value) (value-type* (pre-concat name _t) value-type)
      "returns the address of the value or 0 if no space is left"
      (declare i size-t hash-i size-t)
      (if (set-equal null value) (begin (set *a.values notnull) (return a.values)))
      (set hash-i (+ 1 (set-hash value (- a.size 1))))
      (set i hash-i)
      (while (< i a.size)
        (if (set-equal null (array-get a.values i))
          (begin (set (array-get a.values i) value) (return (+ i a.values))))
        (set+ i 1))
      (sc-comment "wraps over")
      (set i 1)
      (while (< i hash-i)
        (if (set-equal null (array-get a.values i))
          (begin (set (array-get a.values i) value) (return (+ i a.values))))
        (set+ i 1))
      (return 0)))
  (sph-set-declare-type-without-null name value-type set-hash set-equal null size-factor)
  (begin
    (define ((pre-concat name _new) min-size result) (uint8-t size-t (pre-concat name _t*))
      "returns 0 on success or 1 if the memory allocation failed"
      (declare values value-type*)
      (set min-size ((pre-concat name _calculate-size) min-size))
      (set values (calloc min-size (sizeof value-type)))
      (if (not values) (return 1))
      (struct-set *result values values size min-size)
      (return 0))
    (define ((pre-concat name _get) a value) (value-type* (pre-concat name _t) value-type)
      "returns the address of the value or 0 if it was not found"
      (declare i size-t hash-i size-t)
      (set hash-i (set-hash value a.size) i hash-i)
      (while (< i a.size)
        (if (set-equal null (array-get a.values i)) (return 0)
          (if (set-equal value (array-get a.values i)) (return (+ i a.values))))
        (set+ i 1))
      (sc-comment "wraps over")
      (set i 0)
      (while (< i hash-i)
        (if (set-equal null (array-get a.values i)) (return 0)
          (if (set-equal value (array-get a.values i)) (return (+ i a.values))))
        (set+ i 1))
      (return 0))
    (define ((pre-concat name _add) a value) (value-type* (pre-concat name _t) value-type)
      "returns the address of the value or 0 if no space is left"
      (declare i size-t hash-i size-t)
      (set hash-i (set-hash value a.size) i hash-i)
      (while (< i a.size)
        (if (set-equal null (array-get a.values i))
          (begin (set (array-get a.values i) value) (return (+ i a.values))))
        (set+ i 1))
      (sc-comment "wraps over")
      (set i 0)
      (while (< i hash-i)
        (if (set-equal null (array-get a.values i))
          (begin (set (array-get a.values i) value) (return (+ i a.values))))
        (set+ i 1))
      (return 0)))
  (sph-set-declare-type name value-type hash equal null notnull size-factor)
  (begin
    (sph-set-declare-type-shared-1 name value-type hash equal null size-factor)
    (sph-set-declare-type-with-null name value-type hash equal null notnull size-factor)
    (sph-set-declare-type-shared-2 name value-type hash equal null size-factor))
  (sph-set-declare-type-nonull name value-type hash equal null size-factor)
  (begin
    (sph-set-declare-type-shared-1 name value-type hash equal null size-factor)
    (sph-set-declare-type-without-null name value-type hash equal null size-factor)
    (sph-set-declare-type-shared-2 name value-type hash equal null size-factor)))