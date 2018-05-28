; a small fixed size hash table based data structure for sets of integers.
; Copyright (C) 2016-2018 sph <sph@posteo.eu>
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
(pre-include "stdlib.h" "inttypes.h")
; the following definition sets the integer type and size for values
(pre-if-not-defined imht-set-key-t (pre-define imht-set-key-t uint64_t))
; using the following leads to slightly faster set operations but a stored zero will not be found
(pre-if-not-defined imht-set-can-contain-zero? (pre-define imht-set-can-contain-zero? 1))
; the minimum memory usage is size times imht-set-size-factor
(pre-if-not-defined imht-set-size-factor (pre-define imht-set-size-factor 2))

(declare imht-set-primes
  (array
    uint16_t
    ()
    ; performance can be optimised for bigger sets by adding additional primes nearer to the desired set size times set-size-factor
    #f
    3
    7
    13
    19
    29
    37
    43
    53
    61
    71
    79
    89
    101
    107
    113
    131
    139
    151
    163
    173
    181
    193
    199
    223
    229
    239
    251
    263
    271
    281
    293
    311
    317
    337
    349
    359
    373
    383
    397
    409
    421
    433
    443
    457
    463
    479
    491
    503
    521
    541
    557
    569
    577
    593
    601
    613
    619
    641
    647
    659
    673 683 701 719 733 743 757 769 787 809 821 827 839 857 863 881 887 911 929 941 953 971 983 997))

(define imht-set-primes-end uint16_t* (+ imht-set-primes 83))

(declare imht-set-t
  (type
    (struct
      (size size-t)
      (content imht-set-key-t*))))

(define (imht-set-calculate-hash-table-size min-size) (size-t size-t)
  (set min-size (* imht-set-size-factor min-size))
  (define primes uint16_t* imht-set-primes)
  (while (< primes imht-set-primes-end)
    (if (<= min-size (pointer-get primes))
      (return (pointer-get primes))
      (set primes (+ 1 primes))))
  (if (<= min-size (pointer-get primes))
    (return (pointer-get primes)))
  ;if no prime has been found, use size-factor times size made odd as a best guess
  (return (bit-or 1 min-size)))

(define (imht-set-create min-size result) (uint8_t size-t imht-set-t**)
  ;returns 1 on success or 0 if the memory allocation failed
  (set (pointer-get result) (malloc (sizeof imht-set-t)))
  (if (not (pointer-get result))
    (return #f))
  (set min-size (imht-set-calculate-hash-table-size min-size))
  (struct-set (pointer-get (pointer-get result))
    content (calloc min-size (sizeof imht-set-key-t))
    size min-size)
  (return
    (if* (struct-pointer-get (pointer-get result) content)
      #t
      #f)))

(define (imht-set-destroy a) (void imht-set-t*)
  (if a
    (begin
      (free (struct-pointer-get a content))
      (free a))))

(pre-if
  imht-set-can-contain-zero?
  (pre-define (imht-set-hash value hash-table)
    (if* value
      (+ 1 (modulo value (- hash-table.size 1)))
      0))
  (pre-define (imht-set-hash value hash-table) (modulo value hash-table.size)))

(define (imht-set-find a value) (imht-set-key-t* imht-set-t* imht-set-key-t)
  "returns the address of the element in the set, 0 if it was not found.
  caveat: if imht-set-can-contain-zero? is defined, which is the default,
  pointer-geterencing a returned address for the found value 0 will return 1 instead"
  (define h imht-set-key-t*
    (+ (struct-pointer-get a content) (imht-set-hash value (pointer-get a))))
  (if (pointer-get h)
    (begin
      (pre-if
        imht-set-can-contain-zero?
        ;the value zero is stored at a special index and is the only value that can be stored there
        (if (or (= (pointer-get h) value) (= 0 value))
          (return h))
        (if (= (pointer-get h) value)
          (return h)))
      (define content-end imht-set-key-t*
        (+ (struct-pointer-get a content) (- (struct-pointer-get a size) 1)))
      (define h2 imht-set-key-t* (+ 1 h))
      (while (< h2 content-end)
        ;prefer the test for non-existance because the number of possible keys is typically much bigger than the size of the set
        (if (not (pointer-get h2))
          (return 0)
          (if (= value (pointer-get h2))
            (return h2)))
        (set h2 (+ 1 h2)))
      (if (not (pointer-get h2))
        (return 0)
        (if (= value (pointer-get h2))
          (return h2)))
      (set h2 (struct-pointer-get a content))
      (while (< h2 h)
        (if (not (pointer-get h2))
          (return 0)
          (if (= value (pointer-get h2))
            (return h2)))
        (set h2 (+ 1 h2)))))
  (return 0))

(pre-define (imht-set-contains? a value)
  (if* (= 0 (imht-set-find a value))
    #f
    #t))

(define (imht-set-remove a value) (uint8-t imht-set-t* imht-set-key-t)
  "returns 1 if the element was removed, 0 if it was not found"
  (define value-address imht-set-key-t* (imht-set-find a value))
  (if value-address
    (begin
      (set (pointer-get value-address) 0)
      (return #t))
    (return #f)))

(define (imht-set-add a value) (imht-set-key-t* imht-set-t* imht-set-key-t)
  "returns the address of the added or already included element, 0 if there is no space left in the set"
  (define h imht-set-key-t*
    (+ (struct-pointer-get a content) (imht-set-hash value (pointer-get a))))
  (if (pointer-get h)
    (begin
      ;the first element is special for storing 0
      (pre-if
        imht-set-can-contain-zero?
        (if (or (= value (pointer-get h)) (= 0 value))
          (return h))
        (if (= value (pointer-get h))
          (return h)))
      (define content-end imht-set-key-t*
        (+ (struct-pointer-get a content) (- (struct-pointer-get a size) 1)))
      (define h2 imht-set-key-t* (+ 1 h))
      (while (and (<= h2 content-end) (pointer-get h2))
        (set h2 (+ 1 h2)))
      ;has been moved to the next entry without data or past the last entry
      (if (> h2 content-end)
        (begin
          (set h2 (struct-pointer-get a content))
          (while (and (< h2 h) (pointer-get h2))
            (set h2 (+ 1 h2)))
          (if (= h2 h)
            (return 0)
            (pre-if
              imht-set-can-contain-zero?
              (set (pointer-get h2)
                (if* (= 0 value)
                  1
                  value))
              (set (pointer-get h2) value))))
        (pre-if
          imht-set-can-contain-zero?
          (set (pointer-get h2)
            (if* (= 0 value)
              1
              value))
          (set (pointer-get h2) value))))
    (begin
      (pre-if
        imht-set-can-contain-zero?
        (set (pointer-get h)
          (if* (= 0 value)
            1
            value))
        (set (pointer-get h) value))
      (return h))))