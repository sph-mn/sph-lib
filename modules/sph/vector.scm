; (sph vector) - vector processing.
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

(library (sph vector)
  (export
    alist-values->vector
    sph-vector-description
    vector-append
    vector-copy*
    vector-delete-duplicates
    vector-deselect
    vector-each
    vector-each-with-index
    vector-extend
    vector-first
    vector-index-value
    vector-map-with-index
    vector-range
    vector-second
    vector-select
    vector-shrink
    vector-third
    vector-update)
  (import
    (sph)
    (only (guile) vector-copy)
    (only (rnrs sorting) list-sort)
    (only (sph alist) alist-values)
    (only (sph list) map-slice)
    (only (srfi srfi-1) delete-duplicates)
    (rename (rnrs base) (vector-for-each vector-each)))

  (define sph-vector-description "vector processing")

  (define (each-integer n proc)
    ; redefine each-integer from (one) to avoid circular dependency (one -> vector, vector -> one)
    "evaluate a procedure a number of times, passing the current number to proc. starts from 0"
    (let loop ((e-n 0) (prev #f)) (if (<= e-n n) (loop (+ 1 e-n) (proc n)) prev)))

  (define (alist-values->vector alist) "create a vector of alist values"
    (list->vector (alist-values alist)))

  (define (vector-first a) (vector-ref a 0))
  (define (vector-second a) (vector-ref a 1))
  (define (vector-third a) (vector-ref a 2))

  (define (vector-any proc a)
    "procedure:{any:element -> any} vector -> any
     call proc for each element of a and return the first true result or false if there is none"
    (let (a-length (vector-length a))
      (let loop ((index 0))
        (if (< index a-length) (let (b (proc (vector-ref a index))) (if b b (loop (+ 1 index)))) #f))))

  (define (vector-append a b)
    "concatenate \"b\" at the end of \"a\".
     #(1 2) #(3 4) -> #(1 2 3 4)
     create a new bigger vector and copy all elements of \"a\" and \"b\" to it"
    (let* ((a-length (vector-length a)) (r (make-vector (+ a-length (vector-length b)))))
      (vector-each-with-index (l (e index) (vector-set! r index e)) a)
      (vector-each-with-index (l (e index) (vector-set! r (+ a-length index) e)) b) r))

  (define (vector-copy* a proc) "call proc with a copy of vector and after proc finishes return it"
    (let (r (vector-copy a)) (proc r) r))

  (define* (vector-delete-duplicates a #:optional (equal? equal?))
    "vector [procedure] -> vector
     creates a new vector with duplicate elements removed"
    (list->vector (delete-duplicates (vector->list a) equal?)))

  (define (vector-deselect a indices)
    "vector (integer ...) -> vector
     return a new, possibly smaller, vector consisting of values not at specified indices"
    (let*
      ( (a-length (vector-length a)) (indices (list-sort < indices))
        (r (make-vector (- a-length (length indices)))))
      (let loop ((index 0) (omit-index (first indices)) (indices (tail indices)))
        (if (< index a-length)
          (if (eqv? omit-index index) (loop (+ 1 index) (first indices) (tail indices))
            (begin (vector-set! r index (vector-ref a index)) (loop (+ 1 index) omit-index indices)))
          r))))

  (define* (vector-each-with-index proc a #:optional (index-offset 0))
    "procedure vector [integer] -> unspecified
     proc is called as (proc element index).
     index-offset is added to the actual index that is passed to proc"
    (let ((a-length (vector-length a)))
      (let loop ((index index-offset))
        (if (< index a-length) (begin (proc (vector-ref a index) index) (loop (+ 1 index)))))))

  (define (vector-extend a add-size)
    "vector integer -> vector
     increase size of vector.
     new slots are appended to vector"
    (let*
      ( (old-size (vector-length a)) (r (make-vector (+ old-size add-size)))
        (set (l (a index) (vector-set! r index a))))
      (vector-each-with-index set r) r))

  (define* (vector-index-value a value #:optional (equal-proc equal?))
    "vector any [procedure] -> integer/falso
     find the index in vector at which value occurs or return false"
    (let (a-length (vector-length a))
      (let loop ((index 0))
        (if (< index a-length)
          (if (equal-proc value (vector-ref a index)) index (loop (+ index 1))) #f))))

  (define (vector-map-with-index proc a)
    "procedure {any integer -> any} vector -> vector
     map each vector element suppliing its index to the
     mapping procedure and result in a new vector"
    (let* ((last-index (- (vector-length a) 1)) (r (make-vector (+ last-index 1))))
      (let loop ((index 0))
        (vector-set! r index (proc (vector-ref a index) index))
        (if (< index last-index) (loop (+ 1 index)) r))))

  (define* (vector-range a start #:optional (end (- (vector-length a) 1)))
    "vector [integer integer] -> vector
     get a sub-vector.
     start and end are inclusive"
    (let ((r (make-vector (+ 1 (- end start)))))
      (let loop ((b start))
        (begin (vector-set! r (- b start) (vector-ref a b)) (if (>= b end) r (loop (+ 1 b)))))))

  (define (vector-select a indices)
    "vector (integer ...) -> vector
     return a new vector consisting of values at indices specified by vector indices"
    (vector-map (l (index) (vector-ref a index)) indices))

  (define (vector-update a . index/value)
    "vector [integer any] ... -> vector
     create a copy of the given vector with values at indices set to new values.
     index and value are given alternatingly.
     example: (vector-update myvector 1 #\a 4 #\b)"
    (vector-copy* a (l (a) (map-slice 2 (l (index value) (vector-set! a index value)) index/value)))))
