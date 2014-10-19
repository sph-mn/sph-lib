; (sph vector) - vector processing
; written for the guile scheme interpreter
; Copyright (C) 2010-2014 sph <sph@posteo.eu>
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
    vector-append
    vector-delete-duplicates
    vector-deselect
    vector-each
    vector-each-with-index
    vector-extend
    vector-first
    vector-index-value
    vector-map-with-index
    vector-produce
    vector-range-ref
    vector-second
    vector-select
    vector-shrink
    vector-third)
  (import
    (sph)
    (only (rnrs sorting) list-sort)
    (only (sph alist) alist-values)
    (only (srfi srfi-1) delete-duplicates)
    (rename (rnrs base) (vector-for-each vector-each)))

  (define (n-times n proc)
    ;redefine n-times from (one) to avoid mutual library dependency (one -> vector, vector -> one)
    "evaluate a procedure a number of times, passing the current number to proc. starts from 0"
    (let loop ((e-n 0) (prev #f)) (if (<= e-n n) (loop (+ 1 e-n) (proc n)) prev)))

  (define (alist-values->vector alist) "create a vector of alist values"
    (list->vector (alist-values alist)))

  (define (vector-first a) (vector-ref a 0))
  (define (vector-second a) (vector-ref a 1))
  (define (vector-third a) (vector-ref a 2))

  (define (vector-any proc a)
    (let ((a-length (vector-length a)))
      (let loop ((index 0))
        (if (< index a-length)
          ((l (e-result) (if e-result e-result (loop (+ 1 index)))) (proc (vector-ref a index))) #f))))

  (define (vector-append a b)
    "concatenate \"b\" at the end of \"a\".
    #(1 2) #(3 4) -> #(1 2 3 4)
    create a new bigger vector and copy all elements of \"a\" and \"b\" to it"
    (let* ((a-length (vector-length a)) (r (make-vector (+ a-length (vector-length b)))))
      (vector-each-with-index (l (e index) (vector-set! r index e)) a)
      (vector-each-with-index (l (e index) (vector-set! r (+ a-length index) e)) b) r))

  (define (vector-delete-duplicates a) (list->vector (delete-duplicates (vector->list a))))

  (define (vector-deselect a indices)
    "return a new vector consisting of values not at indices specified by list indices"
    (let*
      ( (a-length (vector-length a)) (indices (list-sort < indices))
        (r (make-vector (- a-length (length indices)))))
      (let loop ((index 0) (omit-index (first indices)) (indices (tail indices)))
        (if (< index a-length)
          (if (eqv? omit-index index) (loop (+ 1 index) (first indices) (tail indices))
            (begin (vector-set! r index (vector-ref a index)) (loop (+ 1 index) omit-index indices)))
          r))))

  (define* (vector-each-with-index proc a #:optional (index-offset 0))
    "-> unspecified
    proc is called as (proc element index)"
    (let ((a-length (vector-length a)))
      (let loop ((index index-offset))
        (if (< index a-length) (begin (proc (vector-ref a index) index) (loop (+ 1 index)))))))

  (define* (vector-index-value a value #:optional (equal-proc equal?))
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

  (define (vector-produce proc a b)
    "procedure vector vector -> #(vector ...)
    apply proc to each possible ordered pair of a \"a\" and \"b\" element (cartesian product).
    (#(1 2) #(4 5)) -> (proc 1 4) (proc 1 5) (proc 2 4) (proc 2 5)"
    (vector-map (l (a) (vector-map (l (b) (proc a b)) b)) a))

  (define* (vector-range-ref a #:optional (start 0) (end (- (vector-length a) 1)))
    "vector [integer integer] -> vector
    start and end are inclusive"
    (let ((r (make-vector (+ 1 (- end start)))))
      (let loop ((e start))
        (begin (vector-set! r (- e start) (vector-ref a e)) (if (>= e end) r (loop (+ 1 e)))))))

  (define (vector-extend a add-size)
    "vector integer -> vector
    increase size of vector"
    (let*
      ( (old-size (vector-length a)) (r (make-vector (+ old-size add-size)))
        (set (l (e index) (vector-set! r index e))))
      (vector-each-with-index set r) r))

  (define (vector-select a indices)
    "vector (integer ...) -> vector
    return a new vector consisting of values at indices specified by vector indices"
    (vector-map (l (index) (vector-ref a index)) indices)))