; (sph alist) - association list processing
; written for the guile scheme interpreter
; Copyright (C) 2010-2015 sph <sph@posteo.eu>
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

(library (sph alist)
  (export
    alist
    alist-cond
    alist-keys
    alist-map
    alist-merge
    alist-ref
    alist-select
    alist-select-apply
    alist-set!
    alist-update
    alist-values
    alistq-ref
    alistq-select
    alists-ref
    alists-set!
    alistv-ref
    alistv-select
    bindings->alist
    list->alist
    list->group-alist
    list-alist?
    set-alist-bindings!
    symbol-alist
    symbol-alist-bind
    symbol-alist-ref
    symbol-alist-select-apply)
  (import
    (rnrs base)
    (sph)
    (only (sph list) fold-multiple)
    (only (sph one) quote-odd)
    (only (srfi srfi-1) alist-cons filter)
    (rename (guile)
      (assq-ref alistq-ref)
      (assv-ref alistv-ref)
      (assoc-set! alist-set!)))

  (define list->alist
    (let (proc (l (e alt prev r) (if alt (list #f #f (acons prev e r)) (list #t e r))))
      (lambda (lis)
        "-> alist
        create an association list from the given arguments,
        mapping each list element alternating to a key and value."
        (if (null? lis) lis
          (let (r (fold-multiple proc (tail lis) #t (first lis) (list)))
            (reverse!
              (if (first r) (pair (list (list-ref r 1)) (first (tail (tail r))))
                (first (tail (tail r))))))))))

  (define* (list->group-alist lis #:optional (accessor identity))
    "group elements in list by an attribute of its elements.
    this is the equality of (accessor list-element) between elements and stored under (accessor list-element) as keys.
    example
        (list->group-alist (1 2 2 3) identity) -> ((1 . (1)) (2 . (2 2)) (3 . (3)))
        (list->group-alist ((1 3) (2 5) (2 6)) first) -> ((1 . 3) (2 . (5 6)))"
    (reverse!
      (fold
        (l (e groups)
          (let* ((key (accessor e)) (value (alist-ref groups key)))
            (if value (alist-set! groups key (pair e value))
              (set! groups (alist-cons key (list e) groups)))
            groups))
        (list) lis)))

  (define-syntax-rules alist-ref
    ;a:alist k:key d:default-if-not-found
    ((a k d) ((l (r) (if r (tail r) d)) (assoc k a))) ((a k) (assoc-ref a k)))

  (define-syntax-rules symbol-alist-ref
    ((a (k ...)) (map (lambda (e) (alist-ref a e)) (quote (k ...))))
    ((a k rest ...) (alist-ref a (quote k) rest ...)))

  (define-syntax-rules alists-ref ((a k) (alist-ref a k))
    ((a k ... k-last) (alist-ref (alists-ref a k ...) k-last)))

  (define-syntax-rules alists-set! ((a k v) (alist-set! a k v))
    ((a k ... k-last v) (alist-set! (alists-ref a k ...) k-last v)))

  (define (alist . key/value)
    "key/value ... -> alist
    create an association list from the given arguments,
    mapping each argument alternatingly to key and value.
    (alist (quote a) 1 \"b\" 2 (quote c) 3)"
    (list->alist key/value))

  (define-syntax-rule (symbol-alist key/value ...) (list->alist (quote-odd key/value ...)))

  (define-syntax-rule (symbol-alist-bind alist (key ...) body ...)
    ((lambda (key ...) body ...) (alist-ref alist (quote key)) ...))

  (define-syntax-rule (bindings->alist identifier ...)
    ;create an alist with keys named like the identifiers and values from identified variables
    (list (pair (quote identifier) identifier) ...))

  (define (set-alist-bindings! alist)
    "for each alist part, set a variable named like alist-part-key to alist-part-value"
    (primitive-eval (pair (q begin) (map (l (e) (list (q set!) (first e) (tail e))) alist))))

  (define (alist-cond a alist)
    "any ((procedure:{any -> any/false} alist-tail ...) ...) -> alist-tail/false
    like a cond expression but with an alist for the test conditions where the tail of the alist is returned for which the test suceeds."
    (let next ((cur (first alist)) (rest (tail alist)))
      (if (null? rest) (if ((first cur) a) (tail cur) #f)
        (if ((first cur) a) (tail cur) (next (first rest) (tail rest))))))

  (define-syntax-rule (alist-keys alist)
    ;get all keys of an alist as a list
    (map first alist))

  (define (alist-map proc a) "procedure:{key value -> any} list -> list"
    (map (l (e) (proc (first e) (tail e))) a))

  (define (alist-merge alist-1 alist-2)
    "list list -> list
    create a new alist with the associations of both alists, preferring entries of list-2"
    (append (filter (l (e) (not (alist-ref alist-2 (first e)))) alist-1) alist-2))

  (define (alist-update alist-1 alist-2)
    "list list -> list
    update existing entries of alist-1 with corresponding entries of alist-2"
    (map
      (l (pair-1)
        ( (l (value) (if value (pair (first pair-1) value) pair-1))
          (alist-ref alist-2 (first pair-1))))
      alist-1))

  (define-syntax-rule (alist-values alist) (map tail alist))
  (define (alist-select alist keys) "list list -> list" (map (l (key) (alist-ref alist key)) keys))
  (define (alistq-select alist keys) (map (l (key) (alistq-ref alist key)) keys))
  (define (alistv-select alist keys) (map (l (key) (alistv-ref alist key)) keys))

  (define (list-alist? a)
    "return #t if list is an association list, false otherwise. works only on lists" (every pair? a))

  (define (alist-select-apply a keys proc) (apply proc (alist-select a keys)))

  (define-syntax-rule (symbol-alist-select-apply a (key ...) proc)
    (alist-select-apply a (quote (key ...)) proc))

  (define-syntax-rule (symbol-alist-select a (key ...)) (alist-select a (quote (key ...)))))