(define-module (sph alist))

(export alist alist->list
  alist-bind alist-bind-and*
  alist-cond alist-contains
  alist-containsq alist-containsv
  alist-delete alist-delete-multiple
  alist-keys alist-keys-map
  alist-map alist-merge
  alist-prepend alist-q
  alist-ref alist-ref-q
  alist-select alist-select-apply
  alist-select-q alist-select-q-apply
  alist-set alist-set!
  alist-set-multiple alist-set-multiple-q
  alist-update alist-update-multiple
  alist-update-multiple-q alist-values
  alist? alistq-ref
  alistq-select alists-ref
  alists-ref-p alists-ref-q
  alists-set! alistv-ref
  alistv-select bindings->alist
  keyword-list->alist+keyless list->alist
  list->group-alist list-alist? set-alist-bindings! sph-alist-description)

(use-modules (sph) ((sph list) #:select (contains? fold-multiple)) (srfi srfi-1))
(define alist-prepend acons)
(define alistq-ref assq-ref)
(define alistv-ref assv-ref)
(define alist-set! assoc-set!)
(define sph-alist-description "association list processing")

(define (keyword-list->alist+keyless a)
  "list -> (list:alist any ...)
   parses a list with arguments similar to lambda* arguments. it creates alist entries for keywords (#: notation, #:example) and subsequently following values (#:example 3 other ...).
   if no value follows: (#:example . #t).
   all values that do not directly follow a keyword are collected in the tail of the result"
  (let loop ((rest a) (r (list)) (keyless (list)))
    (if (null? rest) (pair r keyless)
      (let (e (first rest))
        (if (keyword? e)
          (let ((rest (tail rest)) (e (keyword->symbol e)))
            (if (null? rest) (pair (pair (pair e #t) r) keyless)
              (loop (tail rest) (pair (pair e (first rest)) r) keyless)))
          (loop (tail rest) r (pair e keyless)))))))

(define list->alist
  (let (proc (l (a alt prev r) (if alt (list #f #f (alist-prepend prev a r)) (list #t a r))))
    (lambda (lis)
      "-> alist
       create an association list from the given arguments,
       mapping each list element alternating to a key and value."
      (if (null? lis) lis
        (let (r (fold-multiple proc (tail lis) #t (first lis) (list)))
          (reverse!
            (if (first r) (pair (list (list-ref r 1)) (first (tail (tail r))))
              (first (tail (tail r))))))))))

(define-syntax-rules alist-ref ((a k d) ((l (r) (if r (tail r) d)) (assoc k a)))
  ((a k) (assoc-ref a k)))

(define-syntax-rules alist-ref-q ((a k d) (alist-ref a (quote k) d))
  ((a k) (alist-ref a (quote k))))

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

(define-syntax-rules alists-ref-q ((a k) (alist-ref-q a k))
  ((a k ... k-last) (alist-ref-q (alists-ref-q a k ...) k-last)))

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

(define-syntax-rule (alist-q key/value ...) "only the keys are quoted"
  (list->alist (quote-odd key/value ...)))

(define-syntax-rule (alist-bind alist (key ...) body ...)
  "could allow for custom variable names for key values as an extension"
  ((lambda (a) ((lambda (key ...) body ...) (alist-ref a (quote key)) ...)) alist))

(define-syntax-rule (alist-bind-and* alist (key ...) body ...)
  "alist values are bound in order of keys, and false is returned if any key value is false"
  (let (a alist) (and-let* ((key (alist-ref a (quote key))) ...) (begin body ...))))

(define-syntax-rule (bindings->alist identifier ...)
  "create an alist with keys named like the identifiers and values from identified variables
   example: (let ((a 1) (b 2)) (bindings->alist a b)) -> (((quote a) . 1) ((quote b) . 2))"
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

(define-syntax-rule (alist-keys alist) "get all keys of an alist as a list" (map first alist))
(define-syntax-rule (alist-contains-s a key equal?) (any (l (a) (equal? key (first a))) a))
(define (alist-containsq a key) (alist-contains-s a key eq?))
(define (alist-containsv a key) (alist-contains-s a key eqv?))
(define (alist-contains a key) (alist-contains-s a key equal?))

(define (alist-map proc a) "procedure:{key value -> any} list -> list"
  (map (l (a) (proc (first a) (tail a))) a))

(define (alist-merge a b)
  "list list -> list
   create a new alist with the associations of both alists, preferring entries of "b""
  (append (filter (l (a) (not (alist-ref b (first a)))) a) b))

(define (alist-set-multiple a . key/value)
  "list [any:key any:value] ...
   update or add values in alist for specific keys.
   key and value are specified alternatingly"
  (alist-merge a (list->alist key/value)))

(define-syntax-rule (alist-set-multiple-q a key/value ...) "list [any:unquoted-key any:value] ..."
  (apply alist-set-multiple a (quote-odd key/value ...)))

(define (alist-update-multiple a . key/value)
  "list [any:key any:value] ...
   update values in alist for specific keys.
   key and value are specified alternatingly"
  (alist-update a (list->alist key/value)))

(define-syntax-rule (alist-update-multiple-q a key/value ...)
  "list [any:unquoted-key any:value] ..." (apply alist-update-multiple a (quote-odd key/value ...)))

(define (alist-update a b)
  "list list -> list
   update existing entries of a with corresponding entries of b"
  (map
    (l (pair-1)
      ((l (value) (if value (pair (first pair-1) value) pair-1)) (alist-ref b (first pair-1))))
    a))

(define-syntax-rule (alist-values alist) (map tail alist))
(define (alist-select alist keys) "list list -> list" (map (l (key) (alist-ref alist key)) keys))
(define (alistq-select alist keys) (map (l (key) (alistq-ref alist key)) keys))
(define (alistv-select alist keys) (map (l (key) (alistv-ref alist key)) keys))

(define (list-alist? a)
  "list -> boolean
   return #t if list is an association list, #f otherwise. works only on lists"
  (every pair? a))

(define (alist? a) "any -> boolean" (and (list? a) (list-alist? a)))

(define (alist-select-apply a keys proc)
  "list list procedure:{any:key-value ...} -> any
   applies proc with all alist values for keys in order"
  (apply proc (alist-select a keys)))

(define-syntax-rule (alist-select-q-apply a (key ...) proc)
  (alist-select-apply a (quote (key ...)) proc))

(define-syntax-rule (alist-select-q a key ...) (alist-select a (quote (key ...))))
(define (alist-keys-map proc a) (map (l (a) (pair (proc (first a)) (tail a))) a))

(define (alist-set a key value)
  "list any any -> list
   add or update an entry in an association list"
  (let loop ((rest a))
    (if (null? rest) (pair (pair key value) rest)
      (let (e (first rest))
        (if (equal? key (first e)) (pair (pair key value) (tail rest)) (pair e (loop (tail rest))))))))

(define (alists-ref-p a keys) "like alists-ref but as a procedure that accepts a list for keys"
  (let loop ((a a) (keys keys))
    (if (null? keys) a (and a (loop (alist-ref a (first keys)) (tail keys))))))

(define (alist-delete-multiple a . keys) (remove (l (a) (contains? keys (first a))) a))

(define (alist->list a)
  "list -> list
   convert an alist to a list that contains every alist key and value alternatingly.
   the inverse of list->alist.
   example: ((a . 1) (b . 2)) -> (a 1 b 2)"
  (fold (l (a result) (pairs (first a) (tail a) result)) null a))
