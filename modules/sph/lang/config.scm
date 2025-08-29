(define-module (sph lang config))

(use-modules (sph) (sph alist)
  (sph string) (sph hashtable) (sph lang scheme) (sph tree) (sph lang indent-syntax))

(export config-read config-read-file config-read-string config-write)

(define sph-lang-config-description
  "a scheme syntax configuration file format that parses to a alist or hashtable, possibly nested.
   * all elements are scheme literals
   * key and value are specified alternatingly
   * keys are symbols
   * indent of two spaces is used for nesting
   * nesting is unlimited
   example:
     default-type \"text\"
     mode development
     start-id #xa
     preview-image-size (unquote (+ 255 1))
     browse
       page-size 75
       include-types (\"itml\" \"plaintext\")
     other #(1 2 4)")

(define indent-string (string-multiply " " 2))

(define* (config-read port #:key hashtable)
  "port #:hashtable boolean -> list:((symbol:key . any:value) ...)
   if hashtable is true, the result is a hashtable, possibly with nested hashtables"
  ( (if hashtable (l (a) (ht-from-alist a #:hash-f symbol-hash #:depth (inf))) identity)
    (list->alist
      (tree-fold-right
        (l (a result)
          (cond
            ((string? a) (append (string->datums a) result))
            ((list? a) (pair (list->alist a) result))
            (else (pair a result))))
        null (denoted-tree->tree (read-indent-tree->denoted-tree port))))))

(define (config-write a port)
  "alist/hashtable port -> unspecified
   write a config alist or hashtable to port.
   one key/value pair per line and nested values indented after key"
  (let loop ((a (if (ht? a) (ht-alist a (inf)) a)) (indent ""))
    (if (not (null? a))
      (let (b (first a))
        (begin (display indent) (write (first b) port)
          (if (alist? (tail b))
            (begin (newline) (loop (tail b) (string-append indent indent-string)))
            (begin (display " " port) (write (tail b) port) (newline))))
        (loop (tail a) indent)))))

(define (config-read-string a . b) (call-with-input-string a (l (a) (apply config-read a b))))
(define (config-read-file a . b) (call-with-input-file a (l (a) (apply config-read a b))))
