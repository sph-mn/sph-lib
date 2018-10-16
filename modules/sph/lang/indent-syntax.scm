(library (sph lang indent-syntax)
  (export
    denoted-tree->indent-tree
    denoted-tree->indent-tree-lines
    indent-tree->denoted-tree
    indent-tree->prefix-tree
    indent-tree->range-delimited-tree
    line->indent-and-content
    prefix-tree->indent-tree
    prefix-tree-text
    read-indent-tree->denoted-tree
    read-indent-tree->prefix-tree
    read-indent-tree-element->denoted-tree
    string->indent-depth)
  (import
    (guile)
    (ice-9 match)
    (rnrs base)
    (rnrs io ports)
    (sph)
    (sph stream)
    (sph tree)
    (srfi srfi-41)
    (only (sph io) port->lines)
    (only (sph string) string-multiply)
    (only (srfi srfi-1) drop-while))

  (define sph-lang-indent-syntax-description "converting to and from strings with indented lines")

  (define-syntax-rule (char-count->indent-depth a indent-width)
    (if (>= a 0) (ceiling (/ a indent-width)) a))

  (define* (denoted-tree->indent-tree-lines a #:optional (base-depth 0) (indent-string "  "))
    (map (l (a) (string-append (string-multiply indent-string (+ base-depth (first a))) (tail a)))
      a))

  (define (string->indent-depth a indent-width) "string integer -> integer"
    (and-let* ((prefix-count (string-skip a #\space)))
      (char-count->indent-depth prefix-count indent-width)))

  (define (line->indent-and-content a indent-width) "string integer -> (integer string)"
    (let (prefix-count (string-skip a #\space))
      (if prefix-count
        (pair (char-count->indent-depth prefix-count indent-width)
          (string-trim-right (string-drop a prefix-count) #\space))
        (list 0 a))))

  (define* (prefix-tree->indent-tree a #:optional (base-depth 0) (indent-string "  "))
    "list:(string/list ...) -> string
     converts a list to a string of indented lines with indent corresponding to nesting depth"
    (denoted-tree->indent-tree (prefix-tree->denoted-tree a) base-depth indent-string))

  (define* (indent-tree->prefix-tree a #:optional (indent-width 2))
    (read-indent-tree->prefix-tree (open-input-string a) indent-width))

  (define* (indent-tree->denoted-tree a #:optional (indent-width 2))
    (read-indent-tree->denoted-tree (open-input-string a) indent-width))

  (define (read-indent-tree->prefix-tree . a) "same arguments as read-indent-tree->denoted-tree"
    (denoted-tree->prefix-tree (apply read-indent-tree->denoted-tree a)))

  (define read-indent-tree->denoted-tree
    (let
      (empty-line->indent-and-content
        (l (a following-lines indent-width)
          (pair
            (let (lines-next (drop-while string-null? following-lines))
              (if (null? lines-next) 0 (string->indent-depth (first lines-next) indent-width)))
            a)))
      (l* (port #:optional (indent-width 2))
        "port [integer] -> list:((integer:indent . string:line-content) ...)
        approximates indent-width to the next higher indent depth if it is not a multiple of indent-width.
        empty lines are parsed as an empty string with the indent-depth of the following expression,
        or zero if there are no following expressions"
        ; lines are read all at once first for the look-ahead we make when handling empty lines
        (let loop ((rest (port->lines port)))
          (if (null? rest) rest
            (pair
              (let (line (first rest))
                (if (string-null? line)
                  (empty-line->indent-and-content line (tail rest) indent-width)
                  (line->indent-and-content line indent-width)))
              (loop (tail rest))))))))

  (define* (read-indent-tree-element->denoted-tree port #:optional (indent-width 2))
    "port [integer] -> list
     assumes that unread-string works on the port"
    (let loop ((line (get-line port)) (top-depth-reached-count 0) (r (list)))
      (if (eof-object? line) (if (null? r) line (reverse r))
        (let* ((a (line->indent-and-content line indent-width)) (depth (first a)) (data (tail a)))
          (if (= 0 depth)
            (let (top-depth-reached-count (+ 1 top-depth-reached-count))
              (if (= top-depth-reached-count 2)
                (begin (unread-string (string-append line "\n") port) (reverse r))
                (loop (get-line port) top-depth-reached-count (pair (list depth data) r))))
            (loop (get-line port) top-depth-reached-count (pair (list depth data) r)))))))

  (define* (denoted-tree->indent-tree a #:optional (base-depth 0) (indent-string "  "))
    "((integer:indent . string:line-content) ...) [integer string] -> string"
    (string-join (denoted-tree->indent-tree-lines a base-depth indent-string) "\n"))

  (define
    (indent-tree->range-delimited-tree a indent-char indent-width start-delimiter end-delimiter)
    "string char/char-set/procedure integer string string -> string
     convert from indent syntax to one where nesting is specified by a pair of strings
     for start and end of a range"
    (match
      (stream-fold-right-multiple
        (l (e r r-depth)
          (let*
            ( (skip-index (string-skip e indent-char))
              (depth (if skip-index (floor (/ skip-index indent-width)) 0))
              (line (string-append (if (= 0 depth) e (string-drop e skip-index)) "\n")))
            (list
              (if (= depth r-depth) (pairs line r)
                (if (< depth r-depth) (pairs line start-delimiter r)
                  (pairs line (string-multiply end-delimiter (- depth r-depth)) r)))
              depth)))
        (port->line-stream (open-input-string a)) (list) 0)
      ( (r depth)
        (apply string-append ((if (> depth 0) (l (r) (append r (make-list depth ")"))) identity) r)))))

  (define (prefix-tree-text source)
    "(string/list ...) -> string
     convert a list with strings and eventually sub-lists with strings to an indented document structure.
     normalise indent from multiline string literals"
    (prefix-tree->indent-tree
      (tree-map-lists
        (l (a)
          (fold-right
            (l (a result)
              (if (string? a)
                (let (b (indent-tree->denoted-tree a))
                  (if (or (null? b) (null? (tail b))) (pair a result)
                    (append
                      (pair (tail (first b))
                        (denoted-tree->prefix-tree (denoted-tree-minimise-depth (tail b))))
                      result)))
                (pair a result)))
            (list) a))
        source))))
