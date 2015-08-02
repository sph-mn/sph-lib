(library (sph lang indent-syntax)
  (export
    denoted-tree->indent-tree-string
    line->denoted-tree-element
    prefix-tree->indent-tree-string
    read-space-indent-tree->denoted-tree
    read-space-indent-tree->prefix-tree
    read-space-indent-tree-element->denoted-tree
    string-indent->delimited-tree)
  (import
    (guile)
    (ice-9 match)
    (rnrs base)
    (rnrs io ports)
    (sph)
    (sph stream)
    (srfi srfi-41)
    (only (sph string) string-multiply)
    (only (sph tree) denoted-tree->prefix-tree prefix-tree->denoted-tree))

  (define* (prefix-tree->indent-tree-string a #:optional (base-depth 0) (indent-string "  "))
    "list:(string/list ...) -> string
    converts a list to a string of indented lines"
    (denoted-tree->indent-tree-string (prefix-tree->denoted-tree a base-depth)))

  (define (line->denoted-tree-element a indent-width)
    (let (prefix-count (string-skip a #\space))
      (if prefix-count
        (list (if (>= prefix-count 0) (ceiling (/ prefix-count indent-width)) prefix-count)
          (string-trim-right (string-drop a prefix-count) #\space))
        (list 0 a))))

  (define* (read-space-indent-tree->prefix-tree . a)
    (denoted-tree->prefix-tree (apply read-space-indent-tree->denoted-tree a)))

  (define* (read-space-indent-tree->denoted-tree port #:optional (indent-width 2))
    "port [integer] -> list:((integer . string) ...)
    reads a string with indented lines to a list where each elment contains indent-count and line content.
    approximates indent-width to the next higher indent depth if it is not a multiple of indent-width"
    (let loop ((line (get-line port)) (r (list)))
      (if (eof-object? line) (reverse r)
        (loop (get-line port) (pair (line->denoted-tree-element line indent-width) r)))))

  (define* (read-space-indent-tree-element->denoted-tree port #:optional (indent-width 2))
    ;this assumes that unread-string works on the port.
    (let loop ((line (get-line port)) (top-level-reached-count 0) (r (list)))
      (if (eof-object? line) (if (null? r) line (reverse r))
        (apply
          (l (level data)
            (if (= 0 level)
              (let (top-level-reached-count (+ 1 top-level-reached-count))
                (if (= top-level-reached-count 2)
                  (begin (unread-string (string-append line "\n") port) (reverse r))
                  (loop (get-line port) top-level-reached-count (pair (list level data) r))))
              (loop (get-line port) top-level-reached-count (pair (list level data) r))))
          (line->denoted-tree-element line indent-width)))))

  (define* (denoted-tree->indent-tree-string a #:optional (base-depth 0) (indent-string "  "))
    "list integer string -> string"
    (string-join
      (map
        (l (e)
          (apply string-append
            (pair (string-multiply indent-string (+ base-depth (first e))) (tail e))))
        a)
      "\n"))

  (define (string-indent->delimited-tree a indent indent-width start-delimiter end-delimiter)
    "string char or char-set or procedure integer string string -> string
    convert the indented-tree in string to a nested range delimited tree. for example converting
    an indentation based tree to a parenthesis delimited tree. everything except the indentation is preserved"
    (match
      (stream-fold-right-multiple
        (l (e r r-depth)
          (let*
            ( (skip-index (string-skip e indent))
              (depth (if skip-index (floor (/ skip-index indent-width)) 0))
              (line (string-append (if (= 0 depth) e (string-drop e skip-index)) "\n")))
            (list
              (if (= depth r-depth) (pairs line r)
                (if (< depth r-depth) (pairs line start-delimiter r)
                  (pairs line (string-multiply end-delimiter (- depth r-depth)) r)))
              depth)))
        (port->line-stream (open-input-string a)) (list) 0)
      ( (r depth)
        (apply string-append ((if (> depth 0) (l (r) (append r (make-list depth ")"))) identity) r))))))