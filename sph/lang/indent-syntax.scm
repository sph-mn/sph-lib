(library (sph lang indent-syntax)
  (export
    denoted-tree->indent-tree-string
    prefix-tree->indent-tree-string
    read-space-indent-tree-string->denoted-tree
    string-indent->delimited-tree)
  (import
    (guile)
    (ice-9 match)
    (rnrs base)
    (sph)
    (sph stream)
    (srfi srfi-41)
    (only (sph string) string-multiply)
    (only (sph tree) prefix-tree->denoted-tree))

  (define* (prefix-tree->indent-tree-string a #:optional (base-depth 0) (indent-string "  "))
    "list:(string/list ...) -> string
    converts a list to a string of indented lines"
    (denoted-tree->indent-tree-string (prefix-tree->denoted-tree a base-depth)))

  (define* (read-space-indent-tree-string->denoted-tree port #:optional (indent-width 2))
    "port [integer] -> list:((integer . string) ...)
    reads a string with indented lines to a list where each elment contains indent-count and line content.
    approximates indent-width to the next higher indent depth if it is not a multiple of indent-width"
    (stream-fold
      (l (r e)
        (pair
          (let (prefix-count (string-skip e #\space))
            (if prefix-count
              (list (if (>= prefix-count 0) (ceiling (/ prefix-count indent-width)) prefix-count)
                (string-trim-right (string-drop e prefix-count) #\space))
              (list (if (null? r) 0 (first (first r))) "")))
          r))
      (list) (stream-reverse (port->line-stream port))))

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