(library (sph system reader)
  (export
    default-sharp-token-reader-with-comments
    read-for-formatting
    read-with-comments
    sph-read
    sph-read-sharp-token-readers
    sph-read-token-readers
    sph-read-with-upper-case-symbols
    sph-system-reader-description)
  (import
    (ice-9 peg)
    (rnrs base)
    (sph)
    (system reader)
    (except (rnrs io simple) read)
    (only (guile) unread-char char->integer)
    (only (ice-9 rdelim) read-line)
    (only (sph read-write) read-until-string-proc)
    (only (srfi srfi-1) reverse))

  (define sph-system-reader-description "a scheme reader that can include comments")

  (define (read-range-expr->string port start end)
    "calls proc for all characters that are not start or end and builds a nested list with the results of proc.
    this procedure assumes that the first char has already been read"
    (list->string
      (reverse
        (let loop ((char (read-char port)) (depth 0) (r (list)))
          (if (eof-object? char) r
            (if (eqv? start char)
              (loop (read-char port) (+ 1 depth) (if (= 0 depth) r (pair char r)))
              (if (eqv? end char)
                (if (= depth 1) r (loop (read-char port) (- depth 1) (pair char r)))
                (loop (read-char port) depth (if (= depth 0) r (pair char r))))))))))

  (define (read-range-without-nesting port end escape)
    "assumes that the start char has already been read"
    (list->string
      (reverse
        (let loop ((char (read-char port)) (escape? #f) (r (list)))
          (if (eof-object? char) r
            (if escape? (loop (read-char port) #f (pair char r))
              (if (eqv? end char) r (loop (read-char port) (eqv? escape char) (pair char r)))))))))

  (define (read-range-expr->list port start end proc)
    "calls proc for all characters that are not start or end and builds a nested list with the results of proc"
    (let loop ((char (read-char port)) (level (list)))
      (if (or (eqv? end char) (eof-object? char)) (reverse level)
        (let
          (level
            (if (eqv? start char) (pair (loop (read-char port) (list)) level)
              (proc char port level)))
          (loop (read-char port) level)))))

  (define read-scsh-block-comment (read-until-string-proc "!#"))

  (define string-literal
    (make-token-reader #\"
      (l (char port reader toplevel-reader) (read-range-without-nesting port #\" #\\))))

  (define semicolon-comment
    (make-token-reader #\;
      (l (char port reader toplevel-reader) (list (q semicolon-comment) (read-line port)))))

  (define range-comment
    (make-token-reader #\;
      (l (char port reader toplevel-reader)
        (list (q range-comment) (read-range-expr->string port #\( #\))))))

  (define hash-bang
    (make-token-reader #\!
      (l (char port reader toplevel-reader)
        (list (q hash-bang) (let (r (read-line port)) (read-line port) r)))))

  (define scsh-block-comment
    (make-token-reader #\!
      (l (char port reader toplevel-reader)
        (list (q scsh-block-comment) (first (read-scsh-block-comment port))))))

  (define boolean
    (make-token-reader (list #\f #\t) (l (char port reader toplevel-reader) (eqv? #\t char))))

  (define read-paren-expr
    (make-token-reader #\(
      (l (char port reader toplevel-reader)
        (read-range-expr->list port #\(
          #\)
          (l (char port level)
            (if (< (char->integer char) 33) level
              (begin (unread-char char port) (pair (toplevel-reader port) level))))))))

  (define (sph-read-sharp-token-readers)
    (pairs hash-bang boolean
      (map standard-token-reader (q (srfi62-sexp-comment character number+radix keyword vector)))))

  (define (sph-read-token-readers)
    (pair read-paren-expr
      (map standard-token-reader
        (q
          (whitespace guile-symbol-lower-case guile-symbol-misc-chars
            string guile-number semicolon-comment)))))

  (define sph-read
    (make-reader
      (pair (make-token-reader #\# (make-reader (sph-read-sharp-token-readers)))
        (sph-read-token-readers))))

  (define sph-read-with-upper-case-symbols
    (make-reader
      (pair (make-token-reader #\# (make-reader (sph-read-sharp-token-readers)))
        (pair (standard-token-reader (q guile-symbol-upper-case)) (sph-read-token-readers)))))

  (define (default-sharp-token-reader-with-comments)
    (make-token-reader #\#
      (make-reader
        (append (default-sharp-reader-token-readers) (list range-comment scsh-block-comment)))))

  (define read-with-comments
    (make-reader
      (append (default-reader-token-readers)
        (list read-paren-expr string-literal
          semicolon-comment (default-sharp-token-reader-with-comments)))))

  (define read-for-formatting
    (make-reader
      (pairs read-paren-expr string-literal
        semicolon-comment (default-sharp-token-reader-with-comments)
        (map standard-token-reader
          (q
            (whitespace guile-symbol-lower-case guile-symbol-misc-chars
              guile-number quote-quasiquote-unquote guile-symbol-upper-case)))))))
