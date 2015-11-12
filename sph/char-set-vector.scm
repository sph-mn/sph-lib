;redefines guiles preset char-sets that are special objects as vectors

(library (sph char-set-vector)
  (export
    char-set-vector:ascii
    char-set-vector:blank
    char-set-vector:designated
    char-set-vector:digit
    char-set-vector:empty
    char-set-vector:full
    char-set-vector:graphic
    char-set-vector:hex-digit
    char-set-vector:iso-control
    char-set-vector:letter
    char-set-vector:letter+digit
    char-set-vector:lower-case
    char-set-vector:printing
    char-set-vector:punctuation
    char-set-vector:symbol
    char-set-vector:title-case
    char-set-vector:upper-case
    char-set-vector:whitespace)
  (import
    (rnrs base)
    (sph)
    (prefix
      (only (guile)
        char-set:lower-case
        char-set:upper-case
        char-set:title-case
        char-set:letter
        char-set:digit
        char-set:letter+digit
        char-set:graphic
        char-set:printing
        char-set:whitespace
        char-set:blank
        char-set:iso-control
        char-set:punctuation
        char-set:symbol
        char-set:hex-digit
        char-set:ascii
        char-set:empty
        char-set:designated
        char-set:full
        char-set->list
        list->vector)
      guile-))

  (define-syntax-rule (define-char-set-vector name variable)
    (define name (guile-list->vector (guile-char-set->list variable))))

  (define-char-set-vector char-set-vector:lower-case guile-char-set:lower-case)
  (define-char-set-vector char-set-vector:upper-case guile-char-set:upper-case)
  (define-char-set-vector char-set-vector:title-case guile-char-set:title-case)
  (define-char-set-vector char-set-vector:letter guile-char-set:letter)
  (define-char-set-vector char-set-vector:digit guile-char-set:digit)
  (define-char-set-vector char-set-vector:letter+digit guile-char-set:letter+digit)
  (define-char-set-vector char-set-vector:graphic guile-char-set:graphic)
  (define-char-set-vector char-set-vector:printing guile-char-set:printing)
  (define-char-set-vector char-set-vector:whitespace guile-char-set:whitespace)
  (define-char-set-vector char-set-vector:blank guile-char-set:blank)
  (define-char-set-vector char-set-vector:iso-control guile-char-set:iso-control)
  (define-char-set-vector char-set-vector:punctuation guile-char-set:punctuation)
  (define-char-set-vector char-set-vector:symbol guile-char-set:symbol)
  (define-char-set-vector char-set-vector:hex-digit guile-char-set:hex-digit)
  (define-char-set-vector char-set-vector:ascii guile-char-set:ascii)
  (define-char-set-vector char-set-vector:empty guile-char-set:empty)
  (define-char-set-vector char-set-vector:designated guile-char-set:designated)
  (define-char-set-vector char-set-vector:full guile-char-set:full))
