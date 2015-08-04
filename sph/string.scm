; (sph string) - string-processing
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

(library (sph string)
  (export
    any->string
    any->string-write
    list->string-columns
    list-string-append-each
    parenthesise
    regexp-match-replace
    regexp-replace
    string-ascii->utf8
    string-camelcase->dashes
    string-case
    string-contains-any?
    string-contains-every?
    string-downcase-first
    string-drop-suffix
    string-each
    string-enclose
    string-equal?
    string-indices
    string-last-index
    string-longest-prefix
    string-lowercase?
    string-multiply
    string-numeric?
    string-octet-length
    string-quote
    string-remove-prefix
    string-replace-char
    string-replace-chars
    string-replace-string
    string-skip-string
    string-split-regexp
    string-trim-string
    symbol?->string
    tree-string-join)
  (import
    (guile)
    (ice-9 regex)
    (rnrs base)
    (sph)
    (only (rnrs bytevectors) u8-list->bytevector utf8->string))

  (define string-equal? string=)

  (define (tree-string-join a delimiter)
    (string-join (map (l (e) (if (list? e) (tree-string-join e delimiter) e)) a) delimiter))

  (define (regexp-replace str regexp replacement)
    "string string/regex string/procedure:{match-structure -> string} -> string
    replace all occurences of regexp in string with replacement"
    (regexp-substitute/global #f regexp str (q pre) replacement (q post)))

  (define (string-camelcase->dashes a)
    "string -> string
    aA -> a-a
    AA -> a-a
    aa AAa -> aa a-aa"
    (regexp-replace
      (regexp-replace a "(\\s|^)[A-Z]" (l (match) (string-downcase (match:substring match)))) "[A-Z]"
      (l (match) (string-append "-" (string-downcase (match:substring match))))))

  (define (string-downcase-first a)
    "string -> string
    AA -> aA
    Aa -> aa"
    (if (string-null? a) a
      (let (a (string->list a)) (list->string (pair (char-downcase (first a)) (tail a))))))

  (define (any->string a)
    "generalized string conversion function.
    get the string representation for the value of an object.
    symbols like \".\" are converted to \"#{.}\" using display."
    (if (symbol? a) (symbol->string a)
      (if (string? a) a (call-with-output-string (l (port) (display a port))))))

  (define (string-last-index a) (if (string-null? a) 0 (- (string-length a) 1)))
  (define (any->string-write a) (if (symbol? a) (symbol->string a) (object->string a write)))

  (define* (list->string-columns a #:key (justify (q left)) (separator "") (max-width 78))
    "(string ...) #:justify symbol #:separator string #:max-width integer -> string
    create a string of equally sized columns containing elements of the string list a, separated
    by #:separator"
    (string-join
      (map
        (let
          ( (column-width (inexact->exact (round (/ max-width (length a)))))
            (justify (if (eq? (q left) justify) string-pad-right string-pad)))
          (l (e) (justify e column-width)))
        a)
      separator))

  (define* (string-list-append-each a before #:optional after)
    "(string ...) string\\boolean-false [string\\boolean-false] -> list
    append strings before and after respectively to every element of string list a.
    may return a unmodified"
    (if before
      (map (if after (l (e) (string-append before e after)) (l (e) (string-append before e))) a)
      (if after (map (l (e) (string-append e after)) a) a)))

  (define (parenthesise a) "surround string with an open and a closing parenthesis"
    (string-append "(" a ")"))

  (define (string-ascii->utf8 str)
    "convert an ascii string with incorrectly coded utf8 chars to an utf8 string"
    (utf8->string (u8-list->bytevector (map char->integer (string->list str)))))

  (define-syntax-case (string-case-condition str str-n)
    ;auxillary syntax for string-case
    (let ((str-n-datum (syntax->datum (syntax str-n))))
      (if (string? str-n-datum) (syntax (string-equal? str str-n))
        (if (list? str-n-datum) (syntax (member str (quote str-n))) (syntax (member str str-n))))))

  (define-syntax-rule (string-case a (str-n expr ...) ... else-expr)
    ;required else expression
    ( (lambda (str)
        (cond ((string-case-condition str str-n) (begin expr ...)) ... (else else-expr)))
      a))

  (define (string-contains-any? a patterns)
    "result in a boolean indicating if string contains any of the patterns"
    (any (l (e) (string-contains a e)) patterns))

  (define (string-contains-every? a patterns)
    "result in a boolean indicating if string contains all of the patterns"
    (every (l (e) (string-contains a e)) patterns))

  (define string-each string-for-each)

  (define-syntax-rule (string-enclose str enclose-str)
    ;"append enclose-str to beginning and end of argument string"
    (string-append enclose-str str enclose-str))

  (define (string-longest-prefix a prefix-list)
    "string (string ...) -> string\\boolean
    result in the element of prefix-list that is the longest prefix of a relative to other
    elements from prefix-list"
    (fold
      (l (e prev)
        (if (string-prefix? e a) (if prev (if (> (string-length e) (string-length prev)) e prev) e)
          prev))
      #f prefix-list))

  (define (string-remove-prefix prefix a) (substring a (string-length prefix)))

  (define (string-lowercase? str) "test if //str// contains no uppercase characters"
    (not (string-any (l (c) (eq? (char-general-category c) (q Lu))) str)))

  (define (string-multiply a n) "string integer -> string" (apply string-append (make-list n a)))

  (define (string-numeric? a)
    "string -> boolean
    check if string is a valid scheme representation of a number"
    (if (string->number a) #t #f))

  (define (string-replace-chars str spec)
    "spec := ((char [replacements] ...) ...)
    replace chars in string with none, one or multiple chars"
    (list->string
      (string-fold-right
        (l (e prev)
          (let ((info (assoc e spec)))
            (if (and info (not (null? info))) (append (tail info) prev) (pair e prev))))
        (list) str)))

  (define (string-quote str) "string -> string"
    "enclose a string with \" or ' quotes, depending on if the string already
    includes one of these. preferring \". results in false if string already contains both type of quotes."
    (if (string-contains str "\"") (if (string-contains str "'") #f (string-enclose str "'"))
      (string-enclose str "\"")))

  (define* (string-split-regexp str regexp #:optional (handle-delim (q discard)))
    "string string symbol -> list
    split string into a list of substrings delimited by a regular expression."
    ;1. all regexp matches are collected
    ;2. substrings between matches are extracted
    (let (matches (list-matches regexp str))
      (if (null? matches) (list str)
        (call-with-values
          (l ()
            (if (eqv? (q discard) handle-delim)
              (values (l (e prev) (substring str (match:end prev) (match:start e)))
                (substring str 0 (match:start (first matches))))
              (if (eqv? (q concat) handle-delim)
                (values (l (e prev) (substring str (match:end prev) (match:end e)))
                  (substring str 0 (match:end (first matches))))
                (throw (q wrong-argument-for-handle-delim) handle-delim))))
          (l (get-substring init-parts)
            (let loop ((rest (tail matches)) (parts (list init-parts)) (prev (first matches)))
              (if (null? rest) (reverse (pair (substring str (match:end prev)) parts))
                (loop (tail rest) (pair (get-substring (first rest) prev) parts) (first rest)))))))))

  (define (string-skip-string a skip)
    (let (skip-length (string-length skip))
      (let loop ((r #f) (prev (string-contains a skip)))
        (if (and prev (or (not r) (= (- prev skip-length) r)))
          (loop prev (string-contains a skip (+ skip-length prev)))
          (if r (min (- (string-length a) 1) (+ 1 skip-length r)) r)))))

  (define (string-trim-string a trim)
    (let (skip-index (string-skip-string a trim))
      (if skip-index (string-drop a (if (= 0 skip-index) skip-index (- skip-index 1))) a)))

  (define (string-octet-length str)
    "string -> integer
    the number of bytes of string, regardless of the character encoding, without terminator like null"
    (* (string-bytes-per-char str) (string-length str)))

  (define (string-indices a search-string)
    "string string -> (integer ...)
    result in a list of indices at which search-string occurs in a"
    (let ((str-length (string-length search-string)) (a-length (string-length a)))
      (reverse
        (let loop ((index (string-contains a search-string)) (r (list)))
          (if index
            (if (= index a-length) (pair index r)
              (loop (string-contains a search-string (+ index (max 1 str-length))) (pair index r)))
            r)))))

  (define (string-replace-string a replace replacement)
    "string string string -> string
    replace all occurences of string \"replace\" inside string a with string \"replacement\".
    tests with guile 2.06 have shown it to be 22x faster than regexp-substitute/global"
    ;; this procedure is quite nice to debug - comment out one or all string-copy! applications,
    ;; and the result string will be a partial result.
    ;get match positions
    (let (indices (string-indices a replace))
      (if (null? indices) a
        (let
          ( (replace-length (string-length replace))
            (replacement-length (string-length replacement)) (a-length (string-length a)))
          ;calculate result string size and create result string
          (let ((r-length (+ a-length (* (length indices) (- replacement-length replace-length)))))
            (let (r (make-string r-length))
              ;each match index, copy characters before match-end to the result string
              (let loop
                ((r-index 0) (prev-index 0) (cur-index (first indices)) (rest (tail indices)))
                (string-copy! r r-index a prev-index cur-index)
                (let (r-index (- r-index (- prev-index cur-index)))
                  (string-copy! r r-index replacement)
                  (if (null? rest)
                    (begin
                      (if (< (+ cur-index replace-length) a-length)
                        (string-copy! r (+ r-index replacement-length)
                          a (+ cur-index replace-length) a-length))
                      r)
                    (loop (+ r-index replacement-length) (+ cur-index replace-length)
                      (first rest) (tail rest)))))))))))

  (define (string-replace-char a char replacement) "string character character -> string"
    (string-map (l (e) (if (eqv? e char) replacement e)) a))

  (define (string-drop-suffix str suffix) (string-drop-right str (string-length suffix)))

  (define (symbol?->string a)
    "any -> any
    converts a to string only if it is a symbol"
    (if (symbol? a) (symbol->string a) a))

  (define (regexp-match-replace a replacements)
    "string (regexp . string:replacement)/(regexp string:search-string . string:replacement) ... -> string
    replace strings inside string portions matched by regular expressions"
    (fold
      (l (e r)
        (fold-matches (first e) r
          r
          (l (match r)
            (if (pair? (tail e))
              (string-replace-string r (match:substring match)
                (string-replace-string (match:substring match) (first (tail e)) (tail (tail e))))
              (string-replace-string r (match:substring match) (tail e))))))
      a replacements)))