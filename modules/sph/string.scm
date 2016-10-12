; (sph string) - string processing
; written for the guile scheme interpreter
; Copyright (C) 2010-2016 sph <sph@posteo.eu>
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
    any->string-display
    any->string-pretty-print
    any->string-write
    any->string-write*
    list->string-columns
    list-string-append-each
    parenthesise
    regexp-match-replace
    regexp-replace
    string-ascii->utf8
    string-camelcase->dashes
    string-case
    string-contains-any?
    string-contains-char?
    string-contains-every?
    string-downcase-first
    string-compress-space
    string-drop-prefix
    string-drop-prefix-if-exists
    string-drop-suffix
    string-drop-suffix-if-exists
    string-each
    string-enclose
    string-equal?
    string-fill-left
    string-fill-right
    string-indices
    string-indices-char
    string-join-tree
    string-last-index
    string-longest-prefix
    string-lowercase?
    string-multiply
    string-numeric?
    string-octet-length
    string-quote
    string-replace-char
    string-replace-chars
    string-replace-string
    string-skip-string
    string-slice-at-words
    string-split-regexp
    string-trim-string
    symbol?->string)
  (import
    (guile)
    (ice-9 pretty-print)
    (ice-9 regex)
    (rnrs base)
    (sph)
    (only (rnrs bytevectors) u8-list->bytevector utf8->string)
    (only (sph list) fold-multiple))

  (define string-equal? string=)

  (define (string-contains-char? a char) "string character -> boolean"
    (if (string-index a char) #t #f))

  (define (any->string-write* a)
    "write converts symbols like \".\" to #{.}, this procedure avoids this"
    (if (symbol? a) (symbol->string a) (any->string-write a)))

  (define (any->string-write a) (object->string a write))
  (define (any->string-pretty-print a) (object->string a pretty-print))
  (define (any->string-display a) (object->string a display))

  (define (string-slice-at-words a slice-length)
    "string integer -> (string ...)
    split line into slices/chunks of size slice-length, unless it would split words (subsequent characters without the space character),
    in which case the word is moved to the next slice. ignores newlines. can be used for single lines. can be used for \"text-wrapping\""
    ;splits string at spaces, then uses the parts to build lines while checking if the line length with spaces would exceed slice-length.
    ;if yes, the exceeding part is moved to the next line.
    (let
      ( (words (string-split a #\space))
        (prepend-line (l (e r) (pair (string-join (reverse e) " ") r))))
      (let
        (r
          (fold-multiple
            (l (word-length words line line-spaces-length current-slice-length r)
              (if (> (+ word-length line-spaces-length current-slice-length) slice-length)
                (list (tail words) (list (first words)) 0 word-length (prepend-line line r))
                (list (tail words) (pair (first words) line)
                  (+ 1 line-spaces-length) (+ word-length current-slice-length) r)))
            (map string-length words) words (list) 0 0 (list)))
        (apply
          (l (words line line-spaces-length current-slice-length r) (reverse (prepend-line line r)))
          r))))

  (define (string-join-tree a delimiter)
    "(list/string ...) string -> string
    same as (string-join (flatten a) delimiter)"
    (string-join (map (l (e) (if (list? e) (string-join-tree e delimiter) e)) a) delimiter))

  (define (regexp-replace str regexp replacement)
    "string string/regexp string/procedure:{match-structure -> string} -> string
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
    "any -> string
    generalized string conversion function.
    get the string representation for the value of an object.
    symbols like \".\" are converted to \"#{.}\" using display."
    (if (string? a) a
      (if (symbol? a) (symbol->string a) (call-with-output-string (l (port) (display a port))))))

  (define (string-last-index a)
    "string -> integer
    get the last possible index of a string"
    (if (string-null? a) 0 (- (string-length a) 1)))

  (define* (list->string-columns a #:key (justify (q left)) (separator "") (max-width 78))
    "(string ...) #:justify symbol:left/right #:separator string #:max-width integer -> string
    create a string of equally sized columns containing elements of the string list \"a\", separated
    by #:separator"
    (string-join
      (map
        (let
          ( (column-width (inexact->exact (round (/ max-width (length a)))))
            (justify (if (eqv? (q left) justify) string-pad-right string-pad)))
          (l (e) (justify e column-width)))
        a)
      separator))

  (define* (string-list-append-each a before #:optional after)
    "(string ...) string/boolean-false [string/boolean-false] -> (string ...)
    append strings before and after respectively to every element of string list a.
    may return a unmodified"
    (if before
      (map (if after (l (e) (string-append before e after)) (l (e) (string-append before e))) a)
      (if after (map (l (e) (string-append e after)) a) a)))

  (define (parenthesise a)
    "string -> string
    surround string with an open and a closing parenthesis"
    (string-append "(" a ")"))

  (define (string-ascii->utf8 a)
    "string -> string
    convert an ascii string that has incorrectly coded utf8 chars to an utf8 string"
    (utf8->string (u8-list->bytevector (map char->integer (string->list a)))))

  (define-syntax-case (string-case-condition a a-n)
    ;auxillary syntax for string-case
    (let ((a-n-datum (syntax->datum (syntax a-n))))
      (if (string? a-n-datum) (syntax (string-equal? a a-n))
        (if (list? a-n-datum) (syntax (member a (quote a-n))) (syntax (member a a-n))))))

  (define-syntax-rule (string-case a (a-n expr ...) ... else-expr)
    ;requires an else expression
    ((lambda (b) (cond ((string-case-condition b a-n) expr ...) ... (else else-expr))) a))

  (define (string-contains-any? a patterns)
    "string (string ...) -> boolean
    result in a boolean indicating if string contains any of the patterns"
    (any (l (e) (string-contains a e)) patterns))

  (define (string-contains-every? a patterns)
    "string (string ...) -> boolean
    result in a boolean indicating if string contains all of the patterns"
    (every (l (e) (string-contains a e)) patterns))

  (define string-each string-for-each)

  (define-syntax-rule (string-enclose str enclose-str)
    ;"append enclose-str to beginning and end of argument string"
    (string-append enclose-str str enclose-str))

  (define (string-longest-prefix a prefix-list)
    "string (string ...) -> string/boolean
    result in the element of prefix-list that is the longest prefix from prefix-list in string \"a\""
    (fold
      (l (e prev)
        (if (string-prefix? e a) (if prev (if (> (string-length e) (string-length prev)) e prev) e)
          prev))
      #f prefix-list))

  (define (string-drop-prefix prefix a) "string string -> string"
    (string-drop a (string-length prefix)))

  (define (string-drop-prefix-if-exists prefix a)
    "string string -> string
    remove prefix if string has prefix"
    (if (string-prefix? prefix a) (string-drop-prefix prefix a) a))

  (define (string-drop-suffix-if-exists suffix a)
    "string string -> string
    remove suffix if string has suffix"
    (if (string-suffix? suffix a) (string-drop-suffix suffix a) a))

  (define (string-lowercase? a) "test if a string contains no uppercase characters"
    (not (string-any (l (c) (eqv? (char-general-category c) (q Lu))) a)))

  (define (string-multiply a n) "string integer -> string" (apply string-append (make-list n a)))

  (define (string-numeric? a)
    "string -> boolean
    check if string is a valid scheme representation of a number"
    (if (string->number a) #t #f))

  (define (string-replace-chars a spec)
    "string ((char [replacements] ...) ...) -> string
    replace chars in string with none, one or multiple chars"
    (list->string
      (string-fold-right
        (l (e prev)
          (let ((info (assoc e spec)))
            (if (and info (not (null? info))) (append (tail info) prev) (pair e prev))))
        (list) a)))

  (define (string-compress-space a)
    "string -> string
    replace multiple subsequent space characters with one space"
    (regexp-replace a " {2,}" " "))

  (define (string-quote a) "string -> string"
    "enclose a string with \" or ' quotes, depending on if the string already
    includes one of these. preferring \". results in false if string already contains both type of quotes."
    (if (string-contains a "\"") (if (string-contains a "'") #f (string-enclose a "'"))
      (string-enclose a "\"")))

  (define* (string-split-regexp str regexp #:optional (handle-delim (q discard)))
    "string string [symbol:discard/concat] -> (string ...)
    split string into a list of substrings delimited by a regular expression."
    ;1. all regexp matches are collected
    ;2. substrings between matches are extracted
    (let (matches (list-matches regexp str))
      (if (null? matches) (list str)
        (call-with-values
          (thunk
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
    "string string -> integer
    skip over string \"skip\" at the beginning of a string and return the first index afterwards,
    or the first index 0 if skip string is not a prefix"
    (let (skip-length (string-length skip))
      (let loop ((r #f) (prev (string-contains a skip)))
        (if (and prev (or (not r) (= (- prev skip-length) r)))
          (loop prev (string-contains a skip (+ skip-length prev)))
          (if r (min (- (string-length a) 1) (+ 1 skip-length r)) r)))))

  (define (string-trim-string a trim)
    "string string -> string
    remove all occurences of string \"trim\" from the beginning of a string.
    like string-trim but with a trim-string instead of a character"
    (let (skip-index (string-skip-string a trim))
      (if skip-index (string-drop a (if (= 0 skip-index) skip-index (- skip-index 1))) a)))

  (define (string-octet-length a)
    "string -> integer
    the number of bytes of string, regardless of the character encoding, without terminator like \"null\""
    (* (string-bytes-per-char a) (string-length a)))

  (define (string-indices a search-string)
    "string string -> (integer ...)
    result in a list of indices at which search-string occurs in a string"
    (let ((search-string-length (string-length search-string)) (a-length (string-length a)))
      (let loop ((index (string-contains a search-string)))
        (if index
          (if (= index a-length) (list index)
            (pair index
              (loop (string-contains a search-string (+ index (max 1 search-string-length))))))
          (list)))))

  (define (string-indices-char a search-char)
    "string char -> (integer ...)
    create a list of indices at which search-char occurs in a string"
    (let (a-length (string-length a))
      (let loop ((index 0))
        (if (< index a-length)
          (if (eqv? search-char (string-ref a index)) (pair index (loop (+ 1 index)))
            (loop (+ 1 index)))
          (list)))))

  (define (string-replace-string a replace replacement)
    "string string string -> string
    replace all occurences of string \"replace\" inside string \"a\" with string \"replacement\".
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

  (define (string-replace-char a char replacement)
    "string character character -> string
    replace all occurences of \"char\" in a string"
    (string-map (l (e) (if (eqv? e char) replacement e)) a))

  (define (string-drop-suffix suffix a) "string string -> string"
    (string-drop-right a (string-length suffix)))

  (define (symbol?->string a)
    "any -> any
    converts \"a\" to string only if it is a symbol, otherwise results in \"a\""
    (if (symbol? a) (symbol->string a) a))

  (define (string-fill-right a target-length character)
    (let (count (- target-length (string-length a)))
      (if (> count 0) (string-append a (list->string (make-list count character))) a)))

  (define (string-fill-left a target-length character)
    "string character integer -> string
    prepend character to the given string until the string length equals target-length.
    examples
        (string-fill-left \"1\" #\0 2) -> \"01\"
        (string-fill-left \"10\" #\0 2) -> \"10\"
    string-fill-left-zeros"
    (let (count (- target-length (string-length a)))
      (if (> count 0) (string-append (list->string (make-list count character)) a) a)))

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
