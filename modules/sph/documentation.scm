; written for the guile scheme interpreter
; Copyright (C) 2010-2017 sph <sph@posteo.eu>
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

(library (sph documentation)
  (export
    default-format-arguments
    display-module-information-short
    docstring->lines
    docstring-split-signature
    documentation-display-formats
    format-module-documentation
    lines->docstring
    module-description
    module-find-one-information
    module-find-one-information-sorted
    sort-module-information
    sph-documentation-description)
  (import
    (guile)
    (ice-9 peg)
    (ice-9 threads)
    (rnrs base)
    (sph)
    (sph alist)
    (sph binding-info)
    (sph lang indent-syntax)
    (sph lang parser type-signature)
    (sph module)
    (sph record)
    (sph tree)
    (only (ice-9 regex) regexp-substitute/global)
    (only (rnrs sorting) list-sort)
    (only (sph list) fold-multiple)
    (only (sph string) string-multiply string-equal?)
    (only (srfi srfi-1) remove filter-map))

  (define sph-documentation-description "extract and display guile scheme code documentation")

  (define (docstring->lines a) "string -> (string ...)"
    (let (a (regexp-substitute/global #f " +" a (q pre) " " (q post)))
      (map (l (a) (string-trim a)) (string-split a #\newline))))

  (define (docstring-split-signature a line-prefix c)
    "string string procedure:{string:type-signatures string:rest-of-docstring} -> any
     if a string starts with a type-signature, split string at the end of it"
    (if a
      (let (signature (match-pattern peg-type-signature a))
        (if signature
          (c
            (parsed-type-signature->string (type-signature-simplify-tree (peg:tree signature))
              line-prefix)
            (docstring->lines (string-drop a (peg:end signature))))
          (c #f (docstring->lines a))))
      (c #f (list))))

  (define (lines->docstring a indent) "list (string ...) -> string"
    (let (a (remove string-null? a))
      (if (null? a) ""
        (string-append indent "description"
          (string-join a (string-append "\n" indent indent) (q prefix)) "\n"))))

  (define (replace-underscore a state)
    (if (string-equal? "_" a) (number->string (+ 10 state) 32) a))

  (define (list-replace-underscores& a state c) "call (c list-with-replacements updated-state)"
    (apply c
      ( (l (a) (pair (reverse (first a)) (tail a)))
        (fold-multiple
          (l (a result state) (list (pair (replace-underscore a state) result) (+ 1 state))) a
          (list) state))))

  (define (replace-underscores& required optional c)
    (list-replace-underscores& (map symbol->string required) 0
      (l (required state)
        (list-replace-underscores& (map symbol->string optional) state
          (l (optional state) (c required optional state))))))

  (define (default-format-arguments arguments type)
    "pair/list:alist symbol:\"procedure\"/\"syntax\"/\"variable\" -> string
     formats argumets in sph type-signature notation format.
     arguments as retrieved by \"module-binding-info\", which uses (ice-9 session) \"procedure-arguments\""
    ;syntax arguments can be pairs
    (if
      (and (eqv? (q procedure) type) (list? arguments)
        (not (null? arguments)) (pair? (first arguments)))
      (alist-bind arguments (optional rest required keyword allow-other-keys?)
        (replace-underscores& required optional
          (l (required optional state)
            (let
              ( (optional-string
                  (if (or (not optional) (null? optional)) ""
                    (string-append "[" (string-join optional " ") "]")))
                (required-string
                  (if (or (not required) (null? required)) "" (string-join required " ")))
                (keyword-string
                  (if (or (not keyword) (null? keyword)) ""
                    (string-drop-right (string-drop (simple-format #f "~S" (map first keyword)) 1)
                      1)))
                (rest-string
                  (if (or (not rest) (null? rest)) ""
                    (string-append (replace-underscore (symbol->string rest) state) " ..."))))
              (let
                (signature-string
                  (string-append
                    (string-join
                      (filter (l (a) (not (string-null? a)))
                        (list required-string keyword-string optional-string rest-string))
                      " ")
                    " ->"))
                signature-string)))))
      (if (equal? (q syntax) type)
        (let ((r (simple-format #f "~S" arguments)))
          (string-append (substring r 1 (- (string-length r) 1))))
        (if (equal? (q variable) type)
          (if arguments (call-with-output-string (l (port) (display arguments port))) "") ""))))

  (define documentation-display-formats (list))

  (define* (format-module-documentation module-names #:optional (format-handler-name (q default)))
    "((symbol ...) ...)/(symbol ...) symbol ->
     output documentation for a list of module-names. format-handler-name can currently either be
     default, itpn.
     for just retrieving module documentation you might want to consider (sph binding-info)"
    (let
      ( (format-handler
          (or (assoc-ref documentation-display-formats format-handler-name)
            (throw (q no-such-display-format) format-handler-name)))
        (module-names (if (list? (first module-names)) module-names (list module-names))))
      (alist-bind format-handler
        (format-arguments format-binding-info format-module-documentation
          format-modules-documentation)
        (format-modules-documentation
          (map
            (l (module-name)
              (format-module-documentation module-name
                (map
                  (l (binding-info)
                    (format-binding-info binding-info
                      (format-arguments (bi-arguments binding-info) (bi-type binding-info))))
                  (sort-module-binding-info (module-binding-info module-name)))))
            module-names)))))

  (define (module-description name)
    "(symbol ...) -> false/string
     get the module description from an exported variable with a specific name.
     (a b c) -> a-b-c-description"
    (and-let*
      ( (a
          (false-if-exception
            (module-ref (resolve-module name)
              (string->symbol
                (string-append (string-join (map symbol->string name) "-") "-description"))))))
      ; remove excess indentation
      (let (a-tree (indent-tree->denoted-tree a))
        (denoted-tree->indent-tree
          (if (>= 1 (length a-tree)) a-tree
            (pair (first a-tree) (denoted-tree-minimise-depth (tail a-tree))))))))

  (define (sort-module-information a)
    (let
      (b
        (map
          (l (a)
            (alist-bind a (name)
              (pair (if name (apply string-append (map symbol->string name)) "") a)))
          a))
      (map tail (list-sort (l (a b) (string<? (first a) (first b))) b))))

  (define (module-find-one-information . search-path)
    (map
      (l (a) (alist-q name (first a) full-path (tail a) description (module-description (first a))))
      (apply append (filter-map (l (a) (false-if-exception (module-find a))) search-path))))

  (define (module-find-one-information-sorted . search-path)
    (sort-module-information (apply module-find-one-information search-path)))

  (define* (display-module-information-short a #:optional markdown?)
    (let
      ( (get-first-line
          (l (a)
            (let (index (string-index a #\newline))
              (if index (string-trim-right (string-take a index) #\.) a)))))
      (each
        (l (a)
          (alist-bind a (name description)
            (if markdown? (display "* ")) (display name)
            (if description (begin (display " - ") (display (get-first-line description)))))
          (newline))
        a))))
