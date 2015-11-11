; (sph documentation) - retrieve or display documentation from guile scheme libraries
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

(library (sph documentation)
  (export
    default-display-format
    default-format-arguments
    documentation-display-formats
    format-module-documentation
    itpn-docstring-split-signature)
  (import
    (guile)
    (ice-9 peg)
    (rnrs base)
    (sph)
    (sph alist)
    (sph binding-info)
    (sph lang parser type-signature)
    (sph module)
    (sph record)
    (only (ice-9 regex) regexp-substitute/global)
    (only (rnrs sorting) list-sort)
    (only (sph string) string-multiply)
    (only (srfi srfi-1) remove))

  (define (default-format-arguments arguments type)
    "pair/list:alist symbol:\"procedure\"/\"syntax\"/\"variable\" -> string
    formats argumets in sph type-signature notation format.
    arguments as retrieved by \"module-binding-info\", which uses (ice-9 session) \"procedure-arguments\""
    ;syntax arguments can be pairs
    (if
      (and (eqv? (q procedure) type) (list? arguments)
        (not (null? arguments)) (pair? (first arguments)))
      (alist-quoted-bind arguments (optional rest required keyword allow-other-keys?)
        (let
          ( (optional-string
              (if (or (not optional) (null? optional)) ""
                (string-append "[" (string-join (map symbol->string optional) " ") "]")))
            (required-string
              (if (or (not required) (null? required)) ""
                (string-join (map symbol->string required) " ")))
            (keyword-string
              (if (or (not keyword) (null? keyword)) ""
                (string-drop-right (string-drop (simple-format #f "~S" (map first keyword)) 1) 1)))
            (rest-string
              (if (or (not rest) (null? rest)) "" (string-append (symbol->string rest) " ..."))))
          (string-join
            (filter (l (ele) (not (string-null? ele)))
              (list required-string keyword-string optional-string rest-string))
            " ")))
      (if (equal? (q syntax) type)
        (let ((res (simple-format #f "~S" arguments)))
          (string-append (substring res 1 (- (string-length res) 1))))
        (if (equal? (q variable) type)
          (if arguments (call-with-output-string (l (port) (display arguments port))) "")
          (raise (q unknown-binding-type))))))

  (define (docstring->lines arg)
    (let (arg (regexp-substitute/global #f " +" arg (q pre) " " (q post)))
      (map (l (arg) (string-trim arg)) (string-split arg #\newline))))

  (define itpn-indent (string #\space #\space))

 (define (itpn-docstring-split-signature arg continue)
    "string procedure:{string:type-signatures string:rest-of-docstring} -> any
    if a string starts with a type-signature, split string at the end of it"
    (if arg
      (let (signature (match-pattern peg-type-signature arg))
        (if signature
          (continue
            (parsed-type-signature->string (peg:tree signature)
              (string-append itpn-indent itpn-indent))
            (docstring->lines (string-drop arg (peg:end signature))))
          (continue #f (docstring->lines arg))))
      (continue #f (list))))

  (define (lines->docstring arg indent)
    (let (arg (remove string-null? arg))
      (if (null? arg) ""
        (string-append indent "description"
          (string-join arg (string-append "\n" indent indent) (q prefix)) "\n"))))

  (define-as display-format-itpn
    ;this defines the default formatter
    alist-quoted format-arguments
    default-format-arguments format-binding-info
    (l (bi formatted-arguments) "vector:record string -> string"
      (string-append (symbol->string (bi-name bi)) "\n"
        (itpn-docstring-split-signature (bi-documentation bi)
          (l (signature text-lines)
            (string-append
              (if (or signature (not (string-null? formatted-arguments)))
                (string-append itpn-indent "signature\n"
                  itpn-indent itpn-indent
                  (if (string-null? formatted-arguments) formatted-arguments
                    (string-append formatted-arguments "\n"))
                  (if signature (string-append itpn-indent itpn-indent signature "\n") ""))
                "")
              (lines->docstring text-lines itpn-indent))))
        itpn-indent "type: " (symbol->string (bi-type bi))))
    format-module-documentation
    (l (module-name md) "any (string ...) -> string" (string-join md "\n"))
    format-modules-documentation (l (mds) "(string ...) -> string" (apply string-append mds)))

  (define-as documentation-display-formats alist-quoted
    default display-format-itpn itpn display-format-itpn)

  (define* (format-module-documentation module-names #:optional (format-handler-name (q default)))
    "((symbol ...) ...)/(symbol ...) symbol ->
    output documentation for a list of module-names. format-handler-name can currently either be
    default, plaintext, itpn, dokuwiki"
    (let
      ( (format-handler
          (or (assoc-ref documentation-display-formats format-handler-name)
            (throw (q no-such-display-format))))
        (module-names (if (list? (first module-names)) module-names (list module-names))))
      (alist-quoted-bind format-handler
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
            module-names))))))