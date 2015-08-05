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
    binding-info-layout
    default-display-format
    default-format-arguments
    doc-arguments
    doc-documentation
    doc-name
    doc-set-arguments!
    doc-type
    documentation-display-formats
    format-module-documentation
    its-docstring-split-signature
    macro->binding-info
    module-binding-info
    module-interface-fold
    procedure->binding-info
    sort-module-binding-info)
  (import
    (guile)
    (ice-9 peg)
    (rnrs base)
    (sph)
    (sph alist)
    (sph lang parser type-signature)
    (sph record)
    (only (ice-9 regex) regexp-substitute/global)
    (only (ice-9 session) procedure-arguments)
    (only (rnrs sorting) list-sort)
    (only (sph string) string-multiply)
    (only (srfi srfi-1) remove))

  ;specifies if the current values of the variables should be included in the documentation.
  ;might be interesting but can be a security issue for publicly accessible documentation
  (define doc-include-variable-values #f)
  ;record and accessors definition of binding-info. bi - binding-info
  ;the arguments field stores the current-value if it is a variable and doc-include-variable-values is true
  (define binding-info-layout (make-record-layout (quote (name type documentation arguments))))

  (define-record-accessors binding-info-layout (doc-name (q name))
    (doc-type (q type)) (doc-documentation (q documentation)) (doc-arguments (q arguments)))

  (define-record-setters binding-info-layout (doc-set-arguments! (q arguments)))

  (define (macro-arguments name type transformer)
    "symbol symbol macro-transformer -> argument-spec
    get the formal arguments specification for a macro"
    (case type
      ( (syntax-rules)
        (let (patterns (procedure-property transformer (q patterns)))
          (if (pair? patterns) (car patterns) (list))))
      ((identifier-syntax) (list)) (else (list))))

  (define (module-interface-fold proc init module)
    "procedure:{name any:value any:init} any (symbol ...) -> any
    fold over the exported bound variables for the given module name"
    (hash-fold
      (l (key val prev) (if (variable-bound? val) (proc key (variable-ref val) prev) prev)) init
      (module-obarray (resolve-interface module))))

  (define* (procedure->binding-info proc #:optional (name (procedure-name proc)))
    "procedure [string] -> vector"
    (record binding-info-layout name
      (q procedure) (procedure-documentation proc) (procedure-arguments proc)))

  (define (macro->binding-info macro name) "macro-variable -> vector"
    (let (transformer (macro-transformer macro))
      (record binding-info-layout name
        (q syntax) (procedure-documentation transformer)
        (macro-arguments name (procedure-property transformer (q macro-type)) transformer))))

  (define (variable->binding-info name value) "string any -> vector"
    (record binding-info-layout name (q variable) #f (and doc-include-variable-values value)))

  (define module-binding-info
    (let
      ( (get-info
          (l (name val prev) "-> record"
            (if (procedure? val) (pair (procedure->binding-info val name) prev)
              (if (macro? val) (pair (macro->binding-info val name) prev)
                (pair (variable->binding-info name val) prev))))))
      (lambda (module-name) "get properties of all exported bindings for the given module"
        (module-interface-fold get-info (list) module-name))))

  (define (sort-module-binding-info mbi)
    (list-sort
      (l (ele-1 ele-2)
        (string< (symbol->string (doc-name ele-1)) (symbol->string (doc-name ele-2))))
      mbi))

  (define (default-format-arguments arguments type)
    "pair/list:alist symbol:\"procedure\"/\"syntax\"/\"variable\"
    arguments as retrieved by \"module-binding-info\", which uses (ice-9 session) \"procedure-arguments\""
    ;syntax arguments can be pairs
    (if
      (and (eqv? (q procedure) type) (list? arguments)
        (not (null? arguments)) (pair? (first arguments)))
      (symbol-alist-bind arguments (optional rest required keyword allow-other-keys?)
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

  (define its-indent (string #\space #\space))

  (define (its-docstring-split-signature arg continue)
    "string procedure:{string:type-signatures string:rest-of-docstring} -> any
    if a string starts with a type-signature, split string at the end of it"
    (if arg
      (let (signature (match-pattern peg-type-signature arg))
        (if signature
          (continue
            (parsed-type-signature->string (peg:tree signature)
              (string-append its-indent its-indent))
            (docstring->lines (string-drop arg (peg:end signature))))
          (continue #f (docstring->lines arg))))
      (continue #f (list))))

  (define (lines->docstring arg indent)
    (let (arg (remove string-null? arg))
      (if (null? arg) ""
        (string-append indent "description"
          (string-join arg (string-append "\n" indent indent) (q prefix)) "\n"))))

  (define-as display-format-its symbol-alist
    ;this defines the default formatter
    format-arguments default-format-arguments
    format-binding-info
    (l (bi formatted-arguments) "vector:record string -> string"
      (string-append (symbol->string (doc-name bi)) "\n"
        (its-docstring-split-signature (doc-documentation bi)
          (l (signature text-lines)
            (string-append
              (if (or signature (not (string-null? formatted-arguments)))
                (string-append its-indent "signature\n"
                  its-indent its-indent
                  (if (string-null? formatted-arguments) formatted-arguments
                    (string-append formatted-arguments "\n"))
                  (if signature (string-append its-indent its-indent signature "\n") ""))
                "")
              (lines->docstring text-lines its-indent))))
        its-indent "type: " (symbol->string (doc-type bi))))
    format-module-documentation
    (l (module-name md) "any (string ...) -> string" (string-join md "\n"))
    format-modules-documentation (l (mds) "(string ...) -> string" (apply string-append mds)))

  (define-as documentation-display-formats symbol-alist
    default display-format-its its display-format-its)

  (define* (format-module-documentation module-names #:optional (format-handler-name (q default)))
    "((symbol ...) ...)/(symbol ...) symbol ->
    output documentation for a list of module-names. format-handler-name can currently either be
    default, plaintext, its, dokuwiki"
    (let
      ( (format-handler
          (or (assoc-ref documentation-display-formats format-handler-name)
            (throw (q no-such-display-format))))
        (module-names (if (list? (first module-names)) module-names (list module-names))))
      (symbol-alist-bind format-handler
        (format-arguments format-binding-info format-module-documentation
          format-modules-documentation)
        (format-modules-documentation
          (map
            (l (module-name)
              (format-module-documentation module-name
                (map
                  (l (binding-info)
                    (format-binding-info binding-info
                      (format-arguments (doc-arguments binding-info) (doc-type binding-info))))
                  (sort-module-binding-info (module-binding-info module-name)))))
            module-names))))))