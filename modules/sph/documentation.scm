(define-module (sph documentation))

(use-modules (ice-9 peg) (sph)
  ((sph alist) #:select (alist-q alist-bind)) (sph module binding-info)
  ((sph lang indent-syntax) #:select (denoted-tree->indent-tree indent-tree->denoted-tree))
  (sph lang parser type-signature) ((sph lang scm-format format) #:select (format-docstring))
  ((sph list) #:select (contains? any->list fold-multiple))
  ((sph module) #:select (module-find module-exports))
  ((sph tree) #:select (denoted-tree-minimize-depth)) ((rnrs sorting) #:select (list-sort))
  ((sph string) #:select (string-equal? any->string-pretty-print)) (srfi srfi-1) (srfi srfi-2))

(export default-format-arguments doc-bindings
  docstring->lines docstring-split-signature
  format-module-documentation output-format-indent
  output-format-signature output-format-markdown
  output-format-markdown-overview output-format-list
  lines->docstring module-description
  module-find-one-information module-find-one-information-sorted
  sort-module-information sph-documentation-description)

(define sph-documentation-description
  "extract and display documentation (bindings, arguments and docstrings) from modules")

(define indent-string (string #\space #\space))
(define (replace-underscore a state) (if (string-equal? "_" a) (number->string (+ 10 state) 32) a))

(define output-format-markdown
  (alist-q format-arguments default-format-arguments
    format-binding-info
    (l (bi formatted-arguments) "vector:record string -> string"
      (string-append "# " (symbol->string (bi-name bi))
        "\n"
        (docstring-split-signature (bi-documentation bi) #f
          (l (signature docstring)
            (string-append
              (if (or signature (not (string-null? formatted-arguments)))
                (string-append "## signature\n"
                  (if (string-null? formatted-arguments) formatted-arguments
                    (string-append formatted-arguments "\n"))
                  (if signature (string-append signature "\n") ""))
                "")
              (if docstring
                (string-append "## " (lines->docstring (string-split docstring #\newline) "")) ""))))
        "## type\n" (symbol->string (bi-type bi)) "\n"))
    format-module-documentation
    (l (module-name md) "any (string ...) -> string" (string-join md "\n"))))

(define output-format-indent
  (alist-q format-arguments default-format-arguments
    format-binding-info
    (l (bi formatted-arguments) "vector:record string -> string"
      (string-append (symbol->string (bi-name bi)) "\n"
        (docstring-split-signature (bi-documentation bi)
          (string-append indent-string indent-string)
          (l (signature docstring)
            (string-append
              (if (or signature (not (string-null? formatted-arguments)))
                (string-append indent-string "signature\n"
                  indent-string indent-string
                  (if (string-null? formatted-arguments) formatted-arguments
                    (string-append formatted-arguments "\n"))
                  (if signature (string-append indent-string indent-string signature "\n") ""))
                "")
              (if docstring (lines->docstring (string-split docstring #\newline) indent-string) ""))))
        indent-string "type: " (symbol->string (bi-type bi))))
    format-module-documentation
    (l (module-name md) "any (string ...) -> string" (string-join md "\n" (q suffix)))))

(define output-format-signature
  (alist-q format-arguments default-format-arguments
    format-binding-info
    (l (bi formatted-arguments) "vector:record string -> string"
      (docstring-split-signature (bi-documentation bi) ""
        (l (signature text-lines)
          (let
            ( (arguments-string formatted-arguments)
              (docstring
                (string-join (remove string-null? (if text-lines (any->list text-lines) null))
                  "\n  " (q prefix))))
            (string-append (symbol->string (bi-name bi))
              (if (contains? (q (procedure syntax)) (bi-type bi))
                (string-append " :: " arguments-string) ""))))))
    format-module-documentation
    (l (module-name md) "any (string ...) -> string" (string-join md "\n" (q suffix)))))

(define output-format-list
  (alist-q format-arguments default-format-arguments
    format-binding-info
    (l (bi formatted-arguments)
      (pair (symbol->string (bi-name bi))
        (append
          (docstring-split-signature (bi-documentation bi) ""
            (l (signature text)
              (append
                (if signature
                  (list
                    (pair (q signature)
                      (if (string-null? formatted-arguments) (list signature)
                        (list formatted-arguments signature))))
                  (if (string-null? formatted-arguments) (list)
                    (list (list (q signature) formatted-arguments))))
                (if (or (not text) (string-null? text)) null (list (list (q description) text))))))
          (list (list (q type) (bi-type bi))))))
    format-module-documentation (l (module-name md) (any->string-pretty-print md))))

(define (module-name->string name) (string-append "(" (string-join (map symbol->string name) " ") ")"))

(define (first-nonempty-line s)
  (let ((ls (remove string-null? (map string-trim (string-split s #\newline)))))
    (if (null? ls) "" (first ls))))

(define (extract-highlights s)
  (let (ls (map (l (x) (string-trim-right x)) (string-split s #\newline)))
    (let loop ((xs ls) (in? #f) (acc (list)))
      (if (null? xs) (reverse acc)
        (let*
          ((x (car xs)) (is-header (and (>= (string-length x) 1) (char=? #\# (string-ref x 0)))))
          (cond
            ((and (not in?) (string-ci=? x "# highlights")) (loop (cdr xs) #t acc))
            ((and in? is-header) (reverse acc))
            (in?
              (let
                ( (y
                    (string-trim
                      (cond
                        ((string-prefix? "* " x) (string-drop x 2))
                        ((string-prefix? "- " x) (string-drop x 2))
                        (else x)))))
                (loop (cdr xs) #t (if (string-null? y) acc (cons y acc)))))
            (else (loop (cdr xs) #f acc))))))))

(define output-format-markdown-overview
  (alist-q format-arguments default-format-arguments
    format-binding-info (l (bi formatted-arguments) "vector:record string -> string" "")
    format-module-documentation
    (l (module-name md) "any (string ...) -> string"
      (let*
        ( (desc (or (module-description module-name) "")) (first-line (first-nonempty-line desc))
          (high (extract-highlights desc))
          (head
            (string-append "* " (module-name->string module-name)
              (if (string-null? first-line) "" (string-append " - " first-line))))
          (tail
            (if (null? high) ""
              (string-append "\n" (string-join (map (l (h) (string-append "  * " h)) high) "\n")))))
        (string-append head tail)))))

(define (docstring-format a) "string -> string"
  "drop doublequotes of formatted string literal"
  (if (string-null? a) a
    (let* ((a (format-docstring a #f indent-string 0)) (a (substring a 1 (- (string-length a) 1))))
      (if (string-null? a) ""
        (string-join
          (map
            (l (line)
              "remove one eventual extra space from the beginning of lines when the docstring
               is formatted to offset the doublequote of the string literal"
              (let (index (string-skip line #\space))
                (if (and index (odd? index)) (string-drop line 1) line)))
            (string-split a #\newline))
          "\n")))))

(define (docstring-split-signature a line-prefix c)
  "string string procedure:{string:type-signatures string:rest-of-docstring} -> any
   if a string starts with a type-signature, split string at the end of it"
  (if a
    (let (signature (match-pattern peg-type-signature a))
      (if signature
        (c
          (parsed-type-signature->string (type-signature-simplify-tree (peg:tree signature))
            line-prefix)
          (docstring-format (string-trim (string-drop a (peg:end signature)))))
        (c #f (docstring-format a))))
    (c #f #f)))

(define (lines->docstring a indent-string) "list (string ...) -> string"
  (let (a (remove string-null? (if a (any->list a) null)))
    (if (null? a) ""
      (string-append indent-string "description"
        (string-join a (string-append "\n" indent-string indent-string) (q prefix)) "\n"))))

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
  "syntax arguments can be pairs"
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
                  (string-drop-right (string-drop (simple-format #f "~S" (map first keyword)) 1) 1)))
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

(define (module-description name)
  "(symbol ...) -> false/string
   get the module description from an exported variable with a specific name:
     (a b c) -> a-b-c-description"
  (and-let*
    ( (a
        (false-if-exception
          (module-ref (resolve-module name)
            (string->symbol
              (string-append (string-join (map symbol->string name) "-") "-description"))))))
    "remove excess indentation"
    (let (a-tree (indent-tree->denoted-tree a))
      (denoted-tree->indent-tree
        (if (>= 1 (length a-tree)) a-tree
          (pair (first a-tree) (denoted-tree-minimize-depth (tail a-tree))))))))

(define (sort-module-information a)
  (let
    (b
      (map
        (l (a)
          (alist-bind a (name)
            (pair (if name (apply string-append (map symbol->string name)) "") a)))
        a))
    (map tail (list-sort (l (a b) (string<? (first a) (first b))) b))))

(define (module-find-one-information search-paths . module-find-one-arguments)
  (map
    (l (a) (alist-q name (first a) full-path (tail a) description (module-description (first a))))
    (apply append
      (filter-map (l (a) (apply module-find a module-find-one-arguments)) (any->list search-paths)))))

(define (module-find-one-information-sorted search-paths . module-find-arguments)
  (sort-module-information (apply module-find-one-information search-paths module-find-arguments)))

(define* (doc-bindings libraries #:optional (pair pair))
  "(list ...) -> ((symbol:name . list:library-name) ...)
   get a list of all bindings and the library name they belong to for all specified library names"
  (append-map
    (l (library)
      (or
        (false-if-exception
          (map (l (export) (pair export library)) (module-exports (resolve-interface library))))
        null))
    libraries))

(define* (format-module-documentation module-names #:optional (format-config output-format-indent))
  "((symbol ...) ...) [list] ->
   return a string for the documentation found in the modules (binding names, arguments and docstrings).
   for just retrieving module documentation as scheme data consider (sph module binding-info).
   example:
     (format-module-documentation (quote (rnrs sorting)))"
  (alist-bind format-config (format-arguments format-binding-info format-module-documentation)
    (map
      (l (module-name)
        (format-module-documentation module-name
          (map
            (l (binding-info)
              (format-binding-info binding-info
                (format-arguments (bi-arguments binding-info) (bi-type binding-info))))
            (sort-module-binding-info (module-binding-info module-name)))))
      module-names)))
