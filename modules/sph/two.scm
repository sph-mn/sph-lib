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

(library (sph two)
  (export
    alist->regexp-match-replacements
    and-p
    any-ht-keys->values
    any-ht-values->keys
    apply*
    apply-without-arguments
    bash-escape-clear
    begin-first-values
    bindings-select-prefix
    bindings-select-regexp
    boolean->integer
    call
    cons-if-not-included
    copy-file!?
    copy-with-replaced-directory
    create-indent
    create-newline-indent
    create-quote
    debug-log-file
    define-multiple
    define-stack-fluid
    define-string
    define-syntax-identifier
    display-formatted
    display-line*
    every-s
    file-append
    file-append-one
    generalised-length
    generalised-less?
    get-filesystem-root
    get-mime-extensions
    get-mime-extensions-cached
    guile-exception->string
    hash-select
    identity-s
    if-guile-exception
    let-if
    let-if*
    line-reverse-direction
    list->create-list
    list->csv-line
    list->nl-string
    list->string-list
    list->values
    list-replace-from-ht-splice
    list-symbols->string
    listener-on-port-local?
    md5sum
    md5sum-file
    move-and-link
    nl-string->list
    not-null?
    not-scm-file?
    number->integer-string
    or-p
    os-seconds-at-boot
    os-seconds-since-boot
    path->symbol-list
    paths-find-file-size-sum
    plaintext->shtml
    port-column-subtract!
    port-each-line-alternate-direction
    port-skip+count
    prefix-definition-names
    prefix-imply-for
    primitive-eval-port
    process-unique-integer
    prog-sync-with-root
    read-line-crlf
    read-line-crlf-trim
    read-mime.types
    seconds->short-kiloseconds-string
    set-multiple-from-list!
    sort-symbol-lists
    sph-two-description
    srfi-19-date->seconds
    string-remove-leading-zeros
    sxml->xml-string
    system-cat-merge-files
    system-path->mime-type
    system-realpath
    tail-symbols->string
    tree-replace-from-hashtable
    true?
    values-bind
    variable-type
    variadic
    while-do
    while-do-map
    while-store
    with-values)
  (import
    (guile)
    (ice-9 match)
    (ice-9 popen)
    (ice-9 pretty-print)
    (ice-9 rdelim)
    (ice-9 regex)
    (ice-9 threads)
    (rnrs bytevectors)
    (rnrs io ports)
    (rnrs sorting)
    (sph)
    (sph alist)
    (sph cli)
    (sph conditional)
    (sph filesystem)
    (sph hashtable)
    (sph io)
    (sph number)
    (sph one)
    (sph process)
    (sph stream)
    (sph string)
    (sph time)
    (sph tree)
    (srfi srfi-69)
    (sxml simple)
    (except (srfi srfi-1) map)
    (only (sph list) interleave)
    (only (sph string) string-quote)
    (only (sph tree) prefix-tree->denoted-tree)
    (only (srfi srfi-19) time-second date->time-utc))

  (define (path->symbol-list a)
    (let (a (string-trim-both a #\/))
      (if (string-null? a) (list) (map string->symbol (string-split a #\/)))))

  (define-syntax-rule (begin-first-values a b ...)
    ; like begin but returns the values created by the first expression
    (apply-values (l result b ... (apply values result)) a))

  (define sph-two-description "various bindings deemed less useful than the ones in (sph one)")

  (define-syntax-rule (variadic body ...)
    ; create a procedure that accepts (and ignores) any number of arguments and evaluates body when called.
    ; similar to guiles "const" but the result is not cached
    (lambda a body ...))

  (define (sort-symbol-lists a)
    "((symbol ...) ...) -> list
     sort-module-names"
    (let (b (map (l (a) (pair (apply string-append (map symbol->string a)) a)) a))
      (map tail (list-sort (l (a b) (string<? (first a) (first b))) b))))

  (define-syntax-rule (identity-s a) a)

  (define (guile-exception->string key . a)
    "symbol any ... -> string
     create a space separated string from exception key and arguments.
     uses \"any->string\" with the format of \"display\" to convert non-string arguments"
    (string-join (map any->string a) " "))

  (define (list->values a) (apply values a))
  (define-syntax-rule (with-values producer proc) (call-with-values (nullary producer) proc))

  (define (call proc . a)
    "procedure any ... -> any
     apply procedure \"proc\" with arguments \"a\""
    (apply proc a))

  (define-syntax-rules define-multiple
    ;define multiple variables at once from a list. example: (define-multiple (a b c) (list 1 2 3))
    ;generated code looks similar to this:
    ;  (define a expr)
    ;  (define b (tail expr))
    ;  (define b (first b))
    ;  (define a (first a))
    ((() expr) (if #f #f))
    ( ( (identifier . identifiers) expr)
      (begin
        ; first binds identifier to the result of expr
        ; then recurses without the first of all values
        ; then re-binds identifier to the values for the first variable
        (define identifier expr) (define-multiple identifiers (tail identifier))
        (define identifier (first identifier)))))

  (define-syntax-rule (values-bind producer lambda-formals body ...)
    (call-with-values (nullary producer) (lambda lambda-formals body ...)))

  (define* (integer->integer-string a #:optional (radix 10))
    "integer -> string
     return integer as a string without a radix point or fractional value."
    (number->string (inexact->exact a) radix))

  (define (apply-without-arguments procedure) (procedure))
  (define-syntax-rule (true?-s expr) (if expr #t #f))

  (define (alist->regexp-match-replacements a)
    "automatically converts strings at the prefix position to regular expressions"
    (map (l (e) (pair (if (string? (first e)) (make-regexp (first e)) (first e)) (tail e))) a))

  (define-syntax-rule (if-guile-exception expr consequent)
    ;example: (if-guile-exception (throw (q test)) "return this on exception")
    (catch #t (l () expr) (l exc consequent)))

  (define* (display-formatted #:key (port (current-output-port)) . a) (pretty-print a port))

  (define (move-and-link target-path source-path)
    "string ... -> (boolean ...)
     move source-path to target-path and replace original source-path"
    (let (target-path (path->full-path target-path))
      (and (ensure-directory-structure target-path)
        (execute-and-check-result "mv" "-t" target-path source-path)
        (execute-and-check-result "ln" "-s"
          (string-append (ensure-trailing-slash target-path) (basename source-path))
          (dirname source-path)))))

  (define (not-null? a) (not (null? a)))

  (define-syntax-rules every-s
    ;"like 'every' for lists but implemented as syntax. proc is not bound
    ;example: (every-s integer? 1 2 3) is expanded to: (if (and (integer? 1) (integer? 2) (integer? 3)) #t #f)"
    ((proc a) (if (proc a) #t #f)) ((proc a a-n ...) (if (proc a) (every-s proc a-n ...) #f)))

  (define (define-string name value)
    "string any ->
     define a variable by using a string for the name"
    (primitive-eval (quasiquote (define (unquote (string->symbol name)) (unquote value)))))

  (define (boolean->integer a) (if a 1 0))

  (define (debug-log-file . a)
    "writes all arguments in one line prefixed by the process id to an automatically created log file \"/tmp/sph-scm-debug-log\""
    (let (file (open-file "/tmp/sph-scm-debug-log" "a")) (display-line (pair (getpid) a) file)
      (close file)))

  (define (and-p . a)
    "any -> false/any
     like the \"and\" syntax but as a procedure"
    (let loop ((rest a) (previous #t))
      (if (null? rest) previous (if (first rest) (loop (tail rest) (first rest)) #f))))

  (define (or-p . a)
    "any -> false/any
     like the \"or\" syntax but as a procedure"
    (any identity a))

  (define (create-indent size) (list->string (make-list (* size 2) #\space)))
  (define (create-newline-indent size) (string-append "\n" (create-indent size)))

  (define (any-ht-keys->values ht a)
    "r6rs-hashtable any -> any
     replace the given value or values in a list with values in hashtable for the given value as key"
    (if (list? a) (map (l (b) (any-ht-keys->values ht b)) a)
      (if (or (symbol? a) (integer? a)) (ht-ref ht a a) a)))

  (define (any-ht-values->keys ht a) "r6rs-hashtable any -> any"
    (if (list? a) (map (l (b) (any-ht-values->keys ht b)) a)
      (if (or (symbol? a) (integer? a)) (ht-ref ht a a) a)))

  (define (seconds->short-kiloseconds-string a)
    (number-format-float (inexact->exact a) #:decimal-max 3 #:decimal-min 2))

  (define (os-seconds-at-boot)
    (- (nanoseconds->seconds (utc-elapsed-day (utc-current))) (os-seconds-since-boot)))

  (define (os-seconds-since-boot)
    (string->number (first (string-split (shell-eval->string "cat /proc/uptime") #\space))))

  (define (port-column-subtract! port integer)
    (set-port-column! port (- (port-column port) integer)))

  (define (port-skip+count port skip-char) ""
    (let loop ((char (peek-char port)) (count 0))
      (if (eqv? char skip-char) (loop (begin (read-char port) (peek-char port)) (+ count 1)) count)))

  (define (file-lines-fold! path proc)
    (let
      (r
        (call-with-input-file path
          (l (port) (string-join (reverse (port-lines-fold proc (list) port)) "\n"))))
      (call-with-output-file path (l (port) (display r port)))))

  (define (string-with-ignored-indent a proc)
    (let (indent-depth (string-skip a #\space))
      (string-append (string-multiply " " indent-depth) (proc (substring a indent-depth)))))

  (define (sxml->xml-string a) (call-with-output-string (l (b) (sxml->xml a b) b)))

  (define (plaintext->shtml a)
    "string -> list:sxml
     convert double newlines to paragraphs <p> and single newlines to breaks <br>."
    (map (l (e) (list (q p) (interleave (string-split e #\newline) (q (br)))))
      (delete "" (string-split-regexp a "\n\n"))))

  (eval-when (expand load eval)
    ;for "namespace".
    (define (define? a) (and (list? a) (not (null? a)) (eqv? (q define) (first a))))
    (define (add-define-name-prefix a prefix)
      (match a
        ( (define (name formals ...) body ...)
          (pairs (q define) (pair (symbol-append prefix (q -) name) formals) body))
        ((define name body ...) (pairs (q define) (symbol-append prefix (q -) name) body)))))

  (define-syntax-case (namespace spec body ...) s
    ;(symbol ...) any ...
    ;the names of every definition with define in the top-level of the body of this expression
    ;will be prefixed with spec joined with minus character.
    ;example: (namespace (a b c) (define d #t) (define (e f) #t)) -> (define a-b-c-d #t) (define (a-b-c-e f) #t)
    (let*
      ( (name (syntax->datum (syntax spec)))
        (name (if (symbol? name) name (string->symbol (string-join (map symbol->string name) "-")))))
      (datum->syntax s
        (pair (q begin)
          (map (l (e) (if (define? e) (add-define-name-prefix e name) e))
            (syntax->datum (syntax (body ...))))))))

  (define line-reverse-direction
    ( (nullary (define (split-words a) (delete "" (map string-trim-both (string-split a #\space))))
        (define (replace-range-delimiters a)
          (string-replace-chars a (q ((#\( #\)) (#\) #\() (#\[ #\]) (#\] #\[) (#\{ #\}) (#\} #\{)))))
        (define (reverse-sentence-word a)
          (let
            ( (index-right (string-skip-right a char-set:punctuation))
              (index-left (string-skip a char-set:punctuation)))
            (string-append
              (replace-range-delimiters (string-reverse (substring a (+ 1 index-right))))
              (substring a index-left (+ 1 index-right))
              (replace-range-delimiters (string-reverse (substring a 0 index-left))))))
        (define (reverse-sentence-words a)
          (fold (l (e r) (pair (reverse-sentence-word e) r)) (list) a))
        (l (line alt? c)
          "string boolean procedure:{line boolean -> any} -> any
        for the alternation of reading direction of lines.
        it accounts for punctuation and parentheses and does not use utf-8 bidi capabilities.
        calls \"c\" with a possibly word-reversed version of line and the new state for \"alt?\".
        empty lines do not change the \"alt?\" state and are passed unchanged"
          (let (words (split-words line))
            (if (null? words) (c line alt?)
              (c (if alt? (string-join (reverse-sentence-words words) " ") line) (not alt?))))))))

  (define (port-each-line-alternate-direction port proc)
    "port procedure:{line ->}
     alternates reading direction in lines read from port, with the exempt of lines
     which do not only contain letters, digits or punctuation, which are passed as is.
     it accounts for punctuation and parentheses and does not use utf-8 bidi capabilities"
    (define-as char-set:line char-set-union
      char-set:letter+digit char-set:whitespace
      char-set:punctuation (char-set #\( #\) #\[ #\] #\{ #\}))
    (port-lines-fold
      (l (e alt?)
        (if (string-every char-set:line e)
          (line-reverse-direction e alt? (l (e alt?) (proc e) alt?)) (begin (proc e) alt?)))
      #f port))

  (define (srfi-19-date->seconds a) "srfi-14-date -> integer unix-time-seconds"
    (time-second (date->time-utc a)))

  (define (read-line-crlf-trim port)
    "try to read a line that is known to be cr-lf terminated and remove the cr-lf or return eof-object"
    (let (line+delim (%read-line port))
      (let ((line (first line+delim)) (delim (tail line+delim)))
        (if (and (string? line) (char? delim)) (substring line 0 (- (string-length line) 1)) line))))

  (define (read-line-crlf port)
    "try to read a line that is known to be cr-lf terminated or return eof-object"
    (let (line+delim (%read-line port))
      (let ((line (first line+delim)) (delim (tail line+delim)))
        (if (and (string? line) (char? delim)) (string-append line (string delim)) line))))

  (define (bash-escape-clear)
    "display the bash escape sequence for clearing the screen - which usually means to scroll until the current line is at the top"
    (display #\esc) (display "c"))

  (define (list->string-list a)
    "(any ...) -> (string ...)
     convert every element of a list to a string using the format display would use"
    (map any->string a))

  (define-syntax-rules let-if
    ;if all bound values are true, evaluate true-expr, otherwise false-expr.
    ;the bindings will be available to the body expressions in any case
    ( ( ( (variable-name expr) ...) true-expr false-expr)
      ((lambda (variable-name ...) (if (and variable-name ...) true-expr false-expr)) expr ...))
    ( ( (variable-name expr) true-expr false-expr)
      ((lambda (variable-name) (if variable-name true-expr false-expr)) expr)))

  (define-syntax-case (let-if* ((variable-name expr) ...) true-expr false-expr) s
    ;"bind expressions to variables one after another, and stop and evaluate false-expr
    ;if a variable turns out to be false. this is like and-let* with consequent/alternate expressions"
    (let
      ( (variable-names (syntax->datum (syntax (variable-name ...))))
        (expressions (syntax->datum (syntax (expr ...)))))
      (datum->syntax s
        (fold-right
          (lambda (variable-name expr r)
            (quasiquote
              ( (lambda ((unquote variable-name))
                  (if (unquote variable-name) (unquote r)
                    (unquote (syntax->datum (syntax false-expr)))))
                (unquote expr))))
          (syntax->datum (syntax true-expr)) variable-names expressions))))

  (define-syntax-case (primitive-define-stack-fluid name name.add name.remove)
    ;see define-stack-fluid
    (let*
      ( (name.fluid
          (datum->syntax (syntax name) (symbol-append (syntax->datum (syntax name)) (q -fluid)))))
      (quasisyntax
        (begin (define (unsyntax name.fluid) (let ((r (make-fluid))) (fluid-set! r (list)) r))
          (define (name.add a)
            (fluid-set! (unsyntax name.fluid) (cons a (fluid-ref (unsyntax name.fluid)))))
          (define (name.remove)
            (fluid-set! (unsyntax name.fluid) (tail (fluid-ref (unsyntax name.fluid)))))
          (define (name) (fluid-ref (unsyntax name.fluid)))))))

  (define-syntax-rule (swap! a b) ((l (temp-a) (set! a b) (set! b temp-a)) a))
  (define process-unique-integer (let (value 0) (nullary (set! value (+ 1 value)) value)))

  (define (bindings-select-prefix prefix bindings-hash)
    "string guile-hashtable -> (symbol ...)
     return a list of binding names as symbols beginning with prefix"
    (hash-select (lambda (cur-symbol) (string-prefix? prefix (symbol->string cur-symbol)))
      bindings-hash))

  (define (bindings-select-regexp regexp-str bindings-hash)
    "string guile-hashtable -> (symbol ...)
     return a list of binding names as symbols matching regexp"
    (hash-select (l (cur-symbol) (string-match regexp-str (symbol->string cur-symbol)))
      bindings-hash))

  (define* (cons-if-not-included any lis #:optional (contain-proc member)) "add"
    any " to the beginning of lis if an equal element does not already exist in lis"
    (if (contain-proc any lis) lis (cons any lis)))

  (define (copy-file!? source-path target-path)
    "copy file source-path to target-path and check if target-path has the same size as source"
    (copy-file source-path target-path)
    (eqv? (stat:size (stat source-path)) (stat:size (stat target-path))))

  (define (copy-with-replaced-directory source-dir target-dir path)
    "! depends on the cp
     command. replace the source-dir portion in path with target-dir and copy the resulting path."
    (if (string-prefix? source-dir path)
      (let*
        ( (target (substring path (string-length source-dir)))
          (command (list "cp" "-t" (dirname (string-append target-dir target)) "-r" path)))
        (apply system* command))
      (raise (q path-not-in-source-directory))))

  (define-syntax-rule (create-quote a) (list (q quote) a))

  (define-syntax-cases define-stack-fluid
    ;name [name.add name.remove] -> {4 new top-level bindings}
    ;may only be called at the top-level of a program or library.
    ;defines a thread-local stack and 3 procedures to add, remove and retrieve it.
    ;if the names for the add and remove procedures are not given, it is created from the stack name
    ;by appending -add! and -remove! respectively.
    ((name name.add name.remove) (syntax (primitive-define-stack-fluid name name.add name.remove)))
    ( (name)
      (let*
        ( (name-sym (syntax->datum (syntax name)))
          (name.add (datum->syntax (syntax name) (symbol-append name-sym (q -add!))))
          (name.remove (datum->syntax (syntax name) (symbol-append name-sym (q -remove!)))))
        (quasisyntax (primitive-define-stack-fluid name (unsyntax name.add) (unsyntax name.remove))))))

  (define-syntax-rule (define-syntax-identifier identifier expr)
    (define-syntax identifier (identifier-syntax expr)))

  (define (display-line* . args)
    "concatenate all given strings with spaces, add an newline and display"
    (display (string-join args " ")) (display "\n"))

  (define (primitive-eval-port port) "evaluate expressions from port until end of file is reached"
    (while-do (read port) (l (expression) (primitive-eval expression)) eof-object?))

  (define (port-append-one a b)
    "port port ->
     append contents of port b to port a"
    (each-u8 (l (octet) (put-u8 a octet)) b))

  (define (port-append target . ports)
    "port ... ->
     append contents of ports to target"
    (each (l (e) (file-append-one target e)) (tail ports)))

  (define (generalised-length a)
    "any -> integer
     - count of digits for numbers
     - number of chars for strings
     - number of elements for lists
     - false for anything else"
    (if (string? a) (string-length a)
      (if (number? a) (string-length (number->string a)) (if (list? a) (length a) #f))))

  (define (generalised-less? a b)
    "works for strings with string<, numbers with <, length with lists.
     if numbers are compared with a string it takes the string representation of the number"
    (cond ((and (string? a) (string? b)) (string< a b)) ((and (number? a) (number? b)) (< a b))
      ((and (string? a) (number? b)) (string< a (number->string b)))
      ((and (number? a) (string? b)) (string< (number->string a) b))
      ((and (list? a) (list? b)) (< (length a) (length b)))))

  (define (system-realpath a) (string-trim-right (execute->string "realpath" a)))

  (define (get-filesystem-root path)
    "string -> string
     get the path to the root of the filesystem where path is pointing to, relative to the rootfs.
     currently depends on procfs /proc/mounts"
    (let*
      ( (path (system-realpath path))
        (matches
          (fold
            (l (e r)
              (let ((info (string-split e #\space)))
                (if (string-prefix? (list-ref info 1) path) (cons (list-ref info 1) r) r)))
            (list) (nl-string->list (file->string "/proc/mounts")))))
      (if (null? matches) #f
        (let loop ((rest matches) (r #f) (cur-max 0))
          (if (null? rest) r
            (let* ((e (first rest)) (e-length (string-length e)))
              (if (> e-length cur-max) (loop (tail rest) e e-length) (loop (tail rest) r cur-max))))))))

  (define* (get-mime-extensions #:optional (path "/etc/mime.types"))
    "[string] -> (string ...)
     get all filename extensions from \"/etc/mime.types\""
    (delete-duplicates
      (fold (l (e r) (if (null? e) r (let ((t (tail e))) (append t r)))) (list)
        (read-mime.types path))))

  (define get-mime-extensions-cached (procedure->cached-procedure get-mime-extensions))

  (define (hash-select proc bindings-hash)
    "procedure guile-hashtable -> list
     result in a list of bindings satisfying predicate proc"
    (hash-fold (lambda (cur-symbol value r) (if (proc cur-symbol) (cons cur-symbol r) r)) (list)
      bindings-hash))

  (define-syntax-rule (list->create-list a) (pair (q list) a))

  (define (list->csv-line a)
    "(string ...) -> string
     converts list to a comma-separated line"
    (string-append (string-join a ",") "\n"))

  (define (list->nl-string a)
    "list -> string
     convert elements of list to a newline separated string"
    (string-join a "\n"))

  (define (list-symbols->string a)
    "list -> list
     convert symbols in list to string"
    (map (l (a) (if (symbol? a) (symbol->string a) a)) a))

  (define (listener-on-port-local? port-number)
    "integer -> boolean
     probably system and linux dependent. checks using /proc/net/tcp if a process is listening on local network port port-number"
    (let ((port-number (number->string port-number 16)))
      (call-with-input-file "/proc/net/tcp"
        (l (p)
          (stream-any
            (l (line)
              (let ((line (string-split (string-trim line) #\space)))
                (if (or (null? line) (> 2 (length line))) #f
                  (let ((part (string-split (list-ref line 1) #\:)))
                    (if (or (null? part) (null? (cdr part))) #f
                      (equal? port-number (list-ref part 1)))))))
            (port->line-stream p))))))

  (define (md5sum a)
    "string -> string:md5sum
     get the md5sum of string by using the \"md5sum\" and \"echo\" program"
    (string-take (shell-eval->string (string-append "printf " (string-quote a) " |md5sum")) 32))

  (define (md5sum-file path)
    "string:path -> string:md5sum
     get the md5sum of a file by using the \"md5sum\" program"
    (string-take (shell-eval->string (string-append "md5sum " (string-quote path))) 32))

  (define (nl-string->list a)
    "string -> (string ...)
     split a string at newlines. empty lines are discarded"
    (filter (l (e) (not (string-null? e)))
      (map (l (e) (string-trim-both (regexp-replace e "\n" ""))) (string-split a #\newline))))

  (define (not-scm-file? file-path)
    "string -> boolean
     tests if a string has a .scm suffix"
    (not (string-suffix? ".scm" file-path)))

  (define (system-path->mime-type path)
    "string -> string
     get the mime-type of a file by using the \"file\" program.
     see also (sph libmagic) for a more efficient variant"
    (string-trim-right
      (shell-eval->string (string-append "file --mime-type -b " (string-quote path)))))

  (define (prog-sync-with-root source-root target-root on-error)
    "string string procedure:{-> any}
     take the first string from program-arguments and use it with copy-with-replaced-directory.
     all exceptions are catched and on-error installed as a handler"
    (let ((config.exec (command-line)))
      (if (null? (tail config.exec)) (begin (simple-format #t "not enough arguments\n") (exit)))
      (catch #t
        (nullary
          (let ((path (path->full-path (list-ref config.exec 1))))
            (copy-with-replaced-directory source-root target-root path)
            (simple-format #t "sync ~A ~A\n" path target-root)))
        (l args (on-error)))))

  (define (read-mime.types path)
    "string -> list:((string:type-name string:extension ...) ...)
     read a mime.types file into a list. path is for example /etc/mime.types"
    (call-with-input-file path
      (l (file)
        (port-lines-fold
          (l (line r)
            (if (string-prefix? "#" line) r
              (let
                ( (name+extensions
                    (string-split (regexp-replace (regexp-replace line "\t" " ") " +" " ") #\space)))
                (pair (delete "" name+extensions) r))))
          (list) file))))

  (define-syntax-rules set-multiple-from-list! ((() expr) (quote unspecified))
    ;set multiple variables using a list
    ;example: (set-multiple-from-list! (a b c) (list 1 2 3))
    ( ( (identifier . identifiers) expr)
      (begin (set! identifier expr) (set-multiple-from-list! identifiers (tail identifier))
        (set! identifier (first identifier)))))

  (define (string-remove-leading-zeros a) "string -> string" (string-trim a #\0))

  (define* (system-cat-merge-files include-list target-path #:optional (buffer-size 2000000))
    "(string:source-path ...) string:target-path integer -> string:target-path
     list string -> string
     take a list of file-paths and append them to a new file.
     requires the \"cat\" program"
    (let
      ( (cat (apply open-pipe* OPEN_READ "cat" include-list))
        (target-file (open-output-file target-path)))
      (let loop (part (get-bytevector-n cat buffer-size))
        (if (eof-object? part) (begin (close target-file) (close-pipe cat) target-path)
          (begin (put-bytevector target-file part) (loop (get-bytevector-n cat buffer-size)))))))

  (define (tail-symbols->string a)
    "list -> list
     convert all symbols not in prefix position to strings"
    (if (null? a) a
      (cons (first a)
        (let loop ((rest (tail a)))
          (if (null? rest) (list)
            (let ((cur (first rest)))
              (cons
                (if (and (list? cur) (not (null? cur))) (cons (first cur) (loop (tail cur)))
                  (if (symbol? cur) (symbol->string cur) cur))
                (loop (tail rest)))))))))

  (define (variable-type a)
    "any -> symbol
     return a symbol indicating the variable type"
    (cond ((list? a) (q list)) ((pair? a) (q pair))
      ((string? a) (q string)) ((number? a) (q number))
      ((boolean? a) (q boolean)) ((array? a) (q array))
      ((hash-table? a) (q hash-table)) ((bitvector? a) (q bitvector))))

  (define-syntax-rules while-do
    ( (expr proc stop-if)
      (let ((m-proc proc) (m-stop-if stop-if))
        (let loop ((cur expr)) (if (not (stop-if cur)) (begin (proc cur) (loop expr))))))
    ((expr proc) (while-do expr proc not)))

  (define-syntax-rules while-do-map
    ( (expr proc stop-if)
      (let loop ((cur expr) (r (list)))
        (if (stop-if cur) (reverse r) (loop expr (cons (proc cur) r)))))
    ((expr proc) (while-do-map expr proc not)))

  (define-syntax-rules while-store
    ( (expr stop-if)
      (let loop ((cur expr) (r (list))) (if (stop-if cur) (reverse r) (loop expr (cons cur r)))))
    ((expr) (while-store expr not)))

  (eval-when (expand load eval)
    (define (prefix-join a) "symbol -> symbol"
      (string->symbol (string-join (map symbol->string a) "-")))
    (define (symbol-prepend-prefix-proc prefix)
      (if (list? prefix) (l (a) (prefix-join (append prefix (list a))))
        (l (a) (symbol-append prefix a)))))

  (define-syntax-case (prefix-definition-names prefix body ...) s
    ;symbol/(symbol ...) any ...
    ;all defines in "body" are rewritten to bind a name prepended with symbols in prefix joined by "-".
    ;usages are not rewritten.
    ;example: (define-with-prefix (a b) (define c 1) (define (d e) 2)) -> (define a-b-c 1) (define (a-b-d e) 2)
    (let
      ( (symbol-prepend-prefix (symbol-prepend-prefix-proc (syntax->datum (syntax prefix))))
        (body-datum (syntax->datum (syntax (body ...))))
        (is-define? (l (a) (and (list? a) (not (null? a)) (eqv? (quote define) (first a))))))
      (datum->syntax s
        (pair (q begin)
          (map
            (l (e)
              (if (is-define? e)
                (apply
                  (l (define-symbol define-name . define-body)
                    (if (pair? define-name)
                      (pairs define-symbol
                        (pair (symbol-prepend-prefix (first define-name)) (tail define-name))
                        define-body)
                      (pairs define-symbol (symbol-prepend-prefix define-name) define-body)))
                  e)
                e))
            body-datum)))))

  (define-syntax-case (prefix-imply-for prefix (name ...) body ...) s
    ;bind the values of the indentifiers "{prefix}-{name}" to "name" in the scope for "body"
    (let*
      ( (symbol-prepend-prefix (symbol-prepend-prefix-proc (syntax->datum (syntax prefix))))
        (add-prefix-to-formals
          (l (a) (map (l (e) (if (eqv? (q t) e) e (symbol-prepend-prefix e))) a)))
        (name-datum (syntax->datum (syntax (name ...)))))
      (datum->syntax s
        (pair (pairs (q lambda) name-datum (syntax->datum (syntax (body ...))))
          (add-prefix-to-formals name-datum)))))

  (define-syntax-rule (apply* arguments proc) (apply proc arguments)))
