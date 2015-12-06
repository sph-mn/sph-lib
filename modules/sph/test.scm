; (sph test) - automated code testing
; written for the guile scheme interpreter
; Copyright (C) 2015 sph <sph@posteo.eu>
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

(library (sph test)
  (export
    assert-and
    assert-equal
    assert-true
    define-test
    define-test-module
    define-tests
    test-cli
    test-create-result
    test-execute-cli
    test-execute-module
    test-execute-modules-prefix
    test-execute-procedures
    test-formats
    test-lambda
    test-procedures-normalise
    test-resolve-procedure
    test-result-format
    test-result-success?
    test-results-display
    test-settings-default
    test-success?)
  (import
    (ice-9 match)
    (rnrs eval)
    (sph common)
    (sph module)
    (sph record)
    (only (guile)
      resolve-interface
      symbol-append
      defined?
      stat:type
      module-ref
      syntax->datum
      stat
      datum->syntax
      unsyntax
      gensym
      syntax
      readlink
      quasisyntax))

  ;todo: failure-display, nested module results display, exception supression

  (define test-format-compact-one
    (let
      (display-indices
        (l (count port) (n-times count (l (n) (display " " port) (display (+ 1 n) port)))))
      (l (a port) "vector:test-result-record port/any ->"
        (let (index (test-result-index a))
          (if (test-result-success? a)
            (begin (display (test-result-title a) port)
              (if (and index (< 0 index)) (display-indices index port)) (newline port))
            (begin (display (test-result-title a) port)
              (if (and index (< 1 index)) (display-indices (- index 1))) (newline port)
              (display
                (if index (string-append "  failure at " (number->string index) "\n    ")
                  "  failure\n    "))
              (newline port)))))))

  (define (test-format-compact result port) "list/vector port -> list/vector:result/input"
    (if (list? result)
      (let loop ((rest result) (depth 0) (group (list)))
        (if (null? rest) result
          (let (e (first rest))
            (if (list? e)
              (match e
                ( ( (? symbol? group-name) rest ...)
                  (loop (tail rest) (+ 1 depth) (pair group-name group)))
                (_ (loop e (+ 1 depth) group)))
              (begin (test-format-compact-one e port) (loop (tail rest) depth group))))))
      (test-format-compact-one result port))
    result)

  (define (test-format-null a port)
    "list/vector port -> list/vector
    does not display anything"
    a)

  (define-as test-settings-default alist-quoted
    ;p: procedure, for procedure tests
    p-display test-format-compact
    p-display-port (current-output-port)
    p-before #f random-order? #f parallel? #f exceptions? #t exclude #f only #f until #f)

  (define test-cli
    (cli-create #:parameters "options ... source ..."
      #:description
      (string-join
        (list
          "execute tests. source is filesystem paths to files containing modules relative to any guile load-path."
          "if a source path points to a directory, a module with a name corresponding to that path and all modules under that directory are used."
          "exclude/only/until take a list of comma separated module name suffixes or full module names in the format \"a//b//c\" as well as procedure names")
        "\n  ")
      #:options
      (ql ((source ...)) (display-format #f #f #t)
        (add-to-load-path #f #f #t) (exclude #f #f #t) (only #f #f #t) (until #f #f #t))))

  (define (test-path-execute settings a) "alist string -> vector/list/error:test-result"
    (let (load-path (path->load-path a))
      (if load-path
        (case (stat:type (stat (string-append load-path "/" a)))
          ((directory) (test-module-prefix-execute settings (path->module-name a)))
          ((regular) (test-module-execute settings (environment* (path->module-name a))))
          ( (symlink)
            ;as far as we know readlink fails for circular symlinks
            (test-path-execute settings (readlink a))))
        (error-create (q file-not-found-in-load-path)))))

  (define (cli-value-path/module-list a)
    "false/string -> false/list
    parsed a list of comma separated filesystem or module paths, \"/\" or \"//\" separated respectively, without filename suffixes"
    (false-if-not a
      (map
        (l (e) (if (string-contains e "//") (map string->symbol (string-split-regexp e "//")) e))
        (string-split a #\,))))

  (define (cli-value-display-format a) "false/string -> false/procedure"
    (if a (hashtable-ref test-formats (string->symbol a) test-format-compact) test-format-compact))

  (define (add-to-load-path! cli-arguments)
    "list:alist ->
    if the --add-to-load-path option has been specified, add the comma separated list of paths given
    as a value to the option to the beginning of the module load-path"
    (pass-if (alist-quoted-ref cli-arguments add-to-load-path)
      (l (a) (map (l (e) (add-to-load-path e)) (string-split a #\,)))))

  (define (test-execute-cli-get-settings cli-arguments)
    "list -> list
    create the test settings object from program arguments"
    (alist-quoted-bind cli-arguments (display-format exclude only until)
      (add-to-load-path! cli-arguments)
      (alist-quoted-update test-settings-default p-display
        (cli-value-display-format display-format) exclude
        (cli-value-path/module-list exclude) only
        (cli-value-path/module-list only) until (cli-value-path/module-list until))
      test-settings-default))

  (define (test-execute-cli)
    "parse program arguments and run the rest of the program depending on the given arguments"
    (let* ((arguments (test-cli)) (settings (test-execute-cli-get-settings arguments)))
      (alist-quoted-bind arguments (source)
        (if source (map (l (e) (test-path-execute settings e)) source) (list)))))

  (define-syntax-cases test-lambda s
    (((arguments expected) body ...) (syntax (lambda (arguments expected) body ...)))
    ( ( (a ...) body ...)
      (quasisyntax (lambda (a ... . (unsyntax (datum->syntax s (gensym "define-test")))) body ...))))

  (define-syntax-cases define-test
    ;define a new test procedure
    ( ( (name parameter ...) body ...)
      (syntax (define-test name (test-lambda (parameter ...) body ...))))
    ( (name proc)
      (let (name-datum (syntax->datum (syntax name)))
        (quasisyntax
          (define (unsyntax (datum->syntax (syntax name) (symbol-append (q test-) name-datum)))
            proc)))))

  (define-syntax-cases define-tests-one
    ( ( (name data ...))
      (let
        (test-name
          (datum->syntax (syntax name) (symbol-append (q test-) (syntax->datum (syntax name)))))
        (quasisyntax
          (pairs (quote name)
            ;eval, to avoid getting possibly undefined variable warnings
            (eval
              (quote
                (if (defined? (q (unsyntax test-name))) (unsyntax test-name)
                  (l (arguments . rest) (apply name arguments))))
              (current-module))
            (quasiquote (data ...))))))
    ((name) (syntax (define-tests-one (name)))))

  (define-syntax-rule (define-tests name test ...)
    ;symbol symbol/list -> ((symbol procedure any ...) ...)
    ;resolves procedures from name and normalises the test specification
    (define name (list (define-tests-one test) ...)))

  (define-syntax define-test-module
    ;test modules are typical modules/libraries that export only one "export" procedure. this syntax simplifies the definition.
    ;modules are used to give test modules a separated scope, in which other modules that the test might need are integrated.
    ;it is about loading several code files that usually depend on other code without conflicts.
    ;there might be alternatives to using modules, for example evaluating files that use the import statement in custom environments.
    ;this syntax definition must be available in the environment in which the module is loaded.
    ;to archieve this, the module definition is first evaluated in the top-level environment, then the module object is resolved (environment*)
    (l (s)
      (syntax-case s (import)
        ( (_ (name-part ...) (import spec ...) body ...)
          (syntax
            (library (name-part ...) (export execute) (import (rnrs base) (sph) (sph test) spec ...) body ...)))
        ( (_ (name-part ...) body ...)
          (syntax (define-test-module (name-part ...) (import) body ...))))))

  (define-as test-formats symbol-hashtable compact test-format-compact null test-format-null)

  (define* (test-result-format result #:optional (format (q compact)) (port (current-output-port)))
    "result -> result
    result is returned unmodified"
    ((hashtable-ref test-formats format) result port) result)

  (define (test-module-execute settings module) "alist (symbol ...)"
    ;environment* from (sph module) is used to make the define-test-module syntax work
    ((eval (q execute) module) settings))

  (define (test-modules-until a value) (take-while (l (a) (not (equal? a value))) a))
  (define (test-modules-only a values) (filter (l (a) (containsv? values a)) a))
  (define (test-modules-exclude a values) (remove (l (a) (containsv? values a)) a))

  (define (test-modules-apply-settings settings modules)
    "list:alist ((symbol ...) ...) -> ((symbol ...) ...)
    apply settings to a list of test-module names"
    (alist-quoted-bind settings (only exclude until)
      ( (if until test-modules-until (l (a b) a))
        (if only (test-modules-only modules only)
          (if exclude (test-modules-exclude modules exclude) modules))
        until)))

  (define (module-prefix->module-names settings name) "alist list -> ((symbol ...) ...)"
    (module-name->load-path+full-path& name #f
      (l (load-path full-path)
        (test-modules-apply-settings settings (path->module-names full-path #:load-path load-path)))))

  (define (module-prefix->names+modules settings module-prefix) "alist list -> (module ...)"
    (map (l (e) (pair e (environment* e))) (module-prefix->module-names settings module-prefix)))

  (define (test-module-prefix-execute settings . name)
    "list:alist (symbol ...) -> list:test-result:((module-name test-result ...) ...)
    modules must be in load-path. the load-path can be temporarily modified by other means. modules for testing are libraries/guile-modules that export an \"execute\" procedure.
    this procedure is supposed to return a test-result, for example from calling \"test-execute\"
    the implementation is depends on the following features:
    - module names are mapped to filesystem paths
    - modules can be loaded at runtime into a separate environment, and procedures in that environment can be called (this can be done with r6rs)"
    (map
      (l (name+module) (pair (first name+module) (test-module-execute settings (tail name+module))))
      (append-map (l (e) (module-prefix->names+modules settings e)) name)))

  #;(define (test-resolve-procedure name) "symbol -> procedure/boolean-false"
    (let (test-name (symbol-append (q test-) name))

      (if (defined? test-name) (eval test-name (current-module))
        (if (defined? name)
          (let (p (eval name (current-module))) (l (arguments . rest) (apply p arguments)))
          (error-create (q test-procedure-not-found) (list test-name name)
            (q test-resolve-procedure))))))

  (define (filter-module-names a) "list -> list" (filter list? a))
  (define (filter-procedure-names a) "list -> list" (filter symbol? a))

  (define (test-procedures-until a value) "list symbol -> list"
    (take-while (l (spec) (not (equal? (first spec) value))) a))

  (define (test-procedures-only a values) "list list -> list"
    (let (values (filter-procedure-names values))
      (filter (l (spec) (containsv? values (first spec))) a)))

  (define (test-procedures-exclude a values) "list list -> list"
    (let (values (filter-procedure-names values))
      (remove (l (spec) (containsv? values (first spec))) a)))

  (define (test-procedures-apply-settings settings a) "list list -> list"
    (alist-quoted-bind settings (only exclude until random-order?)
      ( (if random-order? randomise identity)
        ( (if until test-procedures-until (l (a b) a))
          (if only (test-procedures-only a only)
            (if exclude (test-procedures-exclude a (exclude)) a))
          until))))

  (define (test-success? result expected) "vector/any any -> boolean"
    (if (test-result? result) (test-result-success? result) (equal? result expected)))

  (define (test-procedures-execute-one p-display p-before exceptions? name test-proc . data)
    "procedure:{test-result -> test-result} procedure [arguments expected] ... -> vector:test-result"
    ;stops on failure. ensures that test-procedure results are test-results.
    ;creates only one test-result
    (let
      ( (title (symbol->string name))
        (test-proc
          (if exceptions? test-proc (l a (catch #t (thunk (apply test-proc a)) exception->string)))))
      (if (null? data)
        (let (r (test-proc (list) #t)) (and p-before (p-before name 0))
          (if (test-result? r)
            (p-display
              (record-update test-result r title (title-extend (test-result-title r) title)))
            (p-display (test-create-result (eqv? #t r) title 0 r (list) #t))))
        (let loop ((d data) (index 0))
          (if (null? d) (p-display (test-create-result #t title index))
            (let*
              ( (d-tail (tail d)) (expected (first d-tail))
                (r
                  (begin (and p-before (p-before name index))
                    (test-proc (any->list (first d)) expected))))
              (if (test-result? r)
                (if (test-result-success? r) (loop (tail d-tail) (+ 1 index))
                  (p-display
                    (record-update test-result r
                      title (title-extend (test-result-title r) title) index index)))
                (if (equal? r expected) (loop (tail d-tail) (+ 1 index))
                  (p-display (test-create-result #f title index r (first d) expected))))))))))

  (define (test-procedures-execute-parallel settings a p-display p-before exceptions?)
    "list:alist list procedure procedure -> list
    executes all tests even if some fail"
    (par-map (l (e) (apply test-procedures-execute-one p-display p-before exceptions? e)) a))

  (define (test-procedures-execute-serial settings a p-display p-before exceptions?)
    "list:alist list procedure procedure -> list
    executes tests one after another and stops if one fails"
    (map-with-continue
      (l (continue e)
        (let (r (apply test-procedures-execute-one p-display p-before exceptions? e))
          (if (test-result-success? r) (continue r) (list r))))
      a))

  (define (test-procedures-get-executor settings) "list -> procedure"
    (if (alist-quoted-ref settings parallel?) test-procedures-execute-parallel
      test-procedures-execute-serial))

  (define (p-display->p-display* p-display p-display-port) "procedure port -> procedure"
    (l (a) (p-display a p-display-port)))

  (define (test-procedures-execute settings a) "list -> list"
    ( (test-procedures-get-executor settings) settings (test-procedures-apply-settings settings a)
      (p-display->p-display* (alist-quoted-ref settings p-display)
        (alist-quoted-ref settings p-display-port))
      (alist-quoted-ref settings p-before) (alist-quoted-ref settings exceptions?)))

  (define (test-result? a) "any -> boolean"
    (and (vector? a) (= 7 (vector-length a)) (eqv? (q test-result) (vector-first a))))

  (define* (test-execute-module name #:optional (settings test-settings-default))
    "(symbol ...) list -> test-result" (test-module-execute settings (environment* name)))

  (define* (test-execute-modules-prefix name-prefix #:optional (settings test-settings-default))
    "execute all test-modules whose names begin with name-prefix.
    for example if there are modules (a b c) and (a d e), (test-execute-modules (a)) will execute both"
    (test-module-prefix-execute settings name-prefix))

  (define* (test-execute-procedures source #:optional (settings test-settings-default))
    "list:((symbol procedure any ...)) list -> test-result
    test-result-group: ([group-name] test-result/test-result-group ...)
    test-result: (test-result-group ...)"
    (test-procedures-execute settings source))

  (define-record test-result type-name success? title index result arguments expected)

  (define (title-extend title addition)
    "string/false string -> string
    prepend adition to title if title is true"
    (if title (string-append addition " " title)))

  (define (test-create-result . values)
    "boolean string integer any list any -> vector
    success? title index result arguments expected -> test-result"
    (apply record test-result (q test-result) values))

  (define (assert-failure-result result expected title arguments)
    "vector/any any false/string any -> vector:test-result"
    (if (test-result? result)
      (if (string? title)
        (record-update test-result result title (title-extend (test-result-title result) title))
        result)
      (if (string? title) (test-create-result #f title #f result arguments expected)
        (test-create-result #f "assertion" #f result arguments expected))))

  (define-syntax-rules assert-true
    ( (optional-title expr)
      (let (r expr) (if r #t (assert-failure-result r #t optional-title (q expr)))))
    ((expr) (assert-true #f expr)))

  (define-syntax-rule (assert-test-result title expr continue)
    ;string/false expression expression -> true/test-result:failure
    ;assertions create test-result vectors only on failure
    (let (r expr)
      (if (and r (or (boolean? r) (and (test-result? r) (test-result-success? r)))) continue
        (assert-failure-result r #t title (q expr)))))

  (define-syntax-rules assert-and-with-explicit-title ((title a) (assert-test-result title a #t))
    ( (title a a-rest ...)
      (assert-test-result title a (assert-and-with-explicit-title title a-rest ...))))

  (define-syntax-case (assert-and optional-title expr ...)
    ;creates a successul test result if all given expressions or assertions are true test results or true
    (let (optional-title-datum (syntax->datum (syntax optional-title)))
      (if (string? optional-title-datum)
        (syntax (assert-and-with-explicit-title optional-title expr ...))
        (syntax (assert-and-with-explicit-title #f optional-title expr ...)))))

  (define-syntax-rules assert-equal
    ( (optional-title expected expr)
      (let (r expr)
        (if (equal? expected r) #t (assert-failure-result r expected optional-title (q expr)))))
    ((expected expr) (assert-equal #f expected expr))))
