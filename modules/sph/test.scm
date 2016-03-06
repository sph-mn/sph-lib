; (sph test) - automated code testing
; written for the guile scheme interpreter
; Copyright (C) 2015-2016 sph <sph@posteo.eu>
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
    define-procedure-tests
    define-test
    define-test-module
    test-cli
    test-create-result
    test-execute-cli
    test-execute-module
    test-execute-modules-by-prefix
    test-execute-procedures
    test-execute-procedures-lambda
    test-lambda
    test-list
    test-path->module-names
    test-result
    test-result-success?
    test-settings-default
    test-settings-default-custom
    test-settings-default-custom-by-list
    test-success?)
  (import
    (guile)
    (rnrs eval)
    (sph)
    (sph alist)
    (sph cli)
    (sph conditional)
    (sph error)
    (sph list)
    (sph list one)
    (sph module)
    (sph record)
    (sph string)
    (sph test base)
    (sph test report)
    (srfi srfi-1)
    (only (sph filesystem) path->full-path)
    (only (sph one)
      quote-odd
      ignore
      exception->string)
    (only (sph two) remove-keyword-associations))

  ;data-structures:
  ;  test-result: ([group-name] test-result ...)/test-result-record

  (define-as test-settings-default alist-quoted
    reporters test-reporters-default
    reporter-name (q default)
    search-type (q prefix)
    hook
    (alist-quoted procedure-before ignore
      procedure-after ignore
      procedure-data-before ignore
      procedure-data-after ignore
      module-before ignore module-after ignore modules-before ignore modules-after ignore)
    random-order? #f parallel? #f exceptions? #t exclude #f only #f until #f)

  (define-syntax-rule (test-settings-default-custom key/value ...)
    ;[any:unquoted-key any:value] ... -> list
    (test-settings-default-custom-by-list (quote-odd key/value ...)))

  (define (test-settings-default-custom-by-list key/value)
    "[key value] ... -> list
    get the default test settings, with values possibly set to the values given with \"key/value\""
    (alist-merge test-settings-default (list->alist key/value)))

  (define (settings->hook a name) "list symbol -> procedure" (alists-ref a (q hook) name))

  (define (settings->reporter a) "list -> (procedure . alist:hooks)"
    (test-reporter-get (alist-quoted-ref a reporters) (alist-quoted-ref a reporter-name)))

  (define (settings->reporter-hook a name) "hashtable -> (report-write . report-hooks)"
    (alist-ref (tail (settings->reporter a)) name))

  (define (identity-if-list settings maybe-new-settings)
    (if (list? maybe-new-settings) maybe-new-settings settings))

  (define (call-settings-update-hook proc settings . a)
    (identity-if-list settings (apply proc settings a)))

  (define (apply-settings-reporter+hook settings name . a) "list symbol any ... -> list"
    (apply (settings->reporter-hook settings name) settings a)
    (identity-if-list (apply (settings->hook settings name) settings a) settings))

  (define (apply-settings-hook+reporter settings name . a) "list symbol any ... ->"
    (apply (settings->hook settings name) settings a)
    (apply (settings->reporter-hook settings name) settings a))

  (define test-cli
    (cli-create #:parameters "options ... source ..."
      #:description
      (string-join
        (list
          "execute tests via a command-line interface. \"source\" is filesystem paths to files containing modules relative to any guile load-path."
          "if a source path points to a directory, a module with a name corresponding to that path and all modules under that directory are used."
          "exclude/only/until take a list of comma separated module name suffixes or full module names in the format \"a//b//c\" as well as individual (test) procedure names (without the internal \"test-\" prefix).
          usage: (test-cli) in a scheme file makes the code parse command-line arguments (arguments to \"exec\")")
        "\n  ")
      #:options
      (ql ((source ...)) (display-format #f #f #t)
        (add-to-load-path #f #f #t) (path-search #f #f #t)
        (search-type) (exclude #f #f #t) (only #f #f #t) (until #f #f #t))))

  (define (path->load-path+path& a c) "string procedure:{? string -> any} -> any"
    (let (load-path (path->load-path a))
      (if load-path (c load-path a) (let (a (string-append a ".scm")) (c (path->load-path a) a)))))

  (define (test-path->module-names a) "string -> list/error"
    (path->load-path+path& a
      (l (load-path a)
        (if load-path
          (case (stat:type (stat (string-append load-path "/" a)))
            ((directory) (find-modules (path->module-name a) (q prefix) %load-path))
            ((regular) (list (path->module-name a)))
            ( (symlink)
              ;as far as we know readlink fails for circular symlinks
              (test-path->module-names (readlink a))))
          (error-create (q file-not-found-in-load-path))))))

  (define (cli-value-path/module-list a)
    "false/string -> false/list
    parsed a list of comma separated filesystem or module paths, \"/\" or \"//\" separated respectively, without filename suffixes"
    (false-if-not a
      (map
        (l (e) (if (string-contains e "//") (map string->symbol (string-split-regexp e "//")) e))
        (string-split a #\,))))

  (define (cli-value-reporter reporters a) "false/string -> procedure"
    (test-reporter-get reporters (string->symbol a)))

  (define (cli-add-to-load-path! cli-arguments)
    "list ->
    if the --add-to-load-path option has been specified, add the comma separated list of paths given
    as a value to the option to the beginning of the module load-path"
    (pass-if (alist-quoted-ref cli-arguments add-to-load-path)
      (l (a) (map (l (e) (add-to-load-path e)) (string-split a #\,)))))

  (define (test-execute-cli-get-settings cli-arguments)
    "list -> list
    create the test settings object from program arguments"
    (alist-quoted-bind cli-arguments (reporter exclude only until)
      (cli-add-to-load-path! cli-arguments)
      (alist-quoted-merge-key/value test-settings-default reporter-name
        (cli-value-reporter (alist-quoted-ref test-settings-default reporters) reporter) exclude
        (cli-value-path/module-list exclude) only
        (cli-value-path/module-list only) until (cli-value-path/module-list until))
      test-settings-default))

  (define (test-execute-cli)
    "parse program arguments and run the rest of the program depending on the given arguments.
    creates a command-line interface for executing tests in test module files"
    (let* ((arguments (test-cli)) (settings (test-execute-cli-get-settings arguments)))
      (alist-quoted-bind arguments (source)
        (if source (test-modules-execute settings (append-map test-path->module-names source))
          (list)))))

  (define-syntax-cases test-lambda s
    (((arguments expected settings) body ...) (syntax (lambda (arguments expected) body ...)))
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

  (define (macro?* a) (false-if-exception (macro? (module-ref (current-module) a))))

  (define-syntax-cases test-list-one
    ;-> (symbol procedure [arguments expected] ...)/error
    ( ( (name data ...))
      (let*
        ( (name-datum (syntax->datum (syntax name)))
          (test-name (datum->syntax (syntax name) (symbol-append (q test-) name-datum))))
        (quasisyntax
          (pairs (quote name)
            ;eval, to avoid "possibly undefined variable" warnings
            (eval
              (quote
                (if (defined? (quote (unsyntax test-name))) (unsyntax test-name)
                  (unsyntax
                    (if (macro?* name-datum)
                      (syntax (error-create (q macro-instead-of-test-procedure) (quote name)))
                      (syntax (lambda (arguments . rest) (apply name arguments)))))))
              (current-module))
            (quasiquote (data ...))))))
    ((name) (syntax (test-list-one (name)))))

  (define-syntax-rule (test-list test-spec ...) (list (test-list-one test-spec) ...))

  (define-syntax-rule (define-procedure-tests name test-spec ...)
    ;symbol symbol/list -> ((symbol procedure any ...) ...)
    ;define procedure tests that can be executed with "test-execute-procedures".
    ;resolves procedures by name in test-specs and normalises the test specification
    (define name (test-list test-spec ...)))

  (define-syntax-rule (test-execute-procedures-lambda test-spec ...)
    ;create a procedure that executes procedure tests corresponding to test-spec
    ((l (tests) (l (settings) (test-execute-procedures settings tests))) (test-list test-spec ...)))

  (define-syntax define-test-module
    ;test modules are typical modules/libraries that export only one procedure named "execute". the syntax defined here abstracts the definition.
    ;modules are used to give test modules a separated scope in which test module dependencies are integrated.
    ;there might be alternatives to using modules - for example evaluating files in custom environments that use the import statement. but modules seem, because of their
    ;possible composition, like the best solution at this point.
    ;the syntax defined here must be available in the environment in which the test module is loaded.
    ;to archieve this, the definition is initially evaluated in the top-level environment (loading/definition) and the module object is later resolved using environment* (resolving)
    (l (s)
      (syntax-case s (import)
        ( (_ (name-part ...) (import spec ...) body ... proc)
          ;using eval because exporting bindings created with define created by syntax was not working
          (syntax
            (eval
              (quote
                (library (name-part ...) (export test-execute) (import (guile) (rnrs base) (sph) (sph test) spec ...) body ... (define test-execute proc)))
              (current-module))))
        ( (_ (name-part ...) body ...)
          (syntax (define-test-module (name-part ...) (import) body ...))))))

  (define (test-module-execute settings module name)
    "alist module/environment (symbol ...) -> test-result"
    (let*
      ( (settings (alist-quoted-merge-key/value settings current-module-name name))
        (settings (apply-settings-reporter+hook settings (q module-before) name))
        (r ((eval (q test-execute) module) settings)))
      (apply-settings-hook+reporter settings (q module-after) name r) r))

  (define (test-modules-until a value) (take-while (l (a) (not (equal? a value))) a))
  (define (test-modules-only a values) (filter (l (a) (containsv? values a)) a))
  (define (test-modules-exclude a values) (remove (l (a) (containsv? values a)) a))

  (define (test-modules-apply-settings settings modules)
    "list ((symbol ...) ...) -> ((symbol ...) ...)
    apply settings to a list of test-module names"
    (alist-quoted-bind settings (only exclude until)
      ( (if until test-modules-until (l (a b) a))
        (if only (test-modules-only modules only)
          (if exclude (test-modules-exclude modules exclude) modules))
        until)))

  (define (find-modules name search-type load-paths)
    "(symbol ...) symbol:exact/prefix/prefix-not-exact (string ...) -> ((symbol ...) ...)"
    (let
      ( (search
          (l (name filename-extension) "list string/boolean -> list"
            (or
              (module-name->load-path+full-path& name filename-extension
                load-paths
                (l (load-path full-path) "(string ...) string -> list"
                  (path->module-names full-path #:load-path load-path)))
              (list))))
        (filename-extensions
          (append
            (if (or (eqv? (q prefix) search-type) (eqv? (q prefix-not-exact) search-type))
              (list #f) (list))
            (if (or (eqv? (q prefix) search-type) (eqv? (q exact) search-type)) (list ".scm")
              (list)))))
      (append-map (l (e) (search name e load-paths)) filename-extensions)))

  (define (test-modules-execute settings module-names) "list list -> test-result"
    (let*
      ( (settings (apply-settings-reporter+hook settings (q modules-before) module-names))
        (r
          (map
            (l (name module) (pair (any->string name) (test-module-execute settings module name)))
            module-names (map environment* module-names))))
      (apply-settings-hook+reporter settings (q modules-after) module-names r) r))

  (define (settings->load-path! a)
    "list -> (string ...)
    adds the value for the \"path-search\" setting to the global load-path and returns the value in a list to be used as a load-path variable, if the path-search value is not false.
    otherwise returns %load-path"
    (let (path-search (alist-quoted-ref a path-search))
      (if path-search
        (let (path (path->full-path path-search)) (add-to-load-path path) (list path)) %load-path)))

  (define (test-modules-by-prefix-execute settings . name)
    "list (symbol ...) ... -> test-result
    execute all test-modules whose names begin with name-prefix.
    for example if there are modules (a b c) and (a d e), (test-execute-modules (a)) will execute both
    modules must be in load-path. the load-path can be temporarily modified by other means. modules for testing are libraries/guile-modules that export an \"execute\" procedure.
    this procedure is supposed to return a test-result, for example from calling \"test-execute\"
    the implementation is depends on the following features:
    - module names are mapped to filesystem paths
    - modules can be loaded at runtime into a separate environment, and procedures in that environment can be called (this can be done with r6rs)"
    (let
      ( (load-path (settings->load-path! settings))
        (search-type (alist-quoted-ref settings search-type)))
      (let
        (module-names
          (every-map (l (e) (false-if-null (find-modules e search-type load-path))) name))
        (if module-names
          (test-modules-execute settings
            (test-modules-apply-settings settings (apply append module-names)))
          (error-create (q module-not-found) name)))))

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

  (define (test-any->result result title index)
    (test-create-result (eqv? #t result) title #f index result (list) #t))

  (define
    (test-procedures-execute-one-data settings data name title test-proc hook-before hook-after
      hook-data-before
      hook-data-after
      report-before
      report-after
      report-data-before
      report-data-after)
    "list (input output [input output] ...) string string procedure ... -> test-result"
    (let loop ((d data) (index 0))
      (if (null? d) (test-create-result #t title #f index)
        (let*
          ( (arguments (any->list (first d)))
            (settings
              (begin (report-data-before settings name index arguments)
                (call-settings-update-hook hook-data-before settings name index arguments)))
            (d-tail (tail d)) (expected (first d-tail))
            (r (test-proc arguments expected settings))
            (r
              (if (test-result? r) (record-update test-result r title title index index)
                (test-create-result (equal? r expected) title #f index r arguments expected))))
          (hook-data-after settings r) (report-data-after settings r)
          (if (test-result-success? r) (loop (tail d-tail) (+ 1 index)) r)))))

  (define
    (test-procedures-execute-without-data settings name title test-proc hook-before hook-after
      hook-data-before
      hook-data-after
      report-before
      report-after
      report-data-before
      report-data-after)
    (report-data-before settings name 0 (list))
    (let*
      ( (settings (call-settings-update-hook hook-data-before settings name 0 (list)))
        (r (test-proc (list) #t settings))
        (r
          (if (test-result? r) (record-update test-result r title title)
            (test-any->result r title 0))))
      (hook-data-after settings r) (report-data-after settings r) r))

  (define (test-procedures-execute-one settings exceptions? hooks name test-proc . data)
    "procedure:{test-result -> test-result} procedure [arguments expected] ... -> vector:test-result"
    ;stops on failure. ensures that test-procedure results are test-results.
    ;creates only one test-result
    (list-bind hooks
      (hook-before hook-after hook-data-before
        hook-data-after report-before report-after report-data-before report-data-after)
      (let
        ( (title (symbol->string name))
          (test-proc
            (if exceptions? test-proc
              (l a (catch #t (thunk (apply test-proc a)) exception->string)))))
        (report-before settings name)
        (let*
          ( (settings (call-settings-update-hook hook-before settings name))
            (r
              (if (null? data)
                (apply test-procedures-execute-without-data settings name title test-proc hooks)
                (apply test-procedures-execute-one-data settings data name title test-proc hooks))))
          (hook-after settings r) (report-after settings r) r))))

  (define (test-procedures-execute-parallel settings a exceptions? hooks)
    "list:alist list -> list
    executes all tests even if some fail"
    (par-map (l (e) (apply test-procedures-execute-one settings exceptions? hooks e)) a))

  (define (test-procedures-execute-serial settings a exceptions? hooks)
    "list:alist list -> list
    executes tests one after another and stops if one fails"
    (map-with-continue
        (l (continue e)
          (let (r (apply test-procedures-execute-one settings exceptions? hooks e))

            (if (test-result-success? r) (continue r) (list r))))
        a))

  (define test-procedures-execute
    (let
      ( (get-executor
          (l (settings) "list -> procedure"
            (if (alist-quoted-ref settings parallel?) test-procedures-execute-parallel
              test-procedures-execute-serial)))
        (settings->procedure-hooks
          (l (settings) "list -> (procedure:hook-before hook-after report-before report-after)"
            (append
              (alist-select (alist-quoted-ref settings hook)
                (ql procedure-before procedure-after procedure-data-before procedure-data-after))
              (alist-select (tail (settings->reporter settings))
                (ql procedure-before procedure-after procedure-data-before procedure-data-after))))))
      (l (settings source)
        "list ((symbol:name procedure:test-proc any:data-in/out ...) ...) -> test-result"
        ( (get-executor settings) settings (test-procedures-apply-settings settings source)
          (alist-quoted-ref settings exceptions?) (settings->procedure-hooks settings)))))

  (define (test-execute-module settings name) "list (symbol ...) -> test-result"
    (test-module-execute settings (environment* name) name))

  (define test-execute-procedures test-procedures-execute)

  (define*
    (test-execute-modules-by-prefix #:key (settings test-settings-default) #:rest module-names)
    "execute all test modules whose module name has the one of the given module name prefixes.
    \"path-search\" restricts the path where to search for test-modules.
    \"search-type\" does not execute modules that exactly match the module name prefix (where the module name prefix resolves to a regular file)"
    (apply test-modules-by-prefix-execute settings (remove-keyword-associations module-names)))

  (define (assert-failure-result result expected title arguments)
    "vector/any any false/string any -> vector:test-result"
    (if (test-result? result)
      (if (string? title) (record-update test-result result assert-title title) result)
      (if (string? title) (test-create-result #f title #f #f result arguments expected)
        (test-create-result #f #f "assertion" #f result arguments expected))))

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
