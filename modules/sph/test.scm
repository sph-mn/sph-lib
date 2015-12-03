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
    test-create-result
    test-execute
    test-execute-list
    test-execute-module
    test-execute-modules
    test-formats
    test-lambda
    test-list-normalise
    test-resolve-procedure
    test-result-format
    test-result-success?
    test-results-display
    test-settings-default
    test-success?)
  (import
    (guile)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph alist)
    (sph error)
    (sph hashtable)
    (sph read-write)
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
      quasisyntax)
    (only (sph conditional) boolean-and)
    (only (sph filesystem) path->full-path)
    (only (sph list)
      any->list
      containsv?
      map-with-continue
      list-suffix?)
    (only (sph list one) randomise)
    (only (sph module)
      current-module-ref
      environment*
      path->module-names
      module-name->load-path+full-path&)
    (only (sph one) n-times)
    (only (sph string) any->string)
    (only (sph vector) vector-first)
    (only (srfi srfi-1) take-while remove))

  (define-syntax define-test-module
    (l (s)
      (syntax-case s (import)
        ( (_ (name-part ...) (import spec ...) body ...)
          (syntax
            (library (name-part ...) (export execute) (import (rnrs base) (sph) (sph test) spec ...) body ...)))
        ( (_ (name-part ...) body ...)
          (syntax (define-test-module (name-part ...) (import) body ...))))))

  (define (test-format-compact-one a group port) "vector:test-result-record port/any -> [any]"
    (display (test-result-title a) port)
    (let (index (test-result-index a))
      (if (and index (< 0 index))
        (begin (n-times index (l (n) (display " " port) (display (+ 1 n) port))) (newline port)))))

  (define (test-format-compact result port) "list port -> result"
    (let loop ((rest result) (depth 0) (group (list)))
      (if (null? rest) result
        (let (e (first rest))
          (if (list? e) (debug-log (q list) e)
            (begin (test-format-compact-one e group port) (loop (tail rest) depth group)))))))

  (define-as test-formats symbol-hashtable compact test-format-compact)

  (define* (test-result-format result #:optional (format (q compact)) (port (current-output-port)))
    "result -> result
    result is returned unmodified"
    ((hashtable-ref test-formats format) result port) result)

  (define-as test-settings-default alist-quoted
    random-order? #f parallel? #f exclude #f only #f until #f before #f exceptions? #t)

  (define (test-module-execute settings name) "alist (symbol ...)"
    ;environment* from (sph module) is used to make the define-test-module syntax work
    ((eval (q execute) (environment* name)) settings))

  (define (test-modules-apply-settings settings modules)
    "list:alist ((symbol ...) ...) -> ((symbol ...) ...)
    apply settings to a list of test-module names"
    (alist-quoted-bind settings (only exclude)
      ( (if only filter (if exclude remove (l (proc modules) modules)))
        (l (module-name) (any (l (only) (apply list-suffix? module-name only)) only)) modules)))

  (define (test-modules-execute settings name)
    "list:alist (symbol ...) -> list:test-result:((module-name test-result ...) ...)
    modules must be in load-path. the load-path can be temporarily modified by other means. modules for testing are libraries/guile-modules that export an \"execute\" procedure.
    this procedure is supposed to return a test-result, for example from calling \"test-execute\"
    the implementation is depends on the following features:
    - module names are mapped to filesystem paths
    - modules can be loaded at runtime into a separate environment, and procedures in that environment can be called (this can be done with r6rs)"
    (module-name->load-path+full-path& name #f
      (l (load-path full-path)
        (map (l (e) (let (r (test-module-execute settings e)) (if (error? r) r (pair e r))))
          (test-modules-apply-settings settings
            (path->module-names full-path #:load-path load-path))))))

  (define (test-resolve-procedure name) "symbol -> procedure/boolean-false"
    (let (test-name (symbol-append (q test-) name))
      (if (defined? test-name) (current-module-ref test-name)
        (let (p (current-module-ref name)) (l (arguments . rest) (apply p arguments))))))

  (define (test-list-normalise a)
    "list -> list
    validate, resolve test-procedures and normalise the list"
    (map
      (l (e)
        (cond ((symbol? e) (list e)) ((list? e) (pair (first e) (tail e)))
          ((procedure? e) (list e)) (else (error-create (q invalid-test-spec)))))
      a))

  (define (test-list-until a value) (take-while (l (e) (not (equal? e value))) a))
  (define (test-list-only a values) (filter (l (spec) (containsv? values (first spec))) a))
  (define (test-list-exclude a values) (remove (l (spec) (containsv? values (first spec))) a))

  (define (test-list-apply-settings settings a)
    (alist-quoted-bind settings (only exclude until random-order?)
      ( (if random-order? randomise identity)
        ( (if until test-list-until identity)
          (if only (test-list-only a only) (if exclude (test-list-exclude a exclude) a))))))

  (define (test-success? result expected)
    (if (test-result? result) (test-result-success? result) (equal? result expected)))

  (define (test-list-execute-one before name . data)
    "procedure [arguments expected] ... -> vector:test-result"
    ;stops on failure, ensures that test-procedure results are test-results.
    ;creates only one test-result
    (let ((test-proc (test-resolve-procedure name)) (title (symbol->string name)))
      (if (null? data)
        (let (r (test-proc (list) #t))
          (if (test-result? r)
            (record-update test-result r title (title-extend (test-result-title r) title))
            (test-create-result (eqv? #t r) title 0 r (list) #t)))
        (let loop ((d data) (index 0))
          (if (null? d) (test-create-result #t title index)
            (let*
              ( (d-tail (tail d)) (expected (first d-tail))
                (r (test-proc (any->list (first d)) expected)))
              (if (test-result? r)
                (if (test-result-success? r) (loop (tail d-tail) (+ 1 index))
                  (record-update test-result r
                    title (title-extend (test-result-title r) title) index index))
                (if (equal? r expected) (loop (tail d-tail) (+ 1 index))
                  (test-create-result #f title index r (first d) expected)))))))))

  (define (test-list-execute-parallel settings a)
    "list:alist list -> list
    executes all tests even if some fail"
    (par-map (l (e) (apply test-list-execute-one #f e)) a))

  (define (test-list-execute-serial settings a)
    (map-with-continue
      (l (continue e)
        (let (r (apply test-list-execute-one #f e))
          (if (test-result-success? r) (continue r) (list r))))
      a))

  (define (test-list-get-executor settings)
    (if (alist-quoted-ref settings parallel?) test-list-execute-parallel test-list-execute-serial))

  (define (test-execute-list settings a) "list -> list"
    ( (test-list-get-executor settings) settings
      (test-list-apply-settings settings (test-list-normalise a))))

  (define (test-result? a) "any -> boolean"
    (and (vector? a) (= 7 (vector-length a)) (eqv? (q test-result) (vector-first a))))

  (define test-result-group? list?)

  (define* (test-execute-module name #:optional (settings test-settings-default))
    (test-module-execute settings name))

  (define* (test-execute-modules name-prefix #:optional (settings test-settings-default))
    "execute all test-modules whose names begin with name-prefix.
    for example if there is a module (a b c) and (a d e), (test-execute-modules (a)) will execute both.
    additional settings are:
    - exclude: do not execute matching modules. ((symbol ...) ...). a list of module name suffixes. for example (b c) matches the module (a b c)
    - only: execute no other tests than matching modules. format is the same as for exclude. if both \"only\" and \"exclude\" are specified, \"only\" is used"
    (test-modules-execute settings name-prefix))

  (define* (test-execute source #:optional (settings test-settings-default))
    "list:alist list/procedure -> test-result
    test-result-group: (group-name test-result/test-result-group ...)
    test-result: (test-result-group ...)"
    (test-execute-list settings source))

  (define-syntax-cases test-lambda s
    (((arguments expected) body ...) (syntax (lambda (arguments expected) body ...)))
    ( ( (a ...) body ...)
      (quasisyntax (lambda (a ... . (unsyntax (datum->syntax s (gensym "define-test")))) body ...))))

  (define-syntax-cases define-test
    ( ( (name parameter ...) body ...)
      (syntax (define-test name (test-lambda (parameter ...) body ...))))
    ( (name proc)
      (let (name-datum (syntax->datum (syntax name)))
        (quasisyntax
          (define (unsyntax (datum->syntax (syntax name) (symbol-append (q test-) name-datum)))
            proc)))))

  (define-record test-result type-name success? title index result arguments expected)
  (define-syntax-rule (define-tests name test ...) (define name (qq (test ...))))

  (define (title-extend title addition) "string/false string -> vector"
    (if title (string-append addition " " title)))

  (define (test-create-result . values)
    "boolean string integer any list any -> vector
    success? name index result arguments expected -> test-result"
    (apply record test-result (q test-result) values))

  (define (assert-failure-result result expected title arguments)
    ;vector/any any false/string any -> vector
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
    (let (optional-title-datum (syntax->datum (syntax optional-title)))
      (if (string? optional-title-datum)
        (syntax (assert-and-with-explicit-title optional-title expr ...))
        (syntax (assert-and-with-explicit-title #f optional-title expr ...)))))

  (define-syntax-rules assert-equal
    ( (optional-title expected expr)
      (let (r expr)
        (if (equal? expected r) #t (assert-failure-result r expected optional-title (q expr)))))
    ((expected expr) (assert-equal #f expected expr))))
