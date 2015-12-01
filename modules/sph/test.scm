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
    define-tests
    test-create-result
    test-execute
    test-execute-list
    test-execute-path
    test-format-get
    test-formats
    test-lambda
    test-list-normalise
    test-resolve-procedure
    test-results-display
    test-settings-default
    test-result-success?
    test-success?)
  (import
    (rnrs base)
    (sph)
    (sph alist)
    (sph error)
    (sph record)
    (only (guile)
      resolve-interface
      symbol-append
      defined?
      current-module
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
    (only (sph list) any->list map-with-continue)
    (only (sph list one) randomise)
    (only (sph module) current-module-ref path->module-names)
    (only (sph string) any->string)
    (only (sph vector) vector-first))

  (define-as test-settings-default alist-quoted
    random-order? #f parallel? #f exclude (list) only (list) until (list) before #f exceptions? #t)

  (define (test-module-execute settings name)
    ((module-ref (resolve-interface name) (q execute)) settings))

  (define (test-project-execute settings path)
    (map (l (e) (test-execute-module settings e)) (path->module-names path)))

  (define (test-execute-path settings a) "alist string -> test-result/error"
    (let (a (path->full-path a))
      (case (stat:type a) ((directory) (test-project-execute settings a))
        ((regular) (test-module-execute settings a))
        ( (symlink)
          ;as far as we know readlink fails for circular symlinks
          (test-execute-path settings (readlink a))))))

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

  (define (test-list-apply-settings settings a)
    ;todo: other settings
    (if (alist-quoted-ref settings random-order?) (randomise a) a))

  (define (test-success? result expected)
    (if (test-result? result) (test-result-success? result) (equal? result expected)))

  (define (test-list-execute-one name . data)
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
    (par-map (l (e) (apply test-list-execute-one e)) a))

  (define (test-list-execute-serial settings a)
    (map-with-continue
      (l (continue e)
        (let (r (apply test-list-execute-one e))
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

  (define* (test-execute source #:optional (settings test-settings-default))
    "list:alist string/list/procedure -> test-result
    test-result-group: (group-name test-result/test-result-group ...)
    test-result: (test-result-group ...)"
    (if (string? source) (test-execute-path settings source) (test-execute-list settings source)))

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
