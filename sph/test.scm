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
    test-create-result
    test-define
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
    test-success?)
  (import
    (rnrs base)
    (sph))

  (define-as test-settings-default vhash-quoted
    random-order? #f parallel? #f exclude (list) only (list) until (list) before #f exceptions? #t)

  (define (test-execute-module settings name)
    ((module-ref (resolve-interface name) (q execute)) settings))

  (define (test-execute-project settings path)
    (map (l (e) (test-execute-module settings e)) (path->module-names path)))

  (define (test-execute-path settings a) "vhash string -> test-result/error"
    (let (a (path->full-path a))
      (case (stat:type a) ((directory) (test-execute-project settings a))
        ((regular) (test-execute-module settings a))
        ( (symlink
            ;as far as we know readlink fails for circular symlinks
            (test-execute-path (readlink a)))))))

  (define (test-resolve-procedure name) "symbol -> procedure/boolean-false"
    (let (test-name (symbol-append (q test-) name))
      (module-ref (current-module) (if (defined? test-name) test-name name))))

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
    (if (alist-ref-quoted? settings random-order?) (randomise a) a))

  (define (test-success? result expected)
    (if (test-result? result) (test-result-success? result) (equal? result expected)))

  (define (test-list-execute-one name . data)
    "procedure [arguments expected] ... -> vector:test-result"
    (let ((test-proc (test-resolve-procedure name)) (title (symbol->string name)))
      (if (null? data) (test-proc (list) #t)
        (let loop ((d data) (index 0))
          (if (null? d) index
            (let* ((d-tail (tail d)) (r (test-proc (first d) (first d-tail))))
              (if (test-result? r)
                (if (test-result-success? r) (loop (tail d-tail) (+ 1 index))
                  (record-update test-result r
                    title (title-extend (test-result-title r) title) index index))
                (if (equal? r expected) (loop (tail d-tail) (+ 1 index))
                  (test-create-result #f title index r (first d) (first d-tail))))))))))

  (define (test-list-execute-serial settings a)
    (map-with-continue
      (l (continue e)
        (let (r (apply test-list-execute-one e))
          (if (test-result-success? r) (continue r) (list r))))))

  (define (test-list-get-executor settings)
    (if (alist-quoted-ref settings parallel?) test-list-execute-parallel test-list-execute-serial))

  (define (test-execute-list settings a) "list -> list"
    ((test-list-get-executor settings) (test-list-apply-settings settings (test-list-normalise a))))

  (define (test-result? a) "any -> boolean"
    (and (vector? a) (= 7 (vector-length a)) (eqv? (q test-result) (vector-first a))))

  (define test-result-group? list?)

  (define (test-execute settings source)
    "vhash string/list/procedure -> test-result
    test-result-group: (group-name test-result/test-result-group ...)
    test-result: (test-result-group ...)"
    (if (string? source) (test-execute-path settings source) (test-execute-list settings source)))

  (define-syntax-cases test-lambda s
    (((arguments expected) body ...) (syntax (lambda (arguments expected) body ...)))
    ( ( (a ...) body ...)
      (quasisyntax (lambda (a ... . (unsyntax (datum->syntax s (gensym "test-define")))) body ...))))

  (define-syntax-cases test-define
    ( ( (name parameter ...) body ...)
      (syntax (test-define name (test-lambda (parameter ...) body ...))))
    ( (name proc)
      (let (name-datum (syntax->datum (syntax name)))
        (quasisyntax
          (define (unsyntax (datum->syntax (syntax name) (symbol-append (q test-) name-datum)))
            proc)))))

  (define-record test-result type-name success? title index result arguments expected)

  (define (title-extend title addition) "string/false string -> vector"
    (if title (string-append addition " " title)))

  (define (test-create-result . values)
    "boolean string integer any list any -> vector
    success? name index result arguments expected -> test-result"
    (apply record (q test-result) values))

  (define-syntax-rule (assert-failure-result result expected optional-title body)
    ;vector/any any false/string any -> vector
    (if (test-result? result)
      (if (string? optional-title)
        (record-update test-result result
          title (title-extend (test-result-title result) optional-title))
        result)
      (if (string? optional-title) (test-create-result #f optional-title #f result #f expected)
        (test-create-result #f "assertion" #f result (any->string (q body)) expected))))

  (define-syntax-rules assert-true
    ( (optional-title body)
      (let (r body) (if (eqv? #t r) #t (assert-failure-result r #t optional-title body))))
    ((body) (assert-true #f body)))

  (define-syntax-case (assert-and optional-title body ...)
    (let (optional-title-datum (syntax->datum (syntax optional-title)))
      (if (string? optional-title-datum)
        (syntax (assert-true optional-title (boolean-and body ...)))
        (syntax (boolean-and optional-title body ...)))))

  (define-syntax-rules assert-equal
    ( (optional-title expected body)
      (let (r body)
        (if (equal? expected r) #t (assert-failure-result r expected optional-title body))))
    ((expected body) (assert-equal #f expected body))))
