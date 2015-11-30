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
    test-execute
    test-format-get
    test-formats
    test-results-display)
  (import
    (rnrs base)
    (sph))

  (define-as test-settings-default vhash-quoted
    random-order? #f
    parallel? #f
    continuation-passing-style? #f exclude (list) only (list) until (list) before #f exceptions? #t)

  ;file -> module, directory -> project

  (define (test-execute-module settings name)
    ((module-ref (resolve-interface name) (q execute)) settings))

  (define (test-execute-project settings path)
    (map (l (e) (test-execute-module settings e)) (path->module-names path)))

  (define (test-execute-path settings a)
    ;symlinks to directories are not recognised
    (let (a (path->full-path a))
      (case (stat:type a) ((directory) (test-execute-project settings a))
        ((regular) (test-execute-module settings a))
        ( (symlink
            ;as far as we know readlink fails for circular symlinks
            (test-execute-path (readlink a)))))))

  (define
    (old-test-execute-module-create-result format index-data-last name index-data data-test r)
    "symbol integer string integer list any -> boolean"
    (let*
      ( (is-expected (equal? (tail data-test) r))
        (is-extended-result (and (not is-expected) (extended-result? r))))
      (if (or is-expected (and is-extended-result (equal? (tail data-test) (vector-ref r 1))))
        (test-create-result-module name (if is-extended-result (vector-ref r 2) #f)
          index-data index-data-last)
        (if is-extended-result
          (test-create-result-module name (vector-ref r 1)
            (vector-ref r 2) (or (vector-ref r 3) (first data-test))
            (vector-ref r 4) index-data index-data-last)
          (test-create-result-module name r
            (tail data-test) (first data-test) #f index-data index-data-last)))))

  (define old-test-execute-module-test-spec
    (let
      ( (with-test-data
          (l (test-spec)
            (fold-unless
              (l (data-test index) (if before-test (before-test name/proc index))
                (if
                  (apply evaluate-result format-display
                    (- (/ (- (length test-spec) 1) 2) 1)
                    (if (procedure? name/proc) (apply-test-proc name/proc #f data-test index)
                      (apply-test-proc-from-name (first test-spec)
                        (test-proc-name (first test-spec)) data-test index)))
                  (+ 1 index) #f))
              not #f 0 (list->alist (tail test-spec)))))
        (without-test-data
          (l (settings name/proc test-spec) (if before-test (before-test name/proc 1))
            (apply evaluate-result format-display
              0
              (if (procedure? name/proc) (apply-test-proc name/proc #f (pair (list) #t) 0)
                (apply-test-proc-from-name (first test-spec) (test-proc-name name/proc)
                  (pair (list) #t) 0))))))
      (l (settings test-spec)
        (let (name/proc (first test-spec))
          (if (> (length test-spec) 1) (with-test-data& settings name/proc test-spec)
            (without-test-data& settings name/proc test-spec))))))

  (define (default-test proc-name index inp exp) "apply a procedure proc-name with arguments inp"
    (apply (module-ref* proc-name) (if (list? inp) inp (list inp))))

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
    (if (alist-ref-quoted? settings randomised?) (randomise a) a))

  (define (old-test-execute-module-tests-serial settings tests)
    "(test-spec ...) -> result-module
    try to execute all tests in list and stop on any test-failure"
    (fold-until
      (l (data-test index) (if before-test (before-test name/proc index))
        (if
          (apply evaluate-result format-display
            (- (/ (- (length test-spec) 1) 2) 1)
            (if (procedure? name/proc) (apply-test-proc name/proc #f data-test index)
              (apply-test-proc-from-name (first test-spec) (test-proc-name (first test-spec))
                data-test index)))
          (+ 1 index) #f))
      not #f 0 (list->alist (tail test-spec))))

  (define (test-list-execute-one name . data)
    "procedure [arguments expected] ... -> vector:test-result"
    (let (test-proc (test-resolve-procedure name))
      (if (null? data) (test-proc (list) #t)
        (let loop ((d data) (index 0))
          (if (null? d) index
            (let* ((d-tail (tail d)) (r (test-proc (first d) (first d-tail))))
              (loop (tail d-tail) (+ 1 index))))))))

  (define (test-list-execute-serial settings a)
    (map-with-continue
      (l (continue e)
        (let (r (apply test-list-execute-spec e)) (if (vector-first r) (continue r) (list r))))))

  (define (test-list-get-executor settings)
    (if (alist-quoted-ref settings parallel?) test-list-execute-parallel test-list-execute-serial))

  (define (test-execute-list settings a) "list -> list"
    ((test-list-get-executor settings) (test-list-apply-settings settings (test-list-normalise a))))

  (define test-result? vector?)
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

  (define-record test-result
    ;boolean integer list any
    success? name index result arguments expected)

  (define (assert-extended-result? a)
    (and (vector? a) (= (vector-length a) 5) (eqv? (q assert-extended-result) (vector-ref a 0))))

  (define-syntax-rule (assert-failure-result result expected optional-title body)
    (if (extended-result? result)
      (begin
        (if (string? optional-title)
          (vector-set! result 2 (string-append optional-title " " (vector-ref result 4))))
        result)
      (test-create-result success? name index result arguments expected)
      (test-create-result result
        (if (or (not (string? optional-title)) (string-null? optional-title))
          (any->string (q body)) optional-title)
        expected)))

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
    ( (optional-title exp body)
      (let (r body) (if (equal? exp r) #t (assert-failure-result r exp optional-title body))))
    ((exp body) (assert-equal #f exp body)))

  ;-- old --;
  ;defines if successful tests produce a success message
  (define sph-test-log-success #t)
  (define before-test #f)
  (define disabled-before-test #f)
  (define prev-message-newline #t)
  (define-syntax-rule (format-index-number a) (number->string (+ a 1)))
  (define (test-disable-before-test) (set! disabled-before-test before-test) (set! before-test #f))
  (define (test-enable-before-test) (set! before-test disabled-before-test))
  (define-syntax-rule (module-ref* name) (module-ref (current-module) name))

  (define (failure-message-compact name out exp inp title index-data index-data-last)
    "symbol any any any integer integer -> unspecified"
    (let
      ( (indent (if (and (> index-data 0) sph-test-log-success) indent-string ""))
        (indent-data
          (if (or (not sph-test-log-success) (= index-data 0)) indent-string indent-string)))
      (string-append (if prev-message-newline "" "\n") indent
        "failure " name
        " " (format-index-number index-data)
        (if title (string-append " - " title) "")
        (if (null? inp) ""
          (string-append "\n" indent-data
            "inp "
            (if (list? inp) (string-join (map any->string-write inp) " ") (any->string-write inp))))
        (if (eqv? (q undefined) exp) ""
          (string-append "\n" indent-data "exp " (any->string-write exp)))
        "\n" indent-data "out " (any->string-write out) (begin (set! prev-message-newline #t) "\n"))))

  (define indent-string (string-multiply " " 2))

  (define (success-message-compact name title index-data index-data-last)
    (let (indent (if (> index-data 0) " " ""))
      (if (or (= index-data 0) title prev-message-newline)
        (string-append (if prev-message-newline "" "\n") indent
          (if (= index-data 0) name "")
          (if (= index-data-last 0) "" (string-append " " (format-index-number index-data)))
          (begin (set! prev-message-newline (or title (= index-data index-data-last)))
            (if prev-message-newline (string-append (if title (string-append " - " title) "") "\n")
              "")))
        (string-append " " (format-index-number index-data)
          (begin
            (set! prev-message-newline (or (= index-data index-data-last) prev-message-newline))
            (if prev-message-newline "\n" ""))))))

  (define (success-message-csv name title index-data index-data-last)
    (list->csv-line (list "success" name (format-index-number index-data) (if title title ""))))

  (define (failure-message-csv name out exp inp title index-data index-data-last)
    (list->csv-line
      (list "failure" name (format-index-number index-data) (if title title "") inp exp out)))

  (define (success-message-scm name title index-data index-data-last)
    (string-append (any->string-write (list "success" name (+ index-data 1) (if title title "")))
      "\n"))

  (define (failure-message-scm name out exp inp title index-data index-data-last)
    (string-append
      (any->string-write (list "failure" name (+ index-data 1) (if title title "") inp exp out)) "\n"))

  (define test-display-formats
    (symbol-hashtable compact (pair failure-message-compact success-message-compact)
      scm (pair failure-message-scm success-message-scm)
      csv (pair failure-message-csv success-message-csv)))

  (define-syntax-rule (success-message-proc format)
    (tail (hashtable-ref test-display-formats format)))

  (define-syntax-rule (failure-message-proc format)
    (first (hashtable-ref test-display-formats format)))

  (define-syntax-rule (execute-tests-quasiquote test-spec ...) (execute-tests (qq (test-spec ...))))

  (define* (execute-tests-randomise tests #:optional (count 1) duplicate-count)
    "(procedure ...) integer integer ->
    execute all tests in list in randomised-order count number of times with each procedure duplicated by duplicate-count.
    stop on any test-failure"
    (let
      (tests
        (randomise
          (if duplicate-count
            (n-times-accumulate duplicate-count tests (l (n prev) (append prev tests))) tests)))
      (every identity (n-times-map count (l n (execute-tests tests))))))

  (define-syntax-rule (test-fail title data) (test-create-result #f title #f #f data #f))

  (define-syntax-rule (define-tests-quasiquote name test ...)
    ;defines a list of tests bound to a variable. like execute-tests-quasiquote without executing
    (define name (qq (test ...))))

  (define (test-compare out exp)
    "return out if it does not match exp (expected-output).
    boolean values for exp are used for checking general true or false value of an expression
    like as a condition in an if expression."
    (if (boolean? exp) (if out (if exp #t out) (if exp out #t)) (if (equal? out exp) #t out))))
