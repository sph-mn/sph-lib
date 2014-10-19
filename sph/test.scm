; (sph test) - automated code testing
; written for the guile scheme interpreter
; Copyright (C) 2010-2014 sph <sph@posteo.eu>
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
    assert-equal
    assert-true
    before-test
    default-test
    define-tests-quasiquote
    execute-tests
    execute-tests-quasiquote
    execute-tests-randomise
    import-unexported
    sph-test-log-success
    test-compare
    test-disable-before-test
    test-display-formats
    test-enable-before-test
    test-fail
    (rename (boolean-and assert-and)))
  (import
    (rnrs base)
    (sph)
    (sph hashtable)
    (only (guile)
      symbol-append
      string-null?
      defined?
      module-ref
      current-module
      string-join
      display
      identity
      procedure-minimum-arity)
    (only (sph alist) list->alist)
    (only (sph conditional) boolean-and)
    (only (sph list) fold-unless n-times-map)
    (only (sph list one) randomise)
    (only (sph one) n-times-accumulate in-range?)
    (only (sph string) any->string any->string-write)
    (only (sph two) list->csv-line)
    (only (srfi srfi-1) last))

  ;defines if successful tests produce a success message
  (define sph-test-log-success #t)
  (define before-test #f)
  (define disabled-before-test #f)
  (define prev-message-newline #t)
  (define-syntax-rule (format-index-number arg) (number->string (+ arg 1)))
  (define (test-disable-before-test) (set! disabled-before-test before-test) (set! before-test #f))
  (define (test-enable-before-test) (set! before-test disabled-before-test))
  (define-syntax-rule (module-ref* name) (module-ref (current-module) name))

  (define* (create-extended-result #:optional output-info title (expected #t) input)
    (vector (q test-result) output-info expected input (any->string title)))

  (define-syntax-rule (extended-result? arg)
    (and (vector? arg) (= (vector-length arg) 5) (eq? (q test-result) (vector-ref arg 0))))

  (define (failure-message-compact name out exp inp title index-data index-data-last)
    "symbol any any any integer integer -> unspecified"
    (let
      ( (indent (if (and (> index-data 0) sph-test-log-success) " " ""))
        (indent-data (if (or (not sph-test-log-success) (= index-data 0)) " " " ")))
      (string-append (if prev-message-newline "" "\n") indent
        "failure " name
        " " (format-index-number index-data)
        (if title (string-append " - " title) "")
        (if (null? inp) ""
          (string-append "\n" indent-data
            "inp "
            (if (list? inp) (string-join (map any->string-write inp) " ") (any->string-write inp))))
        (if (eq? (q undefined) exp) ""
          (string-append "\n" indent-data "exp " (any->string-write exp)))
        "\n" indent-data "out " (any->string-write out) (begin (set! prev-message-newline #t) "\n"))))

  (define (success-message-compact name title index-data index-data-last)
    (let (indent (if (> index-data 0) " " ""))
      (if (or (= index-data 0) title prev-message-newline)
        (string-append (if prev-message-newline "" "\n") indent
          "success" (if (= index-data 0) (string-append " " name) "")
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

  (define (evaluate-result format index-data-last name index-data data-test r)
    "symbol integer string integer list any -> boolean"
    (let*
      ( (is-expected-result (equal? (tail data-test) r))
        (is-extended-result (and (not is-expected-result) (extended-result? r))))
      (if
        (or is-expected-result (and is-extended-result (equal? (tail data-test) (vector-ref r 1))))
        (begin
          (if sph-test-log-success
            (display
              ( (success-message-proc format) name (if is-extended-result (vector-ref r 2) #f)
                index-data index-data-last)))
          #t)
        (begin
          (display
            (if is-extended-result
              ( (failure-message-proc format) name (vector-ref r 1)
                (vector-ref r 2) (or (vector-ref r 3) (first data-test))
                (vector-ref r 4) index-data index-data-last)
              ( (failure-message-proc format) name r
                (tail data-test) (first data-test) #f index-data index-data-last)))
          #f))))

  (define (default-test proc-name index inp exp) "apply a procedure proc-name with arguments inp"
    (apply (module-ref* proc-name) (if (list? inp) inp (list inp))))

  (define-syntax-rule (test-proc-name name)
    (let (proc-name (symbol-append (q test-) name)) (if (defined? proc-name) proc-name name)))

  (define (apply-test-proc name test-proc-name data-test index)
    (if (eq? name test-proc-name)
      (list (symbol->string name) index
        data-test (default-test name index (first data-test) (tail data-test)))
      (let*
        ( (test-proc (module-ref* test-proc-name)) (arity (procedure-minimum-arity test-proc))
          (arity-first (first arity)))
        (list (symbol->string name) index
          data-test
          (if (or (> arity-first 2) (last arity))
            (test-proc name index (first data-test) (tail data-test))
            (if (> arity-first 1) (test-proc (first data-test) (tail data-test))
              (if (> arity-first 0) (test-proc (first data-test)) (test-proc))))))))

  (define (evaluate-test-spec-list test-spec format-display)
    (let (name (first test-spec))
      (if (> (length test-spec) 1)
        ;test-spec with multiple test-data
        (begin
          (fold-unless
            (l (data-test index) (if before-test (before-test name index))
              (if
                (apply evaluate-result format-display
                  (- (/ (- (length test-spec) 1) 2) 1)
                  (apply-test-proc (first test-spec) (test-proc-name (first test-spec))
                    data-test index))
                (+ 1 index) #f))
            not #f 0 (list->alist (tail test-spec))))
        ;test-spec with no test-data
        (begin (if before-test (before-test name 1))
          (apply evaluate-result format-display
            0 (apply-test-proc (first test-spec) (test-proc-name name) (pair (list) #t) 0))))))

  (define (evaluate-test-spec-symbol test-spec format-display)
    (if before-test (before-test test-spec 1))
    (evaluate-result format-display 0
      (symbol->string test-spec) 0 (pair (list) #t) ((module-ref* (test-proc-name test-spec)))))

  (define* (execute-tests tests #:optional (format-display (q compact)))
    "(test-spec ...) symbol:format:compact/scm/csv ->
    try to execute all tests in list and stop on any test-failure"
    (every
      (l (test-spec)
        (if (list? test-spec) (evaluate-test-spec-list test-spec format-display)
          (if (symbol? test-spec) (evaluate-test-spec-symbol test-spec format-display)
            (throw (q syntax-error-in-test-spec)))))
      tests))

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

  (define-syntax-rules define-test
    (((name arg-name ...) body ...) (define-test name (lambda (arg-name ...) body ...)))
    ((name proc) (primitive-eval (qq (define (unquote (symbol-append (q test-) (q name))) proc)))))

  (define-syntax-rule (test-fail title data) (create-extended-result data title))

  (define-syntax-rules assert-true
    ( (optional-title body)
      (let (r body)
        (if (eqv? #t r) #t
          (create-extended-result r
            (if (or (not (string? optional-title)) (string-null? optional-title)) (q body)
              optional-title)
            #t))))
    ((body) (assert-true #f body)))

  (define-syntax-rules assert-equal
    ( (optional-title exp body)
      (let (r body)
        (if (equal? exp r) #t
          (create-extended-result r
            (if (or (not (string? optional-title)) (string-null? optional-title))
              (any->string (q body)) optional-title)
            exp))))
    ((exp body) (assert-equal #f exp body)))

  (define-syntax-rule (define-tests-quasiquote name test ...)
    ;defines a list of tests bound to a variable. like execute-tests-quasiquote without executing
    (define name (qq (test ...))))

  (define-syntax-rule (import-unexported module-name binding-name)
    (define binding-name (@@ module-name binding-name)))

  (define (test-compare out exp)
    "return out if it does not match exp (expected-output).
    boolean values for exp are used for checking general true or false value of an expression
    like as a condition in an if expression."
    (if (boolean? exp) (if out (if exp #t out) (if exp out #t)) (if (equal? out exp) #t out))))