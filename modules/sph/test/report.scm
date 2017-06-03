(library (sph test report)
  (export
    test-report
    test-report-compact
    test-report-null
    test-reporter-get
    test-reporter-names
    test-reporters-default)
  (import
    (guile)
    (ice-9 match)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph string)
    (sph test base)
    (except (rnrs hashtables) hashtable-ref)
    (only (sph one) ignore each-integer)
    (only (sph two) boolean->integer)
    (only (srfi srfi-1) filter-map))

  (define (create-indent depth) (string-multiply "  " depth))
  ;notes:
  ; test reporters for writing to standard output for example while tests are running are implemented as a special hook
  ; configuration with hook procedures to be called by the test execution procedures.

  (define (test-report-compact-display-indices count display)
    (each-integer count (l (n) (display " ") (display (+ 1 n)))))

  (define (test-report-compact-ieo a depth display)
    (display
      (string-join
        (filter-map
          (let (indent (create-indent (+ 1 depth)))
            (l (e) (let (value (tail e)) (and value (string-append indent (first e) ": " value)))))
          (list (pair "i" (any->string-write (test-result-arguments a)))
            (pair "e" (any->string-write (test-result-expected a)))
            (pair "o" (any->string-write (test-result-result a)))))
        "\n")))

  (define (test-report-compact-failure a depth display) "vector integer procedure ->"
    (let ((indent (create-indent depth)) (index (test-result-index a))) (display indent)
      (display (test-result-title a))
      (if (<= 1 index) (test-report-compact-display-indices index display)) (newline)
      (display indent) (display "failure\n") (test-report-compact-ieo a depth display)))

  (define (test-report-compact-success a depth display) "vector integer procedure ->"
    (let ((indent (create-indent depth)) (index (test-result-index a))) (display indent)
      (display (test-result-title a))
      (if (and index (< 0 index)) (test-report-compact-display-indices index display)))
    (display "\n"))

  (define (test-report-compact-one a depth display)
    "vector:test-result-record integer procedure ->"
    (if (test-result-success? a) (test-report-compact-success a depth display)
      (test-report-compact-failure a depth display)))

  (define test-result-tree-fold
    (let*
      ( (default-if-not-list (l (a default) (if (list? a) a default)))
        (with-group
          (l (a group state proc-group-extended c)
            ;when no group-name is given, the nesting is basically ignored.
            (match a
              ( ( (? string? group-name) rest ...)
                (let (group (pair group-name group))
                  (c group rest (default-if-not-list (apply proc-group-extended group state) state))))
              (_ (c group a state))))))
      (l (result proc proc-group-extended . custom-state)
        "test-result procedure ->
        calls proc for each test-result record, memoizing nested group-names"
        (let loop ((e result) (group (list)) (state custom-state))
          (if (list? e)
            (with-group e group
              state proc-group-extended
              (l (group rest state) (fold (l (e state) (loop e group state)) state rest)))
            (if (test-result? e) (default-if-not-list (apply proc e group state) state) state))))))

  (define* (test-report-compact result #:optional (port (current-output-port)))
    "list/vector port -> list/vector:result/input"
    (let (display (l (a) (display a port)))
      (test-result-tree-fold result
        (l (result-record group) (test-report-compact-one result-record (length group) display))
        (l (group) (display (create-indent (max 0 (- (length group) 1))))
          (display (string-join group " ")) (display "\n"))))
    result)

  (define-as test-report-hooks-null alist-q
    ;settings index name ->
    procedure-before ignore
    ;settings index result ->
    procedure-after ignore
    ;settings index name index-data data ->
    procedure-data-before ignore
    ;settings index result ->
    procedure-data-after ignore
    ;settings index module-name ->
    module-before ignore
    ;settings index module-name result ->
    module-after ignore
    ;settings module-names ->
    modules-before ignore
    ;settings module-names result ->
    modules-after ignore)

  (define-as test-report-hooks-compact alist-q
    procedure-before
    (l (s index result)
      (if (alist-q-ref s current-module-name) (newline) (if (not (zero? index)) (newline))))
    procedure-after ignore
    procedure-data-before ignore
    procedure-data-after
    (l (s index index-data result)
      (let (index-data (test-result-index result))
        (if (= 0 (or index-data 0))
          (begin (display (create-indent (boolean->integer (alist-q-ref s current-module-name))))
            (display (test-result-title result))))
        (if (test-result-success? result) (begin (display " ") (display (+ 1 index-data)))
          (begin (newline)
            (display (create-indent (+ 1 (boolean->integer (alist-q-ref s current-module-name)))))
            (display "failure") (newline)
            (test-report-compact-ieo result (boolean->integer (alist-q-ref s current-module-name))
              display)))))
    module-before
    (l (s index name) (if (not (zero? index)) (newline))
      (display (string-join (map symbol->string name) " ")))
    module-after ignore modules-before ignore modules-after (l (s names r) (newline)))

  (define test-report-null ignore)

  (define-as test-reporters-default symbol-hashtable
    ; name -> (report-result . hooks)
    compact (pair test-report-compact test-report-hooks-compact)
    null (pair test-report-null test-report-hooks-null)
    default (pair test-report-null test-report-hooks-null))

  (define (test-reporter-get test-reporters name) "hashtable symbol -> procedure"
    (hashtable-ref test-reporters name (hashtable-ref test-reporters (q default))))

  (define (test-reporter-names test-reporters) "hashtable -> (symbol ...)"
    (hashtable-keys test-reporters))

  (define*
    (test-report-result result #:key data (test-reporters test-reporters-default)
      (format (q compact))
      (port (current-output-port)))
    ((hashtable-ref test-reporters format) result port) result))
