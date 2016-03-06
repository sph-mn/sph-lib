(library (sph test report)
  (export
    test-report
    test-report-compact
    test-report-null
    test-reporter-get
    test-reporter-names
    test-reporters-default)
  (import
    (ice-9 match)
    (sph common)
    (sph test base)
    (except (rnrs hashtables) hashtable-ref))

  (define (create-indent depth) (string-multiply "  " depth))
  ;todo: nested results display, assertion title display
  ; test reporters can create output while tests are running and after tests have run.
  ; test reporters for creating output while tests are running are implemented as a special hook configuration with hook procedures to be called by the test execution procedures.

  (define (test-report-compact-display-indices count display)
    (n-times count (l (n) (display " ") (display (+ 1 n)))))

  (define (test-report-compact-failure a depth display) "vector integer procedure ->"
    (let ((indent (create-indent depth)) (index (test-result-index a))) (display indent)
      (display (test-result-title a))
      (if (and index (> index 0)) (test-report-compact-display-indices index display))
      (display
        (string-append "\n" (create-indent (+ 1 depth))
          "failure" (if index (string-append " " (number->string (+ 1 index))) "") "\n"))
      (each
        (let (indent (create-indent (+ 2 depth)))
          (l (e)
            (let (value (tail e))
              (if value (display (string-append indent (first e) ": " value "\n"))))))
        (list
          (pair "i"
            (string-drop (string-drop-right (any->string-write (test-result-arguments a)) 1) 1))
          (pair "e" (any->string-write (test-result-expected a)))
          (pair "o" (any->string-write (test-result-result a)))))))

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

  (define-as test-report-hooks-null alist-quoted
    ;settings name ->
    procedure-before ignore
    ;settings result ->
    procedure-after ignore
    ;settings name index data ->
    procedure-data-before ignore
    ;settings result ->
    procedure-data-after ignore
    ;settings module-name ->
    module-before ignore
    ;settings module-name result ->
    module-after ignore
    ;settings module-names ->
    modules-before ignore
    ;settings module-names result ->
    modules-after ignore)

  (define-as test-report-hooks-compact alist-quoted
    procedure-before
    (l (s name)
      (display (create-indent (boolean->integer (alist-quoted-ref s current-module-name))))
      (display name))
    procedure-after
    (l (s result)
      (if (test-result-success? result) (newline) (test-report-compact-failure result 0 display)))
    procedure-data-before (l (s name index data) (display " ") (display (+ 1 index)))
    procedure-data-after ignore
    module-before (l (s name) (display (string-join (map symbol->string name) " ")) (newline))
    module-after ignore modules-before ignore modules-after ignore)

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
