(library (sph test report)
  (export
    test-report
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
              (if value
                (display (string-append indent (first e) ": " (any->string-write value) "\n"))))))
        (list (pair "i" (test-result-arguments a)) (pair "e" (test-result-expected a))
          (pair "o" (test-result-result a))))))

  (define (test-report-compact-success a depth display) "vector integer procedure ->"
    (let ((indent (create-indent depth)) (index (test-result-index a))) (display indent)
      (display (test-result-title a))
      (if (and index (< 0 index)) (test-report-compact-display-indices index display)))
    (display "\n"))

  (define (test-report-compact-one a depth display)
    "vector:test-result-record integer procedure ->"
    (if (test-result-success? a) (test-report-compact-success a depth display)
      (test-report-compact-failure a depth display)))

  (define (test-report-compact result port) "list/vector port -> list/vector:result/input"
    (let (display (l (a) (display a port)))
      (if (list? result)
        (let loop ((rest result) (depth 0) (group (list)))
          (if (null? rest) result
            (let (e (first rest))
              (if (list? e)
                (match e
                  ( ( (? symbol? group-name) rest ...)
                    (loop (tail rest) (+ 1 depth) (pair group-name group)))
                  (_ (loop e (+ 1 depth) group)))
                (begin (test-report-compact-one e depth display) (loop (tail rest) depth group))))))
        (test-report-compact-one result 0 display)))
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
    procedure-before (l (s name) (display name))
    procedure-after (l (s name) (newline))
    procedure-data-before ignore
    procedure-data-after ignore
    module-before ignore module-after ignore modules-before ignore modules-after ignore)

  (define test-report-null identity)

  (define-as test-reporters-default symbol-hashtable
    ; name -> (report-result . hooks)
    compact (pair test-report-compact test-report-hooks-compact)
    null (pair test-report-null test-report-hooks-null)
    default (pair test-report-null test-report-hooks-null))

  (define (test-reporter-get test-reporters name) "hashtable symbol -> procedure"
    (hashtable-ref test-reporters name (hashtable-ref test-reporters (q default) test-report-null)))

  (define (test-reporter-names test-reporters) "hashtable -> (symbol ...)"
    (hashtable-keys test-reporters))

  (define*
    (test-report-result result #:key data (test-reporters test-reporters-default)
      (format (q compact))
      (port (current-output-port)))
    ((hashtable-ref test-reporters format) result port) result))
