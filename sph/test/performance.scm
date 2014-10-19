(library (sph test performance)
  (export
    evaluate-performance)
  (import
    (guile)
    (ice-9 pretty-print)
    (ice-9 format)
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph list)
    (sph math)
    (sph string)
    (only (guile)
      current-time
      ceiling
      exact->inexact
      inexact->exact
      string-join)
    (only (sph one)
      number->integer-string
      n-times
      pass)
    (only (sph time) current-time-microseconds))

  "compare performance of procedures with adapting the number of applications until a meaningful run time difference is found"

  (define (multiply-until proc base factor)
    (if (proc base factor) base (multiply-until proc (* base factor) factor)))

  (define (execute-tests tests iterations)
    (map
      (l (test)
        (let ((start-time (current-time-microseconds))) (n-times iterations (tail test))
          (- (current-time-microseconds) start-time)))
      tests))

  (define (multiply-iterations-factor result-max min-microseconds)
    (max (/ min-microseconds result-max) 1.05))

  (define (adapt-iterations n result-max min-microseconds)
    (round (* 1.85 n (multiply-iterations-factor result-max min-microseconds))))

  (define (execute-w-managed-iterations tests microseconds)
    (let next ((iterations 3))
      (let* ((results (execute-tests tests iterations)) (result-max (apply max results)))
        (if (< result-max (- microseconds 500000))
          (next (adapt-iterations iterations result-max microseconds)) results))))

  (define (evaluate-result seconds tests)
    (if (integer? seconds)
      (let*
        ( (seconds (* 1000000 seconds)) (results (execute-w-managed-iterations tests seconds))
          (result-average (apply average results)) (result-max (apply max results))
          (result-percent-factor (/ 100 result-max))
          (results
            (list-sort (l (ele-1 ele-2) (< (list-ref ele-1 1) (list-ref ele-2 1)))
              (map
                (l (result test)
                  (list
                    ( (l (res) (if (string-prefix? "test-" res) (string-drop res 5) res))
                      (symbol->string (first test)))
                    (inexact->exact (floor (/ result 100)))
                    ;(number->integer-string (round (exact->inexact (- result-average result))))
                    (format #f "~,1fx" (exact->inexact (- (/ result-max result) 1)))))
                results tests))))
        (display-line
          (string-join
            (cons* (list->string-columns (list "name" "time" "faster than slowest"))
              (string-multiply "-" 78)
              (map (l (ele) (list->string-columns (map any->string ele)))
                (reverse!
                  (first
                    (fold-multiple
                      (l (ele res count)
                        (list
                          (cons
                            (cons (string-append (number->string count) ". " (first ele))
                              (tail ele))
                            res)
                          (+ count 1)))
                      results (list) 1)))))
            "\n")))
      (throw (q wrong-type-for-argument) seconds)))

  (define-syntax-rules evaluate-performance
    ;takes a list of procedure names, procedures with the signature (n) ->
    ((seconds tests) (evaluate-result seconds tests))
    ( (seconds identifier ...)
      (evaluate-result seconds (list (cons (quote identifier) identifier) ...)))))