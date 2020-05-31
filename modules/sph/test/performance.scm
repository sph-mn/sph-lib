(define-module (sph test performance))

(use-modules (ice-9 format) (ice-9 pretty-print)
  (rnrs sorting) (sph)
  (sph list) (sph math) (sph string) (sph time) ((sph other) #:select (each-integer pass)))

(export evaluate-performance sph-test-performance-description)

(define sph-test-performance-description
  "adaptive performance testing with formatted result display.
   adapts the number of calls until a meaningful run time difference has been found.
   # example
   (define (test-match-regular n) (regexp-replace regular-string \"12\" \"121212\"))
   (define (test-match-varied n) (regexp-replace varied-string \"12\" \"121212\"))
   (define (test-replace-regular n) (string-replace-string regular-string \"12\" \"121212\"))
   (define (test-replace-varied n) (string-replace-string varied-string \"12\" \"121212\"))
   (evaluate-performance 2 test-match-regular
     test-match-varied test-replace-regular test-replace-varied)
   # example output
   name                      time                      faster than slowest
   ------------------------------------------------------------------------------
   1. replace-varied         45090                     790.8x
   2. match-varied           211080                    168.1x
   3. replace-regular        1677740                   20.3x
   4. match-regular          35700210                  0.0x")

(define (multiply-until proc base factor)
  (if (proc base factor) base (multiply-until proc (* base factor) factor)))

(define (execute-tests tests iterations)
  (map
    (l (test)
      (let ((start-time (utc-current))) (each-integer iterations (tail test))
        (- (utc-current) start-time)))
    tests))

(define (multiply-iterations-factor result-max min-nanoseconds)
  (max (/ min-nanoseconds result-max) 1.05))

(define (adapt-iterations n result-max min-nanoseconds)
  (round (* 1.85 n (multiply-iterations-factor result-max min-nanoseconds))))

(define (execute-w-managed-iterations tests nanoseconds)
  (let next ((iterations 3))
    (let* ((results (execute-tests tests iterations)) (result-max (apply max results)))
      (if (< result-max nanoseconds) (next (adapt-iterations iterations result-max nanoseconds))
        results))))

(define (evaluate-result seconds tests)
  (if (integer? seconds)
    (let*
      ( (seconds (s->ns seconds)) (results (execute-w-managed-iterations tests seconds))
        (result-average (arithmetic-mean results)) (result-max (apply max results))
        (result-percent-factor (/ 100 result-max))
        (results
          (list-sort (l (ele-1 ele-2) (< (list-ref ele-1 1) (list-ref ele-2 1)))
            (map
              (l (result test)
                (list
                  ( (l (res) (if (string-prefix? "test-" res) (string-drop res 5) res))
                    (symbol->string (first test)))
                  (inexact->exact (floor (/ result 100)))
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
                          (cons (string-append (number->string count) ". " (first ele)) (tail ele))
                          res)
                        (+ count 1)))
                    results (list) 1)))))
          "\n")))
    (throw (q wrong-type-for-argument) seconds)))

(define-syntax-rules evaluate-performance
  ((minimum-seconds tests) (evaluate-result minimum-seconds tests))
  ( (minimum-seconds identifier ...)
    (evaluate-result minimum-seconds (list (cons (quote identifier) identifier) ...))))
