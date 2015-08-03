; (sph one) - various procedures
; written for the guile scheme interpreter
; Copyright (C) 2010-2015 sph <sph@posteo.eu>
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

(library (sph one)
  (export
    alist->regexp-match-replacements
    apply-values
    call
    apply-without-arguments
    bytevector-append
    bytevector-contains?
    call-at-approximated-interval
    call-at-interval
    call-at-interval-w-state
    call-with-pipe
    call-with-working-directory
    cli-option
    define-string
    each-u8
    eq-any?
    eq-every?
    equal-any?
    equal-every?
    eqv-any?
    eqv-every?
    every-s
    exception->key
    exception->string
    first-as-result
    if-exception
    in-between?
    in-range?
    integer->bytevector
    limit
    limit-max
    limit-min
    n-times
    n-times-accumulate
    number->integer-string
    pass
    procedure->cached-procedure
    procedure->temporarily-cached-procedure
    procedure-cond
    program-path
    quote-odd
    round-even
    search-env-path
    socket-bind
    string->datum
    string-if-exception
    true?
    true?-s)
  (import
    (guile)
    (ice-9 popen)
    (ice-9 rdelim)
    (ice-9 regex)
    (rnrs bytevectors)
    (rnrs io ports)
    (rnrs sorting)
    (sph)
    (sph math)
    (sph string)
    (except (rnrs base) map)
    (only (ice-9 popen)
      close-pipe
      open-pipe
      open-pipe*)
    (only (rnrs hashtables)
      make-hashtable
      equal-hash
      hashtable-ref
      hashtable-set!)
    (only (srfi srfi-1)
      append-map
      unfold-right
      filter
      unfold))

  (define (call proc . a) (apply proc a))

  (define-syntax-rule (apply-values proc producer) (call-with-values (l () producer) proc))

  (define (call-with-pipe proc) "procedure:{port:in port:out -> any} -> any"
    (apply (l (in out) (let (r (proc in out)) (close in) (close out) r))
      (let (ports (pipe)) (list (first ports) (tail ports)))))

  (define* (cli-option name #:optional value)
    "creates a string for one generic command-line option in the gnu format -{single-character} or --{word} or --{word}=.
    optionally with values.
    \"name\"can be a:
    character: short option
    string: long option
    value can be anything, non-strings will be converted to strings in \"display\" format"
    (if value
      (if (char? name) (string-append (string #\- name #\space) (any->string value))
        (string-append "--" name "=" (any->string value)))
      (if (char? name) (string #\- name) (string-append "--" name))))

  (define (search-env-path a)
    "string -> string
    search for any match of path a in the directories in the PATH environment variable and result in the full path"
    (any (l (e) (let (path (string-append e "/" a)) (if (file-exists? path) path #f)))
      (parse-path (getenv "PATH"))))

  (define-syntax-rule (first-as-result result body ...) ((l (r) (begin body ...) r) result))

  (define (integer->bytevector a)
    (let* ((size (bit->byte-length (signed-integer-binary-length a))) (r (make-bytevector size)))
      size (bytevector-sint-set! r 0 a (native-endianness) size) r))

  (define (bytevector-append . a) "bytevector ... -> bytevector"
    (let (r (make-bytevector (fold (l (e prev) (+ prev (bytevector-length e))) 0 a)))
      (fold
        (l (e index)
          (let (len (bytevector-length e)) (bytevector-copy! e 0 r index len) (+ index len)))
        0 a)
      r))

  (define (apply-without-arguments procedure) (procedure))

  (define (round-even a)
    "number -> integer
    floor to the nearest even integer"
    (let (a (inexact->exact (round a))) (+ a (modulo a 2))))

  (define (call-at-interval proc interval)
    "procedure:{current-interval procedure:continue:{next-interval -> any} -> any} microseconds -> unspecified
    call proc after interval of length interval. proc receives the current interval and a procedure to
    continue, with the option to change the interval"
    (let next ((interval interval)) (usleep interval) (proc interval next)))

  (define (call-at-interval-w-state proc interval . init)
    "procedure:{current-interval continue any ...} microseconds any ...
    like call-at-interval but accepting unlimited number of additional parameters
    that are passed to proc and the continue procedure."
    (letrec ((next (l (interval . state) (usleep interval) (apply proc interval next state))))
      (apply next interval init)))

  (define*
    (call-at-approximated-interval proc min-interval max-interval #:optional
      (change-factor-slower 1.1)
      (change-factor-faster (/ 1 change-factor-slower))
      .
      state)
    "{states ... -> integer} microseconds microseconds rational rational any ... ->
    like call-at-interval-w-state but without a continue procedure, automatically adjusting interval after each application of proc
    depending on the result of proc: -1 increases the interval, 1 decreases it and 0 does not change it."
    (apply call-at-interval-w-state
      (l (interval continue . state)
        (let ((change+state (apply proc state)))
          (apply continue
            (if (= 0 (first change+state)) interval
              (limit
                (inexact->exact
                  (round
                    (* interval
                      (if (> (first change+state) 0) change-factor-faster change-factor-slower))))
                min-interval max-interval))
            (tail change+state))))
      min-interval state))

  (define (each-u8 proc port)
    "procedure:{octet -> any} port -> unspecified
    apply proc with each octet read from port until end-of-file is reached."
    (let next ((octet (get-u8 port)))
      (if (eof-object? octet) #t (begin (proc octet) (next (get-u8 port))))))

  (define (in-between? num num-start num-end)
    ;"number number number -> boolean; check if num is between num-start and num-end"
    (and (> num num-start) (< num num-end)))

  (define-syntax-rule (in-range? num num-start num-end)
    ;"number number number -> boolean; check if num is between num-start and num-end"
    (and (>= num num-start) (<= num num-end)))

  (define-syntax-rule (limit a min max)
    ;"-> limited-number; number number number -> number; limit the first argument to be not smaller than min and not greater than max."
    (if (> a max) max (if (< a min) min a)))

  (define-syntax-rule (limit-min a min) (max a min))
  (define-syntax-rule (limit-max a max) (min a max))

  (define (n-times n-last proc)
    "apply a procedure a number of times with a monotonically increasing number with increments of 1. starts from 0"
    (let loop ((n 0)) (if (<= n n-last) (begin (proc n) (loop (+ 1 n))))))

  (define (n-times-accumulate n-last init proc)
    (let loop ((n 0) (r init))
      (if (< n n-last) (loop (+ 1 n) (proc n r)) r)))

  (define* (number->integer-string a #:optional (radix 10))
    "-> string
    return number as an string without a radix point or fractional value."
    (number->string (inexact->exact a) radix))

  (define (pass proc obj)
    "procedure any -> any
    apply proc with obj, return obj unmodified.
    example
    (+ 2 (pass write (+ 1 3)))
    writes 4 to standard output
    results in 6)"
    (proc obj) obj)

  (define (procedure->cached-procedure proc)
    "procedure -> procedure
    results in a new procedure with the same signature and which is an extended version of the given procedure,
    where the extended procedure is only called once for a combination of arguments, and subsequent
    calls with the same arguments return the previous result from a cache."
    (let (cache (make-hashtable equal-hash equal?))
      (l args
        (let (cached-result (hashtable-ref cache args (q --not-in-cache)))
          (if (eqv? (q --not-in-cache) cached-result)
            ((l (r) (hashtable-set! cache args r) r) (apply proc args)) cached-result)))))

  (define (procedure->temporarily-cached-procedure cache-duration proc)
    "integer:seconds procedure -> procedure
    like procedure->cached-procedure but the cache is emptied reqularly"
    (let ((cache (make-hashtable equal-hash equal?)) (last-time 0))
      (l args
        (let (time (current-time))
          (if (> (- time last-time) cache-duration)
            (begin (set! last-time time)
              ((l (result) (hashtable-set! cache args result) result) (apply proc args)))
            (let (cached-result (hashtable-ref cache args (q --not-in-cache)))
              (if (eqv? (q --not-in-cache) cached-result)
                ((l (result) (hashtable-set! cache args result) result) (apply proc args))
                cached-result)))))))

  (define (program-path)
    "-> string
    return the full-path of the currently executed program file"
    (let ((part (first (program-arguments))))
      (if (eq? #\/ (string-ref part 0)) part (string-append (getenv "PWD") "/" part))))

  (define-syntax-rule (true?-s expr) (if expr #t #f))

  (define (true? a)
    "any -> boolean
    the opposite of \"not\""
    (if a #t #f))

  (define-syntax-rule (create-equal-any list-pred member-pred equality-pred)
    ;see *-any?/every? definitions
    (lambda (a b)
      "any/list any/list -> boolean
      check if there is either equality or inclusion between the arguments.
      examples
      eqv-any? (4 2 6) (3 1 2) -> true
      eqv-any? (3 1 2) 2 -> true
      eqv-any? 4 (3 1 2) -> false"
      (if (list? a) (if (list? b) (list-pred (l (e) (member-pred e b)) a) (member-pred b a))
        (if (list? b) (member-pred a b) (equality-pred a b)))))

  (define (call-with-working-directory path proc)
    "string procedure:{} -> any:proc-result
    apply proc with path as the current working directory"
    (let (prev-cwd (getcwd)) (chdir path) (proc) (chdir prev-cwd)))

  (define (define-string name value)
    "string any ->
    define a variable by using a string for the name"
    (primitive-eval (quasiquote (define (unquote (string->symbol name)) (unquote value)))))

  (define eqv-any? (create-equal-any any memv eqv?))
  (define eqv-every? (create-equal-any every memv eqv?))
  (define equal-any? (create-equal-any any member equal?))
  (define equal-every? (create-equal-any every member equal?))
  (define eq-any? (create-equal-any any memq eq?))
  (define eq-every? (create-equal-any every memq eq?))

  (define-syntax-rules every-s
    ;"like 'every' for lists but implemented as syntax. proc is not bound
    ;example: (every-s integer? 1 2 3) is expanded as: (if (and (integer? 1) (integer? 2) (integer? 3)) #t #f)"
    ((proc a) (if (proc a) #t #f)) ((proc a a-n ...) (if (proc a) (every-s proc a-n ...) #f)))

  (define-syntax-rule (procedure-cond a (predicate handler) ... else)
    ;uses list-pairs of a predicate procedure and a handler procedure
    ;passing a to predicate and eventually handler.
    (cond ((predicate a) (handler a)) ... else))

  (define-syntax-rule (exception->key expr) (catch #t (l () expr) (l (key . args) key)))

  (define-syntax-rules quote-odd
    ;quote each argument at odd indexes, thereby not quoting each second argument.
    ((a b) (list (quote a) b))
    ((a b c ...) (quasiquote ((unquote-splicing (quote-odd a b) (quote-odd c ...))))))

  (define (alist->regexp-match-replacements a)
    "automatically converts strings at the prefix position to regular expressions"
    (map (l (e) (pair (if (string? (first e)) (make-regexp (first e)) (first e)) (tail e))) a))

  (define* (string->datum a #:optional (reader read)) "get the first scheme expression in a string"
    (call-with-input-string a reader))

  (define (bytevector-contains? a search-bv) "bytevector bytevector -> boolean"
    (let ((a-length (bytevector-length a)) (search-bv-length (bytevector-length search-bv)))
      (if (> search-bv-length a-length) #f
        (let
          ( (search (list->vector (bytevector->u8-list search-bv)))
            (last-match-index (- search-bv-length 1)))
          (let loop ((index 0) (match-index 0))
            (if (< index a-length)
              (if (= (bytevector-u8-ref a index) (vector-ref search match-index))
                (if (= last-match-index match-index) #t (loop (+ 1 index) (+ 1 match-index)))
                (loop (+ 1 index) 0))
              #f))))))

  (define (exception->string key . args)
    "symbol any ... -> string
    create a space separated string from exception key and arguments"
    (string-join (map any->string args) " "))

  (define-syntax-rule (string-if-exception expr)
    ;result in an exception string created by exception->string if any exception occurs
    (catch #t (l () expr) exception->string))

  (define-syntax-rule (if-exception expr consequent) (catch #t (l () expr) (l exc consequent)))
  (define socket-bind bind))