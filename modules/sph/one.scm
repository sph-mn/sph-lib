; Copyright (C) 2010-2017 sph <sph@posteo.eu>
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
    begin-first
    call-at-approximated-interval
    call-at-interval
    call-at-interval-w-state
    cli-option
    cli-option-join
    each-integer
    guile-exception->key
    ignore
    pass
    procedure->cached-procedure
    procedure->temporarily-cached-procedure
    procedure-append
    procedure-append*
    procedure-append-ignore-result
    procedure-cond
    program-name
    program-path
    remove-keyword-associations
    rnrs-exception->key
    rnrs-exception->object
    search-env-path
    search-env-path-one
    socket-bind
    sph-one-description
    values->list)
  (import
    (guile)
    (ice-9 pretty-print)
    (ice-9 rdelim)
    (ice-9 regex)
    (rnrs exceptions)
    (rnrs io ports)
    (rnrs sorting)
    (sph)
    (sph hashtable)
    (sph list)
    (sph number)
    (sph string)
    (only (rnrs base) set!)
    (only (srfi srfi-1) unfold-right unfold))

  (define sph-one-description "various")
  (define-syntax-rule (values->list producer) (call-with-values (l () producer) list))

  (define (ignore . a)
    "any ... -> unspecified
     ignores all arguments and returns unspecified"
    (if #f #t))

  (define (rnrs-exception->object proc)
    "procedure -> procedure
     wraps the given procedure so that when called and an exception occurs in it,
     the exception is catched and the object passed to raise is returned"
    (l a (guard (obj (#t obj)) (apply proc a))))

  (define* (rnrs-exception->key proc #:optional return-object?)
    "procedure [boolean] -> symbol
     wraps the given procedure so that when called and an exception occurs in it,
     the exception is catched and if the exception object passed to raise is a symbol or list
     with a symbol as the first argument, the symbol is returned. otherwise the exception
     is re-raised or the object returned, depending on the boolean value of the second argument \"return-object?\""
    (l a
      (guard
        (obj
          (#t
            (if (symbol? obj) obj
              (if (and (list? obj) (not (null? obj)) (symbol? (first obj))) (first obj)
                (if return-object? obj (raise obj))))))
        (apply proc a))))

  (define (guile-exception->key proc)
    "procedure -> procedure
     guard catches guile exceptions, but it seems impossible to get the key"
    (l a (catch #t (l () (apply proc a)) (l (key . a) key))))

  (define (remove-keyword-associations a)
    "list -> list
     removes keyword associations from an argument list passed for example to lamdba*
     (3 #:a 1 2 #:b 4) -> (3 2)"
    (let loop ((rest a))
      (if (null? rest) rest
        (let (element (first rest))
          (if (keyword? element) (loop (list-tail rest 2)) (pair element (loop (tail rest))))))))

  (define (search-env-path a)
    "(string ...) -> (string ...)
     search for any match of paths \"a\" in the directories in the PATH environment variable and result in the full path.
     similar to guiles %search-load-path but does not consider filename extensions"
    (let*
      ( (path-parsed (parse-path (getenv "PATH")))
        (search-path
          (l (a)
            (any (l (e) (let (path (string-append e "/" a)) (if (file-exists? path) path #f)))
              path-parsed))))
      (map search-path a)))

  (define (search-env-path-one a) (first-or-false (search-env-path (list a))))

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
              (bound
                (inexact->exact
                  (round
                    (* interval
                      (if (> (first change+state) 0) change-factor-faster change-factor-slower))))
                min-interval max-interval))
            (tail change+state))))
      min-interval state))



  (define (procedure->cached-procedure proc)
    "procedure -> procedure procedure
     returns a new procedure with the same signature that calculates each result only once.
     calling the procedure again with the same arguments will lead to the cached result to be returned.
     the second returned procedure takes no arguments and clears the cache when called.
     example 1:
       (define my-calculate-cached (procedure->cached-procedure my-calculate))
     example 2:
       (let-values (((my-calculate-cached my-calculate-cached-reset) (procedure->cached-procedure my-calculate)))
         (my-calculate-cached-reset))"
    (let (cache (ht-make ht-hash-equal equal?))
      (values
        (l a
          (let (cached-result (ht-ref cache a (q --not-in-cache)))
            (if (eq? (q --not-in-cache) cached-result)
              ((l (r) (ht-set! cache a r) r) (apply proc a)) cached-result)))
        (l () (ht-clear! cache)))))

  (define (procedure->temporarily-cached-procedure cache-duration proc)
    "integer:seconds procedure -> procedure
     like procedure->cached-procedure but the cache is emptied after cache-duration the next time the procedure is called"
    (let ((cache (ht-make ht-hash-equal equal?)) (last-time 0))
      (l a
        (let (time (current-time))
          (if (> (- time last-time) cache-duration)
            (begin (set! last-time time)
              ((l (result) (ht-set! cache a result) result) (apply proc a)))
            (let (cached-result (ht-ref cache a (q --not-in-cache)))
              (if (eq? (q --not-in-cache) cached-result)
                ((l (result) (ht-set! cache a result) result) (apply proc a)) cached-result)))))))

  (define (program-path)
    "-> string
     return the full-path of the currently executed program file. uses the first argument of (program-arguments)"
    (let (part (first (program-arguments)))
      (if (string-null? part) part
        (if (eqv? #\/ (string-ref part 0)) part (string-append (getenv "PWD") "/" part)))))

  (define (program-name)
    "-> string
     return the file-name of the currently executed program file. uses the first argument of (program-arguments)"
    (let (part (first (program-arguments)))
      (if (string-null? part) part (if (eqv? #\/ (string-ref part 0)) (basename part) part))))

  (define (procedure-append . proc)
    "procedure ... -> procedure:{any ... -> (any ...)}
     creates a new procedure that applies each appended procedure with given arguments
     and returns the results of each call in a list"
    (l a (map (l (b) (apply b a)) proc)))

  (define (procedure-append* . proc)
    "procedure ... -> procedure:{any ... -> unspecified}
     like procedure-append but procedure call results must be lists
     which are appended"
    (l a (append-map (l (b) (apply b a)) proc)))

  (define (procedure-append-ignore-result . proc)
    "procedure ... -> procedure:{any ... -> unspecified}
     like procedure-append but does not collect results and returns unspecified"
    (l a (each (l (b) (apply b a)) proc)))



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

  (define (cli-option-join options)
    "((name value ...)/string ...) -> (string ...)
     ((\"a\" 3)) -> -a 3
     ((\"abc\" 3)) -> --abc 3
     creates a command-line options string, automatically appending - or -- to option names.
     - pairs with prefixes that are characters or single char strings become single char options
     - pairs with prefixes that are multi char strings become multi char options
     - the tail of pairs is string-joined with spaces and used as the value for the option
     - strings become keyless arguments"
    (string-join
      (fold
        (l (e r)
          (if (string? e) (pair e r)
            (if (and (list? e) (not (null? e)))
              (pair
                (let*
                  ( (name (first e)) (name (if (char? name) (string name) name)) (value (tail e))
                    (value
                      (if (null? value) ""
                        (string-append " " (if (string? value) value (string-join value " "))))))
                  (string-append (if (= (string-length name) 1) "-" "--") name value))
                r)
              r)))
        (list) options)
      " "))

  (define socket-bind bind)

  (define-syntax-rule (procedure-cond a (predicate handler) ... else)
    ;"passes the result to predicate, and if it evaluates to true then it passes the result to handler and the result
    ;is the result of handler. if predicate evaluates to false, the next predicate is checked.
    ;if no predicate matches, the result of procedure-cond is the result of the last expression.
    ;similar to \"cond\" but with procedures for predicate and handler"
    (let (b a) (cond ((predicate b) (handler b)) ... else)))

  (define-syntax-rule (begin-first result expression ...)
    ;like begin but returns the result of the first expression instead of the last one.
    ((l (r) (begin expression ...) r) result))

  (define (each-integer count proc)
    "integer procedure:{integer ->} ->
     call proc \"count\" times"
    (let loop ((n 0)) (if (< n count) (begin (proc n) (loop (+ 1 n))))))

  (define (pass proc obj)
    "procedure any -> any
     apply proc with obj, return obj.
     example
     (+ 2 (pass write (+ 1 3)))
     writes 4 to standard output
     results in 6"
    (proc obj) obj))
