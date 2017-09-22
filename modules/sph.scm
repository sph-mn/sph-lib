; written for the guile scheme interpreter
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

(library (sph)
  (export
    *
    +
    -
    /
    <
    <=
    =
    >
    >=
    abs
    acos
    and
    and-let*
    angle
    any
    append
    append-map
    apply
    apply-values
    asin
    atan
    begin
    boolean?
    call-with-current-continuation
    call-with-values
    call/cc
    case
    ceiling
    char->integer
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    complex?
    compose-s
    cond
    cos
    datum->syntax
    debug-log
    define
    define*
    define-as
    define-syntax
    define-syntax-case
    define-syntax-cases
    define-syntax-rule
    define-syntax-rules
    delete-duplicates
    denominator
    display-line
    div
    div-and-mod
    div0
    div0-and-mod0
    dynamic-wind
    each
    eq?
    equal?
    eqv?
    even?
    every
    exact
    exact-integer-sqrt
    exact?
    exp
    expt
    filter
    filter-map
    finite?
    first
    floow
    fold
    fold-right
    for-each
    gcd
    identifier-syntax
    identifier?
    identity
    if
    imag-part
    inexact
    inexact?
    infinite?
    integer->char
    integer-valued?
    integer?
    l
    l*
    lambda
    lambda*
    last
    lcm
    length
    let
    let*
    let-keywords
    let-syntax
    let-values
    letrec
    letrec*
    letrec-syntax
    list
    list->string
    list->vector
    list-ref
    list-tail
    list?
    log
    magnitude
    make-polar
    make-rectangular
    make-string
    make-vector
    map
    max
    min
    mod
    mod0
    nan?
    negative?
    not
    null
    null?
    nullary
    number->string
    number?
    numerator
    odd?
    or
    pair
    pair?
    pairs
    positive?
    procedure?
    q
    qq
    quasiquote
    quasisyntax
    quote
    quote-duplicate
    quote-even
    quote-odd
    rational-valued?
    rational?
    rationalize
    real-part
    real-valued?
    real?
    remove
    reverse
    round
    second
    sin
    sph-description
    sqrt
    string
    string->list
    string->number
    string->symbol
    string-append
    string-copy
    string-length
    string-ref
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    symbol->string
    symbol?
    syntax
    syntax->datum
    syntax-rule
    syntax-rules
    syntax-rules_
    tail
    take-while
    tan
    truncate
    unquote
    unquote-splicing
    unsyntax
    unsyntax-splicing
    values
    vector
    vector->list
    vector-fill!
    vector-for-each
    vector-length
    vector-map
    vector-ref
    vector-set!
    vector?
    with-syntax
    zero?)
  (import
    (ice-9 pretty-print)
    (except (rnrs base) let)
    (except (srfi srfi-1) map)
    (only (guile)
      LC_ALL
      cons*
      current-output-port
      datum->syntax
      define-syntax
      display
      exit
      filter
      identifier?
      newline
      quasisyntax
      read-disable
      setlocale
      string=
      syntax
      identity
      syntax->datum
      syntax-case
      unsyntax
      unsyntax-splicing
      with-syntax
      write)
    (only (ice-9 optargs)
      lambda*
      define*
      let-keywords)
    (only (srfi srfi-2) and-let*))

  (define sph-description
    "bindings that are fundamental to all sph libraries.
     exports (rnrs base) except set!. if you need set!, import it with (import (only (rnrs base) set!))
     # syntax
     quote-odd
       any ... -> list
       quotes arguments alternatingly and returns a list
       example: a b c d -> ((quote a) b (quote c) d)
     quote-even
       any ... -> list
       like quote-odd but quotes each second argument instead
       example: a b c d -> (a (quote b) c (quote d))
     quote-duplicate
       any ... -> list
       create two elements from each identifier: one the literal identifier symbol,
       the other the value of the variable bound to identifier.
       example: a b c -> ((quote a) a (quote b) b (quote c) c)
       example 2
         (let ((a 1) (b 2) (c 3)) (quote-duplicate a b c))
         -> (list a 1 b 2 c 3)
     define-as
       example: (define-as list 1 2 3)
       example: (define-as (quasiquote list) 1 (unquote 3))
     compose-s
       (compose-s a (list 1 2)) -> (a (list 1 2))
       (compose-s (a b) (list 1 2)) -> (a (b (list 1 2)))
       this does not fail in the case (_ (quasiquote list) expr ...)
     nullary
       create a procedure that accepts zero arguments and evaluates body when called.
       often used for thunks
     let
       typical scheme let and named-let extended with a simplified syntax for binding just one variable
       example: (let (a 3) a)")

  (define-syntax syntax-rule
    ; like syntax-rules but only for one syntax-rule and without keyword support
    (syntax-rules () ((_ (pattern ...) expansion) (syntax-rules () ((_ pattern ...) expansion)))))

  (define-syntax syntax-rules_
    ; like syntax-rules but without keyword support
    (syntax-rules ()
      ((_ ((pattern ...) expansion) ...) (syntax-rules () ((_ pattern ...) expansion) ...))))

  (define-syntax define-syntax-rule
    ; removes option to define multiple clauses
    ; removes option to define keywords
    (syntax-rule ((name pattern ...) expansion)
      (define-syntax name (syntax-rule (pattern ...) expansion))))

  (define-syntax define-syntax-rules
    ; removes option to define keywords
    (syntax-rule (name ((pattern ...) expansion) ...)
      (define-syntax name (syntax-rules () ((_ pattern ...) expansion) ...))))

  (define-syntax-rules define-syntax-case
    ; removes possibility to define keywords
    ; removes possibility to use custom lambda for define-syntax
    ; removes possibility to define multiple clauses
    ( ( (name . pattern) syntax-name expansion)
      (define-syntax name
        (lambda (syntax-name) (syntax-case syntax-name () ((_ . pattern) expansion)))))
    ( ( (name pattern ...) syntax-name expansion)
      (define-syntax name
        (lambda (syntax-name) (syntax-case syntax-name () ((_ pattern ...) expansion)))))
    ( ( (name . pattern) expansion)
      (define-syntax name (lambda (s) (syntax-case s () ((_ . pattern) expansion)))))
    ( ( (name pattern ...) expansion)
      (define-syntax name (lambda (s) (syntax-case s () ((_ pattern ...) expansion))))))

  (define-syntax-rules define-syntax-cases
    ( (name ((pattern ...) expansion ...) ...)
      (define-syntax name
        (lambda (syntax) (syntax-case syntax () ((_ pattern ...) (begin expansion ...)) ...))))
    ( (name syntax ((pattern ...) expansion ...) ...)
      (define-syntax name
        (lambda (syntax) (syntax-case syntax () ((_ pattern ...) (begin expansion ...)) ...)))))

  (define-syntax-rule (l a ...) (lambda a ...))
  (define-syntax-rule (l* a ...) (lambda* a ...))
  (define-syntax-rule (q a) (quote a))
  (define-syntax-rule (qq a) (quasiquote a))
  (define-syntax-rule (apply-values proc producer) (call-with-values (lambda () producer) proc))

  (define-syntax-rules let
    ((((variable-name expr) ...) body ...) ((lambda (variable-name ...) body ...) expr ...))
    (((variable-name expr) body ...) (let ((variable-name expr)) body ...))
    ( (name ((variable-name expr) ...) body ...)
      ((lambda (name) (set! name (lambda (variable-name ...) body ...)) (name expr ...)) #f))
    ((name (variable-name expr) body ...) (let name ((variable-name expr)) body ...)))

  (define-syntax-case (compose-s name expr ...) s
    (let (name-datum (syntax->datum (syntax name)))
      (if (list? name-datum)
        (let (name-datum (reverse name-datum))
          (datum->syntax s
            (fold list (pair (first name-datum) (syntax->datum (syntax (expr ...))))
              (tail name-datum))))
        (syntax (name expr ...)))))

  (define-syntax-rules define-as
    ((name (wrap-name ...) expr ...) (define name (compose-s (wrap-name ...) expr ...)))
    ((name wrap-name expr ...) (define name (wrap-name expr ...))))

  (define-syntax-rules quote-odd (() (quote ()))
    ((a) (quote (a))) ((a b c ...) (quasiquote (a (unquote b) (unquote-splicing (quote-odd c ...))))))

  (define-syntax-rules quote-even (() (quote ()))
    ((a) (quasiquote ((unquote a))))
    ((a b c ...) (quasiquote ((unquote a) b (unquote-splicing (quote-even c ...))))))

  (define-syntax-rules quote-duplicate ((a) (list (quote a) a))
    ((a b ...) (quasiquote ((unquote-splicing (quote-duplicate a) (quote-duplicate b ...))))))

  (define-syntax-rule (nullary body ...) (lambda () body ...))
  (define pair cons)
  (define pairs cons*)
  (define tail cdr)
  (define each for-each)
  (define null (list))

  (define (debug-log . a)
    "any-1 any-n ... -> any-1
     writes all arguments to standard output and returns the first argument"
    (pretty-print (cons (q --) a)) (first a))

  (define* (display-line a #:optional (port (current-output-port)))
    "any [port] -> unspecified
     like \"display\" but emits a newline at the end"
    (display a port) (newline port))

  ; affects port\string encodings
  ; initialise locale categories based on system environment variables
  (setlocale LC_ALL "")
  ; fail on code that uses square brackets instead of round brackets
  (read-disable (quote square-brackets)))
