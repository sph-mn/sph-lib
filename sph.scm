; (sph) - fundamental bindings for sph-lib
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

(library (sph)
  (export
    and-let*
    any
    catch
    compose-s
    debug-log
    define*
    define-as
    define-multiple
    define-syntax-case
    define-syntax-cases
    define-syntax-rule
    define-syntax-rules
    display-line
    each
    error-data
    error-data-p
    error-name
    error-name-p
    error-origin
    error-origin-p
    error?
    error?-p
    every
    first
    fold
    fold-right
    identity-s
    l
    l*
    lambda*
    let
    make-error
    make-error-p
    pair
    pairs
    par-let
    par-map
    parallel
    q
    ql
    qq
    quoted-list
    second
    syntax-rule
    syntax-rules_
    tail
    throw)
  (import
    (ice-9 pretty-print)
    (ice-9 threads)
    (srfi srfi-9)
    (except (rnrs base) let)
    (only (guile)
      define-syntax
      newline
      datum->syntax
      debug-set!
      debug-disable
      cons*
      syntax->datum
      LC_ALL
      set-port-encoding!
      exit
      setlocale
      string=
      syntax
      throw
      catch
      read-set!
      syntax-case
      write
      display)
    (only (ice-9 optargs) lambda* define*)
    (only (srfi srfi-1)
      any
      second
      every
      first
      fold
      fold-right)
    (only (srfi srfi-2) and-let*))

  ;affects port\string encodings
  ;  initialise all locale categories based on standard system environment variables
  (setlocale LC_ALL "")
  ;this should not be here, because it may overwrite the users preference.
  ;  it sets the depth of backtraces which are printed on error.
  (debug-set! depth 4)

  (define (debug-log . args)
    "writes all arguments to standard-output and results in the first argument"
    (pretty-print (cons (q --) args)) (first args))

  (define-syntax syntax-rule
    (syntax-rules () ((_ (pattern ...) expansion) (syntax-rules () ((_ pattern ...) expansion)))))

  (define-syntax syntax-rules_
    (syntax-rules ()
      ((_ ((pattern ...) expansion) ...) (syntax-rules () ((_ pattern ...) expansion) ...))))

  (define-syntax define-syntax-rule
    ;remove possibility to define keywords
    ;remove possibility to define multiple clauses
    (syntax-rule ((name pattern ...) expansion)
      (define-syntax name (syntax-rule (pattern ...) expansion))))

  (define-syntax define-syntax-rules
    ;remove possibility to define keywords
    (syntax-rule (name ((pattern ...) expansion) ...)
      (define-syntax name (syntax-rules () ((_ pattern ...) expansion) ...))))

  (define-syntax-rules define-multiple ((() expr) (quote unspecified))
    ( ( (identifier . identifiers) expr)
      (begin (define identifier expr) (define-multiple identifiers (tail identifier))
        (define identifier (first identifier)))))

  (define-syntax-rules define-syntax-case
    ;remove possibility to define keywords
    ;remove possibility to use custom lambda for define-syntax
    ;remove possibility to define multiple clauses
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
        (lambda (syntax) (syntax-case syntax () ((_ pattern ...) expansion ...) ...))))
    ( (name syntax ((pattern ...) expansion ...) ...)
      (define-syntax name
        (lambda (syntax) (syntax-case syntax () ((_ pattern ...) expansion ...) ...)))))

  (define-syntax-rule (l arg ...) (lambda arg ...))
  (define-syntax-rule (l* arg ...) (lambda* arg ...))

  (define-syntax-rules let
    ;let and named-let plus a simplified syntax for binding just one variable
    ((((variable-name expr) ...) body ...) ((lambda (variable-name ...) body ...) expr ...))
    (((variable-name expr) body ...) (let ((variable-name expr)) body ...))
    ( (name ((variable-name expr) ...) body ...)
      ((lambda (name) (set! name (lambda (variable-name ...) body ...)) (name expr ...)) #f))
    ((name (variable-name expr) body ...) (let name ((variable-name expr)) body ...)))

  (define-syntax-rule (identity-s arg) arg)
  (define-syntax-rule (q arg) (quote arg))
  (define-syntax-rule (quoted-list args ...) (q (args ...)))
  (define-syntax-rule (ql args ...) (quoted-list args ...))
  (define-syntax-rule (qq arg) (quasiquote arg))
  (define-syntax-rule (qq arg) (quasiquote arg))
  (define pair cons)
  (define pairs cons*)
  (define tail cdr)
  (define each for-each)

  (define-syntax-rule (par-let ((v e) ...) b0 b1 ...)
    (call-with-values (lambda () (parallel e ...)) (lambda (v ...) b0 b1 ...)))

  (define-syntax-case (compose-s name expr ...) s
    ;this does not fail in the case ((quasiquote list) expr ...)
    (let (name-datum (syntax->datum (syntax name)))
      (if (list? name-datum)
        (let (name-datum (reverse name-datum))
          (datum->syntax s
            (fold list (pair (first name-datum) (syntax->datum (syntax (expr ...))))
              (tail name-datum))))
        (syntax (name expr ...)))))

  (define-syntax-rules define-as
    ;example: (define-as customname list 1 2 3)
    ;example: (define-as customname quasiquote list 1 (unquote 3))
    ((name (wrap-name ...) expr ...) (define name (compose-s (wrap-name ...) expr ...)))
    ((name wrap-name expr ...) (define name (wrap-name expr ...))))

  (define (display-line arg) (display arg) (newline))

  (define-record-type error (make-error origin name data)
    error? (origin error-origin) (name error-name) (data error-data))

  (define (make-error-p a b c) "make-error as a procedure for calling it from c" (make-error a b c))
  (define (error-origin-p a) (error-origin a))
  (define (error-name-p a) (error-name a))
  (define (error-data-p a) (error-data a))
  (define (error?-p a) (error? a))
  (define (exit-on-error a) (if (error? a) (begin (debug-log a) (exit -1)) a)))