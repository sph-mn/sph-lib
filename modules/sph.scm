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
    and-let*
    any
    apply-values
    catch
    compose-s
    datum->syntax
    debug-log
    define*
    define-as
    define-syntax-case
    define-syntax-cases
    define-syntax-rule
    define-syntax-rules
    display-line
    each
    every
    first
    fold
    fold-right
    identifier?
    l
    l*
    lambda*
    let
    null
    pair
    pairs
    par-let
    par-map
    parallel
    q
    qq
    quasisyntax
    second
    sph-description
    syntax
    syntax->datum
    syntax-rule
    syntax-rules_
    tail
    throw
    thunk
    unsyntax
    unsyntax-splicing
    with-syntax)
  (import
    (ice-9 pretty-print)
    (ice-9 threads)
    (except (rnrs base) let)
    (only (guile)
      LC_ALL
      catch
      cons*
      current-output-port
      datum->syntax
      define-syntax
      display
      exit
      filter
      identifier?
      module-map
      module-re-export!
      module-uses
      newline
      quasisyntax
      read-disable
      setlocale
      string=
      syntax
      syntax->datum
      syntax-case
      throw
      unsyntax
      unsyntax-splicing
      with-syntax
      resolve-interface
      current-module
      write)
    (only (ice-9 optargs) lambda* define*)
    (only (srfi srfi-1)
      any
      second
      every
      first
      last
      fold
      fold-right)
    (only (srfi srfi-2) and-let*))

  (define sph-description
    "bindings that are fundamental to all sph libraries
    includes (rnrs base)")

  (module-re-export! (current-module)
    ;export (rnrs base). exclude let because it is redefined here
    (filter (lambda (e) (not (or (eq? e (quote %module-public-interface)) (eq? e (quote let)))))
      (module-map (lambda a (car a)) (resolve-interface (q (rnrs base))))))

  ;affects port\string encodings
  ;  initialise all locale categories based on system environment variables
  (setlocale LC_ALL "")
  (read-disable (quote square-brackets))

  (define (debug-log . a)
    "any-1 any-n ... -> any-1
    writes all arguments to standard output and returns the first argument"
    (pretty-print (cons (q --) a)) (first a))

  (define-syntax syntax-rule
    ;like syntax-rules but only for one syntax-rule and without keyword support
    (syntax-rules () ((_ (pattern ...) expansion) (syntax-rules () ((_ pattern ...) expansion)))))

  (define-syntax syntax-rules_
    ;like syntax-rules but without keyword support
    (syntax-rules ()
      ((_ ((pattern ...) expansion) ...) (syntax-rules () ((_ pattern ...) expansion) ...))))

  (define-syntax define-syntax-rule
    ;removes option to define multiple clauses
    ;removes option to define keywords
    (syntax-rule ((name pattern ...) expansion)
      (define-syntax name (syntax-rule (pattern ...) expansion))))

  (define-syntax define-syntax-rules
    ;removes option to define keywords
    (syntax-rule (name ((pattern ...) expansion) ...)
      (define-syntax name (syntax-rules () ((_ pattern ...) expansion) ...))))

  (define-syntax-rules define-syntax-case
    ;removes possibility to define keywords
    ;removes possibility to use custom lambda for define-syntax
    ;removes possibility to define multiple clauses
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

  (define-syntax-rule (l a ...) (lambda a ...))
  (define-syntax-rule (l* a ...) (lambda* a ...))
  (define-syntax-rule (q a) (quote a))
  (define-syntax-rule (qq a) (quasiquote a))
  (define-syntax-rule (thunk body ...) (l () body ...))
  (define-syntax-rule (apply-values proc producer) (call-with-values (thunk producer) proc))
  (define pair cons)
  (define pairs cons*)
  (define tail cdr)
  (define each for-each)
  (define null (list))

  (define-syntax-rules let
    ;let and named-let extended with a simplified syntax for binding just one variable
    ;example: (let (a 3) a)
    ((((variable-name expr) ...) body ...) ((lambda (variable-name ...) body ...) expr ...))
    (((variable-name expr) body ...) (let ((variable-name expr)) body ...))
    ( (name ((variable-name expr) ...) body ...)
      ((lambda (name) (set! name (lambda (variable-name ...) body ...)) (name expr ...)) #f))
    ((name (variable-name expr) body ...) (let name ((variable-name expr)) body ...)))

  (define-syntax-case (compose-s name expr ...) s
    ;(compose-s a (list 1 2)) -> (a (list 1 2))
    ;(compose-s (a b) (list 1 2)) -> (a (b (list 1 2)))
    ;this does not fail in the case (_ (quasiquote list) expr ...)
    (let (name-datum (syntax->datum (syntax name)))
      (if (list? name-datum)
        (let (name-datum (reverse name-datum))
          (datum->syntax s
            (fold list (pair (first name-datum) (syntax->datum (syntax (expr ...))))
              (tail name-datum))))
        (syntax (name expr ...)))))

  (define-syntax-rules define-as
    ;example: (define-as list 1 2 3)
    ;example: (define-as (quasiquote list) 1 (unquote 3)
    ((name (wrap-name ...) expr ...) (define name (compose-s (wrap-name ...) expr ...)))
    ((name wrap-name expr ...) (define name (wrap-name expr ...))))

  (define* (display-line a #:optional (port (current-output-port)))
    "any [port] -> unspecified
    like \"display\" but emits a newline at the end"
    (display a port) (newline port)))
