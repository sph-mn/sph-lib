; (sph) - fundamental bindings for sph-lib
; written for the guile scheme interpreter
; Copyright (C) 2010-2016 sph <sph@posteo.eu>
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
    datum->syntax
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
    every
    first
    fold
    fold-right
    identifier?
    identity-s
    l
    l*
    lambda*
    let
    pair
    pairs
    par-let
    par-map
    parallel
    q
    ql
    qq
    quasisyntax
    quoted-list
    second
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
      debug-disable
      debug-set!
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
      read-set!
      resolve-interface
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

  ;export (rnrs base). exclude let because it is redefined here

  (module-re-export! (current-module)
    (filter (lambda (e) (not (or (eq? e (quote %module-public-interface)) (eq? e (quote let)))))
      (module-map (lambda a (car a)) (resolve-interface (q (rnrs base))))))

  ;affects port\string encodings
  ;  initialise all locale categories based on standard system environment variables
  (setlocale LC_ALL "")
  (read-disable (quote square-brackets))

  (define (debug-log . a)
    "writes all arguments to standard-output and results in the first argument"
    (pretty-print (cons (q --) a)) (first a))

  (define-syntax syntax-rule
    (syntax-rules () ((_ (pattern ...) expansion) (syntax-rules () ((_ pattern ...) expansion)))))

  (define-syntax syntax-rules_
    (syntax-rules ()
      ((_ ((pattern ...) expansion) ...) (syntax-rules () ((_ pattern ...) expansion) ...))))

  (define-syntax define-syntax-rule
    ;removes possibility to define keywords
    ;removes possibility to define multiple clauses
    (syntax-rule ((name pattern ...) expansion)
      (define-syntax name (syntax-rule (pattern ...) expansion))))

  (define-syntax define-syntax-rules
    ;removes possibility to define keywords
    (syntax-rule (name ((pattern ...) expansion) ...)
      (define-syntax name (syntax-rules () ((_ pattern ...) expansion) ...))))

  (define-syntax-rules define-multiple ((() expr) (quote unspecified))
    ( ( (identifier . identifiers) expr)
      (begin (define identifier expr) (define-multiple identifiers (tail identifier))
        (define identifier (first identifier)))))

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
  (define-syntax-rule (identity-s a) a)
  (define-syntax-rule (q a) (quote a))
  (define-syntax-rule (quoted-list a ...) (q (a ...)))
  (define-syntax-rule (ql a ...) (quoted-list a ...))
  (define-syntax-rule (qq a) (quasiquote a))
  (define pair cons)
  (define pairs cons*)
  (define tail cdr)
  (define each for-each)

  (define-syntax-rules let
    ;let and named-let plus a simplified syntax for binding just one variable
    ((((variable-name expr) ...) body ...) ((lambda (variable-name ...) body ...) expr ...))
    (((variable-name expr) body ...) (let ((variable-name expr)) body ...))
    ( (name ((variable-name expr) ...) body ...)
      ((lambda (name) (set! name (lambda (variable-name ...) body ...)) (name expr ...)) #f))
    ((name (variable-name expr) body ...) (let name ((variable-name expr)) body ...)))

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

  (define* (display-line a #:optional (port (current-output-port))) (display a port) (newline port))
  (define-syntax-rule (thunk body ...) (l () body ...)))
