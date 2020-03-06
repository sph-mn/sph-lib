(define-module (sph))

(use-modules (ice-9 pretty-print) ((ice-9 optargs) #:select (lambda* define* let-keywords))
  ((srfi srfi-2) #:select (and-let*)))

(export! let first)

(export sph-description define-syntax-rules
  define-syntax-case define-syntax-cases
  l pair
  pairs tail
  each debug-log
  null l* q qq list-q list-qq apply-values quote-odd quote-even quote-duplicate nullary)

(read-disable (quote square-brackets))

(define sph-description
  "bindings that are fundamental to all sph libraries.
   exports (rnrs base) except set!. if you need set!, import it with (import (only (rnrs base) set!))
   # syntax
   define-syntax-case :: (name pattern ...) syntax-name expansion
     removes possibility to define keywords
     removes possibility to use custom lambda for define-syntax
     removes possibility to define multiple clauses
   quote-odd
     any ... -> list
     quotes each second argument starting with the first
     example: a b c d -> ((quote a) b (quote c) d)
   quote-even
     any ... -> list
     quotes each second argument starting with the second
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
     example: (let (a 3) a)
   list-q list-qq
     list-q :: any ...
     list-qq :: any ...
     same as (quote (a ...)) or (quasiquote (a ...))
     example: (list-q a b c)")

(define-syntax-rule (define-syntax-rules name ((pattern ...) expansion) ...)
  (define-syntax name (syntax-rules () ((_ pattern ...) expansion) ...)))

(define-syntax-rules define-syntax-case
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
(define-syntax-rule (list-q a ...) (q (a ...)))
(define-syntax-rule (list-qq a ...) (qq (a ...)))
(define-syntax-rule (apply-values proc producer) (call-with-values (lambda () producer) proc))

(define-syntax-rules let
  ((((variable-name expr) ...) body ...) ((lambda (variable-name ...) body ...) expr ...))
  (((variable-name expr) body ...) (let ((variable-name expr)) body ...))
  ( (name ((variable-name expr) ...) body ...)
    ((lambda (name) (set! name (lambda (variable-name ...) body ...)) (name expr ...)) #f))
  ((name (variable-name expr) body ...) (let name ((variable-name expr)) body ...)))

(define-syntax-rules quote-odd (() (quote ()))
  ((a) (quote (a))) ((a b c ...) (quasiquote (a (unquote b) (unquote-splicing (quote-odd c ...))))))

(define-syntax-rules quote-even (() (quote ()))
  ((a) (quasiquote ((unquote a))))
  ((a b c ...) (quasiquote ((unquote a) b (unquote-splicing (quote-even c ...))))))

(define-syntax-rules quote-duplicate ((a) (list (quote a) a))
  ((a b ...) (quasiquote ((unquote-splicing (quote-duplicate a) (quote-duplicate b ...))))))

(define-syntax-rule (nullary body ...) (lambda () body ...))
(define first car)
(define pair cons)
(define pairs cons*)
(define tail cdr)
(define each for-each)
(define null (list))

(define (debug-log . a)
  "any-1 any-n ... -> any-1
   writes all arguments to standard output and returns the first argument"
  (pretty-print (cons (q --) a)) (first a))
