(library (sph isml compile-sxml)
  (export
    isml-config-min-level
    isml-eval-environment-set!
    isml->sxml)
  (import
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph lang parser otl)
    (only (sph read-write) port-copy->string)
    (only (sph hashtable) alist->hashtable)
    (only (guile) cons* defined? open-input-string port-filename symbol-append)
    (only (sph list) contains?)
    (only (sph two) define-stack-fluid)
    (only (sph string) string-case)
    (only (sph tree) transform-tree-with-state)
    (only (sph isml environment) isml-env-reset! isml-env-update!)
    (sph isml environment html)
    (sph isml environment base))

  ;stack for circular inclusion prevention
  (define-stack-fluid isml-include-stack)
  (define isml-config-min-level 1)

  (define isml-eval-environment
    (apply environment
      (q ((rnrs base) (except (srfi srfi-1) map) (except (guile) link system system*)
	  (sph) (sph isml environment)
          (sph isml environment base)
          (sph isml environment html)))))

  (define (isml-eval-environment-set! . library-ide)
    (set! isml-eval-environment
      (apply environment
	(cons (q (sph isml environment)) library-ide))))

  (define (compile-sxml-inner-ascend ele . style)
    (cons ele style))

  (define (compile-sxml-inner-descend arg recurse-descend . style)
    (case (first arg)
      ((escape-block escape-line)
        (cons* (cons (q pre) (tail arg)) #f style))
      ((heading)
        (apply
          (l (text . style*)
            (append
              (isml-env-heading (first (tail arg)) text)
              (list #f #f (append style* style))))
          (recurse-descend (tail (tail arg)) style)))
      ((p-expr)
        (apply
          (l (text . style*)
            (cons* text #f (append style* style)))
          (eval (first (tail arg)) isml-eval-environment)))
      ((p-block-delimited-expr)
        (let (tail-arg (tail arg))
          (if (null? tail-arg) (cons (list) style)
            (apply
              (l (text . style*) (cons text (append style* style)))
              (let (env-proc-name (symbol-append (q isml-env-block-tree-) (first tail-arg)))
                (if (defined? env-proc-name isml-eval-environment)
                  (eval
                    (list env-proc-name (list (q quote) (tail tail-arg)))
                    isml-eval-environment)
                  (throw (q no-such-block-expr) env-proc-name)))))))
      (else (cons* #f #t style))))

  (define (compile-sxml-inner-terminal arg . style)
    (if (string? arg)
      (string-case arg
        ("\n" (cons (q (br)) style))
        (cons arg style))
      (cons arg style)))

  (define (compile-sxml-inner arg) "list -> content style
    compiles the inner part of a (isml inner) expression"
    (transform-tree-with-state arg
      compile-sxml-inner-descend
      compile-sxml-inner-ascend
      compile-sxml-inner-terminal
      (list)))

  (define* (isml->sxml port\string #:optional bindings prev-bindings?) "string -> list string
    isml -> sxml css"
    (let* ;string ports dont need port-closing
      ( (port (if (string? port\string) (open-input-string port\string) port\string))
	(filename (port-filename port)))
      (let (bindings (if (list? bindings) (alist->hashtable bindings) bindings))
	;circular inclusion check
	(if (and filename (contains? (isml-include-stack) filename)) (list "" #f)
	  (begin
	    (isml-include-stack-add filename)
	    (if (not prev-bindings?) (isml-env-reset!))
	    (if bindings (isml-env-update! bindings))
            (let
              (res
                (compile-sxml-inner (tail (isml->parsed-isml (port->string port)))))
              (isml-include-stack-remove)
              res)))))))