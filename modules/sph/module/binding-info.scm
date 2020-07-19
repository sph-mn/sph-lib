(define-module (sph module binding-info))

(use-modules (srfi srfi-2) (rnrs sorting)
  (sph vector) (sph) (sph module) ((ice-9 session) #:select (procedure-arguments)))

(export bi-arguments bi-documentation
  bi-include-variable-values bi-name
  bi-arguments-set! bi-type
  bi-documentation-set! bi-name-set!
  bi-type-set! binding-info-layout
  macro->binding-info module-binding-info
  procedure->binding-info sort-module-binding-info sph-binding-info-description)

(define sph-module-binding-info-description "get information about bindings in modules")
(define bi-include-variable-values #f)
(define bi-name (vector-accessor 0))
(define bi-type (vector-accessor 1))
(define bi-documentation (vector-accessor 2))
(define bi-arguments (vector-accessor 3))
(define bi-name-set! (vector-setter 0))
(define bi-type-set! (vector-setter 1))
(define bi-documentation-set! (vector-setter 2))
(define bi-arguments-set! (vector-setter 3))

(define (macro-arguments name type transformer)
  "symbol symbol macro-transformer -> argument-spec
   get the formal arguments specification for a macro"
  (case type
    ( (syntax-rules)
      (let (patterns (procedure-property transformer (q patterns)))
        (if (pair? patterns) (first patterns) (list))))
    ((identifier-syntax) (list))
    (else #f)))

(define* (procedure->binding-info proc #:optional (name (procedure-name proc)))
  "procedure [string] -> vector"
  (vector name (q procedure) (procedure-documentation proc) (procedure-arguments proc)))

(define (macro->binding-info macro name) "macro-variable -> vector"
  (let (transformer (macro-transformer macro))
    (vector name (q syntax)
      (and transformer (procedure-documentation transformer))
      (and transformer
        (macro-arguments name (procedure-property transformer (q macro-type)) transformer)))))

(define (variable->binding-info value name) "string any -> vector"
  (vector name (q variable) #f (and bi-include-variable-values value)))

(define module-binding-info
  (let
    ( (get-info
        (l (name value) "-> vector"
          (and-let* ((value (false-if-exception (variable-ref value))))
            ( (if (procedure? value) procedure->binding-info
                (if (macro? value) macro->binding-info variable->binding-info))
              value name)))))
    (l (module-name)
      "(symbol ...) -> (vector ...)
       get information about all exported bindings of the given module"
      (filter identity (module-map get-info (resolve-interface module-name))))))

(define (sort-module-binding-info a) "(binding-info ...)"
  (list-sort (l (a b) (string< (symbol->string (bi-name a)) (symbol->string (bi-name b)))) a))
