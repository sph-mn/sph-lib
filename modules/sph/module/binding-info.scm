(define-module (sph module binding-info))

(use-modules (srfi srfi-2) (rnrs sorting)
  (sph) (sph module) (sph record) ((ice-9 session) #:select (procedure-arguments)))

(export bi-arguments bi-documentation
  bi-include-variable-values bi-name
  bi-set-arguments! bi-type
  binding-info-layout macro->binding-info
  module-binding-info procedure->binding-info sort-module-binding-info sph-binding-info-description)

(define sph-module-binding-info-description "get information about bindings in modules")
(define bi-include-variable-values #f)
(define binding-info-layout (make-record-layout (quote (name type documentation arguments))))

(define-record-accessors binding-info-layout (bi-name (q name))
  (bi-type (q type)) (bi-documentation (q documentation)) (bi-arguments (q arguments)))

(define-record-setters binding-info-layout (bi-set-arguments! (q arguments)))

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
  (record binding-info-layout name
    (q procedure) (procedure-documentation proc) (procedure-arguments proc)))

(define (macro->binding-info macro name) "macro-variable -> vector"
  (let (transformer (macro-transformer macro))
    (record binding-info-layout name
      (q syntax) (and transformer (procedure-documentation transformer))
      (and transformer
        (macro-arguments name (procedure-property transformer (q macro-type)) transformer)))))

(define (variable->binding-info value name) "string any -> vector"
  (record binding-info-layout name (q variable) #f (and bi-include-variable-values value)))

(define module-binding-info
  (let
    ( (get-info
        (l (name value) "-> record"
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
