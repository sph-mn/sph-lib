(library (sph process create)
  (export
    process-create)
  (import
    (rnrs base))

  (load-extension "libguile-sph-lib" "init-sph-lib")

  (define*
    (process-create executable #:optional (list arguments) input-port output-port error-port #:key
      (env (environ))
      (keep-descriptors (list)))
    (primitive-process-create executable arguments
      input-port output-port error-port env keep-descriptors)))
