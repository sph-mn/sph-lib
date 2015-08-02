(library (sph system)
  (export
    sph-sys-config-path)
  (import
    (guile))

  (define (sph-system-config-path) (string-append (getenv "HOME") "/.config/sph/")))