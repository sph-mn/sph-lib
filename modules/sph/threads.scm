(library (sph threads)
  (export
    parallel
    sph-threads-description
    (rename (letpar let-par)
      (par-map map-par)
      (par-for-each each-par)
      (n-par-map map-par-n)
      (n-par-for-each each-par-n)))
  (import
    (ice-9 threads)
    (rnrs base))

  (define sph-threads-description
    "re-exports some and renames parallel processing bindings from (ice-9 threads). experimental"))
