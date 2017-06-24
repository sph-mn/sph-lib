(library (sph threads)
  (export
    parallel
    sph-threads-description
    (rename (letpar par-let) (par-for-each par-each)))
  (import
    (ice-9 threads)
    (rnrs base))

  (define sph-threads-description
    "re-exports and renames some parallel processing bindings from (ice-9 threads). experimental"))
