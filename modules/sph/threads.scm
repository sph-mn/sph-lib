(library (sph threads)
  (export
    parallel
    (rename
      (letpar let-par)
      (par-map map-par)
      (par-for-each each-par)
      (n-par-map map-par-n)
      (n-par-for-each each-par-n)))
  (import
    (ice-9 threads)))