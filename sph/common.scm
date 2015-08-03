(use-modules (sph module))
;set of commonly used modules.

(module-compose (sph common) (rnrs base)
  (rnrs-sorting) (except (srfi srfi-1) map)
  (sph) (sph alist)
  (sph cli) (sph conditional)
  (sph error) (sph filesystem)
  (sph hashtable) (sph list)
  (sph list one) (sph math)
  (sph one) (sph process)
  (sph random-data) (sph read-write) (sph string) (sph time) (sph tree) (sph vector))