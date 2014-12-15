(use-modules (sph module))

(module-compose (sph base) (rnrs base)
  (sph) (sph conditional) (sph hashtable) (sph list) (sph string) (sph alist) (sph error) (sph filesystem))