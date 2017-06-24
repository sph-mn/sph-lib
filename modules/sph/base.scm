(import (sph module))

(module-compose (sph base) (except (srfi srfi-1) map)
  (rnrs sorting) (rnrs exceptions)
  (except (rnrs hashtables) hashtable-ref symbol-hash) (sph alist)
  (sph filesystem) (sph hashtable)
  (sph io) (sph list) (sph number) (sph one) (sph process) (sph string) (sph vector) (sph))
