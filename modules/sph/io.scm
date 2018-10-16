(import (sph module))

(module-compose (sph io) (sph io other) (rnrs io ports)
  (except (rnrs io simple) call-with-input-file call-with-output-file))
