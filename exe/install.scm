#!/usr/bin/guile
!#

(import (sph install))

(define install-spec-extension
  (let ((source "temp/libguile-sph-lib.so"))
    (if (file-exists? source) (list "/usr/lib" source) (list))))

(install-cli-guile (path-lib-scheme "modules/sph" "modules/sph.scm")
  (unquote install-spec-extension))
