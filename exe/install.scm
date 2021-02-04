#!/usr/bin/guile
!#

(import (sph install))

(define install-config-extension
  (let ((source "temp/libguile-sph-lib.so"))
    (if (file-exists? source) (list (list (quote system-libraries) source)) (list))))

(install-cli ()
  (guile-site-modules "modules/sph" "modules/sph.scm")
  (unquote-splicing install-config-extension))
