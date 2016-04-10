;likely deprecated
(library (sph web html-include)
  (export
    find-file
    find-files
    html-include-proc)
  (import
    (except (srfi srfi-1) map)
    (rnrs base)
    (guile)
    (sph)
    (sph hashtable)
    (sph list)
    (sph one)
    (sph web sxhtml)
    (sph filesystem))

  (define (find-file path suffix->handler)
    (any
      (l (ele)
        (let ((full-path (string-append path (first ele))))
          (if (file-exists? full-path)
            (cons full-path (tail ele))
            #f)))
      suffix->handler))

  (define (find-path+search-path proc paths search-paths)
    (every-map
      (l (path)
        (any (l (search-path) (proc search-path path)) search-paths))
      paths))

  (define (find-files paths search-paths suffix->handler)
    (find-path+search-path
      (l search-path+path
        (find-file (apply string-append search-path+path) suffix->handler))
      paths search-paths))

  (define (file-update-necessary? target-path found-files) "-> boolean"
    (if (file-exists? target-path)
      (let
	( (files-newest-mtime (apply max (map (l (ele) (stat:mtime (stat (first ele)))) found-files)))
	  (target-path-mtime (stat:mtime (stat target-path))))
	(> files-newest-mtime target-path-mtime))
      #t))

  (define (paths->target-file-name arg suffix)
    (string-append (remove-filename-extension (last arg)) "-"
      (number->string (string-hash (apply string-append arg)) 32)
      suffix))

  (define* (prepare-include config search-paths paths)
    (let*
      ( (suffix->handler (vector-ref config 0))
        (found-files (find-files paths search-paths suffix->handler)))
      (if found-files
        (let*
          ( (target-file-name (paths->target-file-name paths (first (first suffix->handler))))
            (target-path (string-append (vector-ref config 2) target-file-name)))
          (if #t
            ;(file-update-necessary? target-path found-files)
            (begin
              (apply merge-files (begin (ensure-directory-structure (dirname target-path)) target-path)
                (map! (l (ele) ((tail ele) (first ele))) found-files))
              ((vector-ref config 1) target-path)))
          (string-append (vector-ref config 3) target-file-name))
        #f)))

  (define (content-type->sxhtml-include-tag arg)
    (if (equal? (q stylesheet) arg) sxhtml-include-style sxhtml-include-script))

  (define (html-include-proc include-config content-type->search-paths)
      #;(include-config (symbol-hashtable content-type (vector (alist suffix single-action ...) action target-path)))
    (lambda (content-type . paths)
      (let
        (path
          (prepare-include
            (hashtable-ref include-config content-type)
            (content-type->search-paths content-type)
            (string-join (map symbol->string paths) "/")))
        (if path
          ((content-type->sxhtml-include-tag content-type) path)
          "")))))
