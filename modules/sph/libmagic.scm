(library (sph libmagic)
  (export
    file->mime-types
    sph-libmagic-description
    string->mime-types)
  (import
    (sph)
    (system foreign)
    (only (guile) dynamic-link dynamic-func)
    (only (sph string) string-octet-length))

  (define sph-libmagic-description
    "binding to the libmagic library from the \"file\" utility that guesses file types")

  (define libmagic (dynamic-link "libmagic"))
  ; MAGIC_* values are from include/magic.h
  (define MAGIC_MIME_TYPE 16)
  (define MAGIC_DEBUG 1)
  (define pointer (q *))

  (define foreign-magic-open
    (pointer->procedure pointer (dynamic-func "magic_open" libmagic) (list int)))

  (define foreign-magic-close
    (pointer->procedure void (dynamic-func "magic_close" libmagic) (list pointer)))

  (define foreign-magic-load
    (pointer->procedure int (dynamic-func "magic_load" libmagic) (list pointer pointer)))

  (define foreign-magic-file
    (pointer->procedure pointer (dynamic-func "magic_file" libmagic) (list pointer pointer)))

  (define foreign-magic-buffer
    (pointer->procedure pointer (dynamic-func "magic_buffer" libmagic)
      (list pointer pointer size_t)))

  (define*
    (call-with-magic-database proc #:optional (magic-open-flags MAGIC_MIME_TYPE)
      (magic-file-path %null-pointer))
    "procedure:{handle -> any} integer string -> any"
    (let (magic-handle (foreign-magic-open magic-open-flags))
      (foreign-magic-load magic-handle magic-file-path)
      (let (result (proc magic-handle)) (foreign-magic-close magic-handle) result)))

  (define (file->mime-types . a) "string:path ... -> (string:mime-type ...)"
    (call-with-magic-database
      (l (handle) (map (l (b) (pointer->string (foreign-magic-file handle (string->pointer b)))) a))))

  (define (string->mime-types . a) "string:content ... -> (string:mime-type ...)"
    (call-with-magic-database
      (l (handle)
        (map
          (l (b)
            (pointer->string
              (foreign-magic-buffer handle (string->pointer b) (string-octet-length b))))
          a)))))
