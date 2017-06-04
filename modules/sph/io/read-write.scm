(library (sph io read-write)
  (export
    rw-any->file
    rw-any->list
    rw-any->port
    rw-any->string
    rw-file->file
    rw-file->list
    rw-file->port
    rw-file->string
    rw-file-indirect->file
    rw-list->file
    rw-list->port
    rw-list->string
    rw-port->file
    rw-port->list
    rw-port->port
    rw-port->string
    rw-port-indirect->file
    rw-string->file
    rw-string->list
    rw-string->port
    rw-string->string
    sph-io-read-write-description)
  (import
    (guile)
    (sph)
    (sph io))

  (define sph-io-read-write-description "port io with specifying a read and write procedure")

  (define* (rw-file-indirect->file read write path-1 #:optional (path-2 path-1))
    "like rw-port->file but takes a path for reading from an input file"
    (temporary-file-port->file (l (port-output) (rw-file->port read write path-1 port-output))
      path-2))

  (define (rw-port-indirect->file read write port path)
    "like rw-port->file but uses a temporary file to buffer all written data before it is written to the target path.
    use case: read from a file for processing and write to the same file.
    also known as: late-write"
    (temporary-file-port->file (l (port-output) (rw-port->port read write port port-output)) path))

  (define (rw-port->list read port)
    (let loop ((e (read port))) (if (eof-object? e) (list) (pair e (loop (read port))))))

  (define (rw-port->port read write port port-2)
    (let loop ((e (read port))) (if (eof-object? e) e (begin (write e port-2) (loop (read port))))))

  (define (rw-port->string read write port)
    (call-with-output-string
      (l (string-port)
        (let loop ((e (read port)))
          (if (eof-object? e) (get-output-string string-port) (write e string-port))))))

  (define (rw-port->file read write port path)
    (call-with-output-file path
      (l (file-port) (let loop ((e (read port))) (if (not (eof-object? e)) (write e file-port))))))

  (define (rw-file->list read path)
    (call-with-input-file path (l (port) (rw-port->list read port))))

  (define (rw-file->port read write path port)
    (call-with-input-file path (l (file-port) (rw-port->port read write file-port port))))

  (define (rw-file->string read write path)
    (call-with-input-file path (l (port) (rw-port->string read write port))))

  (define (rw-file->file read write path path-2)
    "the target file at \"path-2\" is truncated or created before the writing starts"
    (call-with-output-file path-2 (l (port) (rw-file->port read write path port))))

  (define (rw-string->list read a) (call-with-input-string a (l (port) (rw-port->list read port))))

  (define (rw-string->port read write a port)
    (call-with-input-string a (l (string-port) (rw-port->port read write string-port port))))

  (define (rw-string->file read write a path)
    (call-with-output-file path (l (port) (rw-string->port read write a port))))

  (define (rw-string->string read write a)
    (call-with-output-string (l (port) (rw-string->port read write a port))))

  (define (rw-list->port write a port) (map (l (e) (write e port)) list))

  (define (rw-list->string write a)
    (call-with-output-string (l (port) (map (l (e) (write e port)) a))))

  (define (rw-list->file write a path)
    (call-with-output-file path (l (port) (map (l (e) (write a port)) a))))

  (define (rw-any->file read write a path)
    "except file->file
    applies the appropriate rw- procedure depending on data-type of a"
    (cond ((port? a) (rw-port->file read write a path))
      ((string? a) (rw-string->file read write a path)) ((list? a) (rw-list->file write a path))))

  (define (rw-any->string read write a) "except string->string"
    (cond ((port? a) (rw-port->string read write a)) ((string? a) (rw-file->string read write a))
      ((list? a) (rw-list->string write a))))

  (define (rw-any->port read write a port) "except string->port"
    (cond ((port? a) (rw-port->string read write a)) ((string? a) (rw-file->string read write a))
      ((list? a) (rw-list->string write a))))

  (define (rw-any->list read a) "except string->list and list->list"
    (cond ((port? a) (rw-port->list read a)) ((string? a) (rw-file->list read a)))))
