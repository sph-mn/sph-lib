(library (sph read-write)
  (export
    bytevector->file
    file->bytevector
    file->datums
    file->port
    file->string
    port->bytevector
    port->file
    port->lines
    port->string
    port-copy-all
    port-copy-some
    port-lines-each
    port-lines-fold
    port-lines-map
    port-lines-map->port
    read-until-string-proc
    rw-any->file
    rw-any->list
    rw-any->port
    rw-any->string
    rw-chain-with-pipes
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
    rw-with-temporary-file-port->file
    temp-file-port
    (rename (read port->datum)))
  (import
    (guile)
    (ice-9 rdelim)
    (rnrs base)
    (rnrs io ports)
    (sph)
    (sph filesystem)
    (only (srfi srfi-1) drop))

  (define rw-chain-with-pipes
    (letrec
      ( (loop
          (l (in-pipe out-pipe in out proc . rest)
            (if (null? rest) (proc in-pipe #f in out)
              (let (ports (pipe))
                (let ((in-pipe (first ports)) (out-pipe (tail ports)))
                  (proc in-pipe out-pipe in #f) (apply loop in-pipe out-pipe #f out rest)))))))
      (l (port-input port-output . proc)
        "create a pipe for each procedure output and the next procedure input and call procedures with the respective input/output-ports"
        (if (not (null? proc))
          (apply loop #f
            #f (if (and port-input (boolean? port-input)) (current-input-port) port-input)
            (if (and port-output (boolean? port-output)) (current-output-port) port-output) proc)))))

  (define* (temp-file-port #:optional (path "/tmp") (name-part "."))
    "[string] [string:infix] -> port
    create a new unique file in the file system and return a new buffered port for reading and writing to the file"
    (mkstemp! (string-append (ensure-trailing-slash path) name-part "XXXXXX")))

  (define* (rw-file-indirect->file read write path-1 #:optional (path-2 path-1))
    "like rw-port->file but takes a path for reading from an input file"
    (rw-with-temporary-file-port->file
      (l (port-output) (rw-file->port read write path-1 port-output)) path-2))

  (define (file->string path\file)
    "string/file -> string
    open or use an opened file, read until end-of-file is reached and return a string of file contents"
    (if (string? path\file) (call-with-input-file path\file port->string) (port->string path\file)))

  (define (file->bytevector path\file)
    "string -> bytevector
    open or use an opened file, read until end-of-file is reached and return a bytevector of file contents"
    (call-with-input-file path\file port->bytevector #:binary #t))

  (define (bytevector->file a path)
    (call-with-output-file path (l (out) (put-bytevector out a)) #:binary #t))

  (define (rw-with-temporary-file-port->file proc path)
    (let* ((port-temp (temp-file-port (dirname path))) (path-temp (port-filename port-temp)))
      (proc port-temp) (rename-file path-temp path)))

  (define (rw-port-indirect->file read write port path)
    "like rw-port->file but uses a temporary file to buffer all written data before it is written to the target path.
    use case: read from a file for processing and write to the same file.
    also known as: late-write"
    (rw-with-temporary-file-port->file
      (l (port-output) (rw-port->port read write port port-output)) path))

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
    (cond ((port? a) (rw-port->list read a)) ((string? a) (rw-file->list read a))))

  (define (port->file a path)
    "port string ->
    read all available data from port and write it to a file specified by path"
    (call-with-output-file path (l (port) (port-copy-all a port))))

  (define (file->port path port)
    (call-with-input-file path (l (port-file) (port-copy-all port-file port))))

  (define (port-copy-some port port-2 count)
    "port port integer ->
    copy \"count\" number of bytes from \"port\" to \"port-2\""
    (rw-port->port (l (port) (let (r (get-bytevector-n port 512)) (or r (eof-object))))
      (l (data port) (put-bytevector port data)) port port-2))

  (define port->string get-string-all)
  (define port->bytevector get-bytevector-all)

  (define* (port-copy-all port port-2 #:optional (buffer-size 4096))
    (if (not (eof-object? (port-copy-some port port-2 buffer-size)))
      (port-copy-all port port-2 buffer-size)))

  (define-syntax-rule (table-match-or-update char table)
    ;"character (#(current-index max-index string) ...) -> string:match/list:updated-table"
    (let loop ((e (first table)) (rest (tail table)))
      (let ((i (vector-ref e 0)) (s (vector-ref e 2)))
        (if (eqv? char (string-ref s i))
          (if (= i (vector-ref e 1)) s
            (begin (vector-set! e 0 (+ i 1))
              (if (null? rest) table (loop (first rest) (tail table)))))
          (if (null? rest) table (loop (first rest) (tail table)))))))

  (define (read-until-string-proc . strings)
    "string ... -> procedure:{port -> (string:before-string . matched-string)}
    returns a procedure that reads from a port until one of the given strings has been found"
    (let (table-init (map (l (e) (vector 0 (- (string-length e) 1) e)) strings))
      (l (port)
        "port (string ...) -> (string:before-string . matched-string)
      reads from port until any of the given strings has matched"
        (let loop ((char (read-char port)) (before-chars (list)) (table table-init))
          (if (eof-object? char) #f
            (let (match/table (table-match-or-update char table))
              (if (string? match/table)
                (pair
                  (if (null? before-chars) ""
                    (list->string
                      (reverse
                        (drop before-chars
                          (min (length before-chars) (- (string-length match/table) 1))))))
                  match/table)
                (loop (read-char port) (pair char before-chars) match/table))))))))

  (define* (file->datums path #:optional (port->datum read))
    "string procedure:reader -> list
    read all scheme datums of a file specified by path"
    (call-with-input-file path
      (l (port)
        (let loop ((e (port->datum port)))
          (if (eof-object? e) (list) (pair e (loop (port->datum port))))))))

  (define*
    (port-lines-each proc #:optional (port (current-input-port)) #:key (handle-delim (q trim)))
    "procedure:{line ->} port symbol ->
     call proc once with every line read from a port"
    (let loop ((line (read-line port handle-delim)))
      (if (not (eof-object? line)) (begin (proc line) (loop (read-line port handle-delim))))))

  (define* (port-lines-fold proc init #:optional (port (current-input-port)))
    "procedure:{string:line any} any [port] -> any
    fold over lines read from port"
    (let loop ((line (read-line port)) (r init))
      (if (eof-object? line) r (loop (read-line port) (proc line r)))))

  (define* (port-lines-map proc #:optional (port (current-input-port)))
    "procedure:{string:line -> any} [port] -> list
    map each line of port to a list.
    port is the current input port by default"
    (reverse (port-lines-fold (l (a b) (pair (proc a) b)) (list) port)))

  (define*
    (port-lines-map->port proc #:optional (port-input (current-input-port))
      (port-output (current-output-port))
      #:key
      (handle-delim (q concat)))
    "procedure [port port symbol:concat/trim/peek/split] ->
    map lines from port to port. the trailing newline is included by default but this behaviour can be set like for read-line.
    the default ports are the current input and output ports"
    (rw-port->port (l (port) (read-line port handle-delim)) (l (e port) (display (proc e) port))
      port-input port-output))

  (define (port->lines a)
    "port -> (string ...)
    read all lines from port and return them as strings in a list"
    (let loop ((line (get-line a))) (if (eof-object? line) (list) (pair line (loop (get-line a)))))))
