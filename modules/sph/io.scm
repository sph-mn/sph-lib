(library (sph io)
  (export
    bytevector->file
    call-with-input-files
    call-with-pipe
    call-with-pipes
    call-with-temp-file
    each-u8
    file->bytevector
    file->datums
    file->file
    file->port
    file->string
    named-pipe
    named-pipe-chain
    pipe-chain
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
    sph-io-description
    string->file
    temp-file-port
    temp-file-port->file
    (rename (read port->datum)))
  (import
    (guile)
    (ice-9 rdelim)
    (rnrs io ports)
    (sph)
    (sph list)
    (only (sph filesystem) ensure-trailing-slash)
    (only (sph one) begin-first)
    (only (srfi srfi-1) drop))

  (define sph-io-description "port and file input output")

  (define (rw-port->port read write port port-2)
    ;copied from io read-write to avoid circular dependency
    (let loop ((e (read port))) (if (eof-object? e) e (begin (write e port-2) (loop (read port))))))

  (define (string->file a path)
    "string string -> unspecified
    write string into file at path, overwriting the file"
    (call-with-output-file path (l (file) (display a file))))

  (define (each-u8 proc port)
    "procedure:{integer -> any} port -> unspecified
    call proc with each eight bit integer read from port until end-of-file is reached."
    (let next ((octet (get-u8 port)))
      (if (eof-object? octet) #t (begin (proc octet) (next (get-u8 port))))))

  (define*
    (file->file path-input path-output #:optional (proc port-copy-all) #:key (input-binary? #t)
      (output-binary? #t)
      (append? #f))
    "string string procedure:{port port -> any} [#:input-binary boolean #:output-binary? boolean] -> any
    open path-input for reading and path-output for writing and copy all contents of the input file or call proc with the ports.
    the ports are closed when proc returns"
    (let
      ( (in (open-file path-output "r"))
        (out
          (open-file path-output
            (if append? (if output-binary? "ab" "a") (if output-binary? "wb" "w")))))
      (begin-first (proc in out) (close-port out) (close-port in))))

  (define (call-with-input-files proc . paths)
    (let (files (map (l (a) (open-file a "r")) paths))
      (begin-first (apply proc files) (each close-port files))))

  (define* (temp-file-port #:optional (path "/tmp") (name-part "."))
    "[string] [string:infix] -> port
    create a new unique file in the file system and return a new buffered port for reading and writing to the file"
    (mkstemp! (string-append (ensure-trailing-slash path) name-part "XXXXXX")))

  (define* (call-with-temp-file proc #:optional (path "/tmp") (name-part "."))
    "procedure:{port -> any} -> any
    call proc with an output port to a temporary file.
    the file is deleted after proc returns or the current process exits.
    result is the result of calling proc"
    (let (port (temp-file-port))
      (let ((result (proc port)) (path (port-filename port))) (close-port port)
        (delete-file path) result)))

  (define (call-with-pipes count proc)
    "integer procedure:{[pipe-n-in pipe-n-out] ... -> any} -> any
    the pipes are closed after proc finished. they can also be closed by proc.
    reading from the input side might block as long as the output side is not yet closed"
    (let
      (pipes
        (fold-integers count (list)
          (l (n result) (let (a (pipe)) (pairs (first a) (tail a) result)))))
      (begin-first (apply proc pipes) (each (l (a) (if (not (port-closed? a)) (close a))) pipes))))

  (define (call-with-pipe proc) "equivalent to (call-with-pipes 1 proc)" (call-with-pipes 1 proc))

  (define* (named-pipe #:optional path (permissions 438))
    "[string integer] -> string:path
    create a named pipe (fifo).
    named pipes persist in the filesystem"
    (let (path (or path (tmpnam))) (mknod path (q fifo) permissions 0) path))

  (define (pipe-chain first-input last-output . proc)
    "port/true port/true procedure:{pipe-input pipe-output -> false/any} ... -> (procedure-result ...)
    create a pipe for each procedure output and the next procedure input and call procedures with the respective input/output-ports.
    if any result is false then stop and return results up to that point.
    the pipe endpoints are not automatically closed to allow the use of threads in procedures"
    (if (null? proc) proc
      (let loop ((in first-input) (out #f) (proc (first proc)) (rest (tail proc)))
        (if (null? rest) (list (proc in last-output))
          (let (a (pipe))
            (let (result (proc in (tail a)))
              (if result (pair result (loop (first a) #f (first rest) (tail rest))) (list))))))))

  (define (named-pipe-chain first-input last-output . proc)
    "port/true port/true procedure:{pipe-input pipe-output -> false/any} ... -> (procedure-result ...)
    creates a named pipe shared between a procedure output and the next procedure input.
    procedure results are saved in a list which is returned unless a result is false
    in which case it stops and results up to that point are returned.
    the named pipes persist in the file system and are not automatically deleted"
    (if (null? proc) proc
      (let loop ((in first-input) (out #f) (proc (first proc)) (rest (tail proc)))
        (if (null? rest) (list (proc in last-output))
          (let (a (named-pipe))
            (let (result (proc in a))
              (if result (pair result (loop a #f (first rest) (tail rest))) (list))))))))

  (define (file->string path/file)
    "string/file -> string
    open or use an opened file, read until end-of-file is reached and return a string of file contents"
    (if (string? path/file) (call-with-input-file path/file port->string) (port->string path/file)))

  (define (file->bytevector path\file)
    "string -> bytevector
    open or use an opened file, read until end-of-file is reached and return a bytevector of file contents"
    (call-with-input-file path\file port->bytevector #:binary #t))

  (define (bytevector->file a path)
    (call-with-output-file path (l (out) (put-bytevector out a)) #:binary #t))

  (define (temp-file-port->file proc path)
    (let* ((port-temp (temp-file-port (dirname path))) (path-temp (port-filename port-temp)))
      (proc port-temp) (rename-file path-temp path)))

  (define (port->file a path)
    "port string ->
    read all available data from port and write it to a file specified by path"
    (call-with-output-file path (l (port) (port-copy-all a port))))

  (define (file->port path port)
    "string port ->
    copy all content of file at path to port"
    (call-with-input-file path (l (file) (port-copy-all file port))))

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