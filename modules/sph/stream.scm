(library (sph stream)
  (export
    port->buffered-octet-stream
    port->delimited-stream
    port->line-stream
    port->stream
    stream-any
    stream-each
    stream-first
    stream-first-or-null
    stream-fold-right-multiple
    stream-tail)
  (import
    (ice-9 rdelim)
    (rnrs io ports)
    (sph)
    (sph module)
    (except (srfi srfi-41) port->stream)
    (only (guile)
      eof-object?
      current-module
      resolve-interface)
    (only (sph conditional) identity-if))

  (define sph-stream-description
    "srfi-41 stream utilities.
     re-exports (srfi srfi-41)")

  (module-re-export-modules (except (srfi srfi-41) port->stream))
  (define stream-each stream-for-each)
  (define stream-first stream-car)
  (define stream-tail stream-cdr)

  (define (stream-fold-right-multiple proc a . r) "procedure stream any:state ..."
    (if (stream-null? a) r
      (apply proc (stream-first a) (apply stream-fold-right-multiple proc (stream-tail a) r))))

  (define (port->buffered-octet-stream port buffer-size)
    "port integer -> stream
     creates stream that produces bytevectors of length buffer-size filled with data/octets read from port"
    (let (read (l (port) (get-bytevector-n port buffer-size)))
      (stream-let next ((e (read port)))
        (if (eof-object? e) stream-null (stream-cons e (next (read port)))))))

  (define (port->stream port reader)
    "port procedure:{port -> any/eof-object} -> stream
     for comparison, the port->stream of srfi-41 does not support a custom reader.
     reader is supposed to result in eof-object if no more data is available"
    (stream-let next ((e (reader port)))
      (if (eof-object? e) stream-null (stream-cons e (next (reader port))))))

  (define (port->line-stream port)
    "port -> stream -> string ...
     create a stream that produces lines as strings read from port"
    (port->stream port get-line))

  (define* (port->delimited-stream delimiters-string port #:optional (handle-delim (q trim)))
    "string port [symbol:read-delimited-handle-delim] -> stream"
    (port->stream port (l (port) (read-delimited delimiters-string port handle-delim))))

  (define (stream-any proc stream) "procedure stream -> boolean/any"
    (if (stream-null? stream) #f
      (identity-if (proc (stream-first stream)) (stream-any proc (stream-tail stream)))))

  (define (stream-first-or-null a) "stream -> any/list"
    (if (stream-null? a) (list) (stream-first a))))
