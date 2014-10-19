;using (srfi srfi-41) streams

(library (sph stream)
  (export
    port->buffered-octet-stream
    port->line-stream
    stream-any
    stream-each
    stream-first
    stream-first-or-null
    stream-fold-right-multiple
    stream-tail)
  (import
    (rnrs base)
    (rnrs io ports)
    (sph)
    (srfi srfi-41)
    (only (guile) eof-object? close)
    (only (sph conditional) identity-if))

  (define (stream-fold-right-multiple proc arg . prev)
    (if (stream-null? arg) prev
      (apply proc (stream-first arg) (apply stream-fold-right-multiple proc (stream-tail arg) prev))))

  (define (port->buffered-octet-stream port buffer-size)
    (let (read (l (port) (get-bytevector-n port buffer-size)))
      (stream-let next ((e (read port)))
        (if (eof-object? e) stream-null (stream-cons e (next (read port)))))))

  (define (port->stream port reader)
    "port procedure:{port -> any/eof-object}
    the port->stream of srfi-41 does not support a custom reader.
    reader is supposed to result in eof-object if no more data is available"
    (stream-let next ((e (reader port)))
      (if (eof-object? e) stream-null (stream-cons e (next (reader port))))))

  (define (port->line-stream port) (port->stream port get-line))

  (define (stream-any proc stream)
    (if (stream-null? stream) #f
      (identity-if (proc (stream-first stream)) (stream-any proc (stream-tail stream)))))

  (define (stream-first-or-null arg) (if (stream-null? arg) (list) (stream-first arg)))
  (define stream-each stream-for-each)
  (define stream-first stream-car)
  (define stream-tail stream-cdr))