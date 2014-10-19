(library (sph ice-9-stream)
  (export
    make-empty-stream
    port->buffered-stream
    stream-any
    stream-each
    stream-first
    stream-tail
    stream-fold-multiple
    stream-fold-right-multiple)
  (import
    (rnrs base)
    (rnrs io ports)
    (only (rnrs io simple) read)
    (ice-9 streams)
    (sph)
    (only (sph conditional) identity-if))

  (define stream-each stream-for-each)
  (define stream-first stream-car)
  (define stream-tail stream-cdr)

  (define (make-empty-stream) (make-stream (l s #f) #f))

  (define (port->buffered-stream port buffer-size)
    (port->stream port (l (port) (get-bytevector-n port buffer-size))))

  (define (stream-any proc stream)
    (if (stream-null? stream) #f
      (identity-if (proc (stream-first stream))
        (stream-any proc (stream-tail stream)))))

  (define (stream-fold-multiple proc arg . prev) "{any ... -> list} list any ... -> list
    {previous-result-values ... -> list}
    apply proc to the elements of arg with list elements from a previous call or values of init as arguments."
    (if (stream-null? arg) prev
      (apply stream-fold-multiple proc
        (stream-tail arg) (apply proc (stream-first arg) prev))))

  (define (stream-fold-right-multiple proc arg . prev)
    (if (stream-null? arg) prev
      (apply proc (stream-first arg)
        (apply stream-fold-right-multiple proc (stream-tail arg) prev)))))
