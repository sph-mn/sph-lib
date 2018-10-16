(library (sph stream)
  (export
    define-stream
    file->stream
    list->stream
    port->buffered-octet-stream
    port->delimited-stream
    port->line-stream
    port->stream
    stream
    stream->list
    stream-any
    stream-append
    stream-car
    stream-cdr
    stream-concat
    stream-cons
    stream-constant
    stream-deduplicate
    stream-drop
    stream-drop-while
    stream-each
    stream-filter
    stream-first
    stream-first-or-null
    stream-fold
    stream-fold-right-multiple
    stream-for-each
    stream-from
    stream-iterate
    stream-lambda
    stream-length
    stream-let
    stream-map
    stream-match
    stream-null
    stream-null?
    stream-of
    stream-page
    stream-pair?
    stream-range
    stream-ref
    stream-reverse
    stream-scan
    stream-tail
    stream-take
    stream-take-while
    stream-unfold
    stream-unfolds
    stream-zip
    stream?)
  (import
    (ice-9 rdelim)
    (rnrs io ports)
    (sph)
    (sph hashtable)
    (sph module)
    (except (srfi srfi-41) port->stream)
    (only (guile)
      eof-object?
      open
      O_RDONLY
      current-module
      resolve-interface)
    (only (sph other) identity-if))

  (define sph-stream-description
    "srfi-41 stream helpers.
     re-exports (srfi srfi-41)")

  (define stream-each stream-for-each)
  (define stream-first stream-car)
  (define stream-tail stream-cdr)

  (define (stream-page a entry-count number lookahead c)
    "stream integer integer integer procedure:{stream boolean:last-page? -> any} -> any
     pass a stream of \"entry-count\" elements at an offset of (* number entry-count),
     eventually including \"lookahead\" number of elements if they are the last elements,
     and a boolean indicating if it is the last page to continuation procedure \"c\""
    ; stream-drop/-take, different from list drop/take which raise an exception,
    ; return a null stream if not enough elements are available
    (let*
      ( (offset (* (- number 1) entry-count)) (rest (if (< 1 number) (stream-drop offset a) a))
        (lookahead (stream-take lookahead (stream-drop entry-count rest)))
        (page (stream-take entry-count rest)))
      (if (= lookahead (stream-length lookahead)) (c page #f) (c (stream-append page lookahead) #t))))

  (define (stream-fold-right-multiple proc a . r) "procedure stream any:state ..."
    (if (stream-null? a) r
      (apply proc (stream-first a) (apply stream-fold-right-multiple proc (stream-tail a) r))))

  (define (stream-any proc stream) "procedure stream -> boolean/any"
    (if (stream-null? stream) #f
      (identity-if (proc (stream-first stream)) (stream-any proc (stream-tail stream)))))

  (define (stream-first-or-null a) "stream -> any/list"
    (if (stream-null? a) (list) (stream-first a)))

  (define-syntax-rule (primitive-set-create set entries)
    (let (r set) (each (l (a) (ht-set! r a #t)) entries) r))

  (define (set-create-empty initial-size) (ht-make ht-hash-equal equal? initial-size))
  (define (set-create . entries) (primitive-set-create (set-create-empty (length entries)) entries))
  (define (set-contains? a value) (ht-contains? a value))
  (define (set-add! a value) (ht-set! a value #t))

  (define* (stream-deduplicate a #:optional (set-create set-create))
    "stream [procedure] ->
     returns each distinct value only once.
     set-create is a set-creation procedure from (sph set)"
    (let (set (set-create))
      (stream-filter (l (a) (if (set-contains? set a) #f (begin (set-add! set a) #t))) a)))

  ;-- io

  (define (port->buffered-octet-stream port buffer-size)
    "port integer -> stream
     creates stream that produces bytevectors of length buffer-size filled with data/octets read from port"
    (let (read (l (port) (get-bytevector-n port buffer-size)))
      (stream-let next ((e (read port)))
        (if (eof-object? e) stream-null (stream-cons e (next (read port)))))))

  (define (port->line-stream port)
    "port -> stream -> string ...
     create a stream that produces lines as strings read from port"
    (port->stream port get-line))

  (define* (port->delimited-stream delimiters-string port #:optional (handle-delim (q trim)))
    "string port [symbol:read-delimited-handle-delim] -> stream"
    (port->stream port (l (port) (read-delimited delimiters-string port handle-delim))))

  (define* (port->stream port read #:optional close)
    "any procedure:{any -> any/eof-object} [procedure] -> stream
     create a stream that calls (read port) until the result is the end-of-file object and then close port.
     works with any type for port as long as read eventually returns an eof-object.
     port->stream from srfi-41 does not support a custom reader"
    (stream-let loop ((b (read port)))
      (if (eof-object? b) (begin (and close (close port)) stream-null)
        (stream-cons b (loop (read port))))))

  (define* (file->stream path read #:optional close) (port->stream (open path O_RDONLY) read close)))
