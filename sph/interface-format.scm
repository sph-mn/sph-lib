(library (sph interface-format)
  (export
    interface-format-create-lines
    interface-format-create-message
    interface-format-object->type
    interface-format-objects->port
    interface-format-writers)
  (import
    (sph common))

  ;for programs that textual input/output in multiple formats.
  ;uses a generic transmission format for generic program output objects.
  ;example object types: user messages, errors, data
  ;example use case: command-line interfaces that offer both serialised and natural language output.
  (define indent " ")

  (define (interface-format-text-write-error a port)
    (display-line
      (string-append "error: " (any->string (error-name a))
        (let (data (error-data a))
          (if data
            (string-append "\n" indent
              (if (list? data) (string-join (map any->string data) " ") (any->string data)))
            "")))
      port))

  (define (interface-format-create-message a) "string -> list" (list (q message) a))
  (define (interface-format-create-lines a) "string -> list" (pair (q lines) a))
  (define (interface-format-scm-write-error a port) (write (pair (q error) (error->list a)) port))
  (define interface-format-text-write-message display-line)
  (define interface-format-scm-write-message write)
  (define (interface-format-scm-write-lines a port) (write (tail a)))
  (define (interface-format-text-write-lines a port) (map display-line (tail a)))

  (define-as interface-format-writers symbol-hashtable
    text
    (symbol-hashtable error interface-format-text-write-error
      message interface-format-text-write-message
      lines interface-format-text-write-lines data display-line)
    scm
    (symbol-hashtable error interface-format-scm-write-error
      message interface-format-scm-write-message lines interface-format-scm-write-lines data write))

  (define (interface-format-object->type a)
    (if (error? a) (q error) (if (list? a) (if (null? a) (q data) (first a)) (q data))))

  (define (interface-format-objects->port a port format format-writers)
    "(any ...) port hashtable ->
    displays objects corresponding to format.
    format can be a machine-readable serialisation format, or a plain text human language format for example.
    example use-case is a command-line interface that supports multiple message output formats"
    (let (format-writers (hashtable-ref format-writers format))
      (each
        (l (e)
          (let (writer (hashtable-ref format-writers (interface-format-object->type e)))
            (if writer (writer e port))))
        a))))