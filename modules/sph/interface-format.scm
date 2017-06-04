(library (sph interface-format)
  (export
    interface-format-create-data
    interface-format-create-lines
    interface-format-create-message
    interface-format-object->type
    interface-format-objects->port
    interface-format-writers
    sph-interface-format-description)
  (import
    (sph common)
    (sph error))

  (define sph-interface-format-description
    "for programs that offer textual input/output in multiple formats. experimental
    uses an intermediate data-structure.
    example use case: command-line interfaces that offer both serialised and natural language output
    example objects: user-messages, errors, data")

  (define indent " ")
  (define (any->string-exclude-unspecified a) (if (unspecified? a) a (any->string a)))

  (define (interface-format-text-write-error a port)
    (display-line
      (string-append "error: " (any->string (error-id a))
        (let (data (error-data a))
          (if data
            (string-append "\n" indent
              (if (list? data) (string-join (map any->string data) " ") (any->string data)))
            "")))
      port))

  (define (interface-format-create-message a) "string -> list" (list (q message) a))
  (define (interface-format-create-lines a) "string -> list" (pair (q lines) a))
  (define (interface-format-create-data a) "string -> list" (pair (q data) a))
  (define (interface-format-scm-write-error a port) (write (pair (q error) (error->list a)) port))
  (define interface-format-text-write-message display-line)
  (define interface-format-scm-write-message write)
  (define (interface-format-scm-write-lines a port) (write (tail a)))
  (define (interface-format-text-write-lines a port) (map display-line (tail a)))
  (define (interface-format-scm-write-data a port) (if (not (unspecified? a)) (write a)))

  (define (interface-format-text-write-data a port)
    (if (not (unspecified? a))
      (begin (display a) (if (not (and (string? a) (string-suffix? "\n" a))) (newline)))))

  (define-as interface-format-writers symbol-hashtable
    text
    (symbol-hashtable error interface-format-text-write-error
      message interface-format-text-write-message
      lines interface-format-text-write-lines data interface-format-text-write-data)
    scm
    (symbol-hashtable error interface-format-scm-write-error
      message interface-format-scm-write-message
      lines interface-format-scm-write-lines data interface-format-scm-write-data))

  (define object-types (q (error message lines data)))

  (define (interface-format-object->type a) "any -> symbol"
    (if (error? a) (q error)
      (if (list? a)
        (if (null? a) (q data)
          (let (prefix (first a))
            (if (and (symbol? prefix) (containsv? object-types prefix)) prefix (q data))))
        (q data))))

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
