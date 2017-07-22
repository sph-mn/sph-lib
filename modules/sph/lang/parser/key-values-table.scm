(library (sph lang parser key-values-table)
  (export
    ht-associate-words!
    key-values-table-from-file
    key-values-table-from-port
    key-values-table-merge-from-file!
    key-values-table-merge-from-port!
    sph-lang-parser-key-values-table-description)
  (import
    (rnrs io simple)
    (sph)
    (sph hashtable)
    (only (guile) string-split)
    (only (sph io) port-lines-each))

  (define sph-lang-parser-key-values-table-description
    "replacement tables like (pattern replacement ...) from strings read from files or ports.
     the string format consists of lines of the format \"pattern replacement ...\"")

  (define (ht-associate-words! hashtable string)
    "hashtable string ->
     creates a hashtable entry from a string in format \"key value ...\".
     create hashtable entry from space separated words in string"
    (let (string (string-split string #\space)) (ht-set! hashtable (first string) (tail string))))

  (define (key-values-table-merge-from-port! port ht)
    "port hashtable -> hashtable
     see key-values-table-from-port"
    (port-lines-each (l (line) (ht-associate-words! ht line)) port))

  (define (key-values-table-from-port port)
    "port [hashtable] -> hashtable
     read from port to create a hashtable from a string with lines in the format \"key value ...\"
     example use case: configuration files for string pattern replacements"
    (let (r (ht-create-string)) (port-lines-each (l (line) (ht-associate-words! r line)) port) r))

  (define (key-values-table-from-file path)
    (call-with-input-file path (l (a) (key-values-table-from-port a))))

  (define (key-values-table-merge-from-file! path ht)
    (call-with-input-file path (l (a) (key-values-table-merge-from-port! a ht)))))
