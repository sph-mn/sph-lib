(library (sph replacement-table)
  (export
    hashtable-associate-words!
    replacement-table-from-file
    replacement-table-from-port
    replacement-table-merge-from-file!
    replacement-table-merge-from-port!)
  (import
    (rnrs base)
    (sph)
    (sph hashtable)
    (only (guile) string-split call-with-input-file)
    (only (rnrs hashtables) hashtable-set!)
    (only (sph read-write) port-lines-each))

  ;uses a hash table to apply and store loaded replacements

  (define (hashtable-associate-words! hashtable string)
    "hashtable string ->
    creates a hashtable entry from a string in format \"key value ...\".
    create hashtable entry from space separated words in string"
    (let (string (string-split string #\space))
      (hashtable-set! hashtable (first string) (tail string))))

  (define (replacement-table-merge-from-port! port ht)
    "port hashtable -> hashtable
    see replacement-table-from-port"
    (port-lines-each (l (line) (hashtable-associate-words! ht line)) port))

  (define (replacement-table-from-port port)
    "port [hashtable] -> hashtable
    read from port to create a hashtable from a string with lines in the format \"key value ...\"
    example use case: configuration files for string pattern replacements"
    (let (r (string-hashtable))
      (port-lines-each (l (line) (hashtable-associate-words! r line)) port) r))

  (define (replacement-table-from-file path)
    (call-with-input-file path (l (a) (replacement-table-from-port a))))

  (define (replacement-table-merge-from-file! path ht)
    (call-with-input-file path (l (a) (replacement-table-merge-from-port! a ht)))))
