(library (sph web http)
  (export
    http-current-date
    http-date->string
    http-header-line
    http-header-line-set-cookie
    http-header-lines
    http-parse-cookie-header
    http-parse-date->seconds
    http-read-header
    http-read-header-line
    http-read-header-value
    http-seconds->date
    http-status-line
    http-uri-query-alist->string
    http-uri-query-string->alist
    http-write-status-line
    (rename (parse-date http-parse-date))
    (rename (write-date http-write-date)))
  (import
    (guile)
    (rnrs base)
    (sph)
    (srfi srfi-19)
    (only (ice-9 regex) match:substring regexp-substitute/global)
    (only (sph list) map-slice)
    (only (sph module) import-unexported)
    (only (sph string) any->string)
    (only (sph two) read-line-crlf-trim srfi-19-date->seconds))

  ;;the html-uri-encoding procedures are copied from (sph web html) to avoid circular dependency

  (define (html-uri-decode str) "string -> string"
    (regexp-substitute/global #f "\\+|%([0-9A-Fa-f][0-9A-Fa-f])"
      str (q pre)
      (l (m)
        (if (string=? "+" (match:substring m 0)) " "
          (integer->char (string->number (match:substring m 1) 16))))
      (q post)))

  (define html-uri-encode
    (let ((safe-char? (l (char) (or (char-alphabetic? char) (char-numeric? char)))))
      (lambda (str) "string -> string"
        (list->string
          (reverse
            (string-fold
              (l (cur-char r)
                (if (safe-char? cur-char) (pair cur-char r)
                  (append! (reverse (string->list (number->string (char->integer cur-char) 16)))
                    (pair #\% r))))
              (list) str))))))

  (define* (http-uri-query-string->alist a #:optional (separator #\&))
    "string -> alist
    assumes that uri is valid"
    (map
      (l (e)
        (apply (l (key . value) (pair key (if (null? value) #t (first value))))
          (string-split e #\=)))
      (string-split a separator)))

  (define (http-uri-query-alist->string a)
    (string-join (map (l (e) (string-append (first e) "=" (tail e))) a) "&"))

  (define (cookie-config config) "list -> string"
    (string-join
      (fold
        (l (e r)
          (case (first e) ((domain) (pair (string-append "Domain=" (tail e)) r))
            ((path) (pair (string-append "Path=" (tail e)) r))
            ((expires) (pair (string-append "Expires=" (tail e)) r))
            ((max-age) (pair (string-append "Max-Age=" (number->string (tail e))) r))
            ((secure) (if (tail e) (pair "Secure" r) r))
            ((http-only) (if (tail e) (pair "HttpOnly" r) r))
            (else (throw (q no-such-cookie-attribute)))))
        (list) config)
      ";" (q prefix)))

  (import-unexported (web response) code->reason-phrase)

  (define (http-write-status-line code port) (display "HTTP/1.1" port)
    (display #\space port) (display code port)
    (display #\space port) (display (code->reason-phrase code) port) (display "\r\n" port))

  (define-syntax-rule (http-status-line code)
    (call-with-output-string (l (port) (http-write-status-line code port))))

  (define (http-parse-header-value a)
    "string -> pair
    read the value of a header-line and result in a pair"
    (map
      (l (e)
        (let (split-index (string-index e #\=))
          (if split-index
            (pair (string-trim (substring e 0 split-index) #\space)
              ;assumes the values to be \" enclosed
              (string-trim-both (substring e (+ 1 split-index)) #\"))
            (pair (string-trim e #\space) #t))))
      (string-split a #\;)))

  (define (http-parse-header-line line)
    "string -> (name (key . value)
    string -> (key values ...))"
    (let (split-index (string-index line #\:))
      (pair (substring line 0 split-index)
        (http-parse-header-value (substring line (+ 1 split-index))))))

  (define (http-read-header port)
    (let loop ((line (read-line-crlf-trim port)) (result (list)))
      (if (eof-object? line) result
        (if (string-null? line) result
          (loop (read-line-crlf-trim port) (pair (http-parse-header-line line) result))))))

  (define (http-parse-cookie-header ch)
    "string -> alist
    parse one header line for a cookie and retrieve its contents as key value pairs in an alist"
    (map
      (l (e)
        (let* ((e (string-trim-both e)) (split-index (string-index e #\=)))
          (if split-index (pair (substring e 0 split-index) (substring e (+ 1 split-index)))
            (pair e #t))))
      (string-split ch #\;)))

  (define (http-header-line name value)
    "string any -> string
    create a header line"
    (string-append name ":" (any->string value) "\r\n"))

  (define (http-header-lines . name/value) "string ... -> string"
    (apply string-append (map-slice 2 http-header-line name/value)))

  (define* (http-header-line-set-cookie name #:optional value config)
    "string false/string alist:((symbol . string/boolean)) -> string
    result in a header line for setting a cookie. config can contain key value pairs for following keys
    domain
    path
    expires
    max-age
    secure
    http-only"
    (http-header-line "set-cookie"
      (string-append name "=" (if value value "") (if config (cookie-config config)))))

  (import-unexported (web http) parse-date)
  (import-unexported (web http) write-date)
  (define (http-current-date) "-> string" (http-date->string (current-date 0)))

  (define (http-date->string a) "srfi-19-date -> string"
    (call-with-output-string (l (port) (write-date a port))))

  (define (http-parse-date->seconds a) "string -> integer:unix-time-seconds/false"
    (srfi-19-date->seconds (parse-date a)))

  (define (http-seconds->date a)
    "integer:unix-time-seconds -> string
    converts a unix timestamp to a http date string.
    epoch is 1970-01-01 00:00:00 UTC, excluding leap seconds"
    (http-date->string
      (let (t (gmtime a))
        (make-date 0 (tm:sec t)
          (tm:min t) (tm:hour t) (tm:mday t) (+ 1 (tm:mon t)) (+ 1900 (tm:year t)) 0)))))