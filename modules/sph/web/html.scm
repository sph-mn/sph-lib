(define-module (sph web html))

(use-modules (ice-9 pretty-print) (ice-9 rdelim)
  (rnrs bytevectors) (sph)
  ((ice-9 regex) #:select (match:substring regexp-substitute/global))
  ((sph alist) #:select (alist-ref alist-keys-map)) ((sph string) #:select (string-equal?))
  ((sph web http) #:select (http-read-header http-read-header-value)) (srfi srfi-1))

(export sph-web-html-description html-fold-multipart-form-data
  html-multipart-form-data-ref html-multipart-form-data?
  html-parse-urlencoded-form-data html-read-multipart-form-data html-uri-decode html-uri-encode)

(define sph-web-html-description
  "html related methods including an advanced html multipart form data parser")

(define (read-line-crlf-trim port)
  "try to read a line that is known to be cr-lf terminated and remove the cr-lf or return eof-object"
  (let (line+delim (%read-line port))
    (let ((line (first line+delim)) (delim (tail line+delim)))
      (if (and (string? line) (char? delim)) (substring line 0 (- (string-length line) 1)) line))))

(define (read-line-crlf port)
  "try to read a line that is known to be cr-lf terminated or return eof-object"
  (let (line+delim (%read-line port))
    (let ((line (first line+delim)) (delim (tail line+delim)))
      (if (and (string? line) (char? delim)) (string-append line (string delim)) line))))

(define (html-uri-decode a)
  (define (decode-char b)
    (if (char=? b #\+) #\space
      (integer->char (string->number (substring a (+ 1 b) (+ 3 b)) 16))))
  (define (decode a pos)
    (if (>= pos (string-length a)) ""
      (string-append
        (if (char=? (string-ref a pos) #\%) (string (decode-char pos))
          (string (string-ref a pos)))
        (decode a (+ pos (if (char=? (string-ref a pos) #\%) 3 1))))))
  (decode a 0))

(define html-uri-encode
  (let ((safe-char? (l (char) (or (char-alphabetic? char) (char-numeric? char)))))
    (lambda (r) "string -> string"
      (list->string
        (reverse
          (string-fold
            (l (e r)
              (if (safe-char? e) (pair e r)
                (append! (reverse (string->list (number->string (char->integer e) 16)))
                  (pair #\% r))))
            (list) r))))))

(define (html-parse-urlencoded-form-data request-body)
  "string -> alist
   parse an application/x-www-form-urlencoded string and result in an alist"
  (if (string-null? request-body) (list)
    (map
      (l (e)
        (let (split-index (string-index e #\=))
          (pair (substring e 0 split-index) (substring e (+ 1 split-index)))))
      (string-split (html-uri-decode request-body) #\&))))

(define* (html-multipart-form-data? headers #:optional (header-key "content_type"))
  "(string ...) [string] -> boolean"
  (let (content-type (assoc-ref headers header-key))
    (and content-type (string-prefix? "multipart/form-data" content-type))))

(define (get-boundary-from-headers headers) "(string ...) -> string/boolean-false"
  (let (content-type (assoc-ref headers "content_type"))
    (if content-type (assoc-ref (http-read-header-value content-type) "boundary") content-type)))

(define (read-boundary port) "port -> string/eof-object"
  (let (boundary (read-line-crlf-trim port))
    (if (eof-object? boundary) boundary
      (if (string-null? boundary) (raise (pair (q boundary-not-found) boundary)) boundary))))

(define-syntax-rule (header-multipart-mixed? a)
  (let (content-type (or (alist-ref a "content-type") (alist-ref a "Content-Type")))
    (and content-type (alist-ref content-type "multipart/mixed"))))

(define* (html-fold-multipart-form-data proc-part proc-multipart result port #:optional boundary)
  "::
   procedure:{alist:header procedure:fold-lines:{string:line any:result procedure:next:{any:result ->} ->} result -> any}
   procedure:{header port result ->} any port [string]
   ->
   any
   ----
   proc-multipart is only called for multipart/mixed.
   a functional parser for multipart-form-data that allows to stop after any cr-lf-terminated line or part and reads content stream-like via a reader procedure
   and supports nested multipart/mixed data.
   see also html-read-multipart-form-data"
  "see the code of \"html-read-multipart-form-data\" for a usage example"
  (let* ((boundary (if boundary boundary (read-boundary port))) (header (http-read-header port)))
    "todo: also match \"multipart/alternative\", \"multipart/digest\" and \"multipart/parallel\" here,\n     it is othewrise the same as \"multipart/mixed\""
    (if (header-multipart-mixed? header) (proc-multipart header port result)
      (letrec
        ( (fold-lines
            (l (proc-line result-fold-lines after-fold-lines)
              (let (line (read-line-crlf port))
                (if (eof-object? line) (after-fold-lines result-fold-lines result identity)
                  (if (string-contains line boundary)
                    (if (string-contains line (string-append boundary "--"))
                      (after-fold-lines result-fold-lines result identity)
                      (after-fold-lines result-fold-lines result
                        (l (result)
                          (html-fold-multipart-form-data proc-part proc-multipart
                            result port boundary))))
                    (proc-line line result-fold-lines
                      (l (result) (fold-lines proc-line result after-fold-lines)))))))))
        (proc-part header fold-lines result)))))

(define* (html-read-multipart-form-data port #:optional normalise-header-keys?)
  "port [procedure:{string -> string/any}] -> list
   parses all multipart form data available on port into a list.
   for stream-like and conditional parsing see html-fold-multipart-form-data"
  (reverse
    (html-fold-multipart-form-data
      (l (header fold-lines result)
        (fold-lines (l (line result next) (next (pair line result))) (list)
          (l (result-fold-lines result next-part)
            (next-part
              (pair
                (pair (if normalise-header-keys? (alist-keys-map string-downcase header) header)
                  (if (null? result-fold-lines) result-fold-lines
                    (string-join
                      (reverse
                        (pair (string-drop-right (first result-fold-lines) 2)
                          (tail result-fold-lines)))
                      "")))
                result)))))
      (l (header port result)
        (pair
          (pair (if normalise-header-keys? (alist-keys-map string-downcase header) header)
            (html-read-multipart-form-data port))
          result))
      (list) port)))

(define (html-multipart-form-data-ref a name)
  "list string -> pair
   for parsed multipart form data like html-read-multipart-form-data creates.
   retrieves (alist:header . string:body) pairs by content-disposition name"
  (find
    (l (e)
      (string-equal? name
        (assoc-ref
          (or (assoc-ref (first e) "content-disposition")
            (assoc-ref (first e) "Content-Disposition"))
          "name")))
    a))
