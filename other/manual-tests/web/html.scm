(import (sph) (sph alist) (sph web html) (sph test) (sph lang scm-format))

(define (get-data name)
  (open-input-string
    (string-replace-chars (file->string (string-append "data/html/" name))
      (q ((#\newline #\return #\newline))))))

(define-as request-body-5 open-input-string
  "-----------------------------19737173311706079401624261242\r\nContent-Disposition: form-data; name=\"content\"\r\n\r\naeu\r\n-----------------------------19737173311706079401624261242\r\nContent-Disposition: form-data; name=\"names\"\r\n\r\n[\"a\",\"b\"]\r\n-----------------------------19737173311706079401624261242\r\nContent-Disposition: form-data; name=\"type\"\r\n\r\ndocl\r\n-----------------------------19737173311706079401624261242--\r\n")

(define request-body-2 (get-data "request-body-2"))

#;(
(define t (html-read-multipart-form-data request-body-5))
(debug-log t)
(define t (html-read-multipart-form-data request-body-2))
(debug-log t)
(debug-log (html-multipart-form-data-ref t "submit-name"))
)

(define t
  (html-fold-multipart-form-data
    (l (header fold-lines result)
      (if (equal? "content" (alist-ref (tail (first header)) "name"))
        (fold-lines (l (line result next) (next (pair line result))) (list)
          (l (result-fold-lines result next-part)
            (next-part
              (pair
                (pair header
                  (if (null? result-fold-lines) result-fold-lines
                    (string-join
                      (reverse
                        (pair (string-drop-right (first result-fold-lines) 2)
                          (tail result-fold-lines)))
                      "")))
                result))))
        result))
    (l (header port result) (pair (pair header (html-read-multipart-form-data port)) result)) (list)
    request-body-5))

(debug-log (q result) t)
