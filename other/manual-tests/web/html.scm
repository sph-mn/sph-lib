(import (sph) (sph web html) (sph one) (sph filesystem) (sph test) (sph string) (sph lang scm-format))
(define (get-data name) (open-input-string (string-replace-chars (file->string (string-append "data/html/" name)) (q ((#\newline #\return #\newline))))))
;(define (test-html-read-multipart-form-data inp) (html-read-multipart-form-data inp))
;(execute-tests (list (list (q html-read-multipart-form-data) (get-data "request-body-2") #t)))

(define request-body-5
  "-----------------------------19737173311706079401624261242\r\nContent-Disposition: form-data; name=\"content\"\r\n\r\naeu\r\n-----------------------------19737173311706079401624261242\r\nContent-Disposition: form-data; name=\"names\"\r\n\r\n[\"a\",\"b\"]\r\n-----------------------------19737173311706079401624261242\r\nContent-Disposition: form-data; name=\"type\"\r\n\r\ndocl\r\n-----------------------------19737173311706079401624261242--\r\n")

;content, names, type
;(define t (html-read-multipart-form-data (open-input-string request-body-5)))
(define t (html-read-multipart-form-data (get-data "request-body-2")))
(debug-log t)
;(debug-log (html-multipart-form-data-ref t "names"))