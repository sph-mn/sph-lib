(import (sph string) (sph)
  (sph one) (sph alist) (sph list) (srfi srfi-1) (sph hashtable) (sph lang scm-format) (sph test))

(define c scm-format-default-config)
(hashtables-set! c (q format) (q indent-string) "--")

(define (test-scm-format in exp)
  (let*
    ( (config-format
        (if (length-greater-one? in)
          (ht-create (q format) (alist->hashtable (list->alist (last in)))) #f))
      (res (scm-format (first in) 0 config-format)))
    (if (equal? res exp) exp (pass (l (res) (display (string-replace-char res #\- #\space))) res))))

(define library-form
  (q
    (library (scm format test)
      (export
        (rename this-i-a-test this-i-another-test)
        a
        b
        c)
      (import
        (srfi srfi-1)
        (sph pattern synthetical)
        (sph storage record)))))

(execute-tests-quasiquote
  (scm-format
    ;multiple leading parenthesis spacing
    (((let ((a001001001 1) (b001001001 2)) a-relatively-long-string) (max-chars-per-line 17)))
    "(let\n--(-(a001001001 1)\n----(b001001001 2))\n--a-relatively-long-string)"
    ;general nesting
    ((1 2 ((+ 3 4)))) "(1 2 ((+ 3 4)))"
    ;double parenthesis
    ((1 ((2))) (max-chars-per-line 4)) "(1\n--(\n----(2)))"
    ;start\middle exprs limit
    ((1 2 3 4 5) (max-exprs-per-line-start 2 max-exprs-per-line-middle 2 max-exprs-per-line-end 0))
    "(1 2\n--3 4\n--5)"
    ;max-line-chars
    ((111 133 22 1 222) (max-chars-per-line 4)) "(111\n--133\n--22\n--1 222)"
    (;this is a testcomment
      )
    ";this is a testcomment"
    (#;(this is a testcomment
        comment line 2
        comment line 3))
    "#;(this is a testcomment\n-comment line 2\n-comment line 3)"
    ( ( (#;(this is a testcomment
            comment line 2
            comment line 3))))
    "( ( #;(this is a testcomment\n------comment line 2\n------comment line 3)))"
    ((lambda (var-1 var-2 var-3 var-4) body-1 body-2 body-3))
    "(lambda (var-1 var-2 var-3 var-4) body-1 body-2 body-3)" ((unquote library-form))
    "(library (scm format test)\n--(export\n----(rename this-i-a-test this-i-another-test)\n----a\n----b\n----c)\n--(import\n----(srfi srfi-1)\n----(sph pattern syntactical)\n----(sph storage record)))"
    ;control chars in strings
    ((display "\n\t")) "(display \"\\n\\t\")"
    ( (lambda ()
        "a
         b
         c")
      (indent-string "  "))
    "(lambda ()\n  \"a\n  b\n    c\")"))