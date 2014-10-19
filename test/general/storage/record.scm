(import (sph) (sph test) (sph record) (sph storage record))
(define (if-true arg) (if arg #t arg))
(define layout-name "t")
(define field-spec (ql (a integer) (b string)))
(define collection-path (tmpnam))
(define collection)
(define layout)
(define elements)
(define (layout-test name index inp exp) (apply (primitive-eval name) layout inp) exp)

(define (test-srs-init)
  (let (r (srs-init collection-path)) (if r (begin (set! collection r) #t) r)))

(define (test-srs-create-layout)
  (let (r (srs-create-layout collection layout-name field-spec))
    (if r (begin (set! layout r) #t) r)))

(define (test-srs-get-layout inp exp)
  (let (r (srs-get-layout collection inp)) (and r (record-field-names (srs-layout-layout r)))))

(define (test-srs-exit) (srs-exit collection))

(define-as tests quasiquote
  ;interdependent tests
  (srs-init (srs-get-layout (unquote layout-name) #f) srs-create-layout
    (srs-get-layout (unquote layout-name) #(ide a b)) srs-exit))

;(simple-format #t "~A\n" (string-append "sqlite3 " collection-path))
(if (execute-tests tests) (delete-file collection-path))