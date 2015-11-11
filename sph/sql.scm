; (sph sql) - create sql strings from scheme data-structures
; written for the guile scheme interpreter
; Copyright (C) 2010-2015 sph <sph@posteo.eu>
; This program is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sph sql)
  (export
    sql-columns
    sql-columns-list
    sql-create-index
    sql-create-table
    sql-delete
    sql-delete-table
    sql-ele-data->filter
    sql-filter->ele-data
    sql-insert
    sql-insert-values
    sql-select
    sql-string
    sql-update
    sql-value
    sql-where-expr)
  (import
    (ice-9 match)
    (rnrs base)
    (sph)
    (only (guile)
      identity
      object->string
      reverse!
      string-join
      string-fold
      simple-format)
    (only (rnrs io simple) display)
    (only (sph alist) alist-quoted-bind)
    (only (sph list)
      any->list
      contains?
      flat?
      length-eq-one?)
    (only (sph one) in-between?)
    (only (sph string) parenthesise string-replace-chars)
    (only (sph tree)
      flatten
      tree-map-lists-with-level
      prefix-tree-map-with-continue-with-level))

  (define (sql-ele-data->filter expr) "((column-name . value) ...) -> ((column-name value) ...)"
    (if (list? expr) (map (l (ele) (if (list? ele) ele (list (first ele) (tail ele)))) expr)
      (if (pair? expr) (list (first expr) (tail expr)) expr)))

  (define (sql-filter->ele-data arg) "list -> (pair ...)"
    (if (string? arg) arg
      (map
        (l (ele)
          (if (list? ele)
            (if (> (length ele) 2) (pair (list-ref ele 1) (list-ref ele 2))
              (pair (first ele) (list-ref ele 1)))
            ele))
        arg)))

  (define (sql-string arg)
    "string -> string
    escapes string values for sql"
    (string-append "'" (string-replace-chars arg (q ((#\' #\' #\') (#\\ #\\ #\\)))) "'"))

  (define (sql-value arg) "convert types, escape and quote strings"
    (if (number? arg) (number->string arg)
      (if (string? arg) (sql-string arg)
        (if (eq? (q null) arg) "NULL"
          (if (eq? (q isnull) arg) "ISNULL"
            (if (symbol? arg) (sql-string (symbol->string arg))
              (sql-string (object->string arg display))))))))

  (define (sql-column arg) "symbol/string -> string" (if (symbol? arg) (symbol->string arg) arg))
  (define (join-column-value c o v) (string-append (sql-column c) o (sql-value v)))
  (define row-expr-prefixes (q (any every not)))

  (define (operator->sql-operator arg) "symbol/string -> string"
    (case arg ((equal =) "=")
      ((greater >) ">") ((greater-or-equal >=) ">=")
      ((less <) "<") ((less-or-equal <=) "<=")
      ((like) " like ")
      ;not is already used for row-expressions, so use 'isnot
      ((isnot) " is not ") ((space) " ")
      (else
        (if (string? arg) arg
          (if (symbol? arg) (string-append " " (symbol->string arg) " ")
            (throw (q wrong-type-for-argument)))))))

  (define (sql-where-column-expr-produce-proc column-name state->combinator)
    ;this procedure may contain too much at once, split it if you can
    (let*
      ( (join-values
          (l (c values operator sql-operator)
            (first
              (tree-map-lists-with-level
                (l (values value-state)
                  (let
                    ( (value-combinator (state->combinator value-state))
                      (use-sql-in?
                        (and (odd? value-state)
                          (or (eq? (q equal) operator) (eq? (q isnot) operator)) (flat? values))))
                    (let
                      (combine-with-values
                        (if (length-eq-one? values)
                          ;optimisation
                          ( (l (v) (l (c) (join-column-value (column-name c) sql-operator v)))
                            (first values))
                          (if use-sql-in?
                            (l (c)
                              (string-append (column-name c)
                                (if (eq? (q isnot) operator) " not" "") " in("
                                (string-join (map sql-value values) ",") ")"))
                            (l (c)
                              (parenthesise
                                (string-join
                                  (map
                                    (l (v)
                                      (if (list? v) (first v) (join-column-value c sql-operator v)))
                                    values)
                                  value-combinator))))))
                      ((if (eqv? 1 value-state) identity list) (combine-with-values c)))))
                (list (any->list values))))))
        (join-columns-values
          (l (columns values operator sql-operator)
            (first
              (tree-map-lists-with-level
                (l (columns column-state)
                  (let (column-combinator (state->combinator column-state))
                    ( (if (eq? 1 column-state) identity list)
                      (if (length-eq-one? columns)
                        ;optimisation
                        (if (list? (first columns)) (first (first columns))
                          (join-values (first columns) values operator sql-operator))
                        (parenthesise
                          (string-join
                            (map
                              (l (c)
                                (if (list? c) (first c)
                                  (join-values c values operator sql-operator)))
                              columns)
                            column-combinator))))))
                (list (any->list columns)))))))
      (lambda (operator columns values)
        (join-columns-values columns values operator (operator->sql-operator operator)))))

  (define primitive-sql-where-expr
    (let*
      ( (row-expr-binary
          (l (prefix suffix level)
            ( (if (or (eqv? 1 level) (length-eq-one? suffix)) identity parenthesise)
              (string-join suffix (if (eq? (q any) prefix) " or " " and ")))))
        (row-expr-unary (l (prefix suffix level) (string-append "not " (first suffix))))
        (state->combinator (l (state) (if (even? state) " and " " or ")))
        (column-name (l (c) (if (string? c) c (symbol->string c))))
        (column-expr-produce (sql-where-column-expr-produce-proc column-name state->combinator))
        (column-expr
          (l (prefix suffix)
            (apply column-expr-produce
              ;add implicit operator to column-expression
              (if (= (length suffix) 2) (list prefix (first suffix) (first (tail suffix)))
                (list (q equal) prefix (first suffix))))))
        (handle-row-expr
          (l (prefix suffix level)
            ( (case prefix ((any every) row-expr-binary)
                ((not) row-expr-unary) (else (pair (q invalid-row-expr-prefix) prefix)))
              prefix
              (if (pair? (first suffix))
                (map (l (ele) (if (pair? ele) (join-column-value (first ele) "=" (tail ele)) ele))
                  suffix)
                suffix)
              level))))
      (lambda (expr) "sql-where-expr -> sql-string"
        (if (sql-where-filter? expr)
          (prefix-tree-map-with-continue-with-level handle-row-expr
            (l (lis proc continue level)
              (if (or (null? lis) (contains? row-expr-prefixes (first lis))) (continue lis level)
                (column-expr (first lis) (tail lis))))
            expr)
          (throw (q not-a-sql-where-filter))))))

  (define (sql-row-data? expr)
    (if
      (or (string? expr)
        (every (l (ele) (or (and (pair? ele) (not (list? ele))) (string? ele))) expr))
      #t #f))

  (define (sql-where-filter-column? expr)
    (and (list? expr) (in-between? (length expr) 1 5)
      (not (contains? row-expr-prefixes (first expr)))))

  (define (sql-where-filter? expr)
    (match expr
      ( ( (or (quote any) (quote every) (quote not))
          (or (? sql-where-filter?) (? sql-where-filter-column?)) ...)
        #t)
      (_ #f)))

  (define (sql-where-expr expr)
    "for creating a \"where\" part sql separately.
    \"not\" in row-expressions will always be in front of the column-condition in the resulting query. sqlite for example does not understand
    \"not c=NULL\" as one might expect. use \"ISNULL\" instead of \"NULL\" in this case."
    (if (list? expr)
      (if (null? expr) expr
        (primitive-sql-where-expr (if (pair? (first expr)) (pair (q every) expr) expr)))
      (if (pair? expr) (join-column-value (first expr) "=" (tail expr)) expr)))

  (define sql-insert-values
    (let (handle-column (l (c) (if (symbol? (first c)) (symbol->string (first c)) (first c))))
      (lambda (arg)
        "alist -> string
        convert to a sql values specification for the insert statement
        ((a . 1) (b . 2)) -> (a,b)values(1,2)"
        (if (pair? arg)
          (if (null? arg) #f
            (string-append "("
              (if (list? arg) (string-join (map handle-column arg) ",")
                (sql-value (handle-column arg)))
              ")values("
              (if (list? arg) (string-join (map (l (ele) (sql-value (tail ele))) arg) ",")
                (sql-value (tail arg)))
              ")"))
          #f))))

  (define (sql-where where)
    (if (null? where) ""
      (string-append " where "
        (if (pair? where) (sql-where-expr where) (if (string? where) where #f)))))

  (define (sql-config config)
    "constructing the additional options parts of a sql statement, usually some part
    at the beginning of the statement or one at the end"
    ;order - string/(string ...)
    ;limit - string/(offset limit)
    (alist-quoted-bind config (limit add-1 add-2 order)
      ;build sql parts
      (values
        ;beginning part
        (if add-1 (string-append add-1 " ") "")
        ;end part
        (string-append
          (if limit
            (cond ((number? limit) (string-append " limit " (number->string limit)))
              ( (and (pair? limit))
                (string-append " limit " (number->string (first limit))
                  "," (if (list? limit) (number->string (list-ref limit 1)) (tail limit))))
              (else limit))
            "")
          (if order (string-append " order by " (if (list? order) (string-join order " ") order))
            "")
          (if add-2 (string-append " " add-2) "")))))

  (define (sql-columns-list columns)
    (flatten
      (map
        (l (column)
          (if (pair? column)
            (if (list? column)
              (let ((table-name (first column)))
                (map (l (column-name) (string-append table-name "." column-name)) (tail column)))
              (string-append (first column) "." (tail column)))
            column))
        columns)))

  (define (sql-columns columns)
    "pair/list/string/boolean -> string
    construct the \"column\" part of a sql expression"
    (if (pair? columns)
      (if (list? columns) (if (null? columns) #f (string-join (sql-columns-list columns) ","))
        (string-append (first columns) "." (tail columns)))
      (if (string? columns) columns (if (and (boolean? columns) columns) "*" #f))))

  (define* (sql-select tables #:optional (columns #t) where config)
    "string/(string ...) [string/(string ...):columns string/list/pair string/list] -> string"
    (let-values
      ( ( (columns) (sql-columns columns))
        ( (tables)
          (if (and (list? tables) (not (null? tables))) (string-join tables ",")
            (if (string? tables) tables #f)))
        ((where) (if where (sql-where where) "")) ((config-1 config-2) (sql-config config)))
      (if (and columns tables where config-1 config-2)
        (string-append "select " config-1 columns " from " tables where config-2) #f)))

  (define* (sql-update table-name set #:optional where config)
    "(sql-update \"table-name\" ((a . 1) (b . 2))) -> \"update table-name set a=1 and b=2\""
    (let-values
      ( ( (set)
          (if (list? set)
            (string-join
              (map (l (ele) (string-append (sql-column (first ele)) "=" (sql-value (tail ele))))
                set)
              ",")
            (if (pair? set) (string-append (sql-column (first set)) "=" (sql-value (tail set)))
              (if (string? set) set #f))))
        ((where) (if where (sql-where where) "")) ((config-1 config-2) (sql-config config)))
      (if (and table-name where config-1 config-2)
        (string-append "update " config-1 table-name " set " set where config-2) #f)))

  (define* (sql-insert table-name new-data #:optional config)
    (let-values
      ( ( (config-1 config-2) (sql-config config))
        ( (new-data)
          (cond ((string? new-data) new-data) ((and (boolean? new-data)) " default values")
            (else (sql-insert-values new-data)))))
      (if (and table-name (sql-row-data? new-data) config-1 config-2)
        (string-append "insert" config-1 " into " table-name new-data config-2) #f)))

  (define* (sql-delete table-name #:optional where config)
    ;(simple-format #t "~S\n" where)
    (let-values (((config-1 config-2) (sql-config config)))
      (string-append "delete from " table-name (if where (sql-where where) "") config-2)))

  (define* (sql-create-table table-name columns #:optional (replace #t) add)
    "columns (name type options ...) OR string"
    (string-append "create table " (if replace "" "if not exists ")
      table-name "("
      (if (list? columns)
        (string-join (map (l (column) (if (list? column) (string-join column " ") column)) columns)
          ",")
        columns)
      (if add (string-append "," add) "") ")"))

  (define (sql-create-index name table-name . columns)
    (string-append "create index " name " on " table-name " (" (string-join columns ",") ")"))

  (define (sql-delete-table table-name) (string-append "drop table " table-name)))