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
    sph-sql-description
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
    (only (sph alist) alist-bind)
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

  (define sph-sql-description "create sql-statements from scheme data")

  (define (sql-ele-data->filter expr) "((column-name . value) ...) -> ((column-name value) ...)"
    (if (list? expr) (map (l (e) (if (list? e) e (list (first e) (tail e)))) expr)
      (if (pair? expr) (list (first expr) (tail expr)) expr)))

  (define (sql-filter->ele-data a) "string/list -> (pair ...)"
    (if (string? a) a
      (map
        (l (e)
          (if (list? e)
            (if (> (length e) 2) (pair (list-ref e 1) (list-ref e 2))
              (pair (first e) (list-ref e 1)))
            e))
        a)))

  (define (sql-string a)
    "string -> string
    escapes string values for sql"
    (string-append "'" (string-replace-chars a (q ((#\' #\' #\') (#\\ #\\ #\\)))) "'"))

  (define (sql-value a)
    "any -> string
    convert types, escape and quote strings"
    (if (number? a) (number->string a)
      (if (string? a) (sql-string a)
        (if (eq? (q null) a) "NULL"
          (if (eq? (q isnull) a) "ISNULL"
            (if (symbol? a) (sql-string (symbol->string a)) (sql-string (object->string a display))))))))

  (define (sql-column a) "symbol/string -> string" (if (symbol? a) (symbol->string a) a))

  (define (join-column-value c o v) "sql-column string sql-column -> string"
    (string-append (sql-column c) o (sql-value v)))

  (define row-expr-prefixes (q (any every not)))

  (define (operator->sql-operator a) "symbol/string -> string"
    (case a ((equal =) "=")
      ((greater >) ">") ((greater-or-equal >=) ">=")
      ((less <) "<") ((less-or-equal <=) "<=")
      ((like) " like ")
      ;not is already used for row-expressions, so use "isnot"
      ((isnot) " is not ") ((space) " ")
      (else
        (if (string? a) a
          (if (symbol? a) (string-append " " (symbol->string a) " ")
            (throw (q wrong-type-for-argument)))))))

  (define (sql-where-column-expr-produce-proc column-name state->combinator)
    ;this procedure may do too much at once, split it if you can
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
                (map (l (e) (if (pair? e) (join-column-value (first e) "=" (tail e)) e)) suffix)
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

  (define (sql-row-data? expr) "any -> boolean"
    (if (or (string? expr) (every (l (e) (or (and (pair? e) (not (list? e))) (string? e))) expr))
      #t #f))

  (define (sql-where-filter-column? expr) "any -> boolean"
    (and (list? expr) (in-between? (length expr) 1 5)
      (not (contains? row-expr-prefixes (first expr)))))

  (define (sql-where-filter? expr) "any -> boolean"
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
      (l (a)
        "alist -> string
        convert to a sql values specification for the insert statement
        ((a . 1) (b . 2)) -> (a,b)values(1,2)"
        (if (pair? a)
          (if (null? a) #f
            (string-append "("
              (if (list? a) (string-join (map handle-column a) ",") (sql-value (handle-column a)))
              ")values("
              (if (list? a) (string-join (map (l (e) (sql-value (tail e))) a) ",")
                (sql-value (tail a)))
              ")"))
          #f))))

  (define (sql-where where) "string/pair/list -> string"
    (if (null? where) ""
      (string-append " where "
        (if (pair? where) (sql-where-expr where) (if (string? where) where #f)))))

  (define (sql-config config)
    "list:alist -> string:begin-part string:end-part
    constructs the additional-options part of an sql-statement, usually some part
    at the beginning of the statement or one at the end
    alist keys can be: limit, add-1, add-2, order"
    ;order - string/(string ...)
    ;limit - string/(offset limit)
    (alist-bind config (limit add-1 add-2 order)
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

  (define (sql-columns-list columns) "(pair/list/string ...) -> list"
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
    construct the \"column\" part of an sql expression"
    (if (pair? columns)
      (if (list? columns) (if (null? columns) #f (string-join (sql-columns-list columns) ","))
        (string-append (first columns) "." (tail columns)))
      (if (string? columns) columns (if (and (boolean? columns) columns) "*" #f))))

  (define* (sql-select tables #:optional (columns #t) where config)
    "string/(string ...) [string/(string ...)/boolean:all-columns string/list/pair string/list] -> string"
    (let-values
      ( ( (columns) (sql-columns columns))
        ( (tables)
          (if (and (list? tables) (not (null? tables))) (string-join tables ",")
            (if (string? tables) tables #f)))
        ((where) (if where (sql-where where) "")) ((config-1 config-2) (sql-config config)))
      (if (and columns tables where config-1 config-2)
        (string-append "select " config-1 columns " from " tables where config-2) #f)))

  (define* (sql-update table-name set #:optional where config)
    "string pair/list/string sql-where list -> string
    example: (sql-update \"table-name\" ((a . 1) (b . 2))) -> \"update table-name set a=1 and b=2\""
    (let-values
      ( ( (set)
          (if (list? set)
            (string-join
              (map (l (e) (string-append (sql-column (first e)) "=" (sql-value (tail e)))) set) ",")
            (if (pair? set) (string-append (sql-column (first set)) "=" (sql-value (tail set)))
              (if (string? set) set #f))))
        ((where) (if where (sql-where where) "")) ((config-1 config-2) (sql-config config)))
      (if (and table-name where config-1 config-2)
        (string-append "update " config-1 table-name " set " set where config-2) #f)))

  (define* (sql-insert table-name new-data #:optional config)
    "string string/boolean/(pair ...) -> string"
    (let-values
      ( ( (config-1 config-2) (sql-config config))
        ( (new-data)
          (cond ((string? new-data) new-data) ((boolean? new-data) " default values")
            (else (sql-insert-values new-data)))))
      (if (and table-name (sql-row-data? new-data) config-1 config-2)
        (string-append "insert" config-1 " into " table-name new-data config-2) #f)))

  (define* (sql-delete table-name #:optional where config) "string sql-where sql-config -> string"
    (let-values (((config-1 config-2) (sql-config config)))
      (string-append "delete from " table-name (if where (sql-where where) "") config-2)))

  (define* (sql-create-table table-name columns #:optional (replace #t) add)
    "string ((name type options ...)/string ...) [boolean string:custom-suffix]"
    (string-append "create table " (if replace "" "if not exists ")
      table-name "("
      (if (list? columns)
        (string-join (map (l (column) (if (list? column) (string-join column " ") column)) columns)
          ",")
        columns)
      (if add (string-append "," add) "") ")"))

  (define (sql-create-index name table-name . columns) "string string string ... -> string"
    (string-append "create index " name " on " table-name " (" (string-join columns ",") ")"))

  (define (sql-delete-table table-name) "string -> string" (string-append "drop table " table-name)))
