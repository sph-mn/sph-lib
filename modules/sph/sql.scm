; Copyright (C) 2010-2018 sph <sph@posteo.eu>
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
    sql-insert
    sql-insert-values
    sql-select
    sql-string
    sql-update
    sql-value
    sql-where
    sql-where-condition)
  (import
    (ice-9 match)
    (rnrs exceptions)
    (sph)
    (sph tree)
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
      flatten
      any->list
      contains?
      flat?
      length-one?)
    (only (sph number) in-between?)
    (only (sph string) parenthesise string-replace-chars))

  (define sph-sql-description "create sql-statements from scheme data")

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

  (define row-expr-prefixes (q (or and not)))

  (define (operator->sql-operator a) "symbol/string -> string"
    (case a ((equal =) "=")
      ((greater >) ">") ((greater-or-equal >=) ">=")
      ((less <) "<") ((less-or-equal <=) "<=")
      ((like) " like ")
      ; not is already used for row-expressions, so use "isnot"
      ((isnot) " is not ") ((space) " ")
      (else
        (if (string? a) a
          (if (symbol? a) (string-append " " (symbol->string a) " ")
            (raise (q wrong-type-for-argument)))))))

  (define (sql-where-column-expr-produce-proc column-name state->combinator)
    ;this procedure may do too much at once, split it if you can
    (let*
      ( (join-values
          (l (c values operator sql-operator)
            (first
              (tree-map-lists-depth
                (l (values value-state)
                  (let
                    ( (value-combinator (state->combinator value-state))
                      (use-sql-in?
                        (and (odd? value-state)
                          (or (eq? (q equal) operator) (eq? (q isnot) operator)) (flat? values))))
                    (let
                      (combine-with-values
                        (if (length-one? values)
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
              (tree-map-lists-depth
                (l (columns column-state)
                  (let (column-combinator (state->combinator column-state))
                    ( (if (eq? 1 column-state) identity list)
                      (if (length-one? columns)
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

  (define primitive-sql-where-condition
    (let*
      ( (row-expr-binary
          (l (prefix suffix level)
            ( (if (or (eqv? 1 level) (length-one? suffix)) identity parenthesise)
              (string-join suffix (if (eq? (q or) prefix) " or " " and ")))))
        (row-expr-unary (l (prefix suffix level) (string-append "not " (first suffix))))
        (state->combinator (l (state) (if (even? state) " and " " or ")))
        (column-name (l (c) (if (string? c) c (symbol->string c))))
        (column-expr-produce (sql-where-column-expr-produce-proc column-name state->combinator))
        (column-expr
          (l (prefix suffix)
            (apply column-expr-produce
              ; add implicit operator to column-expression
              (if (= (length suffix) 2) (list prefix (first suffix) (first (tail suffix)))
                (list (q equal) prefix (first suffix))))))
        (handle-row-expr
          (l (prefix suffix level)
            ( (case prefix ((or and) row-expr-binary)
                ((not) row-expr-unary) (else (pair (q invalid-row-expr-prefix) prefix)))
              prefix
              (if (pair? (first suffix))
                (map (l (a) (if (pair? a) (join-column-value (first a) "=" (tail a)) a)) suffix)
                suffix)
              level))))
      (lambda (a) "sql-where-condition -> sql-string"
        (if (sql-where-condition? a)
          (prefix-tree-map-c-depth handle-row-expr
            (l (lis proc continue level)
              (if (or (null? lis) (contains? row-expr-prefixes (first lis))) (continue lis level)
                (column-expr (first lis) (tail lis))))
            a)
          (raise (q not-a-sql-where-condition))))))

  (define (sql-insert-values? a) "any -> boolean"
    (if (or (string? a) (every (l (a) (or (and (pair? a) (not (list? a))) (string? a))) a)) #t #f))

  (define (sql-where-condition-column? expr) "any -> boolean"
    (and (list? expr) (in-between? (length expr) 1 5)
      (not (contains? row-expr-prefixes (first expr)))))

  (define (sql-where-condition? a) "any -> boolean"
    (match a
      ( ( (or (quote or) (quote and) (quote not))
          (or (? sql-where-condition?) (? sql-where-condition-column?)) ...)
        #t)
      (_ #f)))

  (define (sql-where-condition expr)
    "for creating a \"where\" part sql separately.
     \"not\" in row-expressions will always be in front of the column-condition in the resulting query. sqlite for example does not understand
     \"not c=NULL\" as one might expect. use \"ISNULL\" instead of \"NULL\" in this case."
    (if (list? expr)
      (if (null? expr) expr
        (primitive-sql-where-condition (if (pair? (first expr)) (pair (q and) expr) expr)))
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
              (if (list? a) (string-join (map (l (b) (sql-value (tail b))) a) ",")
                (sql-value (tail a)))
              ")"))
          #f))))

  (define (sql-where conditions) "string/pair/list -> string"
    (if (null? conditions) ""
      (string-append " where "
        (if (pair? conditions) (sql-where-condition conditions)
          (if (string? conditions) conditions #f)))))

  (define (sql-options options)
    "list:alist -> (string:begin-part string:end-part)
     constructs the additional options parts of an sql-statement, usually some part
     at the beginning of the statement or one at the end.
     alist keys can be the following symbols: order, limit, offset, begin, end
     order: string/(string:space-joined ...)
     limit: string/(integer:offset integer:limit)
     offset: string/integer"
    (alist-bind options (order limit offset begin end)
      (pair (if begin (string-append " " begin) "")
        (string-append
          (if offset
            (string-append " offset " (if (integer? offset) (number->string offset) offset)) "")
          (if limit
            (cond ((number? limit) (string-append " limit " (number->string limit)))
              ( (and (pair? limit))
                (string-append " limit " (number->string (first limit))
                  "," (if (list? limit) (number->string (list-ref limit 1)) (tail limit))))
              (else limit))
            "")
          (if order (string-append " order by " (if (list? order) (string-join order " ") order))
            "")
          (if end (string-append " " end) "")))))

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

  (define* (sql-select tables #:optional (columns #t) where options)
    "string/(string ...) [string/(string ...)/boolean:all-columns string/list/pair string/list] -> string"
    (let
      ( (columns (sql-columns columns))
        (tables
          (if (and (list? tables) (not (null? tables))) (string-join tables ",")
            (if (string? tables) tables #f)))
        (where (if where (sql-where where) "")) (options (sql-options options)))
      (and columns tables
        where options
        (string-append "select " (first options) columns " from " tables where (tail options)))))

  (define* (sql-update table-name set #:optional where options)
    "string pair/list/string sql-where list -> string
     example: (sql-update \"table-name\" ((a . 1) (b . 2))) -> \"update table-name set a=1 and b=2\""
    (let
      ( (set
          (if (list? set)
            (string-join
              (map (l (a) (string-append (sql-column (first a)) "=" (sql-value (tail a)))) set) ",")
            (if (pair? set) (string-append (sql-column (first set)) "=" (sql-value (tail set)))
              (if (string? set) set #f))))
        (where (if where (sql-where where) "")) (options (sql-options options)))
      (and table-name where
        options (string-append "update " (first options) table-name " set " set where (tail options)))))

  (define* (sql-insert table-name data #:optional options)
    "string string/boolean/((column-name . data) ...) -> string"
    (let
      ( (options (sql-options options))
        (values
          (cond ((string? data) data) ((boolean? data) " default values")
            (else (sql-insert-values data)))))
      (and table-name (sql-insert-values? data)
        options (string-append "insert" (first options) " into " table-name values (tail options)))))

  (define* (sql-delete table-name #:optional where options)
    "string sql-where sql-options -> string"
    (let (options (sql-options options))
      (string-append "delete from " table-name (if where (sql-where where) "") (tail options))))

  (define* (sql-create-table table-name columns #:optional (replace #t) add)
    "string ((name type options ...)/string ...) [boolean string:custom-suffix]"
    (string-append "create table " (if replace "" "if not exists ")
      table-name "("
      (if (list? columns)
        (string-join (map (l (column) (if (list? column) (string-join column " ") column)) columns)
          ",")
        columns)
      (if add (string-append "," add) "") ")"))

  (define* (sql-create-index name table-name columns #:optional (replace #t))
    "string string string ... -> string"
    (string-append "create index" (if replace "" " if not exists ")
      name " on " table-name " (" (string-join columns ",") ")"))

  (define (sql-delete-table table-name) "string -> string"
    (string-append "drop table if exists " table-name)))
