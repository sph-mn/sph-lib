(define-test-module (test module sph sql)
  (import  (sph sql))
  (test-execute-procedures-lambda
    (sql-value
      1 "1"
      "a" "'a'"
      "\"'mystring'\"\""
      "'\"''mystring''\"\"'")
    (sql-create-table
      ("t" "c")
      "create table t(c)")
    (sql-select
      ("t" "c" (("aa" "bb")))
      "select c from t where aa='bb'"
      ("t" "c" (("a" 1) ("b" "2")))
      "select c from t where a=1 and b='2'"
      ("t" "c" (("a" 1) ("b" "2")) ((limit . 1)))
      "select c from t where a=1 and b='2' limit 1"
      ("t" "c" (("a" 1) ("b" "2")) ((limit 1 2)))
      "select c from t where a=1 and b='2' limit 1,2"
      ("t" "c" (("a" 1) ("b" "2")) ((limit 1 2) (order "ide")))
      "select c from t where a=1 and b='2' limit 1,2 order by ide")
    (sql-insert
      ("t" (("a" . 1) ("b" . "2")))
      "insert into t(a,b)values(1,'2')")
    (sql-update
      ("t" "c")
      "update t set c")
    (sql-where-condition
      ((and (or (not ("a" 1)) (not (greater "b" 2))) (or (not ("a" 1)) (not (less "b" 2)))))
      "(not a=1 or not b>2) and (not a=1 or not b<2)"
      ;pair
      ((("a" 1)))
      "a=1"
      ;implicit alternating operator
      ((("a" (1 2 (3 (4 5) 6)))))
      "(a=1 or a=2 or (a=3 and a in(4,5) and a=6))"
      ;not
      ((not ("a" 1)))
      "not a=1"
      ;nested row-expr
      ;null
      ((("a" null)))
      "a=NULL"
      (((("a" "b") null)))
      "(a=NULL or b=NULL)")))
