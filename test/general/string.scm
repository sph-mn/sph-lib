(import
  (sph test)
  (sph)
  (sph string))

(execute-tests-quasiquote
  (string-indices
    ("/a////b//c/d" "/") (0 2 3 4 5 7 8 10)
    ("/a////b//c/d" "//") (2 4 7)
    ("abcd" "bc") (1)
    ("abcdbcefbcg" "bc") (1 4 8)
    ("abcd" "cd") (2)
    ("ab" "") (0 1 2)
    ("" "") (0)
    ("" "ab") ()
    ("abcd" "cb") ())
  (string-multiply
    ("a" 0) ""
    ("a" 3) "aaa"
    ("" 3) "")
  (string-replace-string
    ("/a////b//c/d" "//" "/") "/a//b/c/d"
    ("abcd" "bc" "") "ad"
    ("abcdbcefbcg" "bc" "hij") "ahijdhijefhijg"
    ("abcd" "abc" "e") "ed"
    ("abcd" "cd" "efg") "abefg"
    ("ab" "" "") "ab"
    ("ab" "" "/") "/a/b/"
    ("" "" "/") "/"
    ("" "" "") "")
  (string-quote
    "'test'"
    "\"'test'\""
    "t'est"
    "\"t'est\""
    "\"test\""
    "'\"test\"'"
    "te\"st\""
    "'te\"st\"'"
    "te\"'st\""
    #f)
  (string-skip-string
    ("---a-" "--") 3)
  (string-trim-string
    ("---a-" "--") "-a-"))