(define-test-module (test module sph string)
  (import (sph string))
  (test-execute-procedures-lambda
    (string-indices ("/a////b//c/d" "/") (0 2 3 4 5 7 8 10)
      ("/a////b//c/d" "//") (2 4 7)
      ("abcd" "bc") (1)
      ("abcdbcefbcg" "bc") (1 4 8)
      ("abcd" "cd") (2) ("ab" "") (0 1 2) ("" "") (0) ("" "ab") () ("abcd" "cb") ())
    (string-multiply ("a" 0) "" ("a" 3) "aaa" ("" 3) "")
    (string-replace-string ("/a////b//c/d" "//" "/") "/a//b/c/d"
      ("abcd" "bc" "") "ad"
      ("abcdbcefbcg" "bc" "hij") "ahijdhijefhijg"
      ("abcd" "abc" "e") "ed"
      ("abcd" "cd" "efg") "abefg" ("ab" "" "") "ab" ("ab" "" "/") "/a/b/" ("" "" "/") "/" ("" "" "") "")
    (string-quote "'test'" "\"'test'\""
      "t'est" "\"t'est\"" "\"test\"" "'\"test\"'" "te\"st\"" "'te\"st\"'" "te\"'st\"" #f)
    (any->string 1 "1" "string" "string" ((1 "2" 3)) "(1 2 3)" #f "#f" #(1 2 #(3)) "#(1 2 #(3))")
    (string-skip-string ("---a-" "--") 3) (string-trim-string ("---a-" "--") "-a-")
    (string-numeric? "" #f "1f" #f "a1a" #f "123" #t)
    (string-lowercase? "aBc" #f "aa1a" #t "BBB" #f "" #t)
    (string-drop-suffix ("def" "abcdef") "abc" ("def" "abcdefdef") "abcdef")
    (string-replace-char ("abacadae" #\a #\x) "xbxcxdxe" ("" #\a #\b) "" ("a" #\a #\b) "b")
    (string-indices-char ("/a////b//c/d" #\/) (0 2 3 4 5 7 8 10))
    (string-longest-prefix ("abcdabcd" ("ab" "a" "abcda" "abc")) "abcda")
    (string-camelcase->dash "aA" "a-a" "AA" "AA" "abCdEfg" "ab-cd-efg" "aa AAa" "aa AAa")
    (string-camelcase->underscore "aA" "a_a" "AA" "AA" "abCdEfg" "ab_cd_efg" "aa AAa" "aa AAa")
    (string-downcase-first "AAA" "aAA" "" "" "A" "a")
    (string-join-tree (("a" ("b" "c") ("d" ("e" ("f") "g"))) "") "abcdefg")))
