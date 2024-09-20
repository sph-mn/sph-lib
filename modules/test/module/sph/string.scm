(define-test-module (test module sph string)
  (import
    (sph string))

  (define-test (string-case)
    (assert-and (assert-equal #t (string-case "ab" ("cd" #f) (else #t)))
      (assert-equal #t (string-case "ab" ("ab" #t) (else #f)))
      (assert-equal #t (string-case "cd" (("ab" "cd") #t) (else #f)))))

  (define-test (string-brackets-closed? inp) (string-brackets-closed? (first inp) #\( #\)))

  (test-execute-procedures-lambda string-case
    (any->string 1 "1" "string" "string" ((1 "2" 3)) "(1 2 3)" #f "#f" #(1 2 #(3)) "#(1 2 #(3))")
    (string-brackets-closed? "(a b)c d(e f)" #t
      "(a b)c de f)" #f "(ab (bc (e)) f)" #t "(ab (bc (e) f)" #f "xy (ab (bc (e) f)) zv" #t)
    (string-camelcase->dash "aA" "a-a" "AA" "AA" "abCdEfg" "ab-cd-efg" "aa AAa" "aa AAa")
    (string-camelcase->underscore "aA" "a_a" "AA" "AA" "abCdEfg" "ab_cd_efg" "aa AAa" "aa AAa")
    (string-downcase-first "AAA" "aAA" "" "" "A" "a")
    (string-drop-suffix ("def" "abcdef") "abc" ("def" "abcdefdef") "abcdef")
    (string-indices ("/a////b//c/d" "/") (0 2 3 4 5 7 8 10)
      ("/a////b//c/d" "//") (2 4 7)
      ("abcd" "bc") (1)
      ("abcdbcefbcg" "bc") (1 4 8)
      ("abcd" "cd") (2) ("ab" "") (0 1 2) ("" "") (0) ("" "ab") () ("abcd" "cb") ())
    (string-indices-char ("/a////b//c/d" #\/) (0 2 3 4 5 7 8 10))
    (string-join-tree (("a" ("b" "c") ("d" ("e" ("f") "g"))) "") "abcdefg")
    (string-longest-prefix ("abcdabcd" ("ab" "a" "abcda" "abc")) "abcda")
    (string-lowercase? "aBc" #f "aa1a" #t "BBB" #f "" #t)
    (string-multiply ("a" 0) "" ("a" 3) "aaa" ("" 3) "")
    (string-numeric? "" #f "1f" #f "a1a" #f "123" #t)
    (parenthesized? "(b?c:d),(function(e){return(f)})" #f
      "" #f "(" #f "()" #t "(ab))" #f "(a(b)" #f "x(ab)" #f "(ab)x" #f "(a()b)" #t)
    (string-quote "'test'" "\"'test'\""
      "t'est" "\"t'est\"" "\"test\"" "'\"test\"'" "te\"st\"" "'te\"st\"'" "te\"'st\"" #f)
    (string-replace-char ("abacadae" #\a #\x) "xbxcxdxe" ("" #\a #\b) "" ("a" #\a #\b) "b")
    (string-replace-string ("/a////b//c/d" "//" "/") "/a//b/c/d"
      ("abcd" "bc" "") "ad"
      ("abcdbcefbcg" "bc" "hij") "ahijdhijefhijg"
      ("abcd" "abc" "e") "ed"
      ("abcd" "cd" "efg") "abefg"
      ("ab" "" "") "ab" ("ab" "" "/") "/a/b/" ("" "" "/") "/" ("" "" "") "")
    (string-skip-string ("---a-" "--") 3) (string-trim-string ("---a-" "--") "-a-")))
