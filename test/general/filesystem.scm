(import
  (sph test)
  (sph)
  (sph string)
  (sph filesystem))

(define cwd (string-append (getcwd) "/"))

(define (test-path->full-path inp exp)
  (string-replace-string (path->full-path inp) cwd ""))

(execute-tests-quasiquote
  (path->full-path
    ".abc." ".abc."
    "..abc.." "..abc.."
    "../../" (unquote (dirname (dirname cwd)))
    ".././" (unquote (dirname cwd))
    "./../" (unquote (dirname cwd))
    "abc/../def" "def"
    "abc/./def" "abc/def")
  (filename-extension
    ("ab.d.e.fg") "fg"
    ("ab") #f
    ("") #f)
  (ensure-trailing-slash
    "/" "/"
    "ab/cd/ef/" "ab/cd/ef/"
    "ab/cd/ef" "ab/cd/ef/"
    "" "/")
  (dir-ref?
    "." #t
    ".." #t
    ".a" #f
    "..a" #f
    "a" #f)
  (dotfile?
    "." #t
    ".." #t
    ".a" #t
    ".abc" #t
    ".." #t
    "" #f
    "a" #f)
  (path->list
    ""
    ()
    "//a//b//"
    ("" "a" "b")
    "/this//is/a ////test/path/"
    ("" "this" "is" "a " "test" "path")
    "///this//is/a////test/ path/ /"
    ("" "this" "is" "a" "test" " path" " ")
    "this//is/a////test/ path/"
    ("this" "is" "a" "test" " path"))
  (path-append
    ("a/" "b/" "c") "a/b/c"
    ("a/" "/b/" "/c") "a/b/c"
    ("/a/" "/b/" "/c/") "/a/b/c"
    ;the result of this depends alot on the algorithm that is used
    ("/a///" "/b//c/" "d/") "/a/b//c/d"
    "a" "a"
    "/" "/"
    ("" "") "/"
    "" ""
    ("/" "/") "/"
    ("/" "/a") "/a")
  (path-append*
    ("/a" "b/" "c") "/a/b/c"
    ("/a///" "/b//c/" "d/") "/a/b/c/d"
    "a" "a"
    "/" "/"
    ("/" "/") "/"
    ("/" "/a") "/a"
    ("" "") "/"
    "" ""))
