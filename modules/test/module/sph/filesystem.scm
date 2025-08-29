(define-test-module (test module sph filesystem)
  (import
    (except (srfi srfi-1) map)
    (sph string)
    (sph list)
    (sph tree)
    (sph filesystem))

  (define cwd (getcwd))
  (define-test (realpath* arguments) (string-replace-string (apply realpath* arguments) cwd ""))
  (define temp-path (ensure-trailing-slash (system-temp-dir)))
  (define realpath-temp-path (string-append temp-path "realpath"))
  (define glob-temp-path (string-append temp-path "filesystem-glob"))
  (define (realpath-temp-path* path) (string-append realpath-temp-path "/" path))

  (define-test (set-up-realpath*) (ensure-directory-structure (realpath-temp-path* "b"))
    (symlink (realpath-temp-path* "b") (realpath-temp-path* "c"))
    (symlink ".." (realpath-temp-path* "c/d")) #t)

  (define-test (tear-down-realpath*) (delete-file (realpath-temp-path* "c/d"))
    (delete-file (realpath-temp-path* "c")) (rmdir (realpath-temp-path* "b"))
    (rmdir realpath-temp-path) #t)

  (define-test (realpath*)
    (assert-and (equal? ".." (readlink* (realpath-temp-path* "c/d")))
      (equal? realpath-temp-path (realpath* (realpath-temp-path* "c/d")))
      (equal? (dirname realpath-temp-path) (realpath* (realpath-temp-path* "c/d/..")))
      (equal? (dirname (dirname cwd)) (realpath* "../../"))
      (equal? (dirname cwd) (realpath* ".././"))))

  (define-test (directory-tree) (every string? (directory-tree cwd)))

  (define-test (directory-tree-leaf-directories)
    (every (l (a) (and (string? a) (directory? a))) (directory-tree-leaf-directories cwd)))

  (define-test (directory-prefix-tree)
    (every string? (prefix-tree->paths (directory-prefix-tree cwd))))

  (define-test (set-up-filesystem-glob)
    (let
      ( (directories (list "a" "a/b" "c/d/e/f"))
        (files
          (list "test1.txt" "test2.txt"
            "a/testa.txt" "a/b/testab1.txt" "a/b/testab2.txt" "c/d/e/f/testcdef.txt")))
      (each (l (a) (ensure-directory-structure (string-append glob-temp-path "/" a))) directories)
      (each (l (a) (close (open (string-append glob-temp-path "/" a) O_CREAT))) files) #t))

  (define-test (filesystem-glob a expected)
    (let*
      ( (results
          (map (l (a) (string-drop a (+ 1 (string-length glob-temp-path))))
            (filesystem-glob (string-append glob-temp-path "/" (first a)))))
        (results-relative
          (let*
            ( (previous (getcwd))
              (result (begin (chdir glob-temp-path) (filesystem-glob (first a)))))
            (chdir previous) result)))
      (if (equal? results results-relative expected) expected
        (list (q full) results (q relative) results-relative)

        )
      ))

  (test-execute-procedures-lambda set-up-filesystem-glob
    (filesystem-glob "c/d/*/f/testcdef.txt" ("c/d/e/f/testcdef.txt")
      "*.txt" ("test1.txt" "test2.txt")
      "a/b/*" ("a/b/testab1.txt" "a/b/testab2.txt")
      "a/**" ("a/b" "a/testa.txt" "a/b/testab1.txt" "a/b/testab2.txt")
      "a/**/*.txt" ("a/testa.txt" "a/b/testab1.txt" "a/b/testab2.txt")
      "**/*.txt"
      ("test1.txt" "test2.txt" "c/d/e/f/testcdef.txt"
        "a/testa.txt" "a/b/testab1.txt" "a/b/testab2.txt")
      "**1/*" ("a" "c" "test1.txt" "test2.txt" "c/d" "a/b" "a/testa.txt"))
    directory-tree directory-tree-leaf-directories
    directory-prefix-tree
    (path->full-path "/a/b/c" "/a/b/c" "a/b/c" (unquote (string-append cwd "/a/b/c")))
    set-up-realpath* (realpath*)
    tear-down-realpath* (filename-extension ("ab.d.e.fg") "fg" ("ab") "" ("") "")
    (ensure-trailing-slash "/" "/" "ab/cd/ef/" "ab/cd/ef/" "ab/cd/ef" "ab/cd/ef/" "" "/")
    (directory-reference? "." #t ".." #t ".a" #f "..a" #f "a" #f)
    (dotfile? "." #t ".." #t ".a" #t ".abc" #t ".." #t "" #f "a" #f)
    (path->list "" ()
      "//a//b//" ("" "a" "b")
      "/this//is/a ////test/path/" ("" "this" "is" "a " "test" "path")
      "///this//is/a////test/ path/ /" ("" "this" "is" "a" "test" " path" " ")
      "this//is/a////test/ path/" ("this" "is" "a" "test" " path"))
    (path-append ("a/" "b/" "c") "a/b/c"
      ("a/" "/b/" "/c") "a/b/c"
      ("/a/" "/b/" "/c/") "/a/b/c"
      ;the result of this depends alot on the algorithm that is used
      ("/a///" "/b//c/" "d/") "/a/b//c/d"
      "a" "a" "/" "/" ("" "") "/" "" "" ("/" "/") "/" ("/" "/a") "/a")
    (path-append* ("/a" "b/" "c") "/a/b/c"
      ("/a///" "/b//c/" "d/") "/a/b/c/d"
      "a" "a" "/" "/" ("/" "/") "/" ("/" "/a") "/a" ("" "") "/" "" "")))
