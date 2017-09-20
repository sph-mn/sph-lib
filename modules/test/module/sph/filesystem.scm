(define-test-module (test module sph filesystem)
  (import
    (sph string)
    (sph list)
    (sph tree)
    (sph filesystem))

  (define cwd (getcwd))
  (define-test (realpath* arguments) (string-replace-string (apply realpath* arguments) cwd ""))
  (define temp-path (tmpnam))
  (define (temp-path* path) (string-append temp-path "/" path))

  (define-test (set-up-realpath*) (ensure-directory-structure (temp-path* "b"))
    (symlink (temp-path* "b") (temp-path* "c")) (symlink ".." (temp-path* "c/d")) #t)

  (define-test (tear-down-realpath*) (delete-file (temp-path* "c/d"))
    (delete-file (temp-path* "c")) (rmdir (temp-path* "b")) (rmdir temp-path) #t)

  (define-test (realpath*)
    (assert-and (equal? ".." (readlink* (temp-path* "c/d")))
      (equal? temp-path (realpath* (temp-path* "c/d")))
      (equal? (dirname temp-path) (realpath* (temp-path* "c/d/..")))
      (equal? (dirname (dirname cwd)) (realpath* "../../"))
      (equal? (dirname cwd) (realpath* ".././"))))

  (define-test (directory-tree) (every string? (directory-tree cwd)))

  (define-test (directory-tree-leaf-directories)
    (every (l (a) (and (string? a) (directory? a))) (directory-tree-leaf-directories cwd)))

  (define-test (directory-prefix-tree)
    (every string? (prefix-tree->paths (directory-prefix-tree cwd))))

  (test-execute-procedures-lambda directory-tree directory-tree-leaf-directories
    directory-prefix-tree
    (path->full-path "/a/b/c" "/a/b/c" "a/b/c" (unquote (string-append cwd "/a/b/c")))
    (set-up-realpath*) (realpath*)
    (tear-down-realpath*) (filename-extension ("ab.d.e.fg") "fg" ("ab") "" ("") "")
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
