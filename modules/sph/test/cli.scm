(library (sph test cli)
  (export
    test-cli)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph alist)
    (sph cli)
    (sph conditional)
    (sph list)
    (sph string)
    (sph test)
    (sph test report)
    (except (srfi srfi-1) map))

  ;work in progress: this should create a command line interface for running tests.

  (define test-cli
    (cli-create #:parameters "options ... source ..."
      #:description
      (string-join
        (list
          "execute tests via a command-line interface. \"source\" is filesystem paths to files containing modules relative to any guile load-path."
          "if a source path points to a directory, a module with a name corresponding to that path and all modules under that directory are used."
          "exclude/only/until take a list of comma separated module name suffixes or full module names in the format \"a//b//c\" as well as individual (test) procedure names (without the internal \"test-\" prefix).
          usage: (test-cli) in a scheme file makes the code parse command-line arguments (arguments to \"exec\")")
        "\n  ")
      #:options
      (ql ((source ...)) (display-format #f #f #t)
        (add-to-load-path #f #f #t) (path-search #f #f #t)
        (search-type) (exclude #f #f #t) (only #f #f #t) (until #f #f #t))))

  (define (cli-value-path/module-list a)
    "false/string -> false/list
    parsed a list of comma separated filesystem or module paths, \"/\" or \"//\" separated respectively, without filename suffixes"
    (false-if-not a
      (map
        (l (e) (if (string-contains e "//") (map string->symbol (string-split-regexp e "//")) e))
        (string-split a #\,))))

  (define (cli-value-reporter reporters a) "false/string -> procedure"
    (test-reporter-get reporters (string->symbol a)))

  (define (cli-add-to-load-path! cli-arguments)
    "list ->
    if the --add-to-load-path option has been specified, add the comma separated list of paths given
    as a value to the option to the beginning of the module load-path"
    (if-pass (alist-quoted-ref cli-arguments add-to-load-path)
      (l (a) (map (l (e) (add-to-load-path e)) (string-split a #\,)))))

  (define (test-execute-cli-get-settings cli-arguments)
    "list -> list
    create the test settings object from program arguments"
    (alist-quoted-bind cli-arguments (reporter exclude only until)
      (cli-add-to-load-path! cli-arguments)
      (alist-quoted-merge-key/value test-settings-default reporter-name
        (cli-value-reporter (alist-quoted-ref test-settings-default reporters) reporter) exclude
        (cli-value-path/module-list exclude) only
        (cli-value-path/module-list only) until (cli-value-path/module-list until))
      test-settings-default))

  (define (test-execute-cli)
    "parse program arguments and run the rest of the program depending on the given arguments.
    creates a command-line interface for executing tests in test module files"
    (let* ((arguments (test-cli)) (settings (test-execute-cli-get-settings arguments)))
      (alist-quoted-bind arguments (source)
        (if source (test-modules-execute settings (append-map test-path->module-names source))
          (list))))))
