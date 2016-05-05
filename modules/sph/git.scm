(library (sph git)
  (export
    git-archive
    git-archive->file
    git-branch-exists?
    git-current-short-commit-hash
    git-revision-count)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph process)
    (only (guile) status:exit-val waitpid)
    (only (sph two) cli-option))

  (define (git-branch-exists? path-repository name)
    (not
      (string-null?
        (call-with-working-directory path-repository
          (thunk (execute->string "git" "rev-parse" "--quiet" "--verify" name))))))

  (define* (git-archive path-repository #:optional (branch "master") #:rest additional-arguments)
    "string [string] ->
    create a compressed tar archive from the contents of a git repository without (most) git metadata.
    uses the git built-in \"git archive\""
    (zero?
      (status:exit-val
        (apply execute "git"
          (cli-option "git-dir" path-repository) "archive" branch additional-arguments))))

  (define* (git-current-short-commit-hash path-repository #:optional (branch "master"))
    "string -> string
    results in the short commit hash for the latest commit in a git repository"
    (execute->string "git" (cli-option "git-dir" (string-append path-repository ".git"))
      "log" (cli-option #\n 1) (cli-option "pretty" "format:%h") branch))

  (define (git-revision-count path-repository)
    (call-with-working-directory path-repository
      (thunk
        (string->number (string-trim-right (execute->string "git" "rev-list" "--count" "HEAD"))))))

  (define*
    (git-archive->file path-repository target-path #:optional (branch "master") #:rest
      additional-arguments)
    (apply git-archive (string-append path-repository ".git")
      branch (cli-option "output" target-path) additional-arguments)))
