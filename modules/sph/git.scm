(library (sph git)
  (export
    git-archive
    git-archive->file
    git-branch-exists?
    git-current-short-commit-hash
    git-last-commit-date-iso8601-ymd
    git-last-commit-posixtime
    git-revision-count)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph process)
    (sph time)
    (sph time string)
    (only (guile) status:exit-val waitpid)
    (only (sph one) cli-option))

  (define (git-branch-exists? path-repository name)
    (not
      (string-null?
        (call-with-working-directory path-repository
          (thunk (execute->string "git" "rev-parse" "--quiet" "--verify" name))))))

  (define*
    (git-archive path-repository #:optional (branch "master") #:rest additional-git-arguments)
    "string [string] ->
    create a compressed tar archive from the contents of a git repository without (most) git metadata.
    uses the git built-in \"git archive\""
    (zero?
      (status:exit-val
        (apply execute "git"
          (cli-option "git-dir" path-repository) "archive" branch additional-git-arguments))))

  (define* (git-current-short-commit-hash path-repository #:optional (branch "master"))
    "string -> string
    results in the short commit hash for the latest commit in a git repository"
    (execute->string "git" (cli-option "git-dir" (string-append path-repository ".git"))
      "log" (cli-option #\n 1) (cli-option "pretty" "format:%h") branch))

  (define*
    (git-last-commit-posixtime path-repository #:optional (branch "master") #:rest
      additional-git-arguments)
    "string string string ... -> integer
    return a timestamp for when the last commit in branch (master by default) has been made"
    (string->number
      (string-trim-right
        (call-with-working-directory path-repository
          (thunk (execute->string "git" "log" "-1" "--format=%ct" "--quiet" branch))))))

  (define*
    (git-last-commit-date-iso8601-ymd path-repository #:optional (branch "master") #:rest
      additional-git-arguments)
    "string string string ... -> string
    return a date formatted like this 2017-01-25 for when the last commin in branch (master by default) has been made"
    (time->iso8601-ymd
      (time-from-utc
        (time-s->ns
          (apply git-last-commit-posixtime path-repository branch additional-git-arguments)))))

  (define (git-revision-count path-repository)
    (call-with-working-directory path-repository
      (thunk
        (string->number (string-trim-right (execute->string "git" "rev-list" "--count" "HEAD"))))))

  (define*
    (git-archive->file path-repository target-path #:optional (branch "master") #:rest
      additional-git-arguments)
    (apply git-archive (string-append path-repository ".git")
      branch (cli-option "output" target-path) additional-git-arguments)))
