(library (sph linux)
  (export
    inotify-watch
    utsname:parsed-linux-version)
  (import
    (linux inotify)
    (rnrs base)
    (sph)
    (only (guile) string-split utsname:release))

  (define (utsname:parsed-linux-version sys-info)
    "uname-result -> (number number number)
    get the parsed linux version number from a record returned by uname"
    (map string->number (string-split (first (string-split (utsname:release sys-info) #\-)) #\.)))

  (define (inotify-watch paths events proc)
    "watch path for events and call proc if any such events have occurred.
    this procedure starts and infinite loop and can only be exited by
    exiting the process. it may be called inside a separate thread."
    (let*
      ( (inotify (inotify-init))
        (watch-ide
          (if (list? paths) (map (l (path) (inotify-add-watch inotify path events)) paths)
            (inotify-add-watch inotify paths events))))
      (letrec ((loop (l () (proc (inotify-read inotify)) (loop)))) (loop)))))
