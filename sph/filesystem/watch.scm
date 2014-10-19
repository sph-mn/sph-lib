(library (sph filesystem watch) ;library with the same name as future re-exported binding seems to be not possible with guile
  (export watch-path)
  (import
    (rnrs base)
    (only (srfi srfi-1) delete-duplicates filter-map)
    (only (guile)
      logand logior string= uname utsname:sysname
      stat:mtime
      stat:atime
      stat:size
      stat:mode
      stat:uid
      stat:gid
      stat:nlink
      stat:ctime)
    (sph)
    (only (sph tree) flatten)
    (sph filesystem)
    (sph linux)
    (linux inotify))

  (define watch-path-events (q (mtime atime attrib)))

  (define (watch-path-event->inotify-event-id arg)
    (if (eq? (q mtime) arg) IN_MODIFY
      (if (eq? (q atime) arg) IN_ACCESS
        (if (eq? (q attrib) arg) IN_ATTRIB))))

  (define (watch-path-event->poll-event arg)
    (if (eq? (q attrib) arg) (q (size mode uid gid nlink ctime))
      arg))

  (define (watch-path-events->poll-events arg)
    (flatten (map watch-path-event->poll-event arg)))

  (define (watch-path-events->inotify-event arg)
    "(symbol ...) -> integer"
    (apply logior (map watch-path-event->inotify-event-id arg)))

  (define inotify-event->watch-path-events
    ((l (event-info)
        (lambda (arg)
          "integer -> (symbol ...)"
          (filter-map
            (l (ele)
              (if (= (tail ele) (logand arg (tail ele)))
                (first ele)
                #f))
            event-info)))
      (map (l (ele) (cons ele (watch-path-event->inotify-event-id ele))) watch-path-events)))

  (define (stat-accessor->watch-path-event arg)
    (if (or (eq? stat:mtime arg) (eq? stat:size arg)) (q mtime)
      (if (eq? stat:atime arg) (q atime)
        (if
          (or
            (eq? stat:mode arg)
            (eq? stat:uid arg)
            (eq? stat:gid arg)
            (eq? stat:nlink arg)
            (eq? stat:ctime arg))
          (q attrib)))))

  (define (poll-events->watch-path-events! arg)
    (delete-duplicates
      (map stat-accessor->watch-path-event (apply append arg))))

  (define (create-inotify-watch-path)
    (lambda (paths events proc)
      "(string ...) (symbol:event-name ...) {(symbol:event-name ...)} ->
      the supported events are designated by the symbols mtime, atime or attrib"
      (inotify-watch paths (watch-path-events->inotify-event events)
        (l (watch-info)
          (apply proc
            (apply append
              (map (l (ele) (inotify-event->watch-path-events (vector-ref ele 1))) watch-info)))))))

  (define (create-poll-watch-path)
    (l (paths events proc)
      "(string ...) (symbol:event-name ...) {(symbol:event-name ...)} ->
      the supported events are designated by the symbols mtime, atime or attrib.
      utilises poll-watch which is defined in (sph filesystem), see its documentation for more information about the polling process"
      (poll-watch paths (watch-path-events->poll-events events)
        (l (events fdes stat-info)
          (apply proc (poll-events->watch-path-events! events)))
        1000 60000)))

  (define watch-path
    (let ((sys-info (uname)))
      (if (string= "Linux" (utsname:sysname sys-info))
        (create-inotify-watch-path)
        (create-poll-watch-path)))))
