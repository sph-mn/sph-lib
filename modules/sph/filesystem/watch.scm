(library (sph filesystem watch)
  (export
    sph-filesystem-watch-description
    watch-path)
  (import
    (linux inotify)
    (rnrs base)
    (sph)
    (sph filesystem)
    (sph linux)
    (only (guile)
      logand
      logior
      string=
      uname
      utsname:sysname
      stat:mtime
      stat:atime
      stat:size
      stat:mode
      stat:uid
      stat:gid
      stat:nlink
      stat:ctime)
    (only (sph tree) flatten)
    (only (srfi srfi-1) delete-duplicates filter-map))

  (define sph-filesystem-watch-description "observing and acting on file-system changes
    uses inotify if available or polling as a fall-back")

  (define watch-path-events (q (mtime atime attrib)))

  (define (watch-path-event->inotify-event-id a)
    (if (eqv? (q mtime) a) IN_MODIFY
      (if (eqv? (q atime) a) IN_ACCESS (if (eqv? (q attrib) a) IN_ATTRIB))))

  (define (watch-path-event->poll-event a)
    (if (eqv? (q attrib) a) (q (size mode uid gid nlink ctime)) a))

  (define (watch-path-events->poll-events a) (flatten (map watch-path-event->poll-event a)))

  (define (watch-path-events->inotify-event a) "(symbol ...) -> integer"
    (apply logior (map watch-path-event->inotify-event-id a)))

  (define inotify-event->watch-path-events
    ( (l (event-info)
        (lambda (a) "integer -> (symbol ...)"
          (filter-map (l (e) (if (= (tail e) (logand a (tail e))) (first e) #f)) event-info)))
      (map (l (e) (pair e (watch-path-event->inotify-event-id e))) watch-path-events)))

  (define (stat-accessor->watch-path-event a)
    (if (or (eqv? stat:mtime a) (eqv? stat:size a)) (q mtime)
      (if (eqv? stat:atime a) (q atime)
        (if
          (or (eqv? stat:mode a) (eqv? stat:uid a)
            (eqv? stat:gid a) (eqv? stat:nlink a) (eqv? stat:ctime a))
          (q attrib)))))

  (define (poll-events->watch-path-events! a)
    (delete-duplicates (map stat-accessor->watch-path-event (apply append a))))

  (define (create-inotify-watch-path)
    (lambda (paths events proc)
      "(string ...) (symbol:event-name ...) {(symbol:event-name ...)} ->
      the supported events are designated by the symbols mtime, atime or attrib"
      (inotify-watch paths (watch-path-events->inotify-event events)
        (l (watch-info)
          (apply proc
            (apply append
              (map (l (e) (inotify-event->watch-path-events (vector-ref e 1))) watch-info)))))))

  (define (create-poll-watch-path)
    (l (paths events proc)
      "(string ...) (symbol:event-name ...) {(symbol:event-name ...)} ->
      the supported events are designated by the symbols mtime, atime or attrib.
      utilises poll-watch which is defined in (sph filesystem), see its documentation for more information about the polling process"
      (poll-watch paths (watch-path-events->poll-events events)
        (l (events fdes stat-info) (apply proc (poll-events->watch-path-events! events))) 1000 60000)))

  (define watch-path
    (let ((sys-info (uname)))
      (if (string= "Linux" (utsname:sysname sys-info)) (create-inotify-watch-path)
        (create-poll-watch-path)))))
