(library (sph time rfc3339)
  (export
    time-rfc3339->alist
    time-rfc3339->seconds
    time-rfc3339->seconds-and-fraction
    time-rfc3339-parse&
    time-rfc3339-parse-tree
    time-seconds->rfc3339)
  (import
    (guile)
    (ice-9 peg)
    (rnrs base)
    (sph)
    (sph time)
    (only (sph alist) alist-ref alist-quoted)
    (only (sph conditional) pass-if)
    (only (sph string) pad-with-zeros string-equal?)
    (only (sph tree) splice-lists-without-prefix-symbol)
    (only (srfi srfi-1) drop-right))

  (define-peg-pattern digit body (range #\0 #\9))
  (define-peg-pattern year all (and digit digit digit digit))
  (define-peg-pattern month all (and digit digit))
  (define-peg-pattern day all (and digit digit))
  (define-peg-pattern date all (and year (ignore "-") month (ignore "-") day))
  (define-peg-pattern hours all (and digit digit))
  (define-peg-pattern minutes all (and (ignore ":") digit digit))
  (define-peg-pattern seconds all (and (ignore ":") digit digit))
  (define-peg-pattern seconds-fraction all (and (ignore ".") digit (* digit)))

  (define-peg-pattern time all
    (and (ignore "T") hours (? (and minutes (? (and seconds (? seconds-fraction)))))))

  (define-peg-pattern offset all (and (or "+" "-") hours minutes))
  (define-peg-pattern rfc3339-date-time all (and date (? (and time (or (ignore "Z") offset)))))

  (define (time-rfc3339-parse-tree a)
    (pass-if (match-pattern rfc3339-date-time a)
      (l (a) (splice-lists-without-prefix-symbol (peg:tree a)))))

  (define time-rfc3339-parse&
    (let
      ( (parse-date&
          (l (a c) (apply c (tail a) (map (compose string->number second) (tail (first a))))))
        (parse-time&
          (l (a c)
            (apply
              (l* (#:optional (hours 0) (minutes 0) (seconds 0) (seconds-fraction 0))
                (c (tail a) hours minutes seconds seconds-fraction))
              (map (compose string->number second) (alist-ref a (q time) (list))))))
        (parse-offset&
          (l (a c)
            (apply
              (l* (#:optional sign hours minutes)
                (c (and sign (string-equal? "-" sign))
                  (pass-if hours (compose string->number second) 0)
                  (pass-if minutes (compose string->number second) 0)))
              (alist-ref a (q offset) (list))))))
      (l (a c)
        "string procedure:{year month day hours minutes seconds seconds-fraction offset-negative? offset-hours offset-minutes -> any} -> any
        parse an rfc3339 date format string and call \"c\" with the parsed parts as arguments.
        rfc3339 is a profile of iso8601, so they are similar with 3339 being the simpler one.
        source format is usually: yyyy-mm-ddThh:mm:ss+hh:mm , with slight variations to allow second fractions and an implied utc offset with \"Z\".
        example source strings: 2003-12-13T18:30:02.25+01:00, 2003-12-13T18:30:02Z
        see https://www.ietf.org/rfc/rfc3339.txt"
        (false-if-exception
          (parse-date& (tail (time-rfc3339-parse-tree a))
            (l (a year month day)
              (parse-time& a
                (l (a hours minutes seconds seconds-fraction)
                  (parse-offset& a
                    (l (offset-negative? offset-hours offset-minutes)
                      (c year month
                        day hours
                        minutes seconds seconds-fraction offset-negative? offset-hours offset-minutes)))))))))))

  (define (time-rfc3339->alist a) "string -> list/false"
    (time-rfc3339-parse& a
      (l
        (year month day
          hours minutes seconds seconds-fraction offset-negative? offset-hours offset-minutes)
        (alist-quoted year year
          month month
          day day
          hours hours
          minutes minutes
          seconds seconds
          seconds-fraction seconds-fraction
          offset-negative? offset-negative? offset-hours offset-hours offset-minutes offset-minutes))))

  (define (time-rfc3339->seconds a)
    "string -> integer:seconds:posix-time
    does not include fractional seconds"
    (pass-if (time-rfc3339->seconds-and-fraction a) first))

  (define (time-rfc3339->seconds-and-fraction a)
    "string -> (integer:seconds:posix-time . integer:seconds-fraction)"
    (time-rfc3339-parse& a
      (l
        (year month day
          hours minutes seconds seconds-fraction offset-negative? offset-hours offset-minutes)
        (let (offset-factor (if offset-negative? -1 1))
          (pair
            (time-traditional-parts->seconds #:year year
              #:month month
              #:day day
              #:hours hours
              #:minutes minutes
              #:seconds seconds
              #:offset-hours (* offset-factor offset-hours)
              #:offset-minutes (* offset-factor offset-minutes))
            seconds-fraction)))))

  (define (number->padded-string a) (pad-with-zeros (number->string a) 2))

  (define* (time-seconds->rfc3339 a #:optional (offset 0) (seconds-fraction 0))
    "integer:posix-time -> string"
    (let
      ( (date-time
          (let (t (gmtime (+ a offset)))
            (apply
              (l (y m d h mi s)
                (string-append y "-"
                  m "-"
                  d "T"
                  h ":"
                  mi ":"
                  s
                  (if (zero? seconds-fraction) ""
                    (string-append "." (number->string seconds-fraction)))))
              (map number->padded-string
                (list (+ 1900 (tm:year t)) (+ 1 (tm:mon t))
                  (tm:mday t) (tm:hour t) (tm:min t) (tm:sec t))))))
        (offset
          (if (zero? offset) "Z"
            (apply
              (l (sign numbers)
                (string-append sign (string-join (map number->padded-string numbers) ":")))
              (let*
                ((hms (drop-right (seconds->hours-minutes-seconds offset) 1)) (hours (first hms)))
                (if (any negative? hms) (list "-" (map (l (a) (* -1 a)) hms)) (list "+" hms)))))))
      (string-append date-time offset))))
