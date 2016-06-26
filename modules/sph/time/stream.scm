(library (sph time stream)
  (export)
  (import
    (rnrs base)
    (sph)
    (sph time)
    (srfi srfi-41))

  (define (time-date-stream date-start date-end)
    "date date -> stream
    creates an srfi-41 stream of all calendar dates between date-start and date-end inclusively"
    (let
      ( (year-end (time-date-year date-end)) (month-end (time-date-month date-end))
        (day-end (time-date-day date-end)))
      (stream-let loop
        ( (year (time-date-year date-start)) (month (time-date-month date-start))
          (day (time-date-day date-start)))
        (if (and (<= year year-end) (<= month month-end) (<= day day-end))
          (let
            (day-count (vector-ref (greg-month-days-get (greg-year-leap-year? year)) (- month 1)))
            (stream-cons (record time-date year month day 0 0 0 0 0)
              (loop (if (and (= month 12) (= day day-count)) (+ 1 year) year)
                (if (= day day-count) (+ 1 (modulo month 12)) month) (+ 1 (modulo day day-count)))))
          stream-null)))))
