(library (sph time stream)
  (export
    date-stream
    sph-time-stream-description)
  (import
    (guile)
    (sph)
    (sph time)
    (sph time gregorian)
    (srfi srfi-41))

  (define sph-time-stream-description
    "create an srfi-41 stream of (sph time) date vectors between two dates")

  (define (date-stream date-start date-end)
    "date date -> stream:(vector ...)
     creates an srfi-41 stream of all calendar dates between date-start and date-end inclusively.
     dates are (sph time) date records/vectors"
    (let
      ( (year-end (date-year date-end)) (month-end (date-month date-end))
        (day-end (date-day date-end)))
      (stream-let loop
        ((year (date-year date-start)) (month (date-month date-start)) (day (date-day date-start)))
        (if
          (or (< year year-end)
            (and (= year year-end)
              (or (< month month-end) (and (= month month-end) (<= day day-end)))))
          (let
            (day-count (vector-ref (greg-month-days-get (greg-year-leap-year? year)) (- month 1)))
            (stream-cons (date-new* year month day 0 0 0 0 0)
              (loop
                (if (and (= month 12) (= day day-count)) (+ 1 year) year)
                (if (= day day-count) (+ 1 (modulo month 12)) month)
                (+ 1 (modulo day day-count)))))
          stream-null)))))
