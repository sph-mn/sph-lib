(library (sph user-cli)
  (export
    sph-user-cli-description
    user-cli-choice
    user-cli-confirm)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph list)
    (only (ice-9 rdelim) read-line)
    (only (sph number) increment-one decrement-one)
    (only (srfi srfi-1) remove))

  (define sph-user-cli-description
    "text based user interaction. for example choices or confirmations")

  (define* (user-cli-choice proposition names #:optional (max-choices 1) (min-choices 1))
    "string (string:name ...) -> (string:chosen-name ...)" (display proposition)
    (if (or (< 1 max-choices) (< min-choices max-choices)) #f
      (begin
        (display
          (string-append "\nplease choose "
            (if (= max-choices min-choices) (number->string max-choices)
              (if (> min-choices 1) (simple-format #f "~S to ~S" min-choices max-choices)
                (simple-format #f "up to ~S" max-choices)))
            " elements from the following list by entering numbers. abort with ctrl+c or by entering a non-numeric character.\n"))
        (display
          (string-join
            (map-with-index
              (l (index ele) (string-append (number->string (increment-one index)) " " ele)) names)
            "  "))
        (newline)
        (let (choice (map string->number (remove string-null? (string-split (read-line) #\space))))
          (if (any not choice) #f (list-select names (map decrement-one choice)))))))

  (define (user-cli-confirm proposition) (display proposition)
    (display "\nplease enter y or n: ") (string-prefix? "y" (read-line))))
