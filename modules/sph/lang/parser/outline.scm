(define-module (sph lang parser outline))
(use-modules (ice-9 rdelim) (rnrs base) (sph) (sph list))
(export primitive-read-outline read-outline sph-lang-parser-outline-description)

(define sph-lang-parser-outline-description
  "parse a markup structure where headings are prefixed by one or multiple characters for nested sections.
   for example markdown and org-mode")

(define (parse-heading-repeated-char-proc char) "char -> procedure"
  (l (line) "string -> (integer:nesting-depth/false string:heading/false)"
    (let (prefix-count (string-skip line char))
      (if (and prefix-count (> prefix-count 0))
        (list prefix-count (string-trim (string-drop line prefix-count) #\space)) (list #f #f)))))

(define (primitive-read-outline port parse-heading)
  "port procedure:{string:line -> (integer:nesting-depth/false string:heading/false)} -> list"
  (first-or-null
    (reverse
      (let loop ((line (read-line port)) (level (list)) (r (list)) (depth 0))
        (if (eof-object? line) (pair (reverse level) r)
          (apply
            (l (heading-depth heading)
              (if heading-depth
                (if (> heading-depth depth)
                  (let (r-2 (loop (read-line port) (list heading) (list) heading-depth))
                    (loop (read-line port) (append r-2 level) r depth))
                  (if (< heading-depth depth)
                    (begin (unread-string (string-append line "\n") port) (pair (reverse level) r))
                    (loop (read-line port) (list heading) (pair (reverse level) r) depth)))
                (loop (read-line port) (pair line level) r depth)))
            (parse-heading line)))))))

(define (read-outline port parse-heading)
  "port char/procedure:{string:line -> (integer:nesting-depth/false string:heading/false)} -> list
   for parsing structured text where repetitions of a character at the beginning of a line designate
   the section nesting depth of following lines that are not of equal or lower nesting depth"
  (let
    (parse-heading
      (if (procedure? parse-heading) parse-heading
        (if (char? parse-heading) (parse-heading-repeated-char-proc parse-heading)
          (throw (q wrong-type-for-argument)))))
    (primitive-read-outline port parse-heading)))
