#;((sph cli) - quickly initialise a command line interface. the old cli library using the less powerful (ice-9 optargs)
written for the guile scheme interpreter
Copyright (C) 2010-2013 sph <tantalum@online.de> (current maintainer)

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see <http://www.gnu.org/licenses/>.
)
(library (sph cli-old)
  (export
    cli-init
    option-ref)
  (import
    (sph)
    (rnrs base)
    (rnrs programs)
    (only (guile) assoc-ref basename display newline string-join)
    (except (srfi srfi-1) map)
    (ice-9 getopt-long)
    (ice-9 optargs))

  (define (available-options-list option-spec)
    "option-spec -> string
    creates the screen text listing the available options"
    (string-append
      "available options:\n"
      (string-join
	(map
          (lambda (ele)
            (string-append
              "--" (symbol->string (first ele))
              (if (assoc-ref ele (q single-char))
		(string-append
                  " -"
                  (string (first (assoc-ref ele (q single-char)))))
		"")))
          option-spec)
	"\n"
	(q suffix))))

  (define* (cli-init #:optional cl-arguments #:key about help usage version (option-spec (list)))
    "string string string string option-spec list -> getopt-long-result
    gets command-line arguments, parses them and returns the parsed result"
    (let*
      ( (option-spec
          (option-spec-merge
            option-spec
            (default-option-spec version about)))
	(arg (getopt-long (if cl-arguments cl-arguments (command-line)) option-spec)))
      (if (option-ref arg (q help) #f)
	(begin
          (if usage
            (begin
              (display (string-append usage "\n\n"))
              (display (available-options-list option-spec)))
            (if help
              (display help)
              (begin
		(display
                  (string-append
                    "usage: " (basename (first (command-line))) " [--option | -o]"
                    (if (> (length option-spec) 1) " ..." "")
                    "\n\n"
                    (available-options-list option-spec))))))
          (exit)))
      (if (assoc-ref arg (q version))
	(begin
          (display
            (let version-join
              ((v version))
              (cond
		((number? v) (number->string v))
		((list? v) (string-join (map version-join v) "."))
		(else v))))
          (newline)
          (exit)))
      (if (assoc-ref arg (q about)) (begin (display about) (newline) (exit)))
      arg))

  (define (default-option-spec version about)
    "bool bool -> option-spec
    a default option-spec that may be extended by the users supplied option-spec"
    (fold
      (lambda (default-spec-part option-value prev)
	(if option-value
          (cons default-spec-part prev)
          prev))
      (list)
      (list
	(q (help (single-char #\h) (value #f)))
	(q (version (single-char #\v) (value #f)))
	(q (about (single-char #\a) (value #f))))
      (list #t version about)))

  (define (option-spec-merge option-spec-1 option-spec-2)
    (fold
      (lambda (ele prev)
	(if (member (first ele) prev (lambda (key ele-2) (eq? ele-2 key)))
          prev
          (cons ele prev)))
      option-spec-1
      option-spec-2)))
