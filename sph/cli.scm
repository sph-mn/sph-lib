; (sph cli) - creating command line interfaces
; written for the guile scheme interpreter
; Copyright (C) 2010-2015 sph <sph@posteo.eu>
; This program is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sph cli)
  (export
    cli-create)
  (import
    (guile)
    (ice-9 receive)
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph alist)
    (sph conditional)
    (srfi srfi-37)
    (only (sph list)
      contains?
      pattern-match-min-length
      containsv?
      fold-multiple
      split-by-pattern)
    (only (sph string) any->string-write)
    (only (sph string) string-multiply)
    (only (srfi srfi-1)
      drop-right
      drop
      drop-while
      remove
      partition))

  (define (typecheck+conversion type a)
    (cond ((eqv? (q string) type) a) ((eqv? (q number) type) (string->number a))
      ((eqv? (q integer) type) (check integer? (string->number a)))
      ((list? type) (any (l (type) (typecheck+conversion type a)) type)) (else a)))

  (define (processor opt name a r) (pair a r))
  (define (unnamed-processor a r) (pair a r))
  (define (unrecognized-processor opt name a r) (throw (q unsupported-option) opt name))

  (define (display-version-proc version-spec)
    (l (opt name a r)
      (display
        (string-append
          (if (list? version-spec) (string-join (map number->string version-spec) ".")
            (number->string version-spec))
          "\n"))
      (exit 0)))

  (define (unnamed-option? a)
    (or (list? (first a)) (and (not (null? (tail a))) (null? (first (tail a))))))

  (define help-text-line-description-delimiter (string-multiply " " 2))

  (define (format-argument-name input-type required?) "false/symbol boolean -> string"
    (string-append " "
      (let (name (if input-type (symbol->string input-type) "value"))
        (if required? name (string-append "[" name "]")))))

  (define*
    (named-option->help-text-line name/pattern #:optional names required? value-required
      value-optional
      input-type
      description
      custom-processor)
    (string-append
      (string-join
        (map (l (e) (if (string? e) (string-append "--" e) (string #\- e)))
          (if names (pair (symbol->string name/pattern) (if (list? names) names (list names)))
            (list (symbol->string name/pattern))))
        "|")
      (if value-required (format-argument-name input-type value-required)
        (if value-optional (format-argument-name input-type #f) ""))
      (if description (string-append help-text-line-description-delimiter description) "")))

  (define*
    (unnamed-option->help-text-line name/pattern #:optional names value-required value-optional
      input-type
      description
      custom-processor)
    (if (list? name/pattern)
      (let (name/pattern (delete (q ...) name/pattern))
        (if (list? description)
          (map
            (l (name description)
              (string-append (symbol->string name) help-text-line-description-delimiter description))
            name/pattern
            (let (length-difference (- (length name/pattern) (length description)))
              (if (> length-difference 0) (append description (make-list "")) description)))
          (string-append
            (if description
              (string-append (symbol->string (first name/pattern))
                help-text-line-description-delimiter description "\n")
              "")
            (string-join (map symbol->string (tail name/pattern)) "\n"))))
      (string-append (symbol->string name/pattern)
        (if description (string-append help-text-line-description-delimiter description) ""))))

  (define (options->help-text a)
    (string-join
      (map
        (l (e)
          (apply
            (if (unnamed-option? e) unnamed-option->help-text-line named-option->help-text-line) e))
        (list-sort (l (a b) (string< (symbol->string (first a)) (symbol->string (first b)))) a))
      "\n"))

  (define (options-remove-processors a)
    (map (l (e) (reverse (drop-while not (reverse (if (> (length e) 6) (drop-right e 1) e))))) a))

  (define (display-command-line-interface-proc config spec)
    (l args (write (options-remove-processors spec)) (exit 0)))

  (define (display-help-proc text config spec)
    (l (opt name a r)
      (display
        (string-append
          (identity-if (alist-ref config (q help-arguments)) (config->usage-text config spec))
          (if text (string-append "\n" text "\n") "") "\navailable options\n"
          (options->help-text (remove unnamed-option? spec)) "\n"))
      (exit 0)))

  (define (display-about-proc text config)
    (l (opt name a r) (display (string-append (if (procedure? text) (text) text) "\n")) (exit 0)))

  (define (config->usage-text a spec)
    (let
      (arguments
        (string-join
          (map (l (e) (if (list? e) (string-join (map symbol->string e) " ") (symbol->string e)))
            (map first (filter unnamed-option? spec)))
          " "))
      (if (string-null? arguments) arguments (string-append "arguments: " arguments "\n"))))

  (define (add-typecheck type c)
    (if type
      (l (opt name value r)
        (if value
          (let (value-after (typecheck+conversion type value))
            (if value-after (c opt name value-after r)
              (begin
                (simple-format #t
                  "error wrong-type-for-argument option-name:~A expected-type:~A given-argument:~S\n"
                  name type value)
                (exit 1)))
            )
          (c opt name value r)))
      c))

  (define* (option->args-fold-option a)
    (apply
      (lambda*
        (name #:optional names
          required? value-required value-optional input-type description custom-processor)
        (option
          (let
            ( (names (if names (if (list? names) names (list names)) names))
              (name-string (symbol->string name)))
            (if names (if (contains? names name-string) names (append names (list name-string)))
              (list (symbol->string name))))
          value-required value-optional
          (add-typecheck input-type
            (l (opt matched-name a r)
              (if custom-processor (custom-processor opt matched-name a r)
                (pair (pair name (if a a #t)) r))))))
      a))

  (define (config->options-default-options a)
    "alist:config -> (procedure:{options -> list:extended-options/false})"
    (list
      (l (options)
        (pass-if (alist-ref a (q version))
          (l (version-spec)
            (pair
              (list (q version) (q (#\v "version"))
                #f #f #f #f #f (display-version-proc version-spec))
              options))))
      (l (options)
        (pass-if (alist-ref a (q about))
          (l (text) (pair (list (q about) #\a #f #f #f #f #f (display-about-proc text a)) options))))
      (l (options)
        (let
          ( (help-option (q (help #\h #f #f #f #f #f)))
            (cli-option (q (interface #f #f #f #f #f #f))))
          (let*
            ( (options-temp (pairs cli-option help-option options))
              (options
                (pair
                  (append help-option
                    (list (display-help-proc (alist-ref a (q help)) a options-temp)))
                  options)))
            (pair (append cli-option (list (display-command-line-interface-proc a options-temp)))
              options))))))

  (define (config->options a options-config) "config list -> list"
    (remove unnamed-option?
      (fold (l (default-option-proc options) (or (default-option-proc options) options))
        (if options-config options-config (list)) (config->options-default-options a))))

  (define (match-unnamed-options spec unnamed parsed)
    "list list list:matches -> (list:rest list:matches)"
    (let (pattern (first spec))
      (apply
        (l (match rest)
          (if match (list rest (pair match parsed))
            (if (and (> (length spec) 2) (list-ref spec 2))
              (let (unnamed-length (length unnamed))
                (throw (q missing-arguments) (- (pattern-match-min-length pattern) unnamed-length)
                  (drop pattern unnamed-length)))
              (list unnamed parsed))))
        (split-by-pattern pattern unnamed))))

  (define (process-unnamed-options parsed-options unnamed-option-specs)
    (call-with-values (l () (partition string? parsed-options))
      (l (unnamed named)
        (apply
          (l (unnamed parsed)
            (append (if (null? unnamed) unnamed (list (pair (q unnamed) unnamed)))
              (apply append parsed) named))
          (fold-multiple match-unnamed-options unnamed-option-specs unnamed (list))))))

  (define (keyword-list->alist a)
    (list->alist (map (l (e) (if (keyword? e) (keyword->symbol e) e)) a)))

  (define (check-required a spec) "list:options option-spec ->"
    (let*
      ( (given (alist-keys a))
        (missing
          (map first
            (filter
              (l (e)
                (and (if (> (length e) 2) (list-ref e 2) #f) (not (containsv? given (first e)))))
              spec))))
      (if (null? missing) a (throw (q missing-arguments) (length missing) missing))))

  (define (default-missing-arguments-handler key count option-names)
    (format #t "~a missing argument~p ~s\n" count count option-names) (exit 1))

  (define (cli-create . config)
    "::
     #:version string/(integer ...)
     #:about string/procedure:{-> string}
     #:help string
     #:help-arguments string/boolean
     #:arguments (string ...)
     #:options ((symbol/list [character/string/(character/string ...) boolean boolean
     #:missing-arguments-handler procedure:{symbol any ...}
     boolean symbol/(symbol ...) string procedure]) ...)
     ->
     procedure:{string ... -> alist:((symbol . any) ...):parsed-arguments}

     data-structures
     custom-processor: args-fold-processor:{opt matched-name any result ->}
     input-type-names: symbol:string/number/integer
     option: (name/pattern alternative-names required? value-required value-optional input-type description custom-processor)
     pattern: (symbol symbol/ellipsis:... ...)"
    (let*
      ( (config (keyword-list->alist config)) (options-config (alist-ref config (q options)))
        (options (config->options config options-config)))
      (let
        ( (missing-arguments-handler
            (let (v (alist-ref config (q missing-arguments-handler) (q undefined)))
              (if (eqv? (q undefined) v) default-missing-arguments-handler (and (procedure? v) v))))
          (unnamed-options (if options-config (filter unnamed-option? options-config) (list))))
        (l arguments
          (let
            (proc
              (l ()
                (check-required
                  (process-unnamed-options
                    (reverse
                      (args-fold (if (null? arguments) (tail (program-arguments)) arguments)
                        (map option->args-fold-option options) unrecognized-processor
                        unnamed-processor (list)))
                    unnamed-options)
                  options)))
            (if missing-arguments-handler
              (catch (q missing-arguments) proc missing-arguments-handler) (proc))))))))
