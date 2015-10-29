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
    cli-command-match
    cli-create
    cli-option-spec->list)
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
      list-prefix?
      pattern-match-min-length
      first-or-null
      iterate-with-continue
      containsv?
      fold-multiple
      split-by-pattern)
    (only (sph string) string-multiply any->string-write)
    (only (sph tree) prefix-tree-product)
    (only (srfi srfi-1)
      drop-right
      drop
      drop-while
      remove
      append-map
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

  (define (string-join-lines-with-indent a indent) "(string ...) string -> string"
    (string-join a (string-append "\n" indent) (q prefix)))

  (define (options->help-text-lines a) "list -> string"
    (map
      (l (e)
        (apply (if (unnamed-option? e) unnamed-option->help-text-line named-option->help-text-line)
          e))
      (list-sort (l (a b) (string< (symbol->string (first a)) (symbol->string (first b)))) a)))

  (define (options-spec->unnamed-arguments-string a)
    (string-join
      (map (l (e) (if (list? e) (string-join (map symbol->string e) " ") (symbol->string e)))
        (map first (filter unnamed-option? a)))
      " "))

  (define (commands->help-text-lines a) "list:commands-spec -> string"
    (map (l (e)
        (let ((command (if (string? e) e (string-join (first e) " "))) (command-arguments (tail e)))
          (let (options-spec (if (null? command-arguments)
                #f
                (if (null? (tail command-arguments))
                  (if (procedure? (first command-arguments)) #f command-arguments)
                  command-arguments
                  )))
            (string-append
              command
              (if options-spec (string-append " :: " (options-spec->unnamed-arguments-string options-spec)) ""))

            )

          )) a))

  (define (options-remove-processors a)
    "(single-option-spec ...) -> list
    remove processor procedures from option-specs"
    (map (l (e) (reverse (drop-while not (reverse (if (> (length e) 6) (drop-right e 1) e))))) a))

  (define (display-command-line-interface-proc config spec)
    (l args (write (options-remove-processors spec)) (exit 0)))

  (define indent "  ")



  (define (config->usage-text a spec)
    (let
      (arguments (options-spec->unnamed-arguments-string spec)
 )
      (if (string-null? arguments) arguments (string-append "arguments: " arguments "\n"))))

  (define (display-help-proc text commands config spec)
    (l (opt name a r)
      (display
        (string-append
          (identity-if (alist-ref config (q help-arguments)) (config->usage-text config spec))
          (if text (string-append "\n" text "\n") "") "\noptions"
          (string-join-lines-with-indent (options->help-text-lines (remove unnamed-option? spec))
            indent)
          "\n"
          (if commands
            (string-append "\ncommands"
              (string-join-lines-with-indent (commands->help-text-lines commands) indent) "\n")
            "")))
      (exit 0)))

  (define (display-about-proc text config)
    (l (opt name a r) (display (string-append (if (procedure? text) (text) text) "\n")) (exit 0)))

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
                (exit 1))))
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
                    (list
                      (display-help-proc (symbol-alist-ref a help) (symbol-alist-ref a commands)
                        a options-temp)))
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

  (define (process-unnamed-options parsed-options unnamed-option-specs) "list list ->"
    (call-with-values (l () (partition string? parsed-options))
      (l (unnamed named)
        (apply
          (l (unnamed parsed)
            (append (if (null? unnamed) unnamed (list (pair (q unnamed) unnamed)))
              (apply append parsed) named))
          (fold-multiple match-unnamed-options unnamed-option-specs unnamed (list))))))

  (define (check-required a spec) "list:options list:option-spec ->"
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
    "symbol integer integer (string ...) ->
    write message to standard output and exit"
    (format #t "~a missing argument~p ~s\n" count count option-names) (exit 1))

  (define (cli-command-match arguments commands-spec) "list list -> false/any"
    (any (l (e) (if (apply list-prefix? arguments (first e)) e #f)) commands-spec))

  (define (command-dispatch& command-handler arguments commands-spec c)
    "procedure/false list list/false procedure:{-> any} -> any
    if a command from commands-spec is matched at the beginning of the given cli arguments, eventually calls an associated handler procedure,
    and in any case calls the command-handler if available.
    if no command matches, proceeds with thunk \"c\""
    (if commands-spec
      (let (match (cli-command-match arguments commands-spec))
        (if match
          (apply
            (l (command . command-arguments)
              (let (rest-arguments (list-tail arguments (length command)))
                ( (or command-handler (l (a b) b)) command
                  (if (procedure? command-arguments) (command-arguments command rest-arguments)
                    (apply (apply cli-create command-arguments) rest-arguments)))))
            match)
          (c)))
      (c)))

  (define (cli-create . config)
    "::
     #:version string/(integer ...)
     #:about string/procedure:{-> string}
     #:help string
     #:help-arguments string/boolean
     #:arguments (string ...)
     #:missing-arguments-handler procedure:{symbol any ...}
     #:command-handler procedure:{list:(symbol ...):command-name list:rest-arguments -> any}
     #:commands commands-spec
     #:options ((symbol/list [character/string/(character/string ...) boolean boolean
         boolean symbol/(symbol ...) string procedure]) ...)
     option ...
     ->
     procedure:{string ... -> alist:((symbol . any) ...):parsed-arguments}

     data-structures
     custom-processor: args-fold-processor:{opt matched-name any result ->}
     input-type-names: symbol:string/number/integer
     option: (name/pattern alternative-names required? value-required value-optional input-type description custom-processor)
     pattern: (symbol symbol/ellipsis:... ...)
     commands-spec: (((string:command-name ...) procedure:{command arguments}/[cli-create-argument ...]) ...)"
    (let*
      ( (config+keyless (keyword-list->alist+keyless config)) (config (first config+keyless))
        (options-config
          (pass-if (alist-ref config (q options)) (l (a) (append a (tail config+keyless)))
            (tail config+keyless)))
        (options (config->options config options-config)))
      (let
        ( (missing-arguments-handler
            (let (v (alist-ref config (q missing-arguments-handler) (q undefined)))
              (if (eqv? (q undefined) v) default-missing-arguments-handler (and (procedure? v) v))))
          (unnamed-options (if options-config (filter unnamed-option? options-config) (list)))
          (options-spec (map option->args-fold-option options))
          (commands (alist-ref config (q commands)))
          (command-handler (alist-ref config (q command-handler))))
        (l arguments
          (let*
            ( (arguments (if (null? arguments) (tail (program-arguments)) arguments))
              (no-command-cli
                (let
                  (cli
                    (thunk
                      (check-required
                        (process-unnamed-options
                          (reverse
                            (args-fold arguments options-spec
                              unrecognized-processor unnamed-processor (list)))
                          unnamed-options)
                        options)))
                  (if missing-arguments-handler
                    (thunk (catch (q missing-arguments) cli missing-arguments-handler)) cli))))
            (command-dispatch& command-handler arguments commands no-command-cli)))))))