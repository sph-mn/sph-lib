(define-module (sph lang parser type-signature))
(use-modules (ice-9 peg) (sph) (sph list) (sph tree) (srfi srfi-1))

(export parsed-type-signature->string string->parsed-type-signature
  type-signature-simplify-tree (sig . peg-type-signature))

(define sph-lang-parser-type-signature-description
  "a parser and writer for a type signature notation.
   input/output
     expressions separated by: \"->\"
     example: input -> output
     example: input ->
     output arguments are optional. if left-out the output is unspecified
   multiple arguments
     expressions separated by a space character: \" \"
     example: a b c -> d
   alternative data-types
     expressions separated by a slash: \"/\"
     example: a b/c/d -> e
     example: a port/string/integer -> e
   alternative names
     expressions separated by a colon: \":\"
     example: a b:c:d -> e
     data-types always come first
     data-type:helpful-alternative-name:another-alternative-name
     example: a port:input-port:source -> e
   optional arguments
     enclosed with square brackets: []
     example: a [b c] ->\td
   repetition
     none or many consecutive occurences of an expression
     expression followed by a space character and three dots: \" ...\"
     example: a b ... c ... -> d
   function-name specification
     identifier followed by a space character and two colons: \" ::\"
     example: name :: a b -> c
   procedures, dictionaries
     a function signature enclosed with curly brackets: \"{\" \"}\"
     unlimited nesting
     example: {a ->}
     example with data-type: procedure:{a ... -> list:b}
     example: hashtable:{key -> value}
   lists, vectors, pairs, and more
     using scheme read syntax
     example list: (a (b c))
     example vector: #(a #(b c))
     example using both: (integer #(char ...))
   multiline signatures
     two colons followed by a newline, optionally followed by one or multiple
     input expressions per line separated by newlines, a line with only a \"->\",
     optionally followed by output expression separated by newlines,
     ending with end-of-string or two successive newline characters
     example
       ::
       a
       b
       ->
       c

       line outside of signature
   examples
     list any [symbol:exclusive/inclusive] -> (list:left list:right)
     procedure:{any -> boolean} procedure:{list:matched-elements -> list:replacements} list:source -> list
   multiline example
     ::
     procedure:{alist:header procedure:fold-lines:{string:line any:result procedure:next:{any:result -> any} -> any} result -> any}
     procedure:{header port result ->} any port [string]
     ->
     any")

(define-peg-pattern ignored-space none " ")
(define-peg-pattern ignored-ellipsis none "...")
(define-peg-pattern ignored-bracket-opening none "[")
(define-peg-pattern ignored-bracket-closing none "]")
(define-peg-pattern ignored-brace-opening none "{")
(define-peg-pattern ignored-brace-closing none "}")
(define-peg-pattern ignored-newline none "\n")
(define-peg-pattern ignored-arrow none "->")
(define-peg-pattern ignored-colon none ":")
(define-peg-pattern ignored-double-colon none "::")
(define-peg-pattern slash body "/")

(define-peg-pattern parenthetical body
  (and (? "#") "(" (* (or parenthetical (and (not-followed-by ")") peg-any))) ")"))

(define-peg-pattern common-delimiter none
  (or ignored-space ignored-newline
    ignored-brace-closing ignored-bracket-closing slash ignored-arrow ignored-colon))

(define-peg-pattern argument-identifier body
  (+ (or parenthetical (and (not-followed-by common-delimiter) peg-any))))

(define-peg-pattern argument-element body (or sig-procedure argument-identifier))
(define-peg-pattern sig-procedure all (and ignored-brace-opening sig ignored-brace-closing))
(define-peg-pattern alternatives all (and argument-element (* (and slash argument-element))))
(define-peg-pattern association all (and alternatives (+ (and ignored-colon alternatives))))

(define-peg-pattern argument body
  (+ (and (not-followed-by common-delimiter) (or association alternatives))))

(define-peg-pattern repetition-argument all (and argument ignored-space ignored-ellipsis))
(define-peg-pattern any-argument body (or repetition-argument argument))

(define-peg-pattern optional-arguments all
  (and ignored-bracket-opening any-argument
    (* (and ignored-space any-argument)) ignored-bracket-closing))

(define-peg-pattern arguments all
  (and (or optional-arguments any-argument)
    (* (and ignored-space (or optional-arguments any-argument)))))

(define-peg-pattern no-arguments all (and (or "" " ") (followed-by ignored-arrow)))

(define-peg-pattern sig-line-part body
  (or no-arguments (+ (and (not-followed-by (and ignored-space ignored-arrow)) arguments))))

(define-peg-pattern sig-line all
  (and sig-line-part
    (+ (and (? ignored-space) ignored-arrow (? (and ignored-space sig-line-part))))))

(define-peg-pattern sig-multiline-part all
  (+ (and (not-followed-by "\n\n") (or arguments ignored-newline ignored-space))))

(define-peg-pattern sig-multiline all
  (and ignored-double-colon (or ignored-newline ignored-space)
    sig-multiline-part ignored-arrow sig-multiline-part))

(define-peg-pattern sig all
  (or sig-multiline (and sig-line (* (and ignored-newline (* ignored-space) sig-line)))))

(define (type-signature-simplify-tree a)
  "this simplify a few cases like over-nested elements and no-arguments"
  (tree-map-lists
    (l (a)
      (case (first a)
        ( (sig-line)
          (let (a-last (last a))
            (if (and (list? a-last) (not (null? a-last)) (list? (first a-last)))
              (append (drop-right a 1) a-last) a)))
        ((no-arguments) (list (q arguments)))
        (else a)))
    a))

(define (string->parsed-type-signature a) "string -> list/boolean"
  (type-signature-simplify-tree (peg:tree (match-pattern sig a))))

(define* (parsed-type-signature->string a #:optional line-prefix)
  "list [string] -> string
   \"line-prefix\" could be indent space"
  (string-trim-both
    (let (line-delimiter (if line-prefix (string-append "\n" line-prefix) "\n"))
      (first
        (tree-map-lists
          (l (a)
            (case (first a)
              ((alternatives) (first (tail a)))
              ((arguments) (string-join (flatten (tail a)) " "))
              ( (sig-line)
                (let (a (tail a))
                  (if (= 1 (length a)) (string-append (first a) " ->") (string-join a " -> "))))
              ((sig-multiline) (string-append "::\n" (string-join (tail a) "\n->\n")))
              ((sig-multiline-part) (string-join (tail a) "\n"))
              ((sig-procedure) (string-append "{" (second a) "}"))
              ((sig) (string-join (flatten (tail a)) line-delimiter))
              ((repetition-argument) (string-append (second a) " ..."))
              ((association) (string-join (flatten (tail a)) ":"))
              ((optional-arguments) (string-append "[" (string-join (flatten (tail a)) " ") "]"))
              (else a)))
          (list a))))))
