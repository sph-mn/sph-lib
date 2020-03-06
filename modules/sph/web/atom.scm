(define-module (sph web atom))

(use-modules (sph) (sph time rfc3339)
  ((guile) #:select (filter identity)) ((sph other) #:select (remove-keyword-associations)))

(export atom-author atom-category
  atom-content-source atom-content-text
  atom-content-xhtml atom-contributor
  atom-entry atom-feed
  atom-link atom-person atom-text atom-text-xhtml atom-title sph-web-atom-description)

(define sph-web-atom-description
  "create atom syndication feeds with sxml
   https://en.wikipedia.org/wiki/Atom_%28standard%29#Atom_compared_to_RSS_2.0
   http://atomenabled.org/developers/syndication/
   https://tools.ietf.org/html/rfc4287")

(define-syntax-rule (if-pass-string a proc) (if (string? a) (proc a) a))
(define (integer/string->rfc3339 a) (if (string? a) a (utc->rfc3339 a)))

(define-syntax-rules sxml-element ((name value) (list (q name) value))
  ((name-and-value) (list (q name-and-value) name-and-value)))

(define-syntax-rules sxml-element-optional ((name value) (and value (sxml-element name value)))
  ((name-and-value) (and name-and-value (sxml-element name-and-value))))

(define* (atom-person author? name #:key email uri)
  "boolean [#:email string #:uri string] -> sxml
   author or contributor"
  (pairs (if author? (q author) (q contributor)) (sxml-element name)
    (filter identity (list (sxml-element-optional email) (sxml-element-optional uri)))))

(define (atom-author . a) "name [#:email string #:uri string] -> sxml" (apply atom-person #t a))

(define (atom-contributor . a) "name [#:email string #:uri string] -> sxml"
  (apply atom-person #f a))

(define* (atom-category term #:key label scheme) "string [#:scheme string #:label string] -> sxml"
  (list (q category)
    (pairs (q @) (sxml-element term)
      (filter identity (list (sxml-element-optional scheme) (sxml-element-optional label))))))

(define* (atom-text tag #:key (type text) . content)
  "symbol [#:type string] sxml ... -> sxml
   examples for type are \"text\", \"html\", \"xhtml\""
  (qq
    ( (unquote tag) (@ (type (unquote type)))
      (unquote-splicing (remove-keyword-associations content)))))

(define* (atom-text-xhtml tag . content)
  "symbol sxml ... -> sxml
   wraps content in a div as required and sets the type appropriately"
  (qq
    ( (unquote tag) (@ (type "xhtml"))
      (div (@ (xmlns "http://www.w3.org/1999/xhtml")) (unquote-splicing content)))))

(define* (atom-link href #:key hreflang length title type rel)
  "string [#:rel string #:type string #:hreflang string #:title string #:length integer] -> sxml"
  (list (q link)
    (pairs (q @) (sxml-element href)
      (filter identity
        (list (sxml-element-optional rel) (sxml-element-optional type)
          (sxml-element-optional hreflang) (sxml-element-optional title)
          (sxml-element-optional length))))))

(define* (atom-content-source src #:key type) "string [#:type string] -> sxml"
  (list (q content)
    (pairs (q @) (sxml-element src) (filter identity (list (sxml-element-optional type))))))

(define* (atom-content-text . a) "[#:type string] sxml ... -> sxml" (apply atom-text (q content) a))

(define* (atom-content-xhtml . a)
  "sxml ... -> sxml
   wraps content in a div as required and sets the type appropriately"
  (apply atom-text-xhtml (q content) a))

(define* (atom-rights . atom-text-arguments) "[#:type string] string/sxml ... -> sxml"
  (apply atom-text (q rights) atom-text-arguments))

(define* (atom-summary . atom-text-arguments) "[#:type string] string/sxml ... -> sxml"
  (apply atom-text (q summary) atom-text-arguments))

(define* (atom-subtitle . atom-text-arguments) "[#:type string] string/sxml ... -> sxml"
  (apply atom-text (q subtitle) atom-text-arguments))

(define*
  (atom-entry id title updated #:key authors categories content contributors link published rights
    source
    summary)
  "::
   string string integer:utc-posix-time/string
   [#:authors string/atom-author/(atom-author ...)
   #:categories string/atom-category/(atom-category ...)
   #:contributors string/atom-contributor/(atom-contributor ...)
   #:link string/atom-link
   #:published integer:utc-posix-time/string
   #:source sxml
   #:summary text/atom-summary]
   ->
   sxml"
  (pairs (q entry) (sxml-element title)
    (sxml-element updated (integer/string->rfc3339 updated)) (sxml-element id)
    (filter identity
      (list (sxml-element-optional published (and published (integer/string->rfc3339 published)))
        (if-pass-string link atom-link) (if-pass-string authors atom-author)
        (if-pass-string contributors atom-contributor) (if-pass-string categories atom-category)
        (if-pass-string summary atom-summary) (if-pass-string rights atom-rights) source content))))

(define*
  (atom-feed id title updated #:key categories contributors generator icon link logo rights subtitle authors . content)
  "::
   string/number string integer:utc-posix-time/string
   [#:authors string/atom-author/(atom-author ...)
   #:link string/atom-link
   #:categories string/atom-category/(atom-category ...)
   #:contributors string/atom-contributor/(atom-contributor ...) #:generator string
   #:icon string #:logo string #:rights string/atom-rights/(atom-rights ...)
   #:subtitle string/atom-subtitle/(atom-subtitles ...)]
   ->
   sxml"
  (pairs (q feed) (q (@ (xmlns "http://www.w3.org/2005/Atom")))
    (sxml-element title) (sxml-element updated (integer/string->rfc3339 updated))
    (sxml-element id)
    (append
      (filter identity
        (list (if-pass-string link atom-link) (if-pass-string authors atom-author)
          (if-pass-string categories atom-category) (if-pass-string contributors atom-contributor)
          (if-pass-string rights atom-rights) (if-pass-string subtitle atom-subtitle)
          (sxml-element-optional icon) (sxml-element-optional logo) (sxml-element-optional generator)))
      (remove-keyword-associations content))))
