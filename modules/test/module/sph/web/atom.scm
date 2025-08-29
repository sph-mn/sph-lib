(define-test-module (test module sph web atom)
  (import
    (sph web atom))

  (define-test (atom-feed)
    (atom-feed "test-id" "test-title"
      1234567890 #:authors
      (list (atom-author "me") (atom-author "second me")) #:contributors
      (list (atom-contributor "me-me") (atom-contributor "second me-me")) #:categories
      (atom-category "test" #:scheme "test-scheme") #:link
      (atom-link "test-link" #:rel "a" #:type "b") #:rights
      "test"
      (atom-entry "test-entry-id" "test-entry-title"
        1234567890 #:authors
        (atom-author "me") #:contributors
        (atom-contributor "me-me") #:categories
        (atom-category "test-category") #:link
        (atom-link "test-url") #:published
        (- 1234567890 3000) #:rights "your-rights" #:summary "summary-text")))

  (test-execute-procedures-lambda
    (atom-feed ()
      (feed (@ (xmlns "http://www.w3.org/2005/Atom")) (title "test-title")
        (updated "2009-02-13T23:31:30Z") (id "test-id")
        (link (@ (href "test-link") (rel "a") (type "b")))
        ((author (name "me")) (author (name "second me")))
        (category (@ (term "test") (scheme "test-scheme")))
        ((contributor (name "me-me")) (contributor (name "second me-me")))
        (rights (@ (type "text")) "test")
        (entry (title "test-entry-title") (updated "2009-02-13T23:31:30Z")
          (id "test-entry-id") (published "2009-02-13T22:41:30Z")
          (link (@ (href "test-url"))) (author (name "me"))
          (contributor (name "me-me")) (category (@ (term "test-category")))
          (summary (@ (type "text")) "summary-text") (rights (@ (type "text")) "your-rights"))))))
