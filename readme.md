# sph-lib
more than 80 gpl3+ licensed guile scheme modules.
example implementations of various procedures. use the library as is or extract code as needed.
see also [sph.mn](http://sph.mn/computer/software/sph-lib.html).

modules under highlights receive most attention and all issues are tried to be fixed immediately. other modules are to be seen as pools of examples and less mature, less useful features are more likely to be removed.

# dependencies
* [gnu guile](https://www.gnu.org/software/guile/) >= 2
* optional
  * for the installation executable: shell
  * for (sph process create): linux, gcc, glibc
  * for (sph scrypt): [scrypt](https://github.com/jkalbhenn/scrypt)

# installation
~~~
su root
./exe/install
~~~

default installation destination:
* /usr/share/guile/site

more forms:
~~~
./exe/install
./exe/install /usr/bin --symlink
./exe/install "" --symlink
~~~

## extended
if you intend to use (sph process create), also call the following before executing the installer:

default installation destinations:
~~~
./exe/compile-extension
~~~

default installation destination:
* /usr/lib/libguile-sph-lib.so

the extension exists because guile does not come with a generic process creation procedure and it seems that it can not be implemented reliable otherwise.

# modules
## highlights
* (sph base91) - encoder/decoder
* (sph cli) - create command-line interfaces
* (sph lang parser type-signature) - a parser and writer for a type signature notation
* (sph lang plcss) - s-expression language that compiles to css
* (sph lang scm-format) - format scheme code
* (sph libmagic) - binding to the libmagic library from the "file" utility that guesses file types
* (sph process create) - create child processes and process chains
* (sph scgi) - scgi interface. a server that accepts scgi requests and calls a custom procedure to handle them
* (sph scrypt) - bindings to the scrypt key derivation function
* (sph server) - a generic socket data processing server that uses a thread-pool for parallel request processing
* (sph string) - string processing. includes string-replace-string, a fast replacer
* (sph test) - automated code testing with composable modules
* (sph test performance) - adaptive performance testing with formatted result display
* (sph thread-pool) - thread-pool that uses wait-conditions to pause unused threads and has a customisable queue type
* (sph time) - time as tai or utc nanoseconds since the unix epoch or gregorian calendar dates
* (sph time gregorian) - gregorian calendar calculations
* (sph time rfc3339) - parse and create strings in the rfc3339 time format
* (sph web atom) - create atom syndication feeds with sxml

## more
* (sph alist) - association list processing
* (sph base64) - encoder/decoder
* (sph documentation) - extract and display documentation (bindings, arguments and docstrings) from modules
* (sph documentation itpn)
* (sph documentation shtml)
* (sph exception) - rnrs exception helpers. experimental
* (sph filesystem)
* (sph filesystem versioning) - gives a path to the next version of a file and automatically archives the old version
* (sph futures) - fine-grain parallelism based on (sph thread-pool)
* (sph hashtable) - rnrs-hashtable processing
* (sph install) - copy files and set permissions, with an optional, automatically created command line interface for users to set install options
* (sph io) - port and file input/output
* (sph io path-pipe-chain) - call procedures with input/output arguments in a chained manner to allow data flow between them
* (sph json) - a rudimentary and incomplete but fast json writer
* (sph lang config) - a scheme syntax configuration file format that parses to a alist or hashtable, possibly nested
* (sph lang indent-syntax) - converting to and from strings with indented lines
* (sph lang itpn) - helpers for working with a notation that lists space separated tags and then associated text indented in following lines
* (sph lang parser outline) - parse a markup structure where headings are prefixed by one or multiple characters for nested sections
* (sph lang scheme) - scheme parsing helpers including helpers for implicitly quasiquoted configuration files
* (sph lang scm-format base)
* (sph lang scm-format format) - formatters for individual expressions
* (sph lang scm-format transform) - transformations on the abstract syntax tree
* (sph list) - list helpers
* (sph list other) - additional list processing bindings that depend on libraries that depend on (sph list). to avoid circular dependencies
* (sph log) - diagnostic logging with routing by category to none or many configurable output-targets
* (sph math) - a few  mathematics related methods
* (sph module) - guile module system and rnrs library related procedures
* (sph module binding-info) - get information about bindings in modules
* (sph number)
* (sph other) - miscellaneous
* (sph process) - execute programs or evaluate shell or scheme code in subprocesses
* (sph selection) - create and analyse set selections: permutations, combinations and similar
* (sph server base)
* (sph server fibers)
* (sph spline-path) - composable interpolated paths through points
* (sph sql) - create sql-statements from scheme data
* (sph stream) - srfi-41 stream helpers
* (sph system reader) - a scheme reader that can include comments. depends on guile-reader
* (sph test base)
* (sph test report) - test reporters for writing to standard output for example while tests are running are implemented as a special hook
* (sph time stream) - create an srfi-41 stream of (sph time) date vectors between two dates
* (sph time string) - time string conversions
* (sph time utc) - utc related time calculations
* (sph tree) - process tree-like list structures
* (sph uniform-vector) - helpers for srfi-4 and compatible vectors. for example f32vector
* (sph vector) - vector processing
* (sph web html) - html related methods including a powerful html multipart form data parser
* (sph web http)
* (sph web shtml) - helpers to create html via sxml

# documentation
* most procedures and modules have docstrings. extracted documentation can be browsed on [sph.mn](http://sph.mn/computer/software/sph-lib.html)
* modules export the description as a string variable named like the module and "description" joined with minus. for example (sph vector) exports sph-vector-description
* "other/highlights" is a newline separated list of module names to include under "highlights" when creating a module listing with "exe/list-modules"

# automated tests
~~~
test module sph time rfc3339
  utc-ns-from-rfc3339 1 2
  utc-from-rfc3339 1 2 3 4
  utc->rfc3339 1 2 3
test module sph time string
  utc->ymd 1
  utc-duration-from-hms 1 2 3
  utc-current-ymd-ks 1
  utc-duration->hms 1 2 3 4 5 6 7 8 9 10 11
test module sph web atom
  atom-feed 1
test module sph lang parser outline
  read-outline 1 2 3 4 5 6
test module sph lang scm-format
  format-docstring 1
test module sph lang indent-syntax
  prefix-tree->indent-tree 1 2 3
test module sph io path-pipe-chain
  path-pipe-chain-ends 1
  path-pipe-chain-links 1
test module sph time
  greg-years->year 1 2 3 4 5
  greg-year->years 1 2 3 4 5 6 7
  greg-years->leap-days 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38
  greg-days->leap-days 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
  greg-days->year 1 2 3 4 5 6 7 8 9 10
  greg-years->days 1 2 3 4 5 6 7 8 9 10
  greg-days->years 1 2 3 4 5 6 7 8 9 10 11 12 13 14
  greg-year-leap-year? 1 2 3 4 5 6 7 8 9 10 11
  utc-start-last-week 1 2 3 4
  utc-start-year 1 2 3 4
  utc-start-day 1 2 3 4
  utc-start-month 1 2 3 4
  utc-start-week 1 2 3 4
  greg-year-weeks-53? 1 2 3
  greg-week-day 1
  greg-month->days 1 2 3 4 5
  greg-days->years-2 1
  greg-year-days->month-and-day& 1 2 3 4 5
  utc-start-first-week 1 2 3 4
  utc->years 1 2 3 4 5
  utc->week 1 2 3 4 5 6 7 8 9 10 11 12
  utc->week-day 1 2 3 4
  utc-from-date 1 2 3 4 5 6 7 8 9 10 11 12
  utc->date 1 2 3 4 5 6 7 8
test module sph base64
  base64-encode+decode 1
test module sph sql
  sql-value 1 2 3
  sql-create-table 1
  sql-select 1 2 3 4 5
  sql-insert 1
  sql-update 1
  sql-where-condition 1 2 3 4 5 6
test module sph filesystem
  set-up-filesystem-glob 1
  filesystem-glob 1 2 3 4 5 6 7
  directory-tree 1
  directory-tree-leaf-directories 1
  directory-prefix-tree 1
  path->full-path 1 2
  set-up-realpath* 1
  realpath* 1
  tear-down-realpath* 1
  filename-extension 1 2 3
  ensure-trailing-slash 1 2 3 4
  directory-reference? 1 2 3 4 5
  dotfile? 1 2 3 4 5 6 7
  path->list 1 2 3 4 5
  path-append 1 2 3 4 5 6 7 8 9 10
  path-append* 1 2 3 4 5 6 7 8
test module sph selection
  vector-distinct-maximum 1 2 3 4 5 6 7 8 9 10 11
  vector-selection-maximum 1 2 3 4 5
  vector-numeric-increment-be 1 2 3 4 5
  vector-selections 1 2
  vector-distinct-count 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  vector-distinct-stream 1
test module sph test-module
test module sph log
  log-message 1 2
test module sph string
  string-case 1
  any->string 1 2 3 4 5
  string-brackets-closed? 1 2 3 4 5
  string-camelcase->dash 1 2 3 4
  string-camelcase->underscore 1 2 3 4
  string-downcase-first 1 2 3
  string-drop-suffix 1 2
  string-indices 1 2 3 4 5 6 7 8 9
  string-indices-char 1
  string-join-tree 1
  string-longest-prefix 1
  string-lowercase? 1 2 3 4
  string-multiply 1 2 3
  string-numeric? 1 2 3 4
  parenthesised? 1 2 3 4 5 6 7 8 9
  string-quote 1 2 3 4 5
  string-replace-char 1 2 3
  string-replace-string 1 2 3 4 5 6 7 8 9
  string-skip-string 1
  string-trim-string 1
test module sph alist
  list->alist 1 2 3
  alist-merge 1
  alist-update 1
test module sph thread-pool
  thread-pool-create 1
test module sph test
  test-execute-procedures 1
  test-execute-module 1
test module sph base91
  base91 1 2
test module sph vector
  vector-append 1
  vector-range 1 2 3
  vector-select 1
  vector-relative-change-index/value 1 2 3
test module sph hashtable
  ht-tree-merge! 1
  ht-copy* 1
  ht-from-alist 1
  ht-alist 1
test module sph uniform-vector
  integer->bytevector 1 2
test module sph list
  flatten 1
  group-split-at-matches 1 2 3 4
  intersection 1 2 3 4
  difference 1 2 3 4 5
  replace-at-once 1 2
  complement 1 2 3 4 5 6 7
  interleave 1 2 3
  split-by-pattern 1 2 3 4 5 6
  pattern-match-min-length 1 2 3 4 5
  flat? 1 2 3 4 5 6
  count-value 1
  delete-duplicates-sorted 1 2
  group-consecutive 1
  fold-multiple-c 1 2
  list-select 1
  list-logical-contains? 1 2 3 4 5 6 7
  list-logical-condition? 1 2
  list-index-value 1 2
  list-sort-by-list 1 2 3
  map-consecutive 1 2
  map-segments 1 2 3 4
  produce 1 2 3 4
  contains? 1 2
  contains-all? 1 2 3 4
  contains-some? 1 2 3 4
  every-map 1 2
  list-replace-last 1
  list-replace-last-n 1
  map-last-n 1
  simplify 1 2 3
  convolve 1 2
  duplicates 1 2 3
test module sph spline-path
  spline-path? 1 2
  spline-path-constant? 1 2 3
  spline-path-infinite? 1 2
  spline-path 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
  spline-path-modify 1
  spline-path->procedure 1
  spline-path-combine 1
  spline-path-repeat 1
test module sph number
  float-sum 1
  number-container-length 1 2 3 4 5 6 7 8 9 10
  container-length->number-max 1 2
  round-to-increment 1 2 3 4 5 6 7
  round-to-decimal-places 1 2
  truncate-to-decimal-places 1 2 3
  integer-and-fraction 1
  in-between? 1 2 3 4 5
test module sph tree
  tree-find 1
  tree-any 1
  tree-fold-right 1 2
  tree-fold-depth 1
  tree-filter 1
  tree-filter-lists 1
  tree-map-leafs 1
  denoted-tree->prefix-tree 1 2 3 4 5 6 7 8
  prefix-tree->denoted-tree 1
  prefix-tree-produce-with-context 1 2 3 4 5 6 7 8 9 10 11
  prefix-tree-produce-with-context-mm 1 2 3 4 5 6
  prefix-tree-product 1 2 3 4
  prefix-tree-product-mm 1 2 3
  denoted-tree->tree 1 2 3 4 5
test module sph math
  bessel 1 2 3 4 5 6 7 8 9 10 11 12 13 14
  factorial 1 2 3 4 5 6
  taylor-series-sin 1 2 3
  relative-change 1 2 3 4 5 6 7 8 9
  integer-summands 1 2 3 4 5
  percent 1 2 3
  absolute-difference 1 2 3 4 5 6
  list-average 1
  list-center-of-mass 1
  list-median 1
  list-range 1
  list-mode 1
test module sph cli
  general 1
  required 1
test module sph io
  port->lines 1
~~~
