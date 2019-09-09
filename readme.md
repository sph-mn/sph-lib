# sph-lib
more than 80 gpl3+ licensed guile scheme libraries.
example implementations of various procedures. use the library as is or extract the code you need.
see also [sph.mn](http://sph.mn/computer/software/sph-lib.html)

# dependencies
* [gnu guile](https://www.gnu.org/software/guile/) >= 2
* for the installation executable: shell
* for (sph process create): linux, libc, gcc
* for (sph scrypt): [scrypt](https://github.com/jkalbhenn/scrypt)
# installation
```
su root
./exe/install
```
see more options using
```
./exe/install --help
```

or copy or symlink all files modules/* into a directory in guiles load path

## extended
if you intend to use (sph process create) also call the following before install:
```
./exe/compile-c
```

this is unfortunately necessary because guile does not come with a generic process creation procedure and it apparently can not be adequately implemented in scheme

## install destinations
* /usr/share/guile/site/sph/
* /usr/lib/libguile-sph-lib.so

# libraries
## highlights
* (sph base91) - encoder/decoder
* (sph cli) - create command-line interfaces
* (sph filesystem)
* (sph io path-pipe-chain) - call procedures with input/output arguments in a chained manner to allow data flow between them
* (sph lang indent-syntax) - converting to and from strings with indented lines
* (sph lang parser type-signature) - a parser and writer for a type signature notation
* (sph lang plcss) - s-expression language that compiles to css
* (sph lang scm-format) - format scheme code
* (sph libmagic) - binding to the libmagic library from the "file" utility that guesses file types
* (sph list) - helpers for working with lists
* (sph process) - execute programs and evaluate shell or scheme code
* (sph process create) - create child processes and process chains
* (sph scgi) - scgi interface. a server that accepts scgi requests and calls a custom procedure to handle them
* (sph scrypt)
* (sph selection) - create and analyse set selections: permutations, combinations and similar
* (sph server) - a generic socket data processing server that uses a thread-pool for parallel request processing
* (sph spline-path) - interpolated paths through points
* (sph string) - string processing. includes string-replace-string, a fast replacer
* (sph test) - automated code testing with composable modules
* (sph test performance) - adaptive performance testing with formatted result display
* (sph thread-pool) - thread-pool that uses wait-conditions to pause unused threads and has a customisable queue type
* (sph time) - time as tai or utc nanoseconds since the unix epoch or gregorian calendar dates
* (sph time gregorian) - gregorian calendar calculations
* (sph time rfc3339) - parse and create strings in the rfc3339 time format
* (sph tree) - process tree-like list structures
* (sph web atom) - create atom syndication feeds with sxml

## other
* (sph alist) - association list processing
* (sph base64) - encoder/decoder
* (sph documentation) - extract and display guile scheme code documentation
* (sph documentation display-format-itpn)
* (sph documentation display-format-plist)
* (sph documentation display-format-signature)
* (sph documentation itpn)
* (sph documentation shtml)
* (sph exception) - rnrs exception helpers. experimental
* (sph filesystem versioning) - gives a path to the next version of a file and automatically archives the old version
* (sph futures) - fine-grain parallelism based on (sph thread-pool)
* (sph hashtable) - rnrs-hashtable processing
* (sph install) - program and library installer
* (sph io other) - port and file input/output
* (sph json) - a rudimentary but fast json writer
* (sph lang config) - a scheme syntax configuration file format that parses to a alist or hashtable, possibly nested
* (sph lang itpn) - helpers for working with parsed itpn
* (sph lang parser outline) - parse a document with possible nesting of sections where headings are prefixed by one or multiple characters
* (sph lang scheme)
* (sph lang scm-format base)
* (sph lang scm-format format) - formatters for individual expressions
* (sph lang scm-format transform) - transformations on the abstract syntax tree
* (sph list other) - additional list processing bindings that depend on libraries that depend on (sph list). to avoid circular dependencies
* (sph log) - diagnostic logging with routing by category to none or many configurable output-targets
* (sph math) - statistics and more
* (sph module) - guile module system and rnrs library related procedures
* (sph module binding-info) - get information about bindings in modules
* (sph number)
* (sph other) - miscellaneous
* (sph record) - *deprecated* vectors as records
* (sph server base)
* (sph server fibers) - a generic socket data processing server that uses fibers for parallel request processing and non-blocking port input/output
* (sph sql) - create sql-statements from scheme data
* (sph stream) - srfi-41 stream helpers
* (sph system reader) - a scheme reader that can include comments
* (sph test base)
* (sph test report)
* (sph time stream) - create an srfi-41 stream of (sph time) date vectors between two dates
* (sph time string) - time string conversions
* (sph time utc) - utc related time calculations
* (sph uniform-vector) - helpers for srfi-4 and compatible vectors. for example f32vector
* (sph vector) - vector processing
* (sph web html)
* (sph web http)
* (sph web shtml) - helpers to create html as sxml

# documentation
* most procedures and modules have docstrings. extracted documentation can be browsed on [sph.mn](http://sph.mn/computer/software/sph-lib.html)
* modules export the description as a string variable named like the module and "description" joined with minus. for example (sph vector) exports sph-vector-description
* "other/highlights" is a newline separated list of module names to include under "highlights" when creating a module listing with "exe/list-modules"
