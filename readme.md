# sph-lib
see also [sph.mn](http://sph.mn/c/view/2u)

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

all that is necessary is to put the paths under modules/ into a directory that is listed in $GUILE_LOAD_PATH

## extended
if you intend to use (sph process create) also call the following before install:
```
./exe/compile-c
```

this is unfortunately necessary because guile does not come with a generic process creation procedure and it can not be adequately implemented in scheme.

## install destinations
* /usr/share/guile/site/sph/
* /usr/lib/libguile-sph-lib.so

# libraries
## highlights
* (sph base91) - encoder/decoder
* (sph cli) - create command-line interfaces
* (sph filesystem asset-compiler) - configuration format and helpers to concatenate/preprocess code from multiple sources
* (sph filesystem versioning) - gives a path to the next version of a file and automatically archives the old version
* (sph install) - program and library installer
* (sph io path-pipe-chain) - call procedures with input/output arguments in a chained manner to allow data flow between them
* (sph lang parser type-signature) - a parser and writer for the sph type signature notation
* (sph lang plcss) - s-expression language that compiles to css
* (sph lang scm-format) - format scheme code
* (sph lang template) - s-expression quasiquote template processor
* (sph libmagic) - binding to the libmagic library from the "file" utility that guesses file types
* (sph process) - execute programs and evaluate shell or scheme code
* (sph process create) - create child processes and process chains
* (sph random-data) - generate random data. strings, booleans, lists, bytevectors, characters, ...
* (sph record) - vectors as records
* (sph scgi) - scgi interface. a server that accepts scgi requests and calls a custom procedure to handle them
* (sph scrypt) - use the scrypt key derivation function. depends on https://github.com/jkalbhenn/scrypt
* (sph server) - a generic socket data processing server that uses a thread-pool for parallel request processing
* (sph test) - automated code testing with composable modules
* (sph test performance) - adaptive performance testing with formatted result display
* (sph thread-pool) - thread-pool that uses wait-conditions to pause unused threads and has a customisable queue type
* (sph time) - time as tai or utc nanoseconds since the unix epoch or gregorian calendar dates
* (sph time gregorian) - gregorian calendar calculations
* (sph time rfc3339) - parse and create strings in the rfc3339 time format
* (sph vector selection) - create and analyse selections from sets: permutations, combinations, n-tuples
* (sph web atom) - create atom syndication feeds with sxml

## other
* (sph alist) - association list processing
* (sph base64) - encoder/decoder
* (sph binding-info) - get information about bindings in modules
* (sph char-set-vector) - redefines guiles preset char-sets that are special objects as vectors. experimental
* (sph conditional) - branching
* (sph config) - program configuration file management. deprecated
* (sph debug) - debugging helpers. experimental
* (sph deprecation) - display a deprecation warning when specific procedures are used. experimental
* (sph documentation) - extract and display guile scheme code documentation
* (sph documentation display-format-itpn)
* (sph documentation display-format-plist)
* (sph documentation display-format-signature)
* (sph documentation itpn)
* (sph documentation shtml)
* (sph error) - an error object type
* (sph exception) - rnrs exception helpers. experimental
* (sph filesystem)
* (sph filesystem stream) - bindings that create or use srfi-41 streams for reading from directories
* (sph filesystem watch) - for acting on file-system changes. has not been updated in a while
* (sph git)
* (sph hashtable) - rnrs-hashtable processing
* (sph hashtable one) - more hashtable procedures
* (sph ice-9-stream)
* (sph interface-format) - for programs that offer textual input/output in multiple formats. experimental
* (sph io one) - port and file input/output
* (sph io read-write) - port io with specifying a read and write procedure
* (sph json) - a basic but fast json writer
* (sph lang config) - a scheme syntax configuration file format for associative data structures
* (sph lang ecmascript expressions) - create ecmascript syntax strings
* (sph lang indent-syntax) - converting to and from strings with indented lines
* (sph lang itpn) - helpers for working with parsed itpn
* (sph lang itml eval) - evaluate itml inline code expressions and possibly translate to a new format
* (sph lang itml eval env plaintext)
* (sph lang itml eval env shtml)
* (sph lang itml eval plaintext) - evaluate inline code expressions
* (sph lang itml eval shtml) - evaluate inline code expressions and translate itml to shtml
* (sph lang itml read) - parse itml
* (sph lang itml write) - create itml strings from parsed itml
* (sph lang parser key-values-table) - replacement tables like (pattern replacement ...) from strings read from files or ports
* (sph lang parser outline) - parse text with nested headings
* (sph lang scm-format base)
* (sph lang scm-format format) - formatters for individual expressions
* (sph lang scm-format transform) - transformations on the abstract syntax tree
* (sph linux) - linux specific features
* (sph list) - helpers for working with lists
* (sph list one) - additional list processing bindings that depend on libraries that depend on (sph list). to avoid circular dependencies
* (sph log) - diagnostic logging with routing by category to none or many configurable output-targets
* (sph module) - guile module system and rnrs library related procedures
* (sph number)
* (sph one) - various
* (sph server base)
* (sph server fibers) - a generic socket data processing server that uses fibers for parallel request processing and non-blocking port input/output
* (sph set) - hashtables as sets
* (sph sql) - create sql-statements from scheme data
* (sph stream) - srfi-41 stream helpers
* (sph string) - string processing. includes string-replace-string, a fast replacer
* (sph system reader) - a scheme reader that can include comments
* (sph test base)
* (sph test cli)
* (sph test report)
* (sph threads) - re-exports and renames some parallel processing bindings from (ice-9 threads). experimental
* (sph time stream) - create an srfi-41 stream of (sph time) date vectors between two dates
* (sph time string) - time string conversions
* (sph time utc) - utc related time calculations
* (sph tree) - processing tree-like list structures
* (sph tree two)
* (sph two) - various bindings deemed less useful than the ones in (sph one)
* (sph uniform-vector) - helpers for srfi-4 and compatible vectors
* (sph user-cli) - text based user interaction. for example choices or confirmations
* (sph vector) - vector processing
* (sph vhash)
* (sph web html)
* (sph web http)
* (sph web shtml) - helpers to create html as sxml

# documentation
* most procedures and modules have docstrings. extracted documentation can be browsed on [sph.mn](http://sph.mn/c/view/2u)
* modules export the description as a string variable named like the module name and "description" joined with minus. for example (sph vector) exports sph-vector-description
* "other/highlights" is a newline separated list of module names to include under "highlights" when creating a module listing with "exe/list-modules"
