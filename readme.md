# sph-lib
see [sph-lib](http://sph.mn/content/187).
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

## extended
if you intend to use (sph process create) also call the following before install:
```
./exe/compile
```

this is unfortunately necessary because guile does not come with a generic process creation procedure and it can not be adequately implemented in scheme.

## install destinations
* /usr/share/guile/site/sph/*
* /usr/lib/libguile-sph-lib.so

# libraries
## highlights
(sph) - bindings that are fundamental to all sph libraries
(sph base91) - encoder/decoder
(sph cli) - create command-line interfaces
(sph documentation) - extract and display guile scheme code documentation
(sph filesystem asset-compiler) - process and merge files of various formats into one
(sph filesystem versioning) - gives a path to the next version of a file and automatically archives the old version
(sph install) - program and library installer
(sph lang itml)
(sph lang itpn)
(sph lang parser type-signature)
(sph lang plcss) - s-expression language that compiles to css
(sph lang scm-format) - format scheme code
(sph lang template) - generic s-expression template processor
(sph libmagic) - binding to the libmagic library from the "file" utility that guesses file types
(sph process) - execute programs and evaluate shell or scheme code
(sph process create) - create child processes and process chains
(sph random-data) - generate random data. strings, booleans, lists, bytevectors, characters, ...
(sph record) - vectors as records
(sph scgi) - scgi interface. a server that accepts and parses scgi requests
(sph scrypt) - use the scrypt key derivation function. depends on https://github.com/jkalbhenn/scrypt
(sph server) - a generic socket based server
(sph test) - automated code testing with composable modules
(sph test performance) - adaptive performance testing and comparison of procedures with formatted result display
(sph thread-pool) - generic thread-pool that uses signal-conditions and has a customisable queue type
(sph time) - time as integers of international atomic time (tai) nanoseconds since the unix epoch and a vector date object. conversions for utc and the gregorian calendar
(sph time gregorian) - gregorian calendar calculations
(sph time rfc3339) - parse and create strings in the rfc3339 time format
(sph vector selection) - create and analyse selections from sets: permutations, combinations, n-tuples
(sph web atom) - create atom syndication feeds with sxml

## other
(sph alist) - association list processing
(sph base64) - encoder/decoder
(sph binding-info) - get information about bindings in modules
(sph char-set-vector) - redefines guiles preset char-sets that are special objects as vectors. experimental
(sph conditional) - branching
(sph config) - program configuration file management
(sph debug) - debugging helpers. experimental
(sph deprecation) - display a deprecation warning when specific procedures are used. experimental
(sph documentation display-format-itpn)
(sph documentation display-format-plist)
(sph documentation display-format-signature)
(sph documentation itpn)
(sph documentation shtml)
(sph error) - an error object type
(sph exception) - rnrs exception helpers. experimental
(sph filesystem)
(sph filesystem watch) - observing and acting on file-system changes
(sph git)
(sph guile-dbi lib)
(sph guile-dbi sql)
(sph hashtable)
(sph hashtable one)
(sph ice-9-stream)
(sph interface-format) - for programs that offer textual input/output in multiple formats. experimental
(sph json) - a basic but fast json writer
(sph lang docl) - evaluate templates with a specific scheme environment, state values and circular inclusion protection
(sph lang docl env default)
(sph lang docl env itml-to-plaintext)
(sph lang docl env itml-to-shtml)
(sph lang docl itml)
(sph lang docl itml-to-plaintext)
(sph lang docl itml-to-shtml)
(sph lang ecmascript expressions) - create ecmascript syntax strings
(sph lang indent-syntax)
(sph lang itml read)
(sph lang itml write)
(sph lang parser outline) - parse text with nested headings
(sph lang scm-format base)
(sph lang scm-format format)
(sph lang scm-format transform)
(sph lang sxml-element-style-css)
(sph linux) - linux specific features
(sph list)
(sph list one)
(sph log) - diagnostic logging with routing by category to none or many configurable output-targets
(sph module) - guile module system or rnrs library related procedures
(sph number)
(sph one) - various
(sph read-write) - generic port reading/writing
(sph replacement-table) - replacement tables like (key replacement ...) loaded from files or ports
(sph set) - hashtables as sets
(sph sql) - create sql-statements from scheme data
(sph stream)
(sph string) - string processing. includes string-replace-string, a fast replacer
(sph system reader) - a scheme reader that can include comments
(sph test base)
(sph test cli)
(sph test report)
(sph threads)
(sph time stream) - create an srfi-41 stream of calendar dates between two dates
(sph time string) - time string conversions
(sph time utc) - utc related time calculations
(sph tree) - processing tree-like list structures
(sph tree two)
(sph two) - various bindings deemed less useful than the ones in (sph one)
(sph uniform-vector)
(sph user-cli) - text based user interaction. for example choices or confirmations
(sph vector)
(sph vhash)
(sph web html)
(sph web http)
(sph web shtml)
