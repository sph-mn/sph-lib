# sph-lib
more than 80 gpl3+ licensed guile scheme modules.
example implementations of various procedures. use the library as is or extract code as needed.
see also [sph.mn](http://sph.mn/computer/software/sph-lib.html)

modules under highlights receive most attention and all issues are tried to be fixed immediately. other modules are to be seen as pools of examples and less mature, less useful features are more likely to be removed

# dependencies
* [gnu guile](https://www.gnu.org/software/guile/) >= 2
* optional
  * for the installation executable: shell
  * for (sph process create): linux, libc, gcc
  * for (sph scrypt): [scrypt](https://github.com/jkalbhenn/scrypt)

# installation
## without the installer
* copy or symlink everything that is under "modules" into a directory listed when executing "guile -c '(display %load-path)'". a final example path is /usr/share/guile/site/sph/lang/sc.scm. also ensure that the user who will use the modules has the permissions to read the files
* copy or symlink exe/sc into a directory listed when executing "echo $PATH". ensure that the execute permission is set ("chmod +x", "ls -l" rwx)

## with the installer
```
su root
./exe/install
```
see more options using
```
./exe/install --help
```

default install destinations
* /usr/share/guile/site/sph/
* /usr/lib/libguile-sph-lib.so

## extended
if you intend to use (sph process create), also call the following before install:
```
./exe/compile-c
```

then ensure that temp/libguile-sph-lib.so is installed in the shared library directory, for example at /usr/lib/libguile-sph-lib.so

the extension exists because guile does not come with a generic process creation procedure and it seems that it can not be adequately implemented in scheme

## modules
* (sph base91) - encoder/decoder
* (sph cli) - create command-line interfaces
* (sph install) - copy files and set permissions, with an optional, automatically created command line interface for users to set install options
* (sph lang parser type-signature) - a parser and writer for a type signature notation
* (sph lang plcss) - s-expression language that compiles to css
* (sph lang scm-format) - format scheme code
* (sph libmagic) - binding to the libmagic library from the "file" utility that guesses file types
* (sph process create) - create child processes and process chains
* (sph scgi) - scgi interface. a server that accepts scgi requests and calls a custom procedure to handle them
* (sph scrypt)
* (sph server) - a generic socket data processing server that uses a thread-pool for parallel request processing
* (sph string) - string processing. includes string-replace-string, a fast replacer
* (sph test) - automated code testing with composable modules
* (sph test performance) - adaptive performance testing with formatted result display
* (sph thread-pool) - thread-pool that uses wait-conditions to pause unused threads and has a customisable queue type
* (sph time) - time as tai or utc nanoseconds since the unix epoch or gregorian calendar dates
* (sph time gregorian) - gregorian calendar calculations
* (sph time rfc3339) - parse and create strings in the rfc3339 time format
* (sph web atom) - create atom syndication feeds with sxml

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
* (sph process) - execute programs and evaluate shell or scheme code
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
* (sph web shtml) - helpers to create html as sxml

# documentation
* most procedures and modules have docstrings. extracted documentation can be browsed on [sph.mn](http://sph.mn/computer/software/sph-lib.html)
* modules export the description as a string variable named like the module and "description" joined with minus. for example (sph vector) exports sph-vector-description
* "other/highlights" is a newline separated list of module names to include under "highlights" when creating a module listing with "exe/list-modules"
