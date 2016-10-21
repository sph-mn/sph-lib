# sph-lib
see [sph-lib](http://sph.mn/content/187).
# dependencies
* [gnu guile](https://www.gnu.org/software/guile/) >= 2
* for the installation executable: shell
# installation
```
su root
./exe/install
```
see more options using
```
./exe/install --help
```
# non-exhaustive list of included libraries
* (sph alist) association list processing utilities
* (sph base64) encoder/decoder
* (sph base91) encoder/decoder
* (sph cli) quickly initialise a command-line interface
* (sph common) includes commonly used modules
* (sph conditional)
* (sph config) program configuration file management
* (sph documentation) extracting and formatting guile scheme code documentation
* (sph filesystem versioning) gives a path to the next version of a file and automatically archives the old version
* (sph filesystem watch) observing and acting on file-system changes
* (sph filesystem)
* (sph hashtable) hashtable processing
* (sph install) program installer
* (sph json) a basic but fast scheme-datum to json writer
* (sph lang docl itml-to-sxhtml)
* (sph lang docl) generic document language processor
* (sph lang ecmascript expressions) creating ecmascript syntax strings
* (sph lang indent-syntax)
* (sph lang parser itml)
* (sph lang parser outline)
* (sph lang parser type-signature)
* (sph lang scm-format) formats scheme-code
* (sph lang template) generic s-expression template processor
* (sph libmagic) a binding to the libmagic file-type guessing library that is used by the common "file" utility
* (sph list) list processing
* (sph log) routing log-messages with categories to none or many output-targets
* (sph module) guile module system or r6rs-library related procedures
* (sph number)
* (sph one) example implementations of various procedures
* (sph process) create processes and process chains like bash with pipes
* (sph random-data)
* (sph read-write) generic port reading/writing
* (sph record) vectors as records
* (sph scgi)
* (sph scrypt)
* (sph server) a generic socket-based server
* (sph set) hashtables as sets
* (sph sql) create sql-statements from scheme data structures
* (sph string) string processing. includes string-replace-string - a fast replacer procedure
* (sph system reader) a scheme-datum reader that can include comments
* (sph test performance) adaptive performance comparisons of procedures with command-line result display
* (sph test) automated code testing
* (sph thread-pool) generic thread-pool that uses signal-conditions and has a customisable queue
* (sph time gregorian) gregorian calendar calculations
* (sph time rfc3339) parse and create strings in the rfc3339 time format
* (sph time string) time string conversions
* (sph time utc) utc related time calculations
* (sph time) time as integers of international atomic time (tai) nanoseconds since the unix epoch and a vector date object. selected conversions for utc and the gregorian calendar
* (sph tree) processing tree-like list structures
* (sph two)
* (sph uniform-vector)
* (sph user-cli) procedures for user-oriented/interactive command-line interfaces
* (sph vector) vector processing
* (sph web atom) create atom syndication feeds with sxml
* (sph web html)
* (sph web http) procedures for working with http
* (sph) bindings fundamental to all sph libraries
* plcss s-expression language that compiles to css
