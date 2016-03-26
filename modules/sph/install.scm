; (sph install) - file installation/copy helpers
; written for the guile scheme interpreter
; Copyright (C) 2015-2016 sph <sph@posteo.eu>
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

(library (sph install)
  (export
    install
    install-cli-guile
    install-cli-guile-p
    install-one
    install-p)
  (import
    (sph common))

  (define default-mode-directory 493)
  (define default-mode-regular 420)
  (define default-path-lib-scheme "/usr/share/guile/site")
  ;data-structure
  ;  destination: string:path/symbol:placeholder/(string/symbol ...):concatenated-path
  ;  source: string:path/symbol:placeholder/integer:mode-for-following/(string:path/symbol:placeholder ...):concatenated-path
  ;  install-spec: (destination source ...)

  (define (select-mode-by-file-type a mode-regular mode-directory)
    (if (eqv? (q directory) (stat:type (stat a))) mode-directory mode-regular))

  (define (every-mode-and-full-paths proc a)
    "procedure:{integer:mode (string:path ...)} list:install-specs"
    (every
      (l (paths)
        (let (maybe-mode (first paths))
          (if (integer? maybe-mode) (proc maybe-mode (map path->full-path (tail paths)))
            (proc #f (map path->full-path paths)))))
      (group-split-at-matches integer? a)))

  (define (dry-run-log . a) (display (string-join (map any->string a) " ")) (newline) #t)

  (define (system-cp-proc path-destination symlink? dry-run?)
    (l (paths-source)
      (apply (if dry-run? dry-run-log execute+check-result) "cp"
        (qq
          ("--recursive" "--remove-destination"
            (unquote-splicing (if symlink? (list "--symbolic-link") (list)))
            (unquote-splicing paths-source) (unquote path-destination))))))

  (define*
    (install-one destination sources #:key (path-destination-prefix "") symlink?
      (mode-directory default-mode-directory)
      (mode-regular default-mode-regular)
      dry-run?)
    "automatically creates missing directories in target and sets permissions to mode-directory.
    copies source files to target and sets permissions corresponding to mode-directory and mode-regular unless overridden in sources.
    prepends path-destination-prefix to all target paths.
    symlink source files to the destination instead of copying if \"symlink?\" is true.
    currently depends on the \"cp\" utility"
    (let*
      ( (destination (apply path-append path-destination-prefix (any->list destination)))
        (system-cp (system-cp-proc destination symlink? dry-run?)))
      ;without the umask setting the mode might not apply as specified
      (if (not dry-run?)
        (begin (umask 0) (ensure-directory-structure-and-new-mode destination mode-directory)))
      (every-mode-and-full-paths
        (l (mode-explicit paths)
          ;"cp" is not that helpful here. it can not set permissions for files or directories,
          ;and force fails on symlinks on tmpfs
          (and (system-cp paths)
            (every
              (l (path-destination path-source)
                (and
                  (if (is-directory? path-source)
                    ;set permissions for the directory structure created from the source path
                    (fold-directory-tree
                      (let (path-source-length (string-length path-source))
                        (l (path stat-info r)
                          (let
                            (path
                              (path-append path-destination (string-drop path path-source-length)))
                            (if (eqv? (q directory) (stat:type stat-info))
                              (false-if-exception
                                ( (if dry-run? (l a (apply dry-run-log "chmod" a)) chmod) path
                                  (or mode-explicit mode-directory)))))))
                      #t path-source)
                    #t)
                  ( (if dry-run? (l a (apply dry-run-log "chmod" a)) chmod) path-destination
                    (or mode-explicit
                      (select-mode-by-file-type path-source mode-regular mode-directory)))))
              (map (l (e) (path-append destination (basename e))) paths) paths)))
        sources)))

  (define (install-p install-one-arguments install-specs)
    "list list -> boolean
    install multiple files or directory trees with files.
    automatically creates missing directories in target and sets new directory permissions to default or custom specified values.
    currently depends on the \"cp\" utility"
    (every (l (e) (apply install-one (first e) (tail e) install-one-arguments)) install-specs))

  (define-syntax-rule (install (install-one-arguments ...) install-spec ...)
    (install-p (list install-one-arguments ...) (qq (install-spec ...))))

  (define (install-specs-translate-guile-placeholders a path-lib-scheme)
    (map-apply
      (l (destination . source)
        (pair
          (if (list? destination) (replace-value destination (q path-lib-scheme) path-lib-scheme)
            (if (eqv? (q path-lib-scheme) destination) path-lib-scheme destination))
          source))
      a))

  (define (optional-keyword-argument keyword value) (if value (list keyword value) (list)))
  ;ame/pattern alternative-names required? value-required value-optional input-type description custom-processor
  (define (octal-integer->decimal a) (string->number (number->string a) 8))

  (define install-cli-guile-p
    (let
      ( (command-line-interface
          (cli-create #:options
            (qq
              ( (prefix #f #f #t #f string "prepended to each destination path")
                (path-lib-scheme #f #f
                  #t #f
                  string
                  (unquote
                    (string-append "path for installed guile modules. default is "
                      (string-quote default-path-lib-scheme))))
                (symlink #f #f #f #f #f "create symlinks instead of file copies")
                (dry-run #f #f
                  #f #f #f "make no changes and only show the commands that would be executed")
                (mode-directory #f #f #t #f integer "default permissions in octal notation")
                (mode-regular #f #f #t #f integer "default permissions in octal notation"))))))
      (l (program-arguments install-specs)
        "((list/string string ...) ...) -> boolean:success-status
        a command-line interface for installation scripts for guile based projects. currently depends on the \"cp\" command-line utility.
        parses command-line arguments and installs source files to destinations given via \"install-specs\".
        can install multiple files and directories with default or custom filesystem permissions.
        the symbol \"path-lib-scheme\" can be used as a placeholder in \"install-specs\".
        see also \"install\"
        usage example:
        (install-cli-guile (\"/usr/lib\" \"temp/libguile-dg.so\")
          ((path-lib-scheme \"test\") \"test/sph\"))"
        (let (arguments (command-line-interface program-arguments))
          (alist-quoted-bind arguments
            (prefix path-lib-scheme symlink mode-directory mode-regular dry-run)
            (install-p
              (append (list #:symlink? symlink #:dry-run? dry-run)
                (optional-keyword-argument #:path-destination-prefix prefix)
                (optional-keyword-argument #:mode-directory
                  (pass-if mode-directory octal-integer->decimal))
                (optional-keyword-argument #:mode-regular
                  (pass-if mode-regular octal-integer->decimal)))
              (install-specs-translate-guile-placeholders install-specs
                (or path-lib-scheme default-path-lib-scheme))))))))

  (define-syntax-rule (install-cli-guile install-spec ...)
    (install-cli-guile-p (tail (program-arguments)) (qq (install-spec ...)))))
