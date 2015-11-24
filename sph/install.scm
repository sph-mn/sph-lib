; (sph install) - file installation/copy helpers
; written for the guile scheme interpreter
; Copyright (C) 2010-2015 sph <sph@posteo.eu>
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
    install-multiple
    install-multiple-p)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph cli)
    (only (sph alist) alist-quoted-ref)
    (only (sph conditional) pass-if)
    (only (sph filesystem)
      path-append
      path->full-path
      ensure-directory-structure-and-mode)
    (only (sph process) execute+check-result))

  (define (install target target-prefix symlink? directory-mode . source)
    "string string boolean integer string ... -> boolean
    automatically creates missing directories in target and sets permissions to directory-mode.
    copies source files to target and preserve permissions.
    prepends target-prefix to all target paths.
    symlink source files to the destination instead of copying if \"symlink?\" is true.
    currently depends on the \"cp\" utility"
    (let*
      ( (target (path-append target-prefix target))
        (cp-arguments
          (qq
            ("--preserve=mode" "--recursive" "--force"
              (unquote-splicing (if symlink? (list "--symbolic-link") (list)))
              (unquote (string-append "--target-directory=" target))
              (unquote-splicing (map path->full-path source))))))
      ;without the umask the mode settings might not apply as specified
      (umask 0) (ensure-directory-structure-and-mode target directory-mode)
      (apply execute+check-result "cp" cp-arguments)))

  (define (install-multiple-p target-prefix symlink? directory-mode . target+source)
    "string boolean integer (string:target string:source ...) ... -> boolean
    install multiple files or directory trees with files.
    automatically creates missing directories in target and sets new directory permissions to directory-mode.
    copies source files to target while preserving permissions.
    prepends target-prefix to all target paths.
    symlinks source files to the destination instead of copying if \"symlink?\" is true.
    currently depends on the \"cp\" utility"
    (every (l (e) (apply install (first e) target-prefix symlink? directory-mode (tail e)))
      target+source))

  (define-syntax-rule
    (install-multiple target-prefix symlink? directory-mode (install-arguments ...) ...)
    (install-multiple-p target-prefix symlink? directory-mode (list install-arguments ...) ...))

  (define (cli-handle-optional a default) (if a (if (equal? "-" a) default a) default))

  (define install-cli-guile-p
    (let
      ( (get-program-arguments&
          (l (c)
            (apply
              (l* (#:optional target-prefix path-lib-scheme symlink? directory-mode)
                (c (cli-handle-optional target-prefix "")
                  (cli-handle-optional path-lib-scheme "/usr/share/guile/site")
                  (cli-handle-optional symlink? #f)
                  (pass-if (cli-handle-optional directory-mode 493)
                    (l (e) (if (string? e) (string->number directory-mode 8) e)))))
              (alist-quoted-ref
                ( (cli-create #:help-parameters
                    "parameters\n  options ... [target-prefix path-lib-scheme symlink? directory-mode]"))
                unnamed (list)))))
        (replace-placeholders
          (let*
            ( (handle-symbol-path
                (l (a path-lib-scheme) (if (eqv? (q path-lib-scheme) a) path-lib-scheme a)))
              (handle-list-path
                (l (a path-lib-scheme) "list string -> string:path"
                  (apply path-append (map (l (e) (handle-symbol-path e path-lib-scheme)) a))))
              (handle-target+source-one-proc
                (l (path-lib-scheme)
                  (l (e)
                    (let (target (first e))
                      (pair
                        (if (list? target) (handle-list-path target path-lib-scheme)
                          (if (symbol? target) (handle-symbol-path target path-lib-scheme) target))
                        (tail e)))))))
            (l (target+source path-lib-scheme)
              (map (handle-target+source-one-proc path-lib-scheme) target+source)))))
      (l target+source
        "((list/string string ...) ...) -> boolean:success-status
      a cli for installation scripts for guile based projects. currently depends on the \"cp\" command-line utility.
      parses command-line arguments and installs source files to destinations given in \"target+source\" using \"install-multiple\".
      the format for \"target+source\" is: (install-cli-guile (string:target/list:(string/symbol:placeholder ...) string:source ...) ...).
      \"target\" can be a list of strings or symbols which are interpreted as placeholders. currently the placeholder \"path-lib-scheme\" is supported.
      \"path-lib-scheme\" is the path where scheme libraries should be installed, it is /usr/share/guile/site if nothing else is specified.
      command-line arguments are: target-prefix:install-prefix path-lib-scheme:guile-site symlink?:true/false directory-mode.
      symlink? can be \"true\" or \"false\". directory-mode is an integer.
      all command-line arguments are optional and can be \"-\" to be set to the default value.
      see install-multiple for how source files are actually handled.
      usage example:
      (install-cli-guile (\"/usr/lib\" \"temp/libguile-dg.so\")
        ((path-lib-scheme \"test\") \"test/sph\"))"
        (get-program-arguments&
          (l (target-prefix path-lib-scheme symlink? directory-mode)
            (apply install-multiple-p target-prefix
              symlink? directory-mode (replace-placeholders target+source path-lib-scheme)))))))

  (define-syntax-rule (install-cli-guile target+source ...)
    ;see install-cli-guile-p
    (apply install-cli-guile-p (quasiquote (target+source ...)))))
