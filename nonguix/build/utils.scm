;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nonguix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-26)
  #:export (64-bit?
            make-desktop-entry-file
            make-wrapper
            concatenate-files))

(define (64-bit? file)
  "Return true if ELF file is in 64-bit format, false otherwise.
See https://en.wikipedia.org/wiki/Executable_and_Linkable_Format#File_header."
  (with-input-from-file file
    (lambda ()
      (= 2
         (array-ref (get-bytevector-n (current-input-port) 5) 4)))
    #:binary #t))

(define* (make-desktop-entry-file destination #:key
                                  (type "Application") ; One of "Application", "Link" or "Directory".
                                  (version "1.1")
                                  name
                                  (generic-name name)
                                  (no-display #f)
                                  comment
                                  icon
                                  (hidden #f)
                                  only-show-in
                                  not-show-in
                                  (d-bus-activatable #f)
                                  try-exec
                                  exec
                                  path
                                  (terminal #f)
                                  actions
                                  mime-type
                                  (categories "Application")
                                  implements
                                  keywords
                                  (startup-notify #t)
                                  startup-w-m-class
                                  #:rest all-args)
  "Create a desktop entry file at DESTINATION.
You must specify NAME.

Values can be booleans, numbers, strings or list of strings.

Additionally, locales can be specified with an alist where the key is the
locale.  The #f key specifies the default.  Example:

  #:name '((#f \"I love Guix\") (\"fr\" \"J'aime Guix\"))

produces

  Name=I love Guix
  Name[fr]=J'aime Guix

For a complete description of the format, see the specifications at
https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html."
  (define (escape-semicolon s)
    (string-join (string-split s #\;) "\\;"))
  (define* (parse key value #:optional locale)
    (set! value (match value
                  (#t "true")
                  (#f "false")
                  ((?  number? n) n)
                  ((?  string? s) (escape-semicolon s))
                  ((?  list? value)
                   (catch 'wrong-type-arg
                     (lambda () (string-join (map escape-semicolon value) ";"))
                     (lambda args (error "List arguments can only contain strings: ~a" args))))
                  (_ (error "Value must be a boolean, number, string or list of strings"))))
    (format #t "~a=~a~%"
            (if locale
                (format #f "~a[~a]" key locale)
                key)
            value))

  (define key-error-message "This procedure only takes key arguments beside DESTINATION")

  (unless name
    (error "Missing NAME key argument"))
  (unless (member #:type all-args)
    (set! all-args (append (list #:type type) all-args)))
  (mkdir-p (dirname destination))

  (with-output-to-file destination
    (lambda ()
      (format #t "[Desktop Entry]~%")
      (let loop ((args all-args))
        (match args
          (() #t)
          ((_) (error key-error-message))
          ((key value . ...)
           (unless (keyword? key)
             (error key-error-message))
           (set! key
                 (string-join (map string-titlecase
                                   (string-split (symbol->string
                                                  (keyword->symbol key))
                                                 #\-))
                              ""))
           (match value
             (((_ . _) . _)
              (for-each (lambda (locale-subvalue)
                          (parse key
                                 (if (and (list? (cdr locale-subvalue))
                                          (= 1 (length (cdr locale-subvalue))))
                                     ;; Support both proper and improper lists for convenience.
                                     (cadr locale-subvalue)
                                     (cdr locale-subvalue))
                                 (car locale-subvalue)))
                        value))
             (_
              (parse key value)))
           (loop (cddr args))))))))

(define* (make-wrapper wrapper real-file #:rest vars)
  "Like `wrap-program' but create WRAPPER around REAL-FILE.
The wrapper automatically changes directory to that of REAL-FILE.

Example:

  (make-wrapper \"bin/foo\" \"sub-dir/original-foo\"
                '(\"PATH\" \":\" = (\"/gnu/.../bar/bin\"))
                '(\"CERT_PATH\" suffix (\"/gnu/.../baz/certs\"
                                        \"/qux/certs\")))

will create 'bin/foo' with the following
contents:

  #!location/of/bin/bash
  export PATH=\"/gnu/.../bar/bin\"
  export CERT_PATH=\"$CERT_PATH${CERT_PATH:+:}/gnu/.../baz/certs:/qux/certs\"
  cd sub-dir
  exec -a $0 sub-dir/original-foo \"$@\"."
  (define (export-variable lst)
    ;; Return a string that exports an environment variable.
    (match lst
      ((var sep '= rest)
       (format #f "export ~a=\"~a\""
               var (string-join rest sep)))
      ((var sep 'prefix rest)
       (format #f "export ~a=\"~a${~a:+~a}$~a\""
               var (string-join rest sep) var sep var))
      ((var sep 'suffix rest)
       (format #f "export ~a=\"$~a${~a+~a}~a\""
               var var var sep (string-join rest sep)))
      ((var '= rest)
       (format #f "export ~a=\"~a\""
               var (string-join rest ":")))
      ((var 'prefix rest)
       (format #f "export ~a=\"~a${~a:+:}$~a\""
               var (string-join rest ":") var var))
      ((var 'suffix rest)
       (format #f "export ~a=\"$~a${~a:+:}~a\""
               var var var (string-join rest ":")))))

  (mkdir-p (dirname wrapper))
  (call-with-output-file wrapper
    (lambda (port)
      (format port
              "#!~a~%~a~%cd \"~a\"~%exec -a \"$0\" \"~a\" \"$@\"~%"
              (which "bash")
              (string-join (map export-variable vars) "\n")
              (dirname real-file)
              (canonicalize-path real-file))))
  (chmod wrapper #o755))

(define (concatenate-files files result)
  "Make RESULT the concatenation of all of FILES."
  (define (dump file port)
    (put-bytevector
     port
     (call-with-input-file file
       get-bytevector-all)))

  (call-with-output-file result
    (lambda (port)
      (for-each (cut dump <> port) files))))
