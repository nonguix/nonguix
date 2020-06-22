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
