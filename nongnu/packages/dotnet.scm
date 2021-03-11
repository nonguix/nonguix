;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Jelle Licht <jlicht@fsfe.org>
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

(define-module (nongnu packages dotnet)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tls))

(define-public omnisharp
  (package
    (name "omnisharp")
    (version "1.37.7")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri
        (string-append "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v"
                       version "/omnisharp-linux-x64.tar.gz"))
       (sha256
        (base32
         "0x1ynqrfipbqwf182fx27f9xw4khg65nsvpy9vx35jarfshyrqac"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan
       `(("bin/mono"
          ("gcc:lib" "zlib"))
         ("lib/libmono-native.so"
          ("mit-krb5")))
       #:install-plan
       `(("run" "bin/omnisharp-wrapper")
         ("bin" "share/omnisharp/")
         ("etc" "share/omnisharp/")
         ("lib" "share/omnisharp/")
         ("omnisharp" "share/omnisharp/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-wrapper
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "run"
               (("base_dir=.*")
                (string-append "base_dir="
                               (assoc-ref outputs "out") "/share/omnisharp\n"))
               (("chmod.*") ""))))
         (add-before 'patchelf 'patchelf-writable
           (lambda _
             (for-each make-file-writable
                       '("bin/mono" "lib/libmono-native.so")))))))
    (inputs
     `(("gcc:lib" ,gcc "lib")
       ("mit-krb5" ,mit-krb5)
       ("zlib" ,zlib)))
    (home-page "https://github.com/OmniSharp/omnisharp-roslyn")
    (supported-systems '("x86_64-linux"))
    (synopsis "Implementation of Language Server Protocol based on Roslyn workspaces")
    (description "OmniSharp is a @code{.NET} development platform based on
Roslyn workspaces.  It provides project dependencies and C# language services to
various IDEs and plugins.")
    (license license:expat)))
