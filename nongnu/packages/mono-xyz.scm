;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (nongnu packages mono-xyz)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nongnu packages mono)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define* (nuget-fetch #:key name version url sha256)
  (list (string-append name "-" version)
        (origin
          (method url-fetch)
          (uri url)
          (file-name (string-append name "-" version))
          (sha256
           (base32 sha256)))))

(define protobuf-net-inputs
  `(,(nuget-fetch
      #:name "microsoft.build.traversal"
      #:version "2.0.19"
      #:url "https://www.nuget.org/api/v2/package/Microsoft.Build.Traversal/2.0.19"
      #:sha256 "15nz923bqr2xw9g8nxsghdzyj31j7p9m5w9fww46bb7hxyvjqffx")))

(define-public protobuf-net
  (let ((commit "706684b8b56dc54f9a03b1d44a4f85a750911243"))
    (package
      (name "protobuf-net")
      (version (git-version "2.4.5" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/protobuf-net/protobuf-net")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           ;; "1i4d4h7yxiga9r7j8j2vmjz8zg2lz8w9caza6knykr5izq56z949"
           "13mg798hgjcj7rbaa99y9wy6mg8s8r2dvmd86ydg7igkahw1nag7"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("nuget" ,nuget)))
      (inputs
       `(("msbuild" ,msbuild)
         ,@protobuf-net-inputs))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((msbuild (string-append (assoc-ref inputs "msbuild") "/bin/msbuild"))
                     (nuget (string-append (assoc-ref inputs "nuget") "/bin/nuget")))
                 ;; Set up nuget packages.
                 (setenv "HOME" (string-append (getcwd) "/fake-home"))
                 (for-each
                  (lambda (mono-dep)
                    (invoke nuget "add" (assoc-ref inputs mono-dep) "-Source" "guix"))
                  ',(map car protobuf-net-inputs))
                 (invoke nuget "sources" "Disable" "-Name" "nuget.org")
                 (invoke nuget "sources" "Add" "-Name" "guix" "-Source" (string-append (getcwd) "/guix"))
                 ;; Build
                 (invoke msbuild "Build.csproj")))))))
      (supported-systems '("x86_64-linux"))
      (home-page "https://github.com/protobuf-net/protobuf-net")
      (synopsis "")
      (description "This package provides a generic driver for the .NET Core
command line interface.")
      (license (list license:bsd-2 license:asl2.0)))))
