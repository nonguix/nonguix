;;; Copyright © 2021-2022 Petr Hodina <phodina@protonmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (nongnu packages benchmark)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) #:prefix license:))

(define-public geekbench5
  (package
    (name "geekbench5")
    (version "5.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cdn.geekbench.com/Geekbench-"
                                  version "-Linux.tar.gz"))
              (sha256
               (base32
                "0qppx5ivclfwldb4fcmzg3v9a9nzi7d4f44vx634mfzw2symn3r4"))))
    (build-system binary-build-system)
    (arguments
     (list #:strip-binaries? #f ;TODO: For some reason it fails validate-runpath
           #:install-plan #~'(("geekbench5" "bin/")
                              ("geekbench.plar" "bin/")
                              ("geekbench_x86_64" "bin/"))
           #:patchelf-plan #~(list (list "geekbench5"
                                         '("glibc" "gcc:lib"))
                                   (list "geekbench_x86_64"
                                         '("glibc" "gcc:lib")))))
    (supported-systems '("x86_64-linux"))
    (inputs `(("gcc:lib" ,gcc "lib")
              ("glibc" ,glibc)))
    (synopsis "Benchmark that measures processor and memory performance")
    (description
     "This package provides benchmark that measures processor and memory
performance and uploads the results into online database.")
    (home-page "https://www.geekbench.com/")
    (license (license:nonfree
              "https://www.primatelabs.com/legal/terms-of-use.html"))))
