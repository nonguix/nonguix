;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021-2022 Petr Hodina <phodina@protonmail.com>

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
