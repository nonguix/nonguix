;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Romain Garbage <romain.garbage@inria.fr>

(define-module (nongnu packages hugo)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public hugo
  (package
    (name "hugo")
    (version "0.140.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/gohugoio/hugo/releases/download/v" version
             "/" name "_" version "_linux-" (cond ((target-aarch64?)
                                                   "arm64")
                                                  ((target-arm32?)
                                                   "arm")
                                                  ((target-x86-64?)
                                                   "amd64")) ".tar.gz"))
       (sha256
        (base32 (cond ((target-aarch64?)
                       "1dv2k9j3i3294bl94jhwi645pf5r2143hizxd3xpc3fz8w8cfyy8")
                      ((target-arm32?)
                       "0f3mirqn3x2lrj7gzjyqklj081y7jfyxww2zkccg9f6jq0vcfcxd")
                      ((target-x86-64?)
                       "0hs4b3nrr1qajrh7f64ibwjrfipqllvifp526kf2gfxnhpkr67l8"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan ''(("hugo" "/bin/hugo"))))
    (supported-systems (list "aarch64-linux"
                             "armhf-linux"
                             "x86_64-linux"))
    (home-page "https://gohugo.io/")
    (synopsis "Static site generator written in Go")
    (description
     "Hugo is a static site generator written in Go, optimized for speed and
designed for flexibility.  With its advanced templating system and fast asset
pipelines, Hugo renders a complete site in seconds, often less.")
    (license license:asl2.0)))
