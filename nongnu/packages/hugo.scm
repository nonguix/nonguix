;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Romain Garbage <romain.garbage@inria.fr>
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (nongnu packages hugo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages image)
  #:use-module (gnu packages web)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nonguix download))

(define-public hugo
  (package
    (name "hugo")
    (version "0.152.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/gohugoio/hugo")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "093p1k0m2n5b2bbk49kmciwr92wy9b8b4hw5wwmlhs2v304rw9cx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.24
      #:install-source? #f
      #:import-path "."
      #:build-flags
      #~(list "-tags" "extended withdeploy"
              (string-append
               "-ldflags="
               " -X github.com/gohugoio/hugo/common/hugo.vendorInfo=Nonguix"))
      #:test-flags ''("-skip=^TestCommands/mod|^TestCommands/server")
      #:test-subdirs ''(".")
      #:modules
      '(((guix build gnu-build-system) #:prefix gnu:)
        (guix build go-build-system)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda args
              ;; Enable Go modules support.
              (unsetenv "GO111MODULE")
              ;; Unpack source and vendored dependencies.
              (apply (assoc-ref gnu:%standard-phases 'unpack) args)
              (copy-recursively
               #+(this-package-native-input "vendored-go-dependencies")
               "vendor")))
          (replace 'install-license-files
            (assoc-ref gnu:%standard-phases 'install-license-files))
          (add-after 'unpack 'fix-paths
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (setenv
               "C_INCLUDE_PATH"
               (format #f "~a:~a"
                       (getenv "C_INCLUDE_PATH")
                       ((compose dirname dirname dirname)
                        (search-input-file
                         (or native-inputs inputs)
                         "src/dec/alphai_dec.h"))))
              (with-directory-excursion "vendor/github.com/bep/gowebp"
                (substitute* (find-files "internal/libwebp")
                  (("../../libwebp_src/(.*)\"" _ file)
                   (format #f "~a\""
                           (search-input-file
                            (or native-inputs inputs) file)))))
              (with-directory-excursion "vendor/github.com/bep/golibsass"
                (substitute* (find-files "internal/libsass")
                  (("../../libsass_src/(.*)\"" _ file)
                   (format #f "~a\""
                           (search-input-file
                            (or native-inputs inputs) file))))))))))
    (native-inputs
     (list (origin
             (method (go-mod-vendor #:go go-1.24))
             (uri (package-source this-package))
             (file-name "vendored-go-dependencies")
             (sha256
              (base32
               "1yhk8as1jz5459bzkmqjwdp82xqsr7sx1m1jkkk58cfzagznpz78")))
           (package-source libsass)
           (package-source libwebp)))
    (home-page "https://gohugo.io/")
    (synopsis "Static site generator written in Go")
    (description
     "Hugo is a static site generator written in Go, optimized for speed and
designed for flexibility.  With its advanced templating system and fast asset
pipelines, Hugo renders a complete site in seconds, often less.")
    (license license:asl2.0)))
