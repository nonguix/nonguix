;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 dan <i@dan.games>
;;; Copyright © 2022, 2025 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nongnu packages version-control)
  #:use-module (gnu packages base)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix licenses)
  #:use-module (ice-9 match))

(define-public helix-core
  (package
    (name "helix-core")
    (version "r24.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.perforce.com/perforce/" version
                                  "/bin.linux26x86_64/helix-core-server.tgz"))
              (sha256
               (base32
                 "0iwh7cz2hig9hlwqs58bvvmyv4a9kr6q2yv23dyvi38nylrkl4nl"))))
    (build-system binary-build-system)
    (arguments
     `(#:strip-binaries? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'patchelf 'patchelf-writable
                    (lambda _
                      (for-each make-file-writable
                                (find-files ".")))))
       #:patchelf-plan '(("p4" ("glibc"))
                         ("p4d" ("glibc"))
                         ("p4p" ("glibc"))
                         ("p4broker" ("glibc")))
       #:install-plan '(("p4" "bin/")
                        ("p4d" "bin/")
                        ("p4p" "bin/")
                        ("p4broker" "bin/"))))
    (inputs (list glibc))
    (home-page "https://www.perforce.com/products/helix-core")
    (synopsis
     "A version control software for large scale development environments")
    (description
     "Helix Core is a version control software for large scale development
environments. The Helix Version Control System manages a central database and
a master repository of file versions.")
    (supported-systems '("x86_64-linux"))
    (license (nonfree
              "https://www.perforce.com/sites/default/files/pdfs/Helix_Core%20On%20Prem%20Software%20License%20Agmt%20ClickThru_FINAL%2006.28.2021.pdf"))))
