;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>

(define-module (nongnu packages compression)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (nonguix licenses))

(define-public unrar
  (package
    (name "unrar")
    (version "7.0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.rarlab.com/rar/unrarsrc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "09l336li4q7yrpjq22q6da2vrynpqbyb4a9fdxa02k65wkwi6p2h"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; No tests.
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://www.rarlab.com/rar_add.htm")
    (synopsis "Extract files from RAR archives")
    (description "The RAR decompression program.  It is nonfree (as in
freedom), but open-source.")
    (license (nonfree "https://www.win-rar.com/gtb_priv.html?&L=0"))))
