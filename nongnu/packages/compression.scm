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
    (version "6.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.rarlab.com/rar/unrarsrc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1mbw20lh300r541dz4m84rvq7b542mnb70yc29afrjj6waknqza7"))))
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
