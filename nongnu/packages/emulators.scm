;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>

(define-module (nongnu packages emulators)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages sdl)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (nonguix licenses))

(define-public dgen
  (package
    (name "dgen")
    (version "1.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/dgen/dgen/"
             version "/dgen-sdl-" version ".tar.gz"))
       (sha256
        (base32
         "07pnvsw04jq9pdiwfhp0nr728rwqq2y6w648gz3p6a622xhc1qlr"))))
    (build-system gnu-build-system)
    (inputs
     `(("sdl" ,sdl)
       ("libarchive" ,libarchive)
       ("libgl" ,mesa)))
    (home-page "http://dgen.sourceforge.net")
    (synopsis "Emulator for Sega Genesis/Mega Drive systems")
    (description
     "DGen isn't the best Mega Drive/Genesis emulator out there, but it works
and it's probably the most portable.  It's one of the few that's x86_64
compatible.  It's also perfect for command line freaks.")
    (license (list license:bsd-3
                   ;; Many non-free licenses.
                   (nonfree "https://sourceforge.net/p/dgen/dgen/ci/master/tree/COPYING")))))
