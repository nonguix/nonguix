;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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
