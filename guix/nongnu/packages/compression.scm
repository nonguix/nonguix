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

(define-module (nongnu packages compression)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (nonguix licenses))

(define-public unrar
  (package
    (name "unrar")
    (version "6.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.rarlab.com/rar/unrarsrc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1lzdsfb4d00silyk04lkvaklmxaiyqksyxx4h1krg77q6f1iigw1"))))
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
