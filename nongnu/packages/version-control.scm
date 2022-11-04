;;; Copyright © 2022 dan <i@dan.games>
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
    (version "r22.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.perforce.com/perforce/" version
                                  "/bin.linux26"
                                  (match (%current-system)
                                    ("i686-linux" "x86")
                                    ("x86_64-linux" "x86_64"))
                                  "/helix-core-server.tgz"))
              (sha256
               (base32
                (match (%current-system)
                  ("i686-linux"
                   "0f5qs55rspw86axnmml3nxx551lwbxwz1cgi9kmy2f9g5rrplnkn")
                  ("x86_64-linux"
                   "077rfbjgyhdgv76i2727s3yk3p52y75nml8n9wv8g7mvhfs9ypa9"))))))
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
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license (nonfree
              "https://www.perforce.com/sites/default/files/pdfs/Helix_Core%20On%20Prem%20Software%20License%20Agmt%20ClickThru_FINAL%2006.28.2021.pdf"))))
