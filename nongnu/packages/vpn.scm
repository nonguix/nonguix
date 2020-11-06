;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Alexey Abramov <levenson@mmer.org>
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix. If not, see <http://www.gnu.org/licenses/>.


(define-module (nongnu packages vpn)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((nonguix licenses) #:prefix license:))

(define-public zerotier
  (package
    (name "zerotier")
    (version "1.4.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zerotier/ZeroTierOne")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f8hh05wx59dc0fbzdzwq05x0gmrdfl4v103wbcyjmzsbazaw6p3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There is no ./configure
         (delete 'configure)
         (replace 'check
           (lambda _
             (invoke "make" "selftest")
             (invoke "./zerotier-selftest")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin"))
                    (lib (string-append out "/lib"))
                    (man (string-append out "/share/man"))
                    (zerotier-one-lib (string-append lib "/zerotier-one")))
               (mkdir-p sbin)
               (install-file "zerotier-one" sbin)
               (with-directory-excursion sbin
                 (symlink (string-append sbin "/zerotier-one") "zerotier-cli")
                 (symlink (string-append sbin "/zerotier-one") "zerotier-idtool"))

               (mkdir-p zerotier-one-lib)
               (with-directory-excursion zerotier-one-lib
                 (symlink (string-append sbin "/zerotier-one") "zerotier-one")
                 (symlink (string-append sbin "/zerotier-one") "zerotier-cli")
                 (symlink (string-append sbin "/zerotier-one") "zerotier-idtool"))

               (mkdir-p (string-append man "/man8"))
               (install-file "doc/zerotier-one.8" (string-append man "/man8"))

               (mkdir-p (string-append man "/man1"))
               (for-each (lambda (man-page)
                           (install-file man-page (string-append man "/man1")))
                         (list "doc/zerotier-cli.1"
                               "doc/zerotier-idtool.1"))
               #t))))))
    (home-page "https://github.com/zerotier/ZeroTierOne")
    (synopsis "Smart programmable Ethernet switch for planet Earth")
    (description "It allows all networked devices, virtual machines,
containers, and applications to communicate as if they all reside in the same
physical data center or cloud region.

This is accomplished by combining a cryptographically addressed and secure
peer to peer network (termed VL1) with an Ethernet emulation layer somewhat
similar to VXLAN (termed VL2).  Our VL2 Ethernet virtualization layer includes
advanced enterprise SDN features like fine grained access control rules for
network micro-segmentation and security monitoring.")
    (license (license:nonfree "https://mariadb.com/bsl11/"))))
