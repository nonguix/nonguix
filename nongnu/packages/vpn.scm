;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Alexey Abramov <levenson@mmer.org>

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
    (version "1.8.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zerotier/ZeroTierOne")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "101b1k9f3cpbgj0l87ya1cbqs9dv0qiayjap4m29fxyjra8hbkb8"))))
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
