;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (nongnu packages mono)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages mono)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

;; TODO: This can probably be upstreamed since only the check phase doesn't
;; pass (even if most of the tests succeed).
(define-public mono-6
  (package
    (name "mono")
    (version "6.8.0.105")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.mono-project.com/sources/mono/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0y11c7w6r96laqckfxnk1ya42hx2c1nfqvdgbpmsk1iw9k29k1sp"))
              (patches
               (parameterize
                   ((%patch-path
                     (map (lambda (directory)
                            (string-append directory "/nongnu/packages/patches"))
                          %load-path)))
                 (search-patches "mono-pkgconfig-before-gac.patch"
                                 "mono-mdoc-timestamping.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("libxslt" ,libxslt)
       ("perl" ,perl)
       ("python" ,python-2)
       ("cmake" ,cmake)
       ("which" ,which)
       ("libgdiplus" ,libgdiplus)
       ("libx11" ,libx11)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "mono/mini/Makefile.in"
               (("build_date = [^;]*;")
                "build_date = (void*) 0;"))
             #t))
         ;; TODO: Update Mono certs.  We need a certificate bundle, which nss-certs does not have.
         ;; (add-after 'install 'update-mono-key-store
         ;;   (lambda* (#:key outputs inputs #:allow-other-keys)
         ;;     (let* ((out (assoc-ref outputs "out"))
         ;;            (ca (assoc-ref inputs "nss-certs"))
         ;;            (cert-sync (string-append out "/bin/cert-sync"))))
         ;;     (invoke cert-sync (string-append ca "/etc/ssl/certs/ca-bundle.crt")
         (add-after 'install 'install-gmcs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (symlink (string-append out "/bin/mcs")
                        (string-append out "/bin/gmcs")))
             #t))
         (add-after 'unpack 'set-env
           (lambda _ ;;* (#:key inputs #:allow-other-keys)
             ;; all tests under mcs/class fail trying to access $HOME
             (setenv "HOME" "/tmp")
             ;; ZIP files have "DOS time" which starts in Jan 1980.
             (setenv "SOURCE_DATE_EPOCH" "315532800")
             #t)))
       #:configure-flags (list
                          (string-append "--x-includes="
                                         (assoc-ref %build-inputs "libx11")
                                         "/include")
                          (string-append "--x-libraries="
                                         (assoc-ref %build-inputs "libx11")
                                         "/lib")
                          (string-append "--with-libgdiplus="
                                         (assoc-ref %build-inputs "libgdiplus")
                                         "/lib/libgdiplus.so"))
       ;; TODO: Most tests pass but something fails.  See bug#39695 and
       ;; https://github.com/mono/mono/issues/18979.
       #:tests? #f))
    (synopsis "Compiler and libraries for the C# programming language")
    (description "Mono is a compiler, vm, debugger and set of libraries for
C#, a C-style programming language from Microsoft that is very similar to
Java.")
    (home-page "https://www.mono-project.com/")
    ;; TODO: Still x11?
    (license license:x11)))
