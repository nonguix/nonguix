;;; Copyright © 2022 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (nongnu packages radio)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix licenses))

(define-public sdrplay
  (package
    (name "sdrplay")
    (version "3.07.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.sdrplay.com/software/"
                           "SDRplay_RSP_API-Linux-" version ".run"))
       (sha256
        (base32 "1a25c7rsdkcjxr7ffvx2lwj7fxdbslg9qhr8ghaq1r53rcrqgzmf"))))
    (build-system binary-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs
     `(("eudev" ,eudev)
       ("gcc:lib" ,gcc "lib")))
    (arguments
     `(#:patchelf-plan
       (let ((arch ,(match (or (%current-target-system) (%current-system))
                      ("i686-linux" "i686")
                      ("x86_64-linux" "x86_64")
                      (_ "UNSUPPORTED")))
             (major-minor ,(version-major+minor version)))
         `((,(string-append arch "/libsdrplay_api.so." major-minor)
            ("gcc:lib"))
           (,(string-append arch "/sdrplay_apiService")
            ("eudev" "gcc:lib"))))
       #:install-plan
       (let ((arch ,(match (or (%current-target-system) (%current-system))
                      ("i686-linux" "i686")
                      ("x86_64-linux" "x86_64")
                      (_ "UNSUPPORTED")))
             (major-minor ,(version-major+minor version)))
         `(("66-mirics.rules" "lib/udev/rules.d/66-mirics.rules")
           ("inc" "include")
           (,(string-append arch "/libsdrplay_api.so." major-minor)
            ,(string-append "lib/libsdrplay_api.so." major-minor))
           (,(string-append arch "/sdrplay_apiService")
            "bin/sdrplay_apiService")
           ("sdrplay_license.txt"
            ,(string-append "share/doc/sdrplay-" ,version  "/license.txt"))))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((source-script (assoc-ref inputs "source")))
               (invoke "sh" source-script "--noexec" "--target" "source")
               (chdir "source"))))
         (add-after 'install 'create-library-links
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((major ,(version-major version))
                    (major-minor ,(version-major+minor version))
                    (lib (string-append (assoc-ref outputs "out")
                                        "/lib/libsdrplay_api.so"))
                    (lib-major (string-append lib "." major))
                    (lib-major-minor (string-append lib "." major-minor)))
               (symlink lib-major-minor lib-major)
               (symlink lib-major lib)))))))
    (home-page "https://www.sdrplay.com")
    (synopsis "API for SDRplay RSP devices")
    (description
     "This package provides the library and API service for using SDRplay RSP
devices.

To install the udev rules, you must extend @code{udev-service-type} with this
package.  E.g.: @code{(udev-rules-service 'sdrplay sdrplay)}")
    (license (nonfree (string-append "file:///share/doc/sdrplay-"
                                     version
                                     "/license.txt")))))
