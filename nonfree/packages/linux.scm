;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
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

(define-module (nonfree packages linux)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public linux
  (package
    (inherit linux-libre)
    (name "linux")
    (version "5.0.15")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cdn.kernel.org/pub/linux/kernel/v5.x/"
                    "linux-" version ".tar.xz"))
              (sha256
               (base32
                "01zb8lz1lxcff2j8yxzm0ayfazi07c2n7v1i3v8wbq8k9r2vhgjw"))))
    (home-page "https://www.kernel.org/")
    (synopsis "Linux kernel with nonfree binary blobs included")
    (description
     "The unmodified Linux kernel, including nonfree blobs, for running GuixSD
on hardware which requires nonfree software to function.")))

(define-public linux-firmware
  (let ((commit "92e17d0dd2437140fab044ae62baf69b35d7d1fa")
        (revision "1"))
    (package
      (name "linux-firmware")
      (version (string-append "20190502-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url (string-append
                            "https://git.kernel.org/pub/scm/"
                            "linux/kernel/git/firmware/linux-firmware.git"))
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1bsgp124jhs9bbjjq0fzmdsziwx1y5aivkgpj8v56ar0y2zmrw2d"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder (begin
                     (use-modules (guix build utils))
                     (let ((source (assoc-ref %build-inputs "source"))
                           (fw-dir (string-append %output "/lib/firmware/")))
                       (mkdir-p fw-dir)
                       (copy-recursively source fw-dir)
                       #t))))
      (home-page
       "http://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git")
      (synopsis "Non-free firmware blobs for Linux")
      (description "Non-free firmware blobs for enabling support for various
hardware in the Linux kernel.")
      (license
       (license:non-copyleft
        (string-append "https://git.kernel.org/pub/scm/linux/kernel/git/"
                       "firmware/linux-firmware.git/plain/WHENCE"))))))
