;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
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

(define-module (nongnu packages wasm)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages llvm))

(define-public wasi-libc
  (let ((commit "ad5133410f66b93a2381db5b542aad5e0964db96")
        (revision "1"))
    (package
      (name "wasi-libc")
      (version (git-version "0.1-alpha" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/WebAssembly/wasi-libc")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "146jamq2q24vxjfpcwlqj84wzc80cbpbg0ns2wimyvzbanah48j6"))))
      (build-system gnu-build-system)
      (native-inputs (list clang-13))
      (arguments
       (list #:tests? #f ;No test suite
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (add-before 'build 'set-sysroot-include
                   (lambda _
                     (setenv "C_INCLUDE_PATH"
                             (string-append (getcwd) "/sysroot/include"))))
                 (add-before 'install 'set-install-dir
                   (lambda _
                     (setenv "INSTALL_DIR"
                             (string-append #$output "/wasm32-wasi")))))))
      (home-page "https://wasi.dev")
      (synopsis "WASI libc implementation for WebAssembly")
      (description
       "WASI Libc is a libc for WebAssembly programs built on top of WASI
system calls.  It provides a wide array of POSIX-compatible C APIs, including
support for standard I/O, file I/O, filesystem manipulation, memory
management, time, string, environment variables, program startup, and many
other APIs.")
      (license (list
                ;; For wasi-libc, with LLVM exceptions
                license:asl2.0
                ;; For malloc.c.
                license:cc0
                ;; For cloudlibc.
                license:bsd-2
                ;; For wasi-libc and musl-libc.
                license:expat)))))
