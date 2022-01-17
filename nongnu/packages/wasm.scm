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
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python))

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

(define-public wasm32-wasi-clang-runtime
  (package (inherit clang-runtime-13)
    (native-inputs
     (list clang-13
           wasi-libc))
    (inputs (list llvm-13))
    (arguments
     (list
      #:build-type "Release"
      #:tests? #f
      ;; Stripping binaries breaks wasm linking, resulting in the following
      ;; error: "archive has no index; run ranlib to add one".
      #:strip-binaries? #f
      #:configure-flags
      #~(list "-DCMAKE_C_COMPILER=clang"
              "-DCMAKE_C_COMPILER_TARGET=wasm32-wasi"
              (string-append
               "-DCMAKE_SYSROOT=" #$wasi-libc "/wasm32-wasi")
              (string-append
               "-DCMAKE_C_FLAGS=-I " #$wasi-libc "/wasm32-wasi/include")

              "-DCOMPILER_RT_OS_DIR=wasi"

              "-DCOMPILER_RT_BAREMETAL_BUILD=On"
              "-DCOMPILER_RT_DEFAULT_TARGET_ONLY=On"

              ;; WASM only needs libclang_rt.builtins-wasm32.a from
              ;; compiler-rt.
              (string-append "../compiler-rt-"
                             #$(package-version clang-runtime-13)
                             ".src/lib/builtins"))))))

;; FIXME: Ideally we wouldn't need to build a separate compiler because clang
;; can support multiple targets at runtime.  However Guix patches the default
;; clang with a specific clang-runtime package.  It would be good to improve
;; upstream Guix's support for cross-compiling with clang.

(define clang-from-llvm (@@ (gnu packages llvm) clang-from-llvm))

(define-public wasm32-wasi-clang
  (let ((base (clang-from-llvm llvm-13 wasm32-wasi-clang-runtime
                               "0zp1p6syii5iajm8v2c207s80arv00yz5ckfwimn5dng0sxiqqax")))
    (package (inherit base)
      (name "wasm32-wasi-clang")
      (inputs
       (modify-inputs (package-inputs base)
         (prepend wasi-libc)))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags)
          #~(list "-DCLANG_INCLUDE_TESTS=True"
                  ;; Use a sane default include directory.
                  (string-append "-DC_INCLUDE_DIRS="
                                 #$wasi-libc
                                 "/wasm32-wasi/include")))
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'symlink-cfi_ignorelist))))))))

(define-public wasm32-wasi-libcxx
  (package
    (name "wasm32-wasi-libcxx")
    (version "13.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/llvm/llvm-project")
             (commit (string-append "llvmorg-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cjl0vssi4y2g4nfr710fb6cdhxmn5r0vis15sf088zsc5zydfhw"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-S ../source/runtimes")

              "-DLLVM_ENABLE_RUNTIMES=libcxx;libcxxabi"

              (string-append
               "-DCMAKE_SYSROOT=" #$wasi-libc "/wasm32-wasi")

              (string-append
               "-DCMAKE_INCLUDE_PATH=" #$wasi-libc "/wasm32-wasi/include")

              (string-append
               "-DCMAKE_STAGING_PREFIX=" #$output "/wasm32-wasi")

              "-DCMAKE_C_COMPILER=clang"
              "-DCMAKE_C_COMPILER_WORKS=ON"
              "-DCMAKE_CXX_COMPILER=clang++"
              "-DCMAKE_CXX_COMPILER_WORKS=ON"
              "-DCMAKE_C_COMPILER_TARGET=wasm32-wasi"
              "-DCMAKE_CXX_COMPILER_TARGET=wasm32-wasi"

              "-DLIBCXX_LIBDIR_SUFFIX=/wasm32-wasi"

              "-DLIBCXX_ENABLE_EXCEPTIONS=OFF"
              "-DLIBCXX_ENABLE_SHARED=OFF"
              "-DLIBCXX_ENABLE_THREADS=OFF"
              "-DLIBCXX_ENABLE_FILESYSTEM=OFF"

              "-DLIBCXXABI_LIBDIR_SUFFIX=/wasm32-wasi"

              "-DLIBCXXABI_ENABLE_EXCEPTIONS=OFF"
              "-DLIBCXXABI_ENABLE_SHARED=OFF"
              "-DLIBCXXABI_ENABLE_THREADS=OFF"
              "-DLIBCXXABI_ENABLE_FILESYSTEM=OFF")
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
            (lambda _
              (setenv "CPLUS_INCLUDE_PATH"
                      (string-append #$wasi-libc "/wasm32-wasi/include:"
                                     (getenv "CPLUS_INCLUDE_PATH"))))))))
    (native-inputs
     (list lld
           python
           wasm32-wasi-clang))
    (inputs
     (list wasi-libc))
    (home-page "https://libcxx.llvm.org")
    (synopsis "C++ standard library for WebAssembly")
    (description
     "This package provides an implementation of the C++ standard library for
use with Clang, targeting C++11, C++14 and above.  This package targets
WebAssembly with WASI.")
    (license license:expat)))

(define-public wasm32-wasi-clang-toolchain
  (package
    (name "wasm32-wasi-clang-toolchain")
    (version (package-version wasm32-wasi-clang))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      (with-imported-modules '((guix build union)
                               (guix build utils))
        #~(begin
            (use-modules (guix build union)
                         (guix build utils))
            (union-build #$output
                         (list #$wasm32-wasi-clang-runtime
                               #$wasi-libc
                               #$wasm32-wasi-libcxx))
            (mkdir-p (string-append #$output "/bin"))

            ;; We provide clang and clang++ via a wrapped program that sets
            ;; include paths correctly so that it does not include paths from
            ;; the host.

            ;; FIXME: Review how we can provide better support for
            ;; cross-compiling with clang in Guix, maybe adding support for
            ;; the CROSS_C_INCLUDE_PATH and CROSS_CPLUS_INCLUDE_PATH
            ;; environment variables like GCC.

            (for-each
             (lambda (bin)
               (symlink (string-append #$wasm32-wasi-clang bin)
                        (string-append #$output bin))
               (wrap-program (string-append #$output bin)
                 #:sh (string-append #$bash-minimal "/bin/bash")
                 `("C_INCLUDE_PATH" ":" =
                   (,(string-append #$output "/wasm32-wasi/include")))
                 `("CPLUS_INCLUDE_PATH" ":" =
                   ;; Make sure inclure/c++/v1 comes first for #include_next
                   ;; to work.
                   (,(string-append #$output "/wasm32-wasi/include/c++/v1")
                    ,(string-append #$output "/wasm32-wasi/include")))))
             '("/bin/clang" "/bin/clang++"))

            (symlink (string-append #$lld "/bin/wasm-ld")
                     (string-append #$output "/bin/wasm-ld"))))))
    (inputs
     (list bash-minimal
           lld
           wasi-libc
           wasm32-wasi-clang
           wasm32-wasi-clang-runtime
           wasm32-wasi-libcxx))
    (license (cons
              (package-license wasm32-wasi-clang)
              (package-license wasi-libc)))
    (home-page "https://clang.llvm.org")
    (synopsis "Complete Clang toolchain for C/C++ development, for WebAssembly.")
    (description "This package provides a complete Clang toolchain for C/C++
development targeting WebAssembly with WASI.  This includes Clang, as well as
libc, libc++ and wasm-ld.")))
